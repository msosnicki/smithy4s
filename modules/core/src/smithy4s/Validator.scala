/*
 *  Copyright 2021-2026 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s

sealed trait Validator[A, B] {
  def validate(value: A): Either[String, B]

  def toSchema(a: Schema[A]): Schema[B]

  def alsoValidating[C](constraint: C)(implicit
      ev: RefinementProvider.Simple[C, A]
  ): Validator[A, B]

  // def alsoValidatingElement[C, E](constraint: C)(implicit ev: A =:= List[E], rev: RefinementProvider.Simple[C, E]): Validator[A, B]
}

object Validator {

  def of[A, B](bijection: Bijection[A, B]): ValidatorBuilder[A, B] =
    new ValidatorBuilder[A, B](bijection)

  def ofBijection[A, B](bijection: Bijection[A, B]): Validator[A, B] = new BijectedValidator(
    new DirectValidator[A](Nil),
    Bijection.identity,
    bijection
  )

  final class ValidatorBuilder[A, B] private[smithy4s] (
      bijection: Bijection[A, B]
  ) {
    def validating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A]
    ): Validator[A, B] =
      new BijectedValidator(
        new DirectValidator[A](List(ev.make(constraint))),
        Bijection.identity,
        bijection
      )

    def validatingElement[C, Elem](constrait: C)(implicit
        ev: A =:= List[Elem],
        refEv: RefinementProvider.Simple[C, Elem]
    ): Validator[A, B] = {
      val evBijection: Bijection[List[Elem], A] = bijectionFromEv(ev.flip)
      new BijectedValidator(
        new ListValidator(
          mainValidator = None,
          elementRefinements = List(refEv.make(constrait))
        ),
        evBijection,
        evBijection.imapTarget(bijection)
      )
    }
  }

  private def bijectionFromEv[A, B](ev: A =:= B): Bijection[A, B] =
    Bijection(ev.apply, ev.flip.apply)

  private class BijectedValidator[A, B, A0, B0](
      source: Validator[A, B],
      bijectSource: Bijection[A, A0],
      bijectTarget: Bijection[B, B0]
  ) extends Validator[A0, B0] {

    override def validate(value: A0): Either[String, B0] =
      source.validate(bijectSource.from(value)).map(bijectTarget.to)

    override def toSchema(a: Schema[A0]): Schema[B0] =
      // todo: compose them here before calling biject so there is just one wrapper
      source.toSchema(a.biject(bijectSource.swap)).biject(bijectTarget)

    override def alsoValidating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A0]
    ): Validator[A0, B0] = {
      implicit val ev0 = ev.imapFull(bijectSource.swap, bijectSource.swap)
      new BijectedValidator(
        source.alsoValidating(constraint),
        bijectSource,
        bijectTarget
      )
    }
  }

  private class ListValidator[Elem](
      mainValidator: Option[Validator[List[Elem], List[Elem]]],
      elementRefinements: List[Refinement.Aux[_, Elem, Elem]]
  ) extends Validator[List[Elem], List[Elem]] {

    override def validate(value: List[Elem]): Either[String, List[Elem]] = {
      def right[A](value: A): Either[String, A] = Right(value)
      def validateList(
          ref: Refinement.Aux[_, Elem, Elem]
      ): Either[String, Unit] =
        //todo: rewrite so it does not iterate the whole list
        value.foldLeft(right(())) { case (acc, elem) =>
          acc.flatMap(_ => ref(elem)).map(_ => ())
        }
      val elementsValidated = elementRefinements.foldLeft(right(value)) {
        case (valueOrError, ref) =>
          valueOrError.flatMap(_ => validateList(ref)).map(_ => value)
      }
      mainValidator
        .map(_.validate(value))
        .getOrElse(right((Nil)))
        .flatMap(_ => elementsValidated.map(_ => value))
    }

    override def toSchema(a: Schema[List[Elem]]): Schema[List[Elem]] = {
      val main = mainValidator.map(_.toSchema(a)).getOrElse(a)
      elementRefinements.foldLeft(main) {
        // TODO: attach refinement to member
        case (acc, _) => acc
      }
    }

    override def alsoValidating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, List[Elem]]
    ): Validator[List[Elem], List[Elem]] =
      new ListValidator(
        mainValidator = mainValidator
          .map(_.alsoValidating(constraint))
          .orElse(Some(new DirectValidator(List(ev.make(constraint))))),
        elementRefinements = elementRefinements
      )

  }

  private class DirectValidator[A](
      refinements: List[Refinement.Aux[_, A, A]]
  ) extends Validator[A, A] {

    override def validate(value: A): Either[String, A] = {
      refinements
        .foldLeft(Right(value): Either[String, A]) {
          case (valueOrError, refinement) =>
            valueOrError.flatMap(refinement.apply)
        }
    }

    override def alsoValidating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A]
    ): Validator[A, A] =
      new DirectValidator[A](refinements :+ ev.make(constraint))

    override def toSchema(a: Schema[A]): Schema[A] = {
      refinements
        .foldLeft(a) { (schema, refinement) =>
          schema.refined[A](refinement)
        }
    }
  }
}
