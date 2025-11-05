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
}

object Validator {

  def of[A, B](bijection: Bijection[A, B]): ValidatorBuilder[A, B] =
    new ValidatorBuilder[A, B](bijection)

  final class ValidatorBuilder[A, B] private[smithy4s] (
      bijection: Bijection[A, B]
  ) {
    def validating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A]
    ): Validator[A, B] =
      new ValidatorImpl[A, B](List(ev.make(constraint)), bijection)

    def validatingElement[C, Elem](constrait: C)(implicit
        ev: A =:= List[Elem],
        refEv: RefinementProvider.Simple[C, Elem]
    ): Validator[A, B] = {
      val evBijection: Bijection[A, List[Elem]] = bijectionFromEv(ev)
      new Bijected(
        new ListValidator(
          mainValidator = None,
          elementRefinements = List(refEv.make(constrait)),
          bijection = evBijection.swap.imapTarget(bijection)
        ),
        evBijection.swap
      )
    }
  }

  // private def list[Elem, B](
  //     mainValidator: Option[Validator[List[Elem], B]],
  //     elementRefinements: List[Refinement.Aux[_, Elem, Elem]],
  //     bijection: Bijection[List[Elem], B]
  // ): Validator[List[Elem], B] =
  //   new ListValidator(mainValidator, elementRefinements, bijection)

  // private def mapped[A, B, A0](source: Validator[A, B])(
  //     contramap: A => A0
  // ): Validator[A0, B] =
  //   new Mapped(source, contramap)

  private def bijectionFromEv[A, B](ev: A =:= B): Bijection[A, B] =
    Bijection(ev.apply, ev.flip.apply)

  private class Bijected[A, B, A0](
      source: Validator[A, B],
      biject: Bijection[A, A0]
  ) extends Validator[A0, B] {

    override def validate(value: A0): Either[String, B] = 
      source.validate(biject.from(value))

    override def toSchema(a: Schema[A0]): Schema[B] = 
      source.toSchema(a.biject(biject.swap))

    override def alsoValidating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A0] 
    ): Validator[A0, B] = {
      implicit val ev0 = ev.imapFull(biject.swap, biject.swap)
      new Bijected(source.alsoValidating(constraint), biject)
    }
  }

  private class ListValidator[Elem, B](
      mainValidator: Option[Validator[List[Elem], B]],
      elementRefinements: List[Refinement.Aux[_, Elem, Elem]],
      bijection: Bijection[List[Elem], B]
  ) extends Validator[List[Elem], B] {

    override def validate(value: List[Elem]): Either[String, B] = {
      def right[A](value: A): Either[String, A] = Right(value)
      def validateList(
          ref: Refinement.Aux[_, Elem, Elem]
      ): Either[String, Unit] =
        value.foldLeft(right(())) { case (acc, elem) =>
          acc.flatMap(_ => ref(elem)).map(_ => ())
        }
      val elementsValidated = elementRefinements.foldLeft(right(value)) {
        case (valueOrError, ref) =>
          valueOrError.flatMap(_ => validateList(ref)).map(_ => value)
      }
      mainValidator
        .map(_.validate(value))
        .getOrElse(right(bijection.apply(Nil)))
        .flatMap(_ => elementsValidated.map(_ => bijection.apply(value)))
    }

    override def toSchema(a: Schema[List[Elem]]): Schema[B] = {
      val main = mainValidator.map(_.toSchema(a)).getOrElse(a.biject(bijection))
      elementRefinements.foldLeft(main) {
        // TODO: attach refinement to member
        case (acc, _) => acc
      }
    }

    override def alsoValidating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, List[Elem]]
    ): Validator[List[Elem], B] =
      new ListValidator(
        mainValidator = mainValidator
          .map(_.alsoValidating(constraint))
          .orElse(
            Some(Validator.of(bijection).validating(constraint))
          ),
        elementRefinements = elementRefinements,
        bijection
      )

  }

  private class ValidatorImpl[A, B](
      refinements: List[Refinement.Aux[_, A, A]],
      bijection: Bijection[A, B]
  ) extends Validator[A, B] {

    override def validate(value: A): Either[String, B] = {
      refinements
        .foldLeft(Right(value): Either[String, A]) {
          case (valueOrError, refinement) =>
            valueOrError.flatMap(refinement.apply)
        }
        .map(bijection.apply)
    }

    override def alsoValidating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A]
    ): Validator[A, B] =
      new ValidatorImpl[A, B](refinements :+ ev.make(constraint), bijection)

    override def toSchema(a: Schema[A]): Schema[B] = {
      refinements
        .foldLeft(a) { (schema, refinement) =>
          schema.refined[A](refinement)
        }
        .biject(bijection)
    }
  }
}
