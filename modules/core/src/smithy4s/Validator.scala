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

sealed trait Validator[A, B] { self =>
  def validate(value: A): Either[String, B]

  def toSchema(a: Schema[A]): Schema[B]

  def validating[C](constraint: C)(implicit
      ev: RefinementProvider.Simple[C, A]
  ): Validator[A, B]

  def validateRefined[B0, C](constraint: C)(implicit ev: RefinementProvider[C, A, B0]): Validator[A, B0]

  // todo: deprecated
  def alsoValidating[C](constraint: C)(implicit
      ev: RefinementProvider.Simple[C, A]
  ): Validator[A, B] = validating(constraint)

  def biject[B0](implicit bijection: Bijection[B, B0]): Validator[A, B0] = new Validator.BijectedValidator(self, bijection)

}

object Validator {

  sealed trait ForList[E] extends Validator[List[E], List[E]] {
    def validatingElement[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, E]
    ): Validator[List[E], List[E]]
  }

  // todo: add deprecation
  def of[A, B](bijection: Bijection[A, B]): ValidatorBuilder[A, B] =
    new ValidatorBuilder[A, B](bijection)

  def simple[A]: Validator[A, A] = new DirectValidator[A](Vector.empty)

  def list[E]: Validator.ForList[E] = new ListValidator(None, Vector.empty)

  // todo: deprecated
  final class ValidatorBuilder[A, B] private[smithy4s] (
      bijection: Bijection[A, B]
  ) {
    def validating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A]
    ): Validator[A, B] =
      new BijectedValidator(
        new DirectValidator[A](Vector(ev.make(constraint))),
        bijection
      )
  }

  private class BijectedValidator[A, B, B0](
      source: Validator[A, B],
      bijectTarget: Bijection[B, B0]
  ) extends Validator[A, B0] {

    override def validateRefined[B1, C](constraint: C)(implicit ev: RefinementProvider[C,A,B1]): Validator[A,B1] = 
      ???

    override def biject[B1](implicit
        bijection: Bijection[B0, B1]
    ): Validator[A, B1] =
      new BijectedValidator(
        source,
        bijectTarget.imapTarget(bijection)
      )

    override def validate(value: A): Either[String, B0] =
      source.validate(value).map(bijectTarget.to)

    override def toSchema(a: Schema[A]): Schema[B0] =
      source.toSchema(a).biject(bijectTarget)

    override def validating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, A]
    ): Validator[A, B0] =
      new BijectedValidator(
        source.validating(constraint),
        bijectTarget
      )
  }

  private class ListValidator[Elem](
      mainValidator: Option[Validator[List[Elem], List[Elem]]],
      elementRefinements: Vector[Refinement.Aux[_, Elem, Elem]]
  ) extends Validator.ForList[Elem] {

    override def validating[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, List[Elem]]
    ): Validator[List[Elem], List[Elem]] =
      new ListValidator(
        mainValidator = mainValidator
          .map(_.validating(constraint))
          .orElse(Some(new DirectValidator(Vector(ev.make(constraint))))),
        elementRefinements = elementRefinements
      )

    override def validateRefined[B0, C](constraint: C)(implicit ev: RefinementProvider[C,List[Elem],B0]): Validator[List[Elem],B0] = 
      new RefinedValidator(this, ev.make(constraint))

    override def validatingElement[C](constraint: C)(implicit
        ev: RefinementProvider.Simple[C, Elem]
    ): Validator[List[Elem], List[Elem]] =
      new ListValidator(
        mainValidator,
        elementRefinements :+ ev.make(constraint)
      )

    override def validate(value: List[Elem]): Either[String, List[Elem]] = {
      def right[A](value: A): Either[String, A] = Right(value)
      def validateList(
          ref: Refinement.Aux[_, Elem, Elem]
      ): Either[String, Unit] =
        // todo: rewrite so it does not iterate the whole list
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

  }


  //issues: refinements do not compose - the definition of refine on main trait is impossible to be met in RefinedValidator if it wraps Validator[A, B]
  private class RefinedValidator[A, B](underlying: Validator[A, A], refinement: Refinement.Aux[_, A, B]) extends Validator[A, B] {

    override def validateRefined[B0, C](constraint: C)(implicit ev: RefinementProvider[C,A,B0]): Validator[A,B0] = 
      new RefinedValidator(underlying, ev.make(constraint))

    override def validate(value: A): Either[String,B] = underlying.validate(value).flatMap(refinement.apply)

    override def toSchema(a: Schema[A]): Schema[B] = underlying.toSchema(a).refined(refinement)

    override def validating[C](constraint: C)(implicit ev: RefinementProvider.Simple[C,A]): Validator[A,B] = new RefinedValidator(underlying.validating(constraint), refinement)

      
  }

  private class DirectValidator[A](
      refinements: Vector[Refinement.Aux[_, A, A]]
  ) extends Validator[A, A] {

    override def validateRefined[B0, C](constraint: C)(implicit ev: RefinementProvider[C,A,B0]): Validator[A,B0] = 
      new RefinedValidator(this, ev.make(constraint))

    override def validate(value: A): Either[String, A] = {
      refinements
        .foldLeft(Right(value): Either[String, A]) {
          case (valueOrError, refinement) =>
            valueOrError.flatMap(refinement.apply)
        }
    }

    override def validating[C](constraint: C)(implicit
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
