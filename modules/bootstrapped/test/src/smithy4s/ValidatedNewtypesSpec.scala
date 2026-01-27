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

import smithy4s.schema.Schema.string
import smithy4s.schema.Schema.list
import smithy4s.refined.NonEmptyList
import munit.Assertions
import cats.data.Validated.Valid
import smithy.api.Length
import smithy4s.example.NonEmptyListFormat

class ValidatedNewtypesSpec() extends munit.FunSuite {
  val id1 = "id1"
  val id2 = "id2"

  test("Validated newtypes are consistent") {
    expect.same(AccountId.unsafeApply(id1).value, id1)
    expect.different(
      AccountId.unsafeApply(id1).value,
      AccountId.unsafeApply(id2).value
    )
    expect.different(
      implicitly[ShapeTag[AccountId]],
      implicitly[ShapeTag[DeviceId]]
    )
    expect.same(AccountId.unapply(AccountId.unsafeApply(id1)), Some(id1))
  }

  /**
    * newtype collection with constraint on the list (ok)
    * newtype collection with constraint on member (ok)
    * newtype refined primitive - doesn't make sense for now (validator is a noop)
    * newtype refined collection - doesn't make sense for now (validator is a noop)
    * newtype refined collection with a constraint on a member
    * newtype refined collectio nwith a constraint on a refined member
    * 
    */
  // test("Validated name".ignore) {
  //   expect(ValidatedRefinedPrimitive("Joe").isRight)
  //   expect(ValidatedRefinedPrimitive("").isLeft)
  // }

  test("Validated constrained list") {
    expect(ValidatedConstrainedList(List("foo")).isRight)
    expect(ValidatedConstrainedList(List("foo", "bar")).isLeft)
  }

  test("Validated list constrained member") {
    expect(ValidatedListConstrainedMember(List("f")).isRight)
    expect(ValidatedListConstrainedMember(List("fo")).isLeft)
  }

  test("Validated constrained list  constrained member".only) {
    expect(ValidatedConstrainedListConstrainedMember(List("f")).isRight) // both valid
    expect(ValidatedConstrainedListConstrainedMember(List("fg")).isLeft) // member invalid
    expect(
      ValidatedConstrainedListConstrainedMember(List("f", "g")).isLeft
    ) // list invalid
    expect(
      ValidatedConstrainedListConstrainedMember(List("fg", "h")).isLeft
    ) // both invalid
  }

  // test("Validated newtype map".ignore) {
  //   expect(ValidatedMap(Map("foo" -> 1)).isRight)
  //   expect(ValidatedMap(Map("foo" -> 1, "bar" -> 2)).isLeft)
  // }

  test("Newtypes have well defined unapply") {
    val aid = AccountId.unsafeApply(id1)
    aid match {
      case AccountId(id) => expect(id == id1)
    }
  }

  test("Validated newtypes unsafeApply throws exception") {
    val e = Assertions.intercept[IllegalArgumentException] {
      AccountId.unsafeApply("!^%&")
    }

    expect.same(
      e.getMessage(),
      "String '!^%&' does not match pattern '[a-zA-Z0-9]+'"
    )
  }

  type DeviceId = DeviceId.Type
  object DeviceId extends ValidatedNewtype[String] {

    val id: ShapeId = ShapeId("foo", "DeviceId")
    val hints: Hints = Hints.empty

    val underlyingSchema: Schema[String] = string
      .withId(id)
      .addHints(hints)
      .validated(smithy.api.Length(min = Some(1L), max = None))

    val validator: Validator[String, DeviceId] = Validator
      .of[String, DeviceId](
        Bijection[String, DeviceId](_.asInstanceOf[DeviceId], value(_))
      )
      .validating(smithy.api.Length(min = Some(1L), max = None))

    implicit val schema: Schema[DeviceId] =
      validator.toSchema(underlyingSchema)

    @inline def apply(a: String): Either[String, DeviceId] =
      validator.validate(a)

  }

  type AccountId = AccountId.Type

  object AccountId extends ValidatedNewtype[String] {
    def id: smithy4s.ShapeId = ShapeId("foo", "AccountId")
    val hints: Hints = Hints.empty

    val underlyingSchema: Schema[String] = string
      .withId(id)
      .addHints(hints)
      .validated(smithy.api.Length(min = Some(1L), max = None))
      .validated(smithy.api.Pattern("[a-zA-Z0-9]+"))

    val validator: Validator[String, AccountId] = Validator
      .of[String, AccountId](
        Bijection[String, AccountId](_.asInstanceOf[AccountId], value(_))
      )
      .validating(smithy.api.Length(min = Some(1L), max = None))
      .alsoValidating(smithy.api.Pattern("[a-zA-Z0-9]+"))

    implicit val schema: Schema[AccountId] =
      validator.toSchema(underlyingSchema)

    @inline def apply(a: String): Either[String, AccountId] =
      validator.validate(a)

  }

  // DONE ===============================================================
  type ValidatedConstrainedList = ValidatedConstrainedList.Type

  object ValidatedConstrainedList extends ValidatedNewtype[List[String]] {
    val id: ShapeId = ShapeId("smithy4s.example", "ValidatedConstrainedList")
    val hints: Hints = Hints.empty
    val underlyingSchema: Schema[List[String]] = list(string)
      .withId(id)
      .addHints(hints)
      .validated(smithy.api.Length(min = None, max = Some(1L)))
    val validator: Validator[List[String], ValidatedConstrainedList] =
      Validator
        .of[List[String], ValidatedConstrainedList](
          Bijection[List[String], ValidatedConstrainedList](
            _.asInstanceOf[ValidatedConstrainedList],
            value(_)
          )
        )
        .validating(smithy.api.Length(min = None, max = Some(1L)))
    // TODO:
    // Validator
    // .list[String]
    // .validating(smithy.api.Length(min = None, max = Some(1L)))
    // .biject(
    //   Bijection[List[String], ValidatedConstrainedList](
    //     _.asInstanceOf[ValidatedConstrainedList],
    //     value(_)
    //   )
    // )

    implicit val schema: Schema[ValidatedConstrainedList] =
      validator.toSchema(underlyingSchema)
    @inline def apply(
        a: List[String]
    ): Either[String, ValidatedConstrainedList] = validator.validate(a)
  }

  // COMPILING AND VALID ===================================================

  type ValidatedListConstrainedMember = ValidatedListConstrainedMember.Type

  object ValidatedListConstrainedMember extends ValidatedNewtype[List[String]] {
    val id: ShapeId =
      ShapeId("smithy4s.example", "ValidatedListConstrainedMember")
    val hints: Hints = Hints.empty
    val underlyingSchema: Schema[List[String]] = list(
      string
        .addMemberHints()
        .validated(smithy.api.Length(min = None, max = Some(1L)))
    ).withId(id).addHints(hints)
    val validator: Validator[List[String], ValidatedListConstrainedMember] =
      Validator
        .list[String]
        .validatingElement(
          smithy.api.Length(min = None, max = Some(1L))
        )
        .biject(
          Bijection[List[String], ValidatedListConstrainedMember](
            _.asInstanceOf[ValidatedListConstrainedMember],
            value(_)
          )
        )
    implicit val schema: Schema[ValidatedListConstrainedMember] =
      validator.toSchema(underlyingSchema)
    @inline def apply(
        a: List[String]
    ): Either[String, ValidatedListConstrainedMember] =
      validator.validate(a)
  }

  // WIP - SHOULD BE SUPPORTED ======================================================
  type ValidatedConstrainedListConstrainedMember =
    ValidatedConstrainedListConstrainedMember.Type
  object ValidatedConstrainedListConstrainedMember
      extends ValidatedNewtype[List[String]] {
    val id: ShapeId =
      ShapeId("smithy4s.example", "ValidatedConstrainedListConstrainedMember")
    val hints: Hints = Hints.empty
    val underlyingSchema: Schema[List[String]] = list(
      string
        .addMemberHints()
        .validated(smithy.api.Length(min = None, max = Some(1L)))
    ).withId(id)
      .addHints(hints)
      .validated(smithy.api.Length(min = None, max = Some(1L)))
    val validator
        : Validator[List[String], ValidatedConstrainedListConstrainedMember] =
      Validator
        .list[String]
        .validatingElement(smithy.api.Length(min = None, max = Some(1L)))
        .validating(smithy.api.Length(min = None, max = Some(1L)))
        .biject(
          Bijection[List[String], ValidatedConstrainedListConstrainedMember](
            _.asInstanceOf[ValidatedConstrainedListConstrainedMember],
            value(_)
          )
        )
    // Validator
    //   .of[List[String], ValidatedConstrainedListConstrainedMember](
    //     Bijection[List[String], ValidatedConstrainedListConstrainedMember](
    //       _.asInstanceOf[ValidatedConstrainedListConstrainedMember],
    //       value(_)
    //     )
    //   )
    //   .validating(smithy.api.Length(min = None, max = Some(1L)))
    implicit val schema: Schema[ValidatedConstrainedListConstrainedMember] =
      validator.toSchema(underlyingSchema)
    @inline def apply(
        a: List[String]
    ): Either[String, ValidatedConstrainedListConstrainedMember] =
      validator.validate(a)
  }

  // type ValidatedRefinedMemberRefinedList = ValidatedRefinedMemberRefinedList.Type
  // object ValidatedRefinedMemberRefinedList extends Newtype[NonEmptyList[ValidatedName]] {
  //   val id: ShapeId = ShapeId("smithy4s.example", "ValidatedRefinedMemberRefinedList")
  //   val hints: Hints = Hints.empty
  //   val underlyingSchema: Schema[NonEmptyList[ValidatedName]] = list(ValidatedName.schema).refined[NonEmptyList[ValidatedName]](smithy4s.example.NonEmptyListFormat()).withId(id).addHints(hints)
  //   implicit val schema: Schema[ValidatedRefinedMemberRefinedList] = bijection(underlyingSchema, asBijection)
  // }

  // NOT SUPPORTED

  // type ValidatedRefinedPrimitive = ValidatedRefinedPrimitive.Type

  // in the current form, it does not make sense to mark as validated newtype in cases like this one - it is a noop
  // import smithy4s.example.NameFormat
  // object ValidatedRefinedPrimitive extends ValidatedNewtype[smithy4s.refined.Name] {
  //   val id: ShapeId = ShapeId("smithy4s.example", "ValidatedRefinedPrimitive")
  //   val hints: Hints = Hints(NameFormat()).lazily
  //   val underlyingSchema: Schema[smithy4s.refined.Name] = string
  //     .refined[smithy4s.refined.Name](NameFormat())
  //     .withId(id)
  //     .addHints(hints)
  //   val validator: Validator[String, ValidatedRefinedPrimitive] =
  //     Validator.simple.refining[smithy4s.refined.Name, NameFormat](NameFormat()).biject(
  //       Bijection[smithy4s.refined.Name, ValidatedRefinedPrimitive](
  //         _.asInstanceOf[ValidatedRefinedPrimitive],
  //         value(_)
  //       )
  //     )
  //   implicit val schema: Schema[ValidatedRefinedPrimitive] =
  //     validator.toSchema(underlyingSchema)
  //   @inline def apply(a: smithy4s.refined.Name): Either[String, ValidatedRefinedPrimitive] =
  //     validator.validate(a)
  // }

  // type ValidatedNonEmptyList = ValidatedNonEmptyList.Type
  // todo: also noop for validation - doesn't make sense to add such constraint
  // object ValidatedNonEmptyList extends ValidatedNewtype[NonEmptyList[String]] {
  //   val id: ShapeId = ShapeId("smithy4s.example", "ValidatedNonEmptyList")
  //   val hints: Hints = Hints(
  //     smithy4s.example.NonEmptyListFormat()
  //   ).lazily
  //   val underlyingSchema: Schema[NonEmptyList[String]] = list(string)
  //     .refined[NonEmptyList[String]](smithy4s.example.NonEmptyListFormat())
  //     .withId(id)
  //     .addHints(hints)
  //   val validator: Validator[NonEmptyList[String], ValidatedNonEmptyList] =
  //     Validator.of[NonEmptyList[String], ValidatedNonEmptyList](
  //       Bijection[NonEmptyList[String], ValidatedNonEmptyList](
  //         _.asInstanceOf[ValidatedNonEmptyList],
  //         value(_)
  //       )
  //     )
  //   implicit val schema: Schema[ValidatedNonEmptyList] =
  //     validator.toSchema(underlyingSchema)
  //   @inline def apply(
  //       a: NonEmptyList[String]
  //   ): Either[String, ValidatedNonEmptyList] = validator.validate(a)
  // }

  // type ValidatedRefinedListConstrainedMember = ValidatedRefinedListConstrainedMember.Type

  // object ValidatedRefinedListConstrainedMember
  //     extends ValidatedNewtype[NonEmptyList[String]] {
  //   val id: ShapeId = ShapeId("smithy4s.example", "ValidatedRefinedListConstrainedMember")
  //   val hints: Hints = Hints(
  //     smithy4s.example.NonEmptyListFormat()
  //   ).lazily
  //   val underlyingSchema: Schema[NonEmptyList[String]] = list(
  //     string
  //       .addMemberHints()
  //       .validated(smithy.api.Length(min = None, max = Some(1L)))
  //   ).refined[NonEmptyList[String]](smithy4s.example.NonEmptyListFormat())
  //     .withId(id)
  //     .addHints(hints)
  //   val validator
  //       : Validator[NonEmptyList[String], ValidatedRefinedListConstrainedMember] = {

  //     Validator.list[String].validatingElement(Length(max = Some(1L))).validateRefined(NonEmptyListFormat())[]
  //     // Validator.of[NonEmptyList[String], ValidatedRefinedListConstrainedMember](
  //     //   Bijection[NonEmptyList[String], ValidatedRefinedListConstrainedMember](
  //     //     _.asInstanceOf[ValidatedRefinedListConstrainedMember],
  //     //     value(_)
  //     //   )
  //     // )
  //     ???
  //   }
  //   implicit val schema: Schema[ValidatedRefinedListConstrainedMember] =
  //     validator.toSchema(underlyingSchema)
  //   @inline def apply(
  //       a: NonEmptyList[String]
  //   ): Either[String, ValidatedRefinedListConstrainedMember] = validator.validate(a)
  // }

}
