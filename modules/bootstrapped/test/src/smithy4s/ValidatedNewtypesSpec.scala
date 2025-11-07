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
import smithy4s.example.ValidatedList
import smithy4s.example.ValidatedMap
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

  test("Validated name".only) {
    expect(ValidatedName("Joe").isRight)
    expect(ValidatedName("").isLeft)
  }

  test("Validated newtype list".only) {
    expect(ValidatedList(List("foo")).isRight)
    expect(ValidatedList(List("foo", "bar")).isLeft)
  }

  test("Validated newtype member list".only) {
    expect(ValidatedMemberList(List("f")).isRight)
    expect(ValidatedMemberList(List("fo")).isLeft)
  }

  test("Validated newtype map") {
    expect(ValidatedMap(Map("foo" -> 1)).isRight)
    expect(ValidatedMap(Map("foo" -> 1, "bar" -> 2)).isLeft)
  }

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

  type ValidatedName = ValidatedName.Type

  object ValidatedName extends ValidatedNewtype[smithy4s.refined.Name] {
    val id: ShapeId = ShapeId("smithy4s.example", "ValidatedName")
    val hints: Hints = Hints(
      smithy4s.example.NameFormat()
    ).lazily
    val underlyingSchema: Schema[smithy4s.refined.Name] = string
      .refined[smithy4s.refined.Name](smithy4s.example.NameFormat())
      .withId(id)
      .addHints(hints)
    val validator: Validator[smithy4s.refined.Name, ValidatedName] =
      Validator.simple.refined(NameFormat).biject(
        Bijection[smithy4s.refined.Name, ValidatedName](
          _.asInstanceOf[ValidatedName],
          value(_)
        )
      )
    implicit val schema: Schema[ValidatedName] =
      validator.toSchema(underlyingSchema)
    @inline def apply(a: smithy4s.refined.Name): Either[String, ValidatedName] =
      validator.validate(a)
  }

  type ValidatedList = ValidatedList.Type

  object ValidatedList extends ValidatedNewtype[List[String]] {
    val id: ShapeId = ShapeId("smithy4s.example", "ValidatedList")
    val hints: Hints = Hints.empty
    val underlyingSchema: Schema[List[String]] = list(string)
      .withId(id)
      .addHints(hints)
      .validated(smithy.api.Length(min = None, max = Some(1L)))
    val validator: Validator[List[String], ValidatedList] = Validator
      .of[List[String], ValidatedList](
        Bijection[List[String], ValidatedList](
          _.asInstanceOf[ValidatedList],
          value(_)
        )
      )
      .validating(smithy.api.Length(min = None, max = Some(1L)))
    implicit val schema: Schema[ValidatedList] =
      validator.toSchema(underlyingSchema)
    @inline def apply(a: List[String]): Either[String, ValidatedList] =
      validator.validate(a)
  }

  type ValidatedMemberList = ValidatedMemberList.Type

  object ValidatedMemberList extends ValidatedNewtype[List[String]] {
    val id: ShapeId = ShapeId("smithy4s.example", "ValidatedMemberList")
    val hints: Hints = Hints.empty
    val underlyingSchema: Schema[List[String]] = list(
      string
        .addMemberHints()
        .validated(smithy.api.Length(min = None, max = Some(1L)))
    ).withId(id).addHints(hints)
    val validator: Validator[List[String], ValidatedMemberList] =
      Validator
        .list[String]
        .validatingElement(
          smithy.api.Length(min = None, max = Some(1L))
        )
        .biject(
          Bijection[List[String], ValidatedMemberList](
            _.asInstanceOf[ValidatedMemberList],
            value(_)
          )
        )
    implicit val schema: Schema[ValidatedMemberList] =
      validator.toSchema(underlyingSchema)
    @inline def apply(a: List[String]): Either[String, ValidatedMemberList] =
      validator.validate(a)
  }

  type ValidatedMemberRefinedList = ValidatedMemberRefinedList.Type

  object ValidatedMemberRefinedList
      extends ValidatedNewtype[NonEmptyList[String]] {
    val id: ShapeId = ShapeId("smithy4s.example", "ValidatedMemberRefinedList")
    val hints: Hints = Hints(
      smithy4s.example.NonEmptyListFormat()
    ).lazily
    val underlyingSchema: Schema[NonEmptyList[String]] = list(
      string
        .addMemberHints()
        .validated(smithy.api.Length(min = None, max = Some(1L)))
    ).refined[NonEmptyList[String]](smithy4s.example.NonEmptyListFormat())
      .withId(id)
      .addHints(hints)
    val validator
        : Validator[NonEmptyList[String], ValidatedMemberRefinedList] = {

      Validator.list[String].validatingElement(Length(max = Some(1L)))
      // Validator.of[NonEmptyList[String], ValidatedMemberRefinedList](
      //   Bijection[NonEmptyList[String], ValidatedMemberRefinedList](
      //     _.asInstanceOf[ValidatedMemberRefinedList],
      //     value(_)
      //   )
      // )
      ???
    }
    implicit val schema: Schema[ValidatedMemberRefinedList] =
      validator.toSchema(underlyingSchema)
    @inline def apply(
        a: NonEmptyList[String]
    ): Either[String, ValidatedMemberRefinedList] = validator.validate(a)
  }

}
