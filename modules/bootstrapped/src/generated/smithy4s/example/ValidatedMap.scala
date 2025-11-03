package smithy4s.example

import smithy4s.Bijection
import smithy4s.Hints
import smithy4s.Schema
import smithy4s.ShapeId
import smithy4s.ValidatedNewtype
import smithy4s.Validator
import smithy4s.schema.Schema.int
import smithy4s.schema.Schema.map
import smithy4s.schema.Schema.string

object ValidatedMap extends ValidatedNewtype[Map[String, Int]] {
  val id: ShapeId = ShapeId("smithy4s.example", "ValidatedMap")
  val hints: Hints = Hints.empty
  val underlyingSchema: Schema[Map[String, Int]] = map(string, int).withId(id).addHints(hints).validated(smithy.api.Length(min = None, max = Some(1L)))
  val validator: Validator[Map[String, Int], ValidatedMap] = Validator.of[Map[String, Int], ValidatedMap](Bijection[Map[String, Int], ValidatedMap](_.asInstanceOf[ValidatedMap], value(_))).validating(smithy.api.Length(min = None, max = Some(1L)))
  implicit val schema: Schema[ValidatedMap] = validator.toSchema(underlyingSchema)
  @inline def apply(a: Map[String, Int]): Either[String, ValidatedMap] = validator.validate(a)
}
