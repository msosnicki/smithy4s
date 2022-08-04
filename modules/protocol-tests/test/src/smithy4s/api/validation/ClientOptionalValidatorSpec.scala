package smithy4s.api.validation

import software.amazon.smithy.model.shapes._
import software.amazon.smithy.model.traits.ClientOptionalTrait
import software.amazon.smithy.model.Model
import scala.jdk.CollectionConverters._
import software.amazon.smithy.model.validation.ValidationEvent
import software.amazon.smithy.model.validation.Severity
import software.amazon.smithy.model.traits.InputTrait

final class ClientOptionalValidatorSpec extends munit.FunSuite {

  test("client optional trait should not be allowed") {
    val validator = new ClientOptionalValidator()
    val member = MemberShape
      .builder()
      .id("test#struct$testing")
      .target("smithy.api#String")
      .addTrait(new ClientOptionalTrait())
      .build()
    val struct =
      StructureShape.builder().id("test#struct").addMember(member).build()

    val model =
      Model.builder().addShape(struct).build()

    val result = validator.validate(model).asScala.toList

    val expected = List(
      ValidationEvent
        .builder()
        .id("ClientOptional")
        .shape(member)
        .severity(Severity.WARNING)
        .message(
          "@clientOptional trait is not allowed"
        )
        .build()
    )
    assertEquals(result, expected)
  }

  test("input trait is not allowed (adds client optional implicitly)") {
    val validator = new ClientOptionalValidator()
    val struct =
      StructureShape
        .builder()
        .id("test#struct")
        .addTrait(new InputTrait)
        .build()

    val model =
      Model.builder().addShape(struct).build()

    val result = validator.validate(model).asScala.toList

    val expected = List(
      ValidationEvent
        .builder()
        .id("ClientOptional")
        .shape(struct)
        .severity(Severity.WARNING)
        .message(
          "@input trait is not allowed"
        )
        .build()
    )
    assertEquals(result, expected)
  }

}
