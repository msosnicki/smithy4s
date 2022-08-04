package smithy4s.api.validation;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.traits.*;
import software.amazon.smithy.model.validation.AbstractValidator;
import software.amazon.smithy.model.validation.ValidationEvent;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class ClientOptionalValidator extends AbstractValidator {

	List<ValidationEvent> getValidationEvents(Model model, Class<? extends Trait> trait, String traitName) {
		return model.getShapesWithTrait(trait).stream().flatMap(clientOptionalShape -> {
			return Stream.of(warning(clientOptionalShape, String.format("@%s trait is not allowed", traitName)));
		}).collect(Collectors.toList());
	}

	@Override
	public List<ValidationEvent> validate(Model model) {
		List<ValidationEvent> cOptionalEvents = getValidationEvents(model, ClientOptionalTrait.class, "clientOptional");
		List<ValidationEvent> inputEvents = getValidationEvents(model, InputTrait.class, "input");
		cOptionalEvents.addAll(inputEvents);
		return cOptionalEvents;
	}

}
