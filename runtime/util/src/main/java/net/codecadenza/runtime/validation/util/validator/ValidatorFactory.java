/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.validation.util.validator;

import java.lang.annotation.Annotation;
import java.util.HashMap;
import java.util.Map;
import net.codecadenza.runtime.validation.FutureDate;
import net.codecadenza.runtime.validation.MaxDecimalValue;
import net.codecadenza.runtime.validation.MaxFloatValue;
import net.codecadenza.runtime.validation.MaxIntegerValue;
import net.codecadenza.runtime.validation.MaxLength;
import net.codecadenza.runtime.validation.MinDecimalValue;
import net.codecadenza.runtime.validation.MinFloatValue;
import net.codecadenza.runtime.validation.MinIntegerValue;
import net.codecadenza.runtime.validation.MinLength;
import net.codecadenza.runtime.validation.NotEmpty;
import net.codecadenza.runtime.validation.NotNull;
import net.codecadenza.runtime.validation.PastDate;
import net.codecadenza.runtime.validation.RegularExpression;

/**
 * <p>
 * A factory (registry) for validators of annotation-based constraints. This is a singleton for providing a single access point to
 * the registered validators. Note that this class is thread safe.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ValidatorFactory {
	private static ValidatorFactory instance = new ValidatorFactory();

	private final Map<Class<? extends Annotation>, ConstraintValidator> validators = new HashMap<>();

	/**
	 * Constructor
	 */
	private ValidatorFactory() {
		register(NotNull.class, new NotNullValidator());
		register(MinLength.class, new MinLengthValidator());
		register(MaxLength.class, new MaxLengthValidator());
		register(RegularExpression.class, new RegularExpressionValidator());
		register(PastDate.class, new PastDateValidator());
		register(FutureDate.class, new FutureDateValidator());
		register(MaxIntegerValue.class, new MaxIntegerValueValidator());
		register(MinIntegerValue.class, new MinIntegerValueValidator());
		register(MinFloatValue.class, new MinFloatValueValidator());
		register(MaxFloatValue.class, new MaxFloatValueValidator());
		register(MinDecimalValue.class, new MinDecimalValueValidator());
		register(MaxDecimalValue.class, new MaxDecimalValueValidator());
		register(NotEmpty.class, new NotEmptyValidator());
	}

	/**
	 * @return the singleton instance
	 */
	public static ValidatorFactory getInstance() {
		return instance;
	}

	/**
	 * Get the validator for the given constraint type or null if there is no registered validator
	 * @param constraintType
	 * @return the validator
	 */
	public ConstraintValidator getValidatorFor(Class<? extends Annotation> constraintType) {
		return validators.get(constraintType);
	}

	/**
	 * Register a validator
	 * @param constraintType
	 * @param validator
	 */
	public void register(Class<? extends Annotation> constraintType, ConstraintValidator validator) {
		validators.put(constraintType, validator);
	}

}
