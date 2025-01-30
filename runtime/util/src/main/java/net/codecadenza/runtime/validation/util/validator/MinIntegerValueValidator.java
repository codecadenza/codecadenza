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
import net.codecadenza.runtime.validation.MinIntegerValue;
import net.codecadenza.runtime.validation.util.ConstraintViolation;

/**
 * <p>
 * Min. integer validator
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MinIntegerValueValidator implements ConstraintValidator {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.validation.util.validator.ConstraintValidator#validate(java.lang.Object,
	 * java.lang.annotation.Annotation)
	 */
	@Override
	public ConstraintViolation validate(Object objValue, Annotation constraint) {
		ConstraintViolation result = null;
		final var c = (MinIntegerValue) constraint;

		if (objValue == null)
			return null;

		if ((objValue instanceof final Long longValue && longValue < c.value())
				|| (objValue instanceof final Integer integerValue && integerValue < c.value())) {
			if (c.message() != null && !c.message().isEmpty())
				result = new ConstraintViolation(c.message());
			else
				result = new ConstraintViolation("Min. value constraint violated!");
		}

		return result;
	}

}
