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
import net.codecadenza.runtime.validation.util.ConstraintViolation;

/**
 * <p>
 * A validator for annotation-based constraints. Validators may validate one or more constraint types.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface ConstraintValidator {
	/**
	 * Validate the given value against the supplied constraint
	 * @param value the value that will be checked
	 * @param constraint the constraint that the value will be checked against
	 * @return a {@link ConstraintViolation} object if there was a violation of the constraint, or null otherwise
	 * @throws IllegalArgumentException if the constraint is either null or not one of the constraint types supported by this
	 *           validator
	 */
	ConstraintViolation validate(Object value, Annotation constraint);

}
