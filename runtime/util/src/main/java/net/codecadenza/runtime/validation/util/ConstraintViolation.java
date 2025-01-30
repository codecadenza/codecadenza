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
package net.codecadenza.runtime.validation.util;

/**
 * <p>
 * Represents a violation of a constraint. Captures a message describing the violation. Note there may be violations of several
 * constraints during the validation of a property. All these violations are captured as separate ConstraintViolation objects and
 * typically rolled up as part of an overall validation result.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ConstraintViolation {
	private final String message;

	/**
	 * Constructor
	 * @param message
	 * @throws IllegalArgumentException if the message is null
	 */
	public ConstraintViolation(String message) {
		if (message == null)
			throw new IllegalArgumentException("Null is not a legal value for message");

		this.message = message;
	}

	/**
	 * Text describing the violation. Note that applications do not necessarily need to make use of this message, as they have
	 * enough information to create their own messages. In particular they know the constraint that was violated and the value that
	 * violated the constraint. As such this message can be considered as a default message and is useful for logging etc.
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

}
