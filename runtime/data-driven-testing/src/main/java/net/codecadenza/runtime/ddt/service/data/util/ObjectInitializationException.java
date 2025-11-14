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
package net.codecadenza.runtime.ddt.service.data.util;

/**
 * <p>
 * Exception that indicates that an object could not be initialized
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ObjectInitializationException extends RuntimeException {
	private static final long serialVersionUID = -5778052224607704684L;

	/**
	 * Constructor
	 * @param message a human-readable message explaining the reason for the failure
	 * @param cause the {@link Throwable} that caused this exception to be thrown
	 */
	public ObjectInitializationException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor
	 * @param objectType the class that represents the type of the object
	 * @param cause the {@link Throwable} that caused this exception to be thrown
	 */
	public ObjectInitializationException(Class<?> objectType, Throwable cause) {
		this("Error initializing object of type '" + objectType.getName() + "'", cause);
	}

}
