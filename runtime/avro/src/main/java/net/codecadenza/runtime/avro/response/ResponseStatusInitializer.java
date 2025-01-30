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

package net.codecadenza.runtime.avro.response;

/**
 * <p>
 * Utility class for creating response status objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ResponseStatusInitializer {
	/**
	 * Private constructor
	 */
	private ResponseStatusInitializer() {

	}

	/**
	 * @return a new {@link ResponseStatus} object with response code {@link ResponseCode#SUCCESS}
	 */
	public static final ResponseStatus withSuccessStatus() {
		return with(ResponseCode.SUCCESS, null);
	}

	/**
	 * Create a new error response status object based on the provided exception
	 * @param exception
	 * @return a new {@link ResponseStatus} object with response code {@link ResponseCode#ERROR}
	 */
	public static final ResponseStatus fromException(Exception exception) {
		return with(ResponseCode.ERROR, exception.getMessage());
	}

	/**
	 * Create a new response status object based on the provided response code and message
	 * @param code
	 * @param message
	 * @return a new {@link ResponseStatus} object
	 */
	public static final ResponseStatus with(ResponseCode code, String message) {
		return ResponseStatus.newBuilder().setCode(code).setMessage(message).build();
	}

}
