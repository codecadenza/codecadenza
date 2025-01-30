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
package net.codecadenza.runtime.authentication;

import java.util.Base64;

/**
 * <p>
 * Utility class for HTTP basic access authentication
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicAuthentication {
	public static final String HTTP_HEADER_AUTHORIZATION = "Authorization";
	private static final String BASIC_AUTH_PREFIX = "Basic ";
	private static final String DELIMITER = ":";

	/**
	 * Prevent instantiation
	 */
	private BasicAuthentication() {

	}

	/**
	 * Create a basic authentication string by using the provided credentials
	 * @param userName
	 * @param password
	 * @return the authentication string
	 */
	public static String createAuthentication(String userName, String password) {
		final String userNameAndPassword = userName + DELIMITER + password;

		return BASIC_AUTH_PREFIX + Base64.getEncoder().encodeToString(userNameAndPassword.getBytes());
	}

	/**
	 * Extract the user name from the given authentication string
	 * @param authenticationString
	 * @return the user name
	 * @throws IllegalArgumentException if the authentication string is either empty or if it has an illegal format
	 */
	public static String getUserName(String authenticationString) {
		return extractField(authenticationString, true);
	}

	/**
	 * Extract the password from the given authentication string
	 * @param authenticationString
	 * @return the password
	 * @throws IllegalArgumentException if the authentication string is either empty or if it has an illegal format
	 */
	public static String getPassword(String authenticationString) {
		return extractField(authenticationString, false);
	}

	/**
	 * Extract the user name or the password from the given authentication string
	 * @param authenticationString
	 * @param extractUserName if true the user name will be returned
	 * @return either the user name or the password
	 * @throws IllegalArgumentException if the authentication string is either empty or if it has an illegal format
	 */
	private static String extractField(String authenticationString, boolean extractUserName) {
		if (authenticationString == null || authenticationString.isEmpty())
			throw new IllegalArgumentException("The parameter 'authenticationString' must not be null or empty!");

		final String decodedString = new String(
				Base64.getDecoder().decode(authenticationString.substring(BASIC_AUTH_PREFIX.length())));

		if (!decodedString.contains(DELIMITER))
			throw new IllegalArgumentException("Authentication string '" + authenticationString + "' has an illegal format!");

		if (extractUserName)
			return decodedString.substring(0, decodedString.indexOf(DELIMITER));

		return decodedString.substring(decodedString.indexOf(DELIMITER) + 1);
	}

}
