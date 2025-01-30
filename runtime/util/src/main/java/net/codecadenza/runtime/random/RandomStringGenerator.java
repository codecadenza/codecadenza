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
package net.codecadenza.runtime.random;

import java.util.concurrent.ThreadLocalRandom;

/**
 * <p>
 * Utility class for creating random alphanumeric strings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RandomStringGenerator {

	private static final String ALPHANUMERIC_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	/**
	 * Prevent instantiation
	 */
	private RandomStringGenerator() {

	}

	/**
	 * Generate a random string that contains characters from 0 to 9 and A to Z with the given length
	 * @param length the length of the string
	 * @return the generated random string
	 * @throws IllegalArgumentException if the length is smaller than 1
	 */
	public static String generateRandomString(int length) {
		if (length < 1)
			throw new IllegalArgumentException("The length must be greater than 0!");

		final var charArray = new char[length];

		for (int i = 0; i < length; i++)
			charArray[i] = ALPHANUMERIC_CHARS.charAt(ThreadLocalRandom.current().nextInt(ALPHANUMERIC_CHARS.length()));

		return new String(charArray);
	}
}
