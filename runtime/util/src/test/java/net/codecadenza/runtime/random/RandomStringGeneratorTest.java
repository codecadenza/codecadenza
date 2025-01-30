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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link RandomStringGenerator}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class RandomStringGeneratorTest {
	private static final String EXPECTED_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	private static final int MIN_LENGTH = 1;
	private static final int MAX_LENGTH = 20;

	@Test
	void testCreateRandomString() {
		for (int length = MIN_LENGTH; length < MAX_LENGTH + 1; length++) {
			final String generatedString = RandomStringGenerator.generateRandomString(length);

			assertNotNull(generatedString);
			assertEquals(length, generatedString.length());

			// Validate that the generated string only contains alphanumeric characters
			for (final char character : generatedString.toCharArray())
				assertTrue(EXPECTED_CHARS.contains(String.valueOf(character)));
		}
	}

	@Test
	void testThrowIllegalArgumentExceptionIfLengthIsSmallerThanOne() {
		assertThrows(IllegalArgumentException.class, () -> RandomStringGenerator.generateRandomString(0));
	}

}
