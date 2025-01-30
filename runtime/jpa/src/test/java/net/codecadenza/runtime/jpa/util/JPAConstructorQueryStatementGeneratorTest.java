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
package net.codecadenza.runtime.jpa.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link JPAConstructorQueryStatementGenerator}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class JPAConstructorQueryStatementGeneratorTest {

	private static final String TOKEN_ID = "a.id";
	private static final String TOKEN_NAME = "a.name";
	private static final String CLASS_NAME = "com.somedomain.CustomerDTO";

	private JPAConstructorQueryStatementGenerator generator;

	@BeforeEach
	void setUp() {
		generator = new JPAConstructorQueryStatementGenerator();
	}

	@Test
	void testCreateStatementWithMultipleSelectTokens() {
		generator.setClassName(CLASS_NAME);
		generator.setSelectTokens(List.of(TOKEN_ID, TOKEN_NAME));

		final var expectedQuery = "select new com.somedomain.CustomerDTO(a.id,a.name)";
		final String actualQuery = generator.createStatement().trim();

		assertEquals(expectedQuery, actualQuery);
	}

	@Test
	void testCreateStatementWithOneSelectToken() {
		generator.setClassName(CLASS_NAME);
		generator.setSelectTokens(List.of(TOKEN_ID));

		final var expectedQuery = "select new com.somedomain.CustomerDTO(a.id)";
		final String actualQuery = generator.createStatement().trim();

		assertEquals(expectedQuery, actualQuery);
	}

	@Test
	void testCreateStatementWithoutSelectTokens() {
		generator.setClassName(CLASS_NAME);

		assertThrows(IllegalArgumentException.class, () -> generator.createStatement());
	}

	@Test
	void testCreateStatementWithoutClassName() {
		generator.setSelectTokens(List.of(TOKEN_ID, TOKEN_NAME));

		assertThrows(IllegalArgumentException.class, () -> generator.createStatement());
	}

}
