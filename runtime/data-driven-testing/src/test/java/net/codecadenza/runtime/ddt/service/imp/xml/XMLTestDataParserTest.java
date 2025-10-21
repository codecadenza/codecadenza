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
package net.codecadenza.runtime.ddt.service.imp.xml;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import net.codecadenza.runtime.ddt.model.TestData;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the basic functionality of {@link XMLTestDataParser}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class XMLTestDataParserTest {
	private static final String XML_TEST_DIRECTORY = "src/test/resources/data";
	private static final String XML_FILE_PATH = XML_TEST_DIRECTORY + "/customer_testdata.xml";

	@Test
	void testUnmarshalXMLFromFile() {
		final var testDataParser = new XMLTestDataParser(new File(XML_FILE_PATH));
		final TestData testData = testDataParser.parseTestData();

		assertNotNull(testData);
		assertEquals(6, testData.getMethodInvocations().size());
	}

	@Test
	void testThrowsExceptionIfFileIsNull() {
		final var testDataParser = new XMLTestDataParser(null);

		assertThrows(IllegalArgumentException.class, testDataParser::parseTestData);
	}

	@Test
	void testThrowsExceptionIfFileIsAFolder() {
		final var testDataParser = new XMLTestDataParser(new File(XML_TEST_DIRECTORY));

		assertThrows(IllegalArgumentException.class, testDataParser::parseTestData);
	}

	@Test
	void testThrowsExceptionIfFileNotExists() {
		final var nonExistingFilePath = XML_TEST_DIRECTORY + "/not_existing.xml";
		final var testDataParser = new XMLTestDataParser(new File(nonExistingFilePath));

		assertThrows(IllegalArgumentException.class, testDataParser::parseTestData);
	}

	@Test
	void testThrowsExceptionIfFileCannotBeParsed() {
		final var nonExistingFilePath = XML_TEST_DIRECTORY + "/invalid.xml";
		final var testDataParser = new XMLTestDataParser(new File(nonExistingFilePath));

		assertThrows(IllegalStateException.class, testDataParser::parseTestData);
	}

}
