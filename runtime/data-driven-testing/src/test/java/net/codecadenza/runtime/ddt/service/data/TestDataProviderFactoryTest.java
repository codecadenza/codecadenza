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
package net.codecadenza.runtime.ddt.service.data;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import net.codecadenza.runtime.service.ServiceInitializationException;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link TestDataProviderFactory}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class TestDataProviderFactoryTest {
	private static final String XML_FILE_PATH = "src/test/resources/data/country_testdata.xml";
	private static final String INVALID_PROPERTIES = "src/test/resources/invalid_test.properties";

	@Test
	void testGetTestDataProvider() {
		final var properties = new TestDataProviderProperties();
		properties.load();

		final ITestDataProvider testDataProvider = TestDataProviderFactory.getTestDataProvider(new File(XML_FILE_PATH), properties);
		assertNotNull(testDataProvider);
	}

	@Test
	void testThrowsExceptionIfProviderDoesNotExist() {
		final var properties = new TestDataProviderProperties();
		properties.load(new File(INVALID_PROPERTIES));

		assertThrows(ServiceInitializationException.class,
				() -> TestDataProviderFactory.getTestDataProvider(new File(XML_FILE_PATH), properties));
	}

}
