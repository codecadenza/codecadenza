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
package net.codecadenza.runtime.selenium.data;

import net.codecadenza.runtime.selenium.util.SeleniumTestProperties;

/**
 * <p>
 * Common interface that must be implemented by all test data providers
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface ITestDataProvider {
	/**
	 * Set basic properties for the initialization of a test data provider
	 * @param properties
	 */
	void setProperties(SeleniumTestProperties properties);

	/**
	 * Initialize the test data provider for a given test case
	 * @param testCaseClass
	 * @throws RuntimeException if the initialization has failed due to an internal error
	 */
	void init(Class<?> testCaseClass);

	/**
	 * Provides the next page action test data object for a given page object
	 * @param pageClass
	 * @return the page action test data for the given page object or null if no respective object is (no longer) available
	 * @throws RuntimeException if the provider doesn't contain page action test data for the given page object
	 */
	PageActionTestData getNextTestDataForPage(Class<?> pageClass);

}
