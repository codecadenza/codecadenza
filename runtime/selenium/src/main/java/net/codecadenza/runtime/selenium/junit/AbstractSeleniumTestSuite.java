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
package net.codecadenza.runtime.selenium.junit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;

/**
 * <p>
 * Abstract base class for all generated JUnit test suites that need a Selenium WebDriver
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSeleniumTestSuite {
	private static SeleniumTestContext context;

	/**
	 * Create a Selenium test context
	 */
	@BeforeAll
	protected static void setUp() {
		// Initialize the Selenium test context
		context = SeleniumTestContextFactory.initContext();

		// Propagate the context to all test cases
		AbstractSeleniumTestCase.setContext(context);
	}

	/**
	 * Close the driver
	 */
	@AfterAll
	protected static void tearDown() {
		if (context != null) {
			context.getDriver().quit();

			// Clear the context as subsequent tests should use their own!
			context = null;

			// The context used by the test cases must be cleared also!
			AbstractSeleniumTestCase.setContext(null);
		}
	}

}
