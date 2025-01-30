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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Base class for all generated JUnit Selenium tests. This class is responsible for providing a Selenium WebDriver and
 * initializing the test data provider.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSeleniumTestCase {
	private static SeleniumTestContext context;
	protected Logger logger;

	/**
	 * Create a Selenium WebDriver and initialize the test data provider
	 */
	@BeforeEach
	protected void setUp() {
		logger = LoggerFactory.getLogger(getClass());

		// Create the Selenium test context if it hasn't been initialized yet (e.g. by a test suite)
		if (context == null) {
			context = SeleniumTestContextFactory.initContext();

			// Close the driver after finishing the test!
			context.setCloseDriver(true);
		}

		context.getDataProvider().init(getClass());
	}

	/**
	 * If applicable, close the driver
	 */
	@AfterEach
	protected void tearDown() {
		if (context.isCloseDriver()) {
			logger.debug("Close driver");

			context.getDriver().quit();

			// Clear the context as subsequent tests should use their own!
			context = null;
		}
	}

	/**
	 * Run one test in order to transfer the test context to the implementation method!
	 */
	@Test
	protected void test() {
		logger.debug("Start test");

		test(context);

		logger.debug("Finish test");
	}

	/**
	 * Every implementation must override this method in order to be able to consume the test context!
	 * @param context
	 */
	protected abstract void test(SeleniumTestContext context);

	/**
	 * Set the Selenium test context if it has been initialized externally (e.g. in a test suite)
	 * @param context
	 */
	protected static void setContext(SeleniumTestContext context) {
		AbstractSeleniumTestCase.context = context;
	}

}
