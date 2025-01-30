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

import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.selenium.data.ITestDataProvider;
import net.codecadenza.runtime.selenium.data.TestDataFactory;
import net.codecadenza.runtime.selenium.driver.DriverProducerFactory;
import net.codecadenza.runtime.selenium.util.SeleniumTestProperties;
import org.openqa.selenium.WebDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Factory that is responsible for creating a Selenium test context
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumTestContextFactory {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * Prevent instantiation
	 */
	private SeleniumTestContextFactory() {

	}

	/**
	 * Create and initialize a Selenium test context
	 * @return a Selenium test context
	 */
	public static final SeleniumTestContext initContext() {
		// Load global configuration properties
		final var properties = new SeleniumTestProperties();
		properties.load();

		logger.debug("Configuration properties loaded");

		// Create the Selenium WebDriver
		final WebDriver driver = DriverProducerFactory.getDriverProducer(properties).getDriverInstance();

		// Create the test data provider
		final ITestDataProvider testDataProvider = TestDataFactory.getTestDataProvider(properties);

		logger.debug("Create test context");

		// Create the test context
		return new SeleniumTestContext(driver, testDataProvider, properties);
	}

}
