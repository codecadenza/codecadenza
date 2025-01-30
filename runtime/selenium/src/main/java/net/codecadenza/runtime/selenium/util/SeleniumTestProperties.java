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
package net.codecadenza.runtime.selenium.util;

import java.io.File;
import java.io.FileInputStream;
import java.lang.invoke.MethodHandles;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Specialized properties for Selenium tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumTestProperties extends Properties {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -2863977666186515210L;
	private static final String PROPERTY_BASE_URL = "BASE_URL";
	private static final String PROPERTY_PAGE_LOAD_TIMEOUT = "PAGE_LOAD_TIMEOUT";
	private static final String PROPERTY_IMPLICIT_WAIT_TIME = "IMPLICIT_WAIT_TIME";
	private static final String PROPERTY_DRIVER_NAME = "DRIVER_NAME";
	private static final String PROPERTY_DRIVER_PATH = "DRIVER_PATH";
	private static final String PROPERTY_DRIVER_CLASS = "DRIVER_CLASS";
	private static final String PROPERTY_DRIVER_MAXIMIZE = "DRIVER_MAXIMIZE_WINDOW";
	private static final String PROPERTY_DATA_PROVIDER_CLASS = "DATA_PROVIDER_CLASS";
	private static final String PROPERTY_TEST_DATA_PATH = "TEST_DATA_PATH";
	private static final String PROPERTY_DRIVER_PRODUCER_CLASS = "DRIVER_PRODUCER_CLASS";
	private static final String PROPERTY_PAGE_LOAD_DELAY = "PAGE_LOAD_DELAY";
	private static final String PROPERTY_HTTP_WAIT_TIMEOUT = "HTTP_WAIT_TIMEOUT";
	private static final String PROPERTY_HTTP_CHECK_DELAY = "HTTP_CHECK_DELAY";
	private static final String DEFAULT_FILE_LOCATION = "src/test/resources/test.properties";

	/**
	 * @return the base URL that should be used for all tests
	 */
	public String getBaseURL() {
		return getStringProperty(PROPERTY_BASE_URL);
	}

	/**
	 * @return the name of the Selenium driver
	 */
	public String getDriverName() {
		return getStringProperty(PROPERTY_DRIVER_NAME);
	}

	/**
	 * @return the path where the driver is located
	 */
	public String getDriverPath() {
		return getStringProperty(PROPERTY_DRIVER_PATH);
	}

	/**
	 * @return the fully qualified class name of the Selenium driver
	 */
	public String getDriverClassName() {
		return getStringProperty(PROPERTY_DRIVER_CLASS);
	}

	/**
	 * @return the page load timeout in seconds
	 */
	public long getPageLoadTimeout() {
		return getLongProperty(PROPERTY_PAGE_LOAD_TIMEOUT);
	}

	/**
	 * @return the implicit wait time in seconds
	 */
	public long getImplicitWaitTime() {
		return getLongProperty(PROPERTY_IMPLICIT_WAIT_TIME);
	}

	/**
	 * @return the time in milliseconds for delaying a test after creating a page object
	 */
	public long getPageLoadDelay() {
		return getLongProperty(PROPERTY_PAGE_LOAD_DELAY);
	}

	/**
	 * @return the time in seconds to wait until all pending HTTP requests should have been finished
	 */
	public long getHTTPTimeout() {
		return getLongProperty(PROPERTY_HTTP_WAIT_TIMEOUT);
	}

	/**
	 * @return the time in milliseconds to wait before checking for pending HTTP requests
	 */
	public long getHTTPCheckDelay() {
		return getLongProperty(PROPERTY_HTTP_CHECK_DELAY);
	}

	/**
	 * @return true if the browser window should be maximized
	 */
	public boolean isMaximizeWindow() {
		final String propertyValue = getStringProperty(PROPERTY_DRIVER_MAXIMIZE);

		return Boolean.parseBoolean(propertyValue);
	}

	/**
	 * @return the fully qualified name of the data provider class
	 */
	public String getDataProviderClassName() {
		return getStringProperty(PROPERTY_DATA_PROVIDER_CLASS);
	}

	/**
	 * @return the relative path in the workspace where test data files are located
	 */
	public String getTestDataPath() {
		return getStringProperty(PROPERTY_TEST_DATA_PATH);
	}

	/**
	 * @return the fully qualified name of the Selenium WebDriver producer class
	 */
	public String getDriverProducerClass() {
		return getStringProperty(PROPERTY_DRIVER_PRODUCER_CLASS);
	}

	/**
	 * @param propertyName
	 * @return the string value of a property
	 */
	public String getStringProperty(String propertyName) {
		final String propertyValue = getProperty(propertyName);

		if (propertyValue == null)
			logger.warn("Property '{}' not found!", propertyName);
		else
			logger.trace("Property '{}' with value '{}' found", propertyName, propertyValue);

		return propertyValue;
	}

	/**
	 * @param propertyName
	 * @return the long value of a property
	 */
	public long getLongProperty(String propertyName) {
		final String propertyValue = getStringProperty(propertyName);

		if (propertyValue != null)
			try {
				return Long.parseLong(propertyValue);
			}
			catch (final NumberFormatException e) {
				logger.warn("Value '{}' for property '{}' could not be converted to long!", propertyValue, propertyName);
			}

		return 0;
	}

	/**
	 * Load the properties from the default resource file
	 */
	public void load() {
		logger.debug("Load test properties from file '{}'", DEFAULT_FILE_LOCATION);

		final var propertyFile = new File(DEFAULT_FILE_LOCATION);

		try (FileInputStream fis = new FileInputStream(propertyFile)) {
			load(fis);
		}
		catch (final Exception e) {
			final var message = "Error while loading test properties from file!";

			logger.error(message, e);

			throw new RuntimeException(message, e);
		}
	}

}
