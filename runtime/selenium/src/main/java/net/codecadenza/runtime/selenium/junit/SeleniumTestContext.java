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
import java.time.Duration;
import net.codecadenza.runtime.selenium.data.ITestDataProvider;
import net.codecadenza.runtime.selenium.util.SeleniumTestProperties;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.Sleeper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * The test context is responsible for providing essential objects for Selenium tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumTestContext {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final WebDriver driver;
	private final ITestDataProvider dataProvider;
	private final SeleniumTestProperties properties;
	private final String baseURL;
	private boolean closeDriver;

	/**
	 * Constructor
	 * @param driver
	 * @param dataProvider
	 * @param properties
	 */
	public SeleniumTestContext(WebDriver driver, ITestDataProvider dataProvider, SeleniumTestProperties properties) {
		this.driver = driver;
		this.dataProvider = dataProvider;
		this.properties = properties;
		this.baseURL = properties.getBaseURL();
	}

	/**
	 * @return the Selenium WebDriver
	 */
	public WebDriver getDriver() {
		return driver;
	}

	/**
	 * @return the test data provider
	 */
	public ITestDataProvider getDataProvider() {
		return dataProvider;
	}

	/**
	 * @return the base URL
	 */
	public String getBaseURL() {
		return baseURL;
	}

	/**
	 * @return true if the driver should be closed after finishing the test
	 */
	public boolean isCloseDriver() {
		return closeDriver;
	}

	/**
	 * @param closeDriver
	 */
	public void setCloseDriver(boolean closeDriver) {
		this.closeDriver = closeDriver;
	}

	/**
	 * @return the global test properties
	 */
	public SeleniumTestProperties getProperties() {
		return properties;
	}

	/**
	 * Delay the test by putting the current thread to sleep
	 * @param millis the delay time in milliseconds
	 */
	public void delayTest(long millis) {
		try {
			Sleeper.SYSTEM_SLEEPER.sleep(Duration.ofMillis(millis));
		}
		catch (final InterruptedException e) {
			Thread.currentThread().interrupt();

			logger.warn("Test delay has been interrupted!", e);
		}
	}

}
