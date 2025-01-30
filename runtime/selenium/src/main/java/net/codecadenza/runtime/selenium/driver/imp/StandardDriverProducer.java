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
package net.codecadenza.runtime.selenium.driver.imp;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.time.Duration;
import net.codecadenza.runtime.selenium.driver.IDriverProducer;
import net.codecadenza.runtime.selenium.util.SeleniumTestProperties;
import org.openqa.selenium.WebDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Standard implementation of a producer for Selenium WebDriver objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StandardDriverProducer implements IDriverProducer {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private SeleniumTestProperties properties;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.driver.IDriverProducer#
	 * setProperties(net.codecadenza.runtime.selenium.util.SeleniumTestProperties)
	 */
	@Override
	public void setProperties(SeleniumTestProperties properties) {
		this.properties = properties;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.driver.IDriverProducer#getDriverInstance()
	 */
	@Override
	public WebDriver getDriverInstance() {
		final String driverName = properties.getDriverName();
		final String driverPath = properties.getDriverPath();

		if (driverName != null && !driverName.isEmpty() && driverPath != null && !driverPath.isEmpty())
			System.setProperty(driverName, driverPath);

		try {
			logger.debug("Create driver '{}'", properties.getDriverClassName());

			final Class<? extends WebDriver> webDriverClass = Class.forName(properties.getDriverClassName())
					.asSubclass(WebDriver.class);
			final Constructor<? extends WebDriver> constructor = webDriverClass.getConstructor();

			final WebDriver driver = constructor.newInstance();
			driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(properties.getImplicitWaitTime()));
			driver.manage().timeouts().pageLoadTimeout(Duration.ofSeconds(properties.getPageLoadTimeout()));

			if (properties.isMaximizeWindow())
				driver.manage().window().maximize();

			return driver;
		}
		catch (final Exception e) {
			final var message = "Error while creating Selenium WebDriver!";

			logger.error(message, e);

			throw new RuntimeException(e);
		}
	}

}
