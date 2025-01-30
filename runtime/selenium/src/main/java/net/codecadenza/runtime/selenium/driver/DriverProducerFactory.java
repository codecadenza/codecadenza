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
package net.codecadenza.runtime.selenium.driver;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import net.codecadenza.runtime.selenium.util.SeleniumTestProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Factory for objects that create and initialize a Selenium WebDriver. The producer that should be used can be selected by the
 * 'DRIVER_PRODUCER_CLASS' property in the test.properties file.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DriverProducerFactory {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * Prevent instantiation
	 */
	private DriverProducerFactory() {

	}

	/**
	 * @param properties
	 * @return an instance of the selected Selenium WebDriver producer
	 * @throws RuntimeException if the Selenium WebDriver producer could not be initialized
	 */
	public static IDriverProducer getDriverProducer(SeleniumTestProperties properties) {
		logger.debug("Create Selenium WebDriver producer '{}'", properties.getDriverProducerClass());

		try {
			final Class<? extends IDriverProducer> driverProducerImpClass = Class.forName(properties.getDriverProducerClass())
					.asSubclass(IDriverProducer.class);
			final Constructor<? extends IDriverProducer> constructor = driverProducerImpClass.getConstructor();

			// Initialize the driver producer
			final IDriverProducer driverProducer = constructor.newInstance();
			driverProducer.setProperties(properties);

			return driverProducer;
		}
		catch (final Exception e) {
			final var message = "Error while creating producer for Selenium WebDriver!";

			logger.error(message, e);

			throw new RuntimeException(message, e);
		}
	}

}
