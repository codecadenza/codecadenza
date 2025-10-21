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
package net.codecadenza.runtime.ddt.service;

import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.service.ServiceInitializationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Factory for creating a test data provider instance. The provider that should be used can be selected by the
 * 'TEST_DATA_PROVIDER_CLASS_NAME' property in the respective properties file.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestDataProviderFactory {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * Prevent instantiation
	 */
	private TestDataProviderFactory() {

	}

	/**
	 * Create and initialize a {@link ITestDataProvider} instance based on the respective properties
	 * @param id the ID for finding the respective {@link TestData}
	 * @param properties the properties that should be used
	 * @return a {@link ITestDataProvider} instance
	 * @throws ServiceInitializationException if the {@link ITestDataProvider} could not be initialized
	 */
	public static ITestDataProvider getTestDataProvider(Object id, TestDataProviderProperties properties) {
		logger.debug("Create test data provider '{}'", properties.getTestDataProviderClassName());

		try {
			final Class<? extends ITestDataProvider> providerClass = Class.forName(properties.getTestDataProviderClassName())
					.asSubclass(ITestDataProvider.class);
			return providerClass.getConstructor(id.getClass(), TestDataProviderProperties.class).newInstance(id, properties);
		}
		catch (final Exception ex) {
			throw new ServiceInitializationException("Could not create test data provider!", ex);
		}
	}

}
