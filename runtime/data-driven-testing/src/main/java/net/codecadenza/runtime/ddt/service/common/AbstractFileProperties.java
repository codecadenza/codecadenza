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
package net.codecadenza.runtime.ddt.service.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.invoke.MethodHandles;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for providing properties loaded from a file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFileProperties extends Properties {
	private static final long serialVersionUID = -532068038412219206L;
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final String DEFAULT_FILE_LOCATION = "src/test/resources/test.properties";

	/**
	 * Get a string property
	 * @param propertyName the property name
	 * @return the string
	 */
	public String getStringProperty(String propertyName) {
		final String propertyValue = getProperty(propertyName);

		if (propertyValue == null)
			throw new IllegalStateException("Property '" + propertyName + "' not found!");
		else {
			logger.trace("Property '{}' with value '{}' found", propertyName, propertyValue);
			return propertyValue;
		}
	}

	/**
	 * Get a property of type long
	 * @param propertyName the property name
	 * @return the long value
	 */
	public long getLongProperty(String propertyName) {
		final String propertyValue = getStringProperty(propertyName);

		if (propertyValue != null)
			try {
				return Long.parseLong(propertyValue);
			}
			catch (final NumberFormatException _) {
				throw new IllegalStateException(
						"The value '" + propertyValue + "' for property '" + propertyName + "' could not be converted to long!");
			}
		else
			return 0L;
	}

	/**
	 * Get a character property
	 * @param propertyName the property name
	 * @return the character
	 */
	public char getCharacterProperty(String propertyName) {
		final String propertyValue = getStringProperty(propertyName);

		if (propertyValue != null && propertyValue.length() == 1)
			return propertyValue.charAt(0);
		else
			throw new IllegalStateException("The property '" + propertyName + "' requires one character");
	}

	/**
	 * Load the test properties from a given file
	 * @param testPropertiesFile the file where the test properties are stored
	 */
	public void load(File testPropertiesFile) {
		logger.debug("Load test properties from file '{}'", testPropertiesFile.getAbsolutePath());

		try (final FileInputStream fis = new FileInputStream(testPropertiesFile)) {
			load(fis);
		}
		catch (final IOException ex) {
			throw new UncheckedIOException("Error while loading test properties from file!", ex);
		}
	}

	/**
	 * Load the properties from the the default location
	 */
	public void load() {
		load(new File(DEFAULT_FILE_LOCATION));
	}

}
