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
package net.codecadenza.runtime.property;

import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Simple utility that provides methods to access properties. Note that some properties have to be provided in a predefined format
 * within the properties file: java.util.Date: "dd.MM.yyyy HH:mm:ss" double: "0.00" float: "0.00" Furthermore it is important that
 * the application that uses this service has to provide a property file at a predefined location within the library!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PropertyService {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String PROP_EXCHANGE_FOLDER = "codecadenza.application.file-exchange-folder";
	public static final String PROP_LOGGING_LEVEL = "codecadenza.application.logging-level";
	public static final String PROP_TRANSPORT_AUTHENTICATION = "codecadenza.application.transport.authentication.enabled";
	public static final String PROP_TRANSPORT_SECRET = "codecadenza.application.transport.secret";
	public static final String PROP_REPOSITORY_FOLDER = "codecadenza.application.document-repository-folder";
	private static final String FILE_NAME = "application.properties";
	private static final String DEFAULT_PATH = "config/" + FILE_NAME;
	private static final String DATE_FORMAT = "dd.MM.yyyy HH:mm:ss";
	private static final String DECIMAL_FORMAT = "0.00";

	private boolean propertiesLoaded;
	private Properties properties;
	private String pathToFile = DEFAULT_PATH;

	/**
	 * Constructor
	 */
	public PropertyService() {

	}

	/**
	 * Constructor
	 * @param pathToFile
	 */
	public PropertyService(String pathToFile) {
		this.pathToFile = pathToFile;
	}

	/**
	 * Get a property
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException it the property doesn't exist
	 */
	private String getProperty(String name) {
		if (!propertiesLoaded)
			loadProperties();

		if (!properties.containsKey(name))
			throw new IllegalPropertyValueException("Property '" + name + "' does not exist!");

		final String value = properties.getProperty(name);

		if (name.equals(PROP_TRANSPORT_SECRET))
			logger.debug("Property '{}' with value '******' found", name);
		else
			logger.debug("Property '{}' with value '{}' found", name, value);

		return value;
	}

	/**
	 * Get a property of type String
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property doesn't exist
	 */
	public String getStringProperty(String name) {
		return getProperty(name);
	}

	/**
	 * Get a property of type int
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent an integer
	 */
	public int getIntProperty(String name) {
		final String propertyValue = getProperty(name);

		try {
			return Integer.parseInt(propertyValue);
		}
		catch (final NumberFormatException e) {
			throw new IllegalPropertyValueException("Property " + name + " does not represent a valid integer value!");
		}
	}

	/**
	 * Get a property of type long
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent a long
	 */
	public long getLongProperty(String name) {
		final String propertyValue = getProperty(name);

		try {
			return Long.parseLong(propertyValue);
		}
		catch (final NumberFormatException e) {
			throw new IllegalPropertyValueException("Property " + name + " does not represent a valid long value!");
		}
	}

	/**
	 * Get a property of type double
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent a double
	 */
	public double getDoubleProperty(String name) {
		final String propertyValue = getProperty(name);
		final var decimalFormat = new DecimalFormat(DECIMAL_FORMAT);

		try {
			final Number n = decimalFormat.parse(propertyValue);
			return n.doubleValue();
		}
		catch (final ParseException e) {
			throw new IllegalPropertyValueException("Property " + name + " does not represent a valid double value!");
		}
	}

	/**
	 * Get a property of type float
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent a float
	 */
	public float getFloatProperty(String name) {
		final String propertyValue = getProperty(name);
		final var decimalFormat = new DecimalFormat(DECIMAL_FORMAT);

		try {
			final Number n = decimalFormat.parse(propertyValue);
			return n.floatValue();
		}
		catch (final ParseException e) {
			throw new IllegalPropertyValueException("Property " + name + " does not represent a valid float value!");
		}
	}

	/**
	 * Get a property of type char
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent a character
	 */
	public char getCharProperty(String name) {
		final String propertyValue = getProperty(name);

		if (propertyValue.isEmpty() || propertyValue.length() > 1)
			throw new IllegalPropertyValueException("Property " + name + " does not represent a valid character value!");

		return propertyValue.charAt(0);
	}

	/**
	 * Get a property of type java.util.Date
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent a date
	 */
	public synchronized Date getDateProperty(String name) {
		final String propertyValue = getProperty(name);
		final var dateFormat = new SimpleDateFormat(DATE_FORMAT);

		try {
			return dateFormat.parse(propertyValue);
		}
		catch (final ParseException e) {
			throw new IllegalPropertyValueException("Property " + name + " does not represent a valid java.util.Date value!");
		}
	}

	/**
	 * Get a property of type boolean
	 * @param name the property name
	 * @return the property value
	 * @throws IllegalPropertyValueException if the property value doesn't represent a boolean
	 */
	public boolean getBooleanProperty(String name) {
		final String propertyValue = getProperty(name);

		return Boolean.parseBoolean(propertyValue);
	}

	/**
	 * Load the properties from the respective file
	 * @throws IllegalStateException if the method was not able to load the given configuration file
	 */
	private synchronized void loadProperties() {
		logger.debug("Loading properties from file '{}'", pathToFile);

		final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

		InputStream inputStream = classLoader.getResourceAsStream(pathToFile);

		if (inputStream == null) {
			inputStream = classLoader.getResourceAsStream(FILE_NAME);

			if (inputStream == null)
				throw new IllegalStateException("The file 'application.properties' could not be found!");
		}

		try {
			properties = new Properties();
			properties.load(inputStream);
		}
		catch (final Exception e) {
			logger.error("Error while loading properties!", e);

			throw new IllegalStateException("Error while loading properties! Message: " + e.getMessage());
		}
		finally {
			try {
				inputStream.close();
			}
			catch (final IOException e) {
				logger.warn("Could not close input stream!", e);
			}
		}

		propertiesLoaded = true;
	}

}
