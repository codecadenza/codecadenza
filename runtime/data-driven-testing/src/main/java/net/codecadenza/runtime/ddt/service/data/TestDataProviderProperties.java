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
package net.codecadenza.runtime.ddt.service.data;

import net.codecadenza.runtime.ddt.service.common.AbstractFileProperties;

/**
 * <p>
 * Properties that are used to initialize an {@link ITestDataProvider} implementation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestDataProviderProperties extends AbstractFileProperties {
	private static final long serialVersionUID = -2010023816819264967L;
	private static final String PROPERTY_PROVIDER_CLASS_NAME = "TEST_DATA_PROVIDER_CLASS_NAME";
	private static final String PROPERTY_DATE_FORMAT = "DATE_FORMAT";
	private static final String PROPERTY_DATE_TIME_FORMAT = "DATE_TIME_FORMAT";
	private static final String PROPERTY_DECIMAL_FORMAT = "DECIMAL_FORMAT";
	private static final String PROPERTY_GROUPING_SEPARATOR = "GROUPING_SEPARATOR";
	private static final String PROPERTY_DECIMAL_SEPARATOR = "DECIMAL_SEPARATOR";

	/**
	 * @return the fully-qualified provider class name of the {@link ITestDataProvider} that should be used
	 */
	public String getTestDataProviderClassName() {
		return getStringProperty(PROPERTY_PROVIDER_CLASS_NAME);
	}

	/**
	 * @return the date format
	 */
	public String getDateFormat() {
		return getStringProperty(PROPERTY_DATE_FORMAT);
	}

	/**
	 * @return the date time format
	 */
	public String getDateTimeFormat() {
		return getStringProperty(PROPERTY_DATE_TIME_FORMAT);
	}

	/**
	 * @return the decimal format
	 */
	public String getDecimalFormat() {
		return getStringProperty(PROPERTY_DECIMAL_FORMAT);
	}

	/**
	 * @return the decimal grouping separator character
	 */
	public char getGroupingSeparator() {
		return getCharacterProperty(PROPERTY_GROUPING_SEPARATOR);
	}

	/**
	 * @return decimal separator character
	 */
	public char getDecimalSeparator() {
		return getCharacterProperty(PROPERTY_DECIMAL_SEPARATOR);
	}

}
