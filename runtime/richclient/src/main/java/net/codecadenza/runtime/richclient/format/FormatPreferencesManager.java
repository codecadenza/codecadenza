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
package net.codecadenza.runtime.richclient.format;

import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.richclient.property.UserPropertyManager;
import net.codecadenza.runtime.richclient.transport.ServiceLocator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * The format preferences manager takes control over reading and writing format-specific information
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormatPreferencesManager {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String DATE_FORMAT = "dd.MM.yyyy";
	public static final String DATE_TIME_FORMAT = "dd.MM.yyyy HH:mm:ss";
	public static final String DECIMAL_FORMAT = "###,###,##0.0000";
	public static final String CSV_SEPARATOR = ",";

	public static final String PROP_KEY_DATE_FORMAT = "date_format";
	public static final String PROP_KEY_DATE_TIME_FORMAT = "date_time_format";
	public static final String PROP_KEY_DECIMAL_FORMAT = "decimal_format";
	public static final String PROP_KEY_CSV_SEPARATOR = "csv_separator";

	/**
	 * Prevent instantiation
	 */
	private FormatPreferencesManager() {

	}

	/**
	 * Save the format
	 * @param dto the data transfer object that contains formatting information to be saved in a local file
	 */
	public static synchronized void saveFormat(FormatDTO dto) {
		try {
			UserPropertyManager.setProperty(ServiceLocator.getUserName(), PROP_KEY_DATE_FORMAT, dto.getDateFormat());
			UserPropertyManager.setProperty(ServiceLocator.getUserName(), PROP_KEY_DATE_TIME_FORMAT, dto.getDateTimeFormat());
			UserPropertyManager.setProperty(ServiceLocator.getUserName(), PROP_KEY_DECIMAL_FORMAT, dto.getDecimalFormat());
			UserPropertyManager.setProperty(ServiceLocator.getUserName(), PROP_KEY_CSV_SEPARATOR, dto.getCsvSeparator());
		}
		catch (final Exception e) {
			logger.error("Error while saving user format preferences!", e);
		}
	}

	/**
	 * Get the format data transfer object
	 * @return the data transfer object containing formatting information
	 */
	public static FormatDTO getFormatDTO() {
		String dateFormat = UserPropertyManager.getPropertyValue(ServiceLocator.getUserName(), PROP_KEY_DATE_FORMAT);

		if (dateFormat == null)
			dateFormat = DATE_FORMAT;

		String dateTimeFormat = UserPropertyManager.getPropertyValue(ServiceLocator.getUserName(), PROP_KEY_DATE_TIME_FORMAT);

		if (dateTimeFormat == null)
			dateTimeFormat = DATE_TIME_FORMAT;

		String decimalFormat = UserPropertyManager.getPropertyValue(ServiceLocator.getUserName(), PROP_KEY_DECIMAL_FORMAT);

		if (decimalFormat == null)
			decimalFormat = DECIMAL_FORMAT;

		String csvSep = UserPropertyManager.getPropertyValue(ServiceLocator.getUserName(), PROP_KEY_CSV_SEPARATOR);

		if (csvSep == null)
			csvSep = CSV_SEPARATOR;

		return new FormatDTO(dateFormat, dateTimeFormat, decimalFormat, csvSep);
	}

}
