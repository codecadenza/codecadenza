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
package net.codecadenza.runtime.richclient.eclipse.rap.services;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import org.eclipse.rap.rwt.RWT;
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

	public static final String ATTR_DATE_FORMAT = "date_format";
	public static final String ATTR_DATE_TIME_FORMAT = "date_time_format";
	public static final String ATTR_DECIMAL_FORMAT = "decimal_format";
	public static final String ATTR_CSV_SEPARATOR = "csv_separator";

	/**
	 * Prevent instantiation
	 */
	private FormatPreferencesManager() {

	}

	/**
	 * Save the format
	 * @param settings the data transfer object that contains formatting information to be saved on local hard drive
	 */
	public static synchronized void saveFormat(FormatDTO settings) {
		try {
			RWT.getSettingStore().setAttribute(ATTR_DATE_FORMAT, settings.getDateFormat());
			RWT.getSettingStore().setAttribute(ATTR_DATE_TIME_FORMAT, settings.getDateTimeFormat());
			RWT.getSettingStore().setAttribute(ATTR_DECIMAL_FORMAT, settings.getDecimalFormat());
			RWT.getSettingStore().setAttribute(ATTR_CSV_SEPARATOR, settings.getCsvSeparator());
		}
		catch (final IOException e) {
			logger.error("Error while saving user format preferences!", e);
		}
	}

	/**
	 * Get the format object
	 * @return the data transfer object containing formatting information
	 */
	public static FormatDTO getFormatDTO() {
		String dateFormat = RWT.getSettingStore().getAttribute(ATTR_DATE_FORMAT);

		if (dateFormat == null || dateFormat.isEmpty())
			dateFormat = DATE_FORMAT;

		String dateTimeFormat = RWT.getSettingStore().getAttribute(ATTR_DATE_TIME_FORMAT);

		if (dateTimeFormat == null || dateTimeFormat.isEmpty())
			dateTimeFormat = DATE_TIME_FORMAT;

		String decimalFormat = RWT.getSettingStore().getAttribute(ATTR_DECIMAL_FORMAT);

		if (decimalFormat == null || decimalFormat.isEmpty())
			decimalFormat = DECIMAL_FORMAT;

		String csvSep = RWT.getSettingStore().getAttribute(ATTR_CSV_SEPARATOR);

		if (csvSep == null || csvSep.isEmpty())
			csvSep = CSV_SEPARATOR;

		return new FormatDTO(dateFormat, dateTimeFormat, decimalFormat, csvSep);
	}

}
