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

import java.io.Serializable;

/**
 * <p>
 * Data transfer object that holds format information
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormatDTO implements Serializable {
	private static final long serialVersionUID = 1L;

	private final String dateFormat;
	private final String dateTimeFormat;
	private final String decimalFormat;
	private final String csvSeparator;

	/**
	 * Create new format DTO
	 * @param dateFormat
	 * @param dateTimeFormat
	 * @param decimalFormat
	 * @param csvSeparator
	 */
	public FormatDTO(String dateFormat, String dateTimeFormat, String decimalFormat, String csvSeparator) {
		this.dateFormat = dateFormat;
		this.dateTimeFormat = dateTimeFormat;
		this.decimalFormat = decimalFormat;
		this.csvSeparator = csvSeparator;
	}

	/**
	 * @return the date format
	 */
	public String getDateFormat() {
		return dateFormat;
	}

	/**
	 * @return the date time format
	 */
	public String getDateTimeFormat() {
		return dateTimeFormat;
	}

	/**
	 * @return the decimal format
	 */
	public String getDecimalFormat() {
		return decimalFormat;
	}

	/**
	 * @return the CSV separator character(s)
	 */
	public String getCsvSeparator() {
		return csvSeparator;
	}

}
