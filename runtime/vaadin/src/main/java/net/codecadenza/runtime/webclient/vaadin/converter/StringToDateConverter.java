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
package net.codecadenza.runtime.webclient.vaadin.converter;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * <p>
 * {@link String} to {@link Date} converter that uses a given format pattern
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StringToDateConverter extends com.vaadin.flow.data.converter.StringToDateConverter {
	private static final long serialVersionUID = 5491798688405882135L;

	private final SimpleDateFormat dateFormat;

	/**
	 * Constructor
	 * @param dateFormatPattern
	 */
	public StringToDateConverter(String dateFormatPattern) {
		this.dateFormat = new SimpleDateFormat(dateFormatPattern);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.util.converter.StringToDateConverter#getFormat(java.util.Locale)
	 */
	@Override
	protected DateFormat getFormat(Locale locale) {
		return dateFormat;
	}

}
