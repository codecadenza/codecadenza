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

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

/**
 * <p>
 * {@link GregorianCalendar} to {@link String} converter that uses a given format pattern
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StringToCalendarConverter implements Converter<String, GregorianCalendar> {
	private static final long serialVersionUID = -6252963452762707747L;

	private final SimpleDateFormat dateFormat;

	/**
	 * Constructor
	 * @param dateFormatPattern
	 */
	public StringToCalendarConverter(String dateFormatPattern) {
		this.dateFormat = new SimpleDateFormat(dateFormatPattern);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.Converter#convertToModel(java.lang.Object, com.vaadin.data.ValueContext)
	 */
	@Override
	public Result<GregorianCalendar> convertToModel(String value, ValueContext context) {
		if (value == null || value.isEmpty())
			return Result.ok(null);

		try {
			final var cal = new GregorianCalendar();
			cal.setTime(dateFormat.parse(value));

			return Result.ok(cal);
		}
		catch (final ParseException e) {
			return Result.error(e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.Converter#convertToPresentation(java.lang.Object, com.vaadin.data.ValueContext)
	 */
	@Override
	public String convertToPresentation(GregorianCalendar value, ValueContext context) {
		if (value == null)
			return null;

		return dateFormat.format(value.getTime());
	}

}
