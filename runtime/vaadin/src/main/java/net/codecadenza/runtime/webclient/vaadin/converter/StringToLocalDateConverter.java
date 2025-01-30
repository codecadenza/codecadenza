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
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * <p>
 * {@link LocalDate} to {@link String} converter that uses a given format pattern
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StringToLocalDateConverter implements Converter<String, LocalDate> {
	private static final long serialVersionUID = -8949112185187059720L;

	private final transient DateTimeFormatter formatter;

	/**
	 * Constructor
	 * @param dateFormatPattern
	 */
	public StringToLocalDateConverter(String dateFormatPattern) {
		this.formatter = DateTimeFormatter.ofPattern(dateFormatPattern).withZone(ZoneId.systemDefault());
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.Converter#convertToModel(java.lang.Object, com.vaadin.data.ValueContext)
	 */
	@Override
	public Result<LocalDate> convertToModel(String value, ValueContext context) {
		if (value == null || value.isEmpty())
			return Result.ok(null);

		try {
			return Result.ok(LocalDate.from(formatter.parse(value)));
		}
		catch (final Exception e) {
			return Result.error(e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.Converter#convertToPresentation(java.lang.Object, com.vaadin.data.ValueContext)
	 */
	@Override
	public String convertToPresentation(LocalDate value, ValueContext context) {
		if (value == null)
			return null;

		return formatter.format(value);
	}

}
