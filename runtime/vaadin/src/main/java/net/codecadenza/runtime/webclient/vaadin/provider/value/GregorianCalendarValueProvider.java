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
package net.codecadenza.runtime.webclient.vaadin.provider.value;

import com.vaadin.flow.function.ValueProvider;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

/**
 * <p>
 * Value provider for columns that are mapped to a {@link GregorianCalendar} field. The values are converted to a string by using
 * the provided date format.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the object that contains the actual value
 */
public class GregorianCalendarValueProvider<T> implements ValueProvider<T, String> {
	private static final long serialVersionUID = -6186568809886029231L;

	private final ValueProvider<T, GregorianCalendar> input;
	private final SimpleDateFormat dateTimeFormat;

	/**
	 * Constructor
	 * @param dateTimeFormat
	 * @param input
	 */
	public GregorianCalendarValueProvider(String dateTimeFormat, ValueProvider<T, GregorianCalendar> input) {
		this.input = input;
		this.dateTimeFormat = new SimpleDateFormat(dateTimeFormat);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.ValueProvider#apply(java.lang.Object)
	 */
	@Override
	public String apply(T source) {
		final GregorianCalendar value = input.apply(source);

		return value != null ? dateTimeFormat.format(value.getTime()) : "";
	}

}
