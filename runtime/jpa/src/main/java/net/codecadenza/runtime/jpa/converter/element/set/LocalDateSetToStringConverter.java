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
package net.codecadenza.runtime.jpa.converter.element.set;

import jakarta.persistence.Converter;
import java.time.LocalDate;
import net.codecadenza.runtime.conversion.ValueConverter;

/**
 * <p>
 * Converter for sets containing values of type {@link LocalDate}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Converter
public class LocalDateSetToStringConverter extends AbstractSetToStringConverter<LocalDate> {
	public static final String DATE_FORMAT = "dd.MM.yyyy";
	private static final ValueConverter<LocalDate> valueConverter = new ValueConverter<>(null, null, DATE_FORMAT, LocalDate.class);

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.jpa.converter.element.set.AbstractSetToStringConverter#convertToString(java.lang.String)
	 */
	@Override
	protected String convertToString(LocalDate value) {
		return valueConverter.convertToString(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.jpa.converter.element.set.AbstractSetToStringConverter#convertToValue(java.lang.String)
	 */
	@Override
	protected LocalDate convertToValue(String string) {
		return valueConverter.convertToValue(string);
	}
}
