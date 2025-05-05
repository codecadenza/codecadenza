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
package net.codecadenza.runtime.jpa.converter.element.list;

import static net.codecadenza.runtime.jpa.converter.element.ElementCollectionConverterUtil.DELIMITER;
import static net.codecadenza.runtime.jpa.converter.element.ElementCollectionConverterUtil.QUOTE;

import jakarta.persistence.AttributeConverter;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import net.codecadenza.runtime.jpa.converter.element.ElementCollectionConverterUtil;

/**
 * <p>
 * Base class for all converters that are responsible for converting value lists to a string
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the values in the list
 */
public abstract class AbstractListToStringConverter<T> implements AttributeConverter<List<T>, String> {
	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.AttributeConverter#convertToDatabaseColumn(java.lang.Object)
	 */
	@Override
	public String convertToDatabaseColumn(List<T> valueList) {
		if (valueList == null || valueList.isEmpty())
			return null;

		return valueList.stream().map(this::convertToString).map(ElementCollectionConverterUtil::escapeQuotes)
				.map(element -> QUOTE + element + QUOTE).collect(Collectors.joining(DELIMITER));
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.AttributeConverter#convertToEntityAttribute(java.lang.Object)
	 */
	@Override
	public List<T> convertToEntityAttribute(String string) {
		if (string == null || string.isEmpty())
			return new ArrayList<>();

		final List<String> elements = ElementCollectionConverterUtil.extractElements(string);

		return elements.stream().map(this::convertToValue).collect(Collectors.toCollection(ArrayList::new));
	}

	/**
	 * Convert the given value to a string
	 * @param value the value to be converted
	 * @return the string representation of the value
	 */
	protected String convertToString(T value) {
		return value.toString();
	}

	/**
	 * Convert the given string to the requested type
	 * @param string the string to be converted
	 * @return the value
	 */
	protected abstract T convertToValue(String string);

}
