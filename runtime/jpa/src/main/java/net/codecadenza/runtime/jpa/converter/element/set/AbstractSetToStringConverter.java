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

import static net.codecadenza.runtime.jpa.converter.element.ElementCollectionConverterUtil.DELIMITER;
import static net.codecadenza.runtime.jpa.converter.element.ElementCollectionConverterUtil.QUOTE;

import jakarta.persistence.AttributeConverter;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.runtime.jpa.converter.element.ElementCollectionConverterUtil;

/**
 * <p>
 * Base class for all converters that are responsible for converting a set of values to a string
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the values in the set
 */
public abstract class AbstractSetToStringConverter<T> implements AttributeConverter<Set<T>, String> {
	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.AttributeConverter#convertToDatabaseColumn(java.lang.Object)
	 */
	@Override
	public String convertToDatabaseColumn(Set<T> values) {
		if (values == null || values.isEmpty())
			return null;

		return values.stream().map(this::convertToString).map(ElementCollectionConverterUtil::escapeQuotes)
				.map(element -> QUOTE + element + QUOTE).collect(Collectors.joining(DELIMITER));
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.AttributeConverter#convertToEntityAttribute(java.lang.Object)
	 */
	@Override
	public Set<T> convertToEntityAttribute(String string) {
		final var elementSet = new HashSet<T>();

		if (string == null || string.isEmpty())
			return elementSet;

		final List<String> elements = ElementCollectionConverterUtil.extractElements(string);

		elements.stream().map(this::convertToValue).forEach(elementSet::add);

		return elementSet;
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
