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

/**
 * <p>
 * Converter for sets containing values of type {@link Double}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Converter
public class DoubleSetToStringConverter extends AbstractSetToStringConverter<Double> {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.jpa.converter.element.set.AbstractSetToStringConverter#convertToString(java.lang.String)
	 */
	@Override
	protected String convertToString(Double value) {
		return String.format("%.10f", value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.jpa.converter.element.set.AbstractSetToStringConverter#convertToValue(java.lang.String)
	 */
	@Override
	protected Double convertToValue(String string) {
		return Double.parseDouble(string);
	}
}
