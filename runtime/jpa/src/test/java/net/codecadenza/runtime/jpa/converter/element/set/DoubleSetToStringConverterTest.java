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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Set;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the {@link DoubleSetToStringConverter}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class DoubleSetToStringConverterTest {
	private static final double VALUE_1 = 113.0123456789;
	private static final double VALUE_2 = -13.9876543211;

	private final DoubleSetToStringConverter converter = new DoubleSetToStringConverter();

	@Test
	void testConvert() {
		final Set<Double> values = Set.of(VALUE_1, VALUE_2);

		final String stringValue = converter.convertToDatabaseColumn(values);

		assertEquals(values, converter.convertToEntityAttribute(stringValue));
	}
}
