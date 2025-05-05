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
import java.util.UUID;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the {@link UUIDSetToStringConverter}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class UUIDSetToStringConverterTest {
	private static final UUID VALUE_1 = UUID.fromString("a80b904f-d60f-409a-b0a1-11d955e57671");
	private static final UUID VALUE_2 = UUID.fromString("a55559d2-cdba-4ba6-80d1-ff0c40cd49f2");

	private final UUIDSetToStringConverter converter = new UUIDSetToStringConverter();

	@Test
	void testConvert() {
		final Set<UUID> values = Set.of(VALUE_1, VALUE_2);

		final String stringValue = converter.convertToDatabaseColumn(values);

		assertEquals(values, converter.convertToEntityAttribute(stringValue));
	}
}
