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

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.Set;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the {@link LocalDateTimeSetToStringConverter}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class LocalDateTimeSetToStringConverterTest {
	private static final String VALUE_1 = "01.02.2025 11:21:13";
	private static final String VALUE_2 = "02.02.2025 12:01:00";

	private final LocalDateTimeSetToStringConverter converter = new LocalDateTimeSetToStringConverter();

	@Test
	void testConvert() {
		final DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(LocalDateTimeSetToStringConverter.DATE_TIME_FORMAT)
				.withZone(ZoneId.systemDefault());
		final TemporalAccessor temporalValue1 = dateTimeFormat.parse(VALUE_1);
		final LocalDateTime value1 = LocalDateTime.from(temporalValue1);
		final TemporalAccessor temporalValue2 = dateTimeFormat.parse(VALUE_2);
		final LocalDateTime value2 = LocalDateTime.from(temporalValue2);
		final Set<LocalDateTime> values = Set.of(value1, value2);

		final String stringValue = converter.convertToDatabaseColumn(values);

		assertEquals(values, converter.convertToEntityAttribute(stringValue));
	}
}
