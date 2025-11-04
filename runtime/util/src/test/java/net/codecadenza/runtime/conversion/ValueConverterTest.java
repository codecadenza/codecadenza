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
package net.codecadenza.runtime.conversion;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * <p>
 * Test the functionality of the {@link ValueConverter}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class ValueConverterTest {
	private static final String TEST_STRING = "Hello World";
	private static final char TEST_CHAR = 'X';
	private static final String TEST_CHAR_STRING = "X";
	private static final int TEST_INTEGER = 123;
	private static final String TEST_INTEGER_STRING = "123";
	private static final short TEST_SHORT = 12;
	private static final String TEST_SHORT_STRING = "12";
	private static final byte TEST_BYTE = 0x61;
	private static final String TEST_BYTE_STRING = "97";
	private static final long TEST_LONG = 12345678L;
	private static final String TEST_LONG_STRING = "12345678";
	private static final double TEST_DOUBLE = 123.45678;
	private static final String TEST_DOUBLE_STRING = "123.45678";
	private static final float TEST_FLOAT = 123.45f;
	private static final String TEST_FLOAT_STRING = "123.45000";
	private static final BigDecimal TEST_BIG_DECIMAL = BigDecimal.valueOf(123.45678);
	private static final String TEST_BIG_DECIMAL_STRING = "123.45678";
	private static final String TEST_BOOLEAN_STRING = "true";
	private static final String TEST_DATE_STRING = "2025-01-01 10:11:12";
	private static final String TEST_LOCAL_DATE_STRING = "2025-01-01";
	private static final String TEST_UUID_STRING = "123e4567-e89b-12d3-a456-426655440000";
	private static final String TEST_INVALID_INTEGER = "abc";

	private static final String DECIMAL_FORMAT = "0.00000";
	private static final String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
	private static final String DATE_FORMAT = "yyyy-MM-dd";

	@Test
	void testStringConversion() {
		final var converter = new ValueConverter<>(null, null, null, String.class);
		final String convertedValue = converter.convertToValue(TEST_STRING);

		assertEquals(TEST_STRING, convertedValue);

		final String convertedString = converter.convertToString(TEST_STRING);

		assertEquals(TEST_STRING, convertedString);
	}

	@Test
	void testCharacterConversion() {
		final var converter = new ValueConverter<>(null, null, null, char.class);
		final char convertedValue = converter.convertToValue(TEST_CHAR_STRING);

		assertEquals(TEST_CHAR, convertedValue);

		final String convertedString = converter.convertToString(TEST_CHAR);

		assertEquals(TEST_CHAR_STRING, convertedString);
	}

	@Test
	void testCharacterWrapperConversion() {
		final var converter = new ValueConverter<>(null, null, null, Character.class);
		final Character convertedValue = converter.convertToValue(TEST_CHAR_STRING);

		assertEquals(TEST_CHAR, convertedValue);

		final String convertedString = converter.convertToString(TEST_CHAR);

		assertEquals(TEST_CHAR_STRING, convertedString);
	}

	@Test
	void testIllegalCharacterConversion() {
		final var converter = new ValueConverter<>(null, null, null, Character.class);

		assertThrows(IllegalArgumentException.class, () -> converter.convertToValue(TEST_STRING));

		assertThrows(IllegalArgumentException.class, () -> converter.convertToValue(""));
	}

	@Test
	void testIntegerWrapperConversion() {
		final var converter = new ValueConverter<>(null, null, null, Integer.class);
		final Integer convertedValue = converter.convertToValue(TEST_INTEGER_STRING);

		assertEquals(TEST_INTEGER, convertedValue);

		final String convertedString = converter.convertToString(TEST_INTEGER);

		assertEquals(TEST_INTEGER_STRING, convertedString);
	}

	@Test
	void testIntegerConversion() {
		final var converter = new ValueConverter<>(null, null, null, int.class);
		final int convertedValue = converter.convertToValue(TEST_INTEGER_STRING);

		assertEquals(TEST_INTEGER, convertedValue);

		final String convertedString = converter.convertToString(TEST_INTEGER);

		assertEquals(TEST_INTEGER_STRING, convertedString);
	}

	@Test
	void testShortWrapperConversion() {
		final var converter = new ValueConverter<>(null, null, null, Short.class);
		final Short convertedValue = converter.convertToValue(TEST_SHORT_STRING);

		assertEquals(TEST_SHORT, convertedValue);

		final String convertedString = converter.convertToString(TEST_SHORT);

		assertEquals(TEST_SHORT_STRING, convertedString);
	}

	@Test
	void testShortConversion() {
		final var converter = new ValueConverter<>(null, null, null, short.class);
		final short convertedValue = converter.convertToValue(TEST_SHORT_STRING);

		assertEquals(TEST_SHORT, convertedValue);

		final String convertedString = converter.convertToString(TEST_SHORT);

		assertEquals(TEST_SHORT_STRING, convertedString);
	}

	@Test
	void testByteWrapperConversion() {
		final var converter = new ValueConverter<>(null, null, null, Byte.class);
		final Byte convertedValue = converter.convertToValue(TEST_BYTE_STRING);

		assertEquals(TEST_BYTE, convertedValue);

		final String convertedString = converter.convertToString(TEST_BYTE);

		assertEquals(TEST_BYTE_STRING, convertedString);
	}

	@Test
	void testByteConversion() {
		final var converter = new ValueConverter<>(null, null, null, byte.class);
		final byte convertedValue = converter.convertToValue(TEST_BYTE_STRING);

		assertEquals(TEST_BYTE, convertedValue);

		final String convertedString = converter.convertToString(TEST_BYTE);

		assertEquals(TEST_BYTE_STRING, convertedString);
	}

	@Test
	void testLongWrapperConversion() {
		final var converter = new ValueConverter<>(null, null, null, Long.class);
		final Long convertedValue = converter.convertToValue(TEST_LONG_STRING);

		assertEquals(TEST_LONG, convertedValue);

		final String convertedString = converter.convertToString(TEST_LONG);

		assertEquals(TEST_LONG_STRING, convertedString);
	}

	@Test
	void testLongConversion() {
		final var converter = new ValueConverter<>(null, null, null, long.class);
		final long convertedValue = converter.convertToValue(TEST_LONG_STRING);

		assertEquals(TEST_LONG, convertedValue);

		final String convertedString = converter.convertToString(TEST_LONG);

		assertEquals(TEST_LONG_STRING, convertedString);
	}

	@Test
	void testDoubleConversion() {
		final var converter = new ValueConverter<>(DECIMAL_FORMAT, null, null, double.class);
		final double convertedValue = converter.convertToValue(TEST_DOUBLE_STRING);

		assertEquals(TEST_DOUBLE, convertedValue, 0.01);

		final String convertedString = converter.convertToString(TEST_DOUBLE);

		assertEquals(TEST_DOUBLE_STRING, convertedString);
	}

	@Test
	void testDoubleWrapperConversion() {
		final var converter = ValueConverter.getDoubleConverter(DECIMAL_FORMAT);
		final Double convertedValue = converter.convertToValue(TEST_DOUBLE_STRING);

		assertEquals(TEST_DOUBLE, convertedValue, 0.01);

		final String convertedString = converter.convertToString(TEST_DOUBLE);

		assertEquals(TEST_DOUBLE_STRING, convertedString);
	}

	@Test
	void testFloatConversion() {
		final var converter = new ValueConverter<>(DECIMAL_FORMAT, null, null, float.class);
		final float convertedValue = converter.convertToValue(TEST_FLOAT_STRING);

		assertEquals(TEST_FLOAT, convertedValue, 0.01);

		final String convertedString = converter.convertToString(TEST_FLOAT);

		assertEquals(TEST_FLOAT_STRING, convertedString);
	}

	@Test
	void testFloatWrapperConversion() {
		final var converter = ValueConverter.getFloatConverter(DECIMAL_FORMAT);
		final Float convertedValue = converter.convertToValue(TEST_FLOAT_STRING);

		assertEquals(TEST_FLOAT, convertedValue, 0.01);

		final String convertedString = converter.convertToString(TEST_FLOAT);

		assertEquals(TEST_FLOAT_STRING, convertedString);
	}

	@Test
	void testBigDecimalConversion() {
		final var converter = ValueConverter.getBigDecimalConverter(DECIMAL_FORMAT);
		final BigDecimal convertedValue = converter.convertToValue(TEST_BIG_DECIMAL_STRING);

		assertEquals(TEST_BIG_DECIMAL, convertedValue);

		final String convertedString = converter.convertToString(TEST_BIG_DECIMAL);

		assertEquals(TEST_BIG_DECIMAL_STRING, convertedString);
	}

	@Test
	void testBooleanConversion() {
		final var converter = new ValueConverter<>(null, null, null, boolean.class);
		final boolean convertedValue = converter.convertToValue(TEST_BOOLEAN_STRING);

		assertTrue(convertedValue);

		final String convertedString = converter.convertToString(true);

		assertEquals(TEST_BOOLEAN_STRING, convertedString);
	}

	@Test
	void testBooleanWrapperConversion() {
		final var converter = new ValueConverter<>(null, null, null, Boolean.class);
		final Boolean convertedValue = converter.convertToValue(TEST_BOOLEAN_STRING);

		assertTrue(convertedValue);

		final String convertedString = converter.convertToString(true);

		assertEquals(TEST_BOOLEAN_STRING, convertedString);
	}

	@Test
	void testDateConversion() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, Date.class);
		final var dateTimeFormat = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT).withZone(ZoneId.systemDefault());
		final TemporalAccessor temporalValue = dateTimeFormat.parse(TEST_DATE_STRING);
		final ZonedDateTime expectedValue = LocalDateTime.from(temporalValue).atZone(ZoneId.systemDefault());
		final Date convertedValue = converter.convertToValue(TEST_DATE_STRING);

		assertEquals(Date.from(expectedValue.toInstant()), convertedValue);

		final String convertedString = converter.convertToString(convertedValue);

		assertEquals(TEST_DATE_STRING, convertedString);
	}

	@Test
	void testLocalDateTimeConversion() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, LocalDateTime.class);
		final var dateTimeFormat = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT).withZone(ZoneId.systemDefault());
		final var expectedValue = LocalDateTime.from(dateTimeFormat.parse(TEST_DATE_STRING)).atZone(ZoneId.systemDefault())
				.toLocalDateTime();
		final LocalDateTime convertedValue = converter.convertToValue(TEST_DATE_STRING);

		assertEquals(expectedValue, convertedValue);

		final String convertedString = converter.convertToString(convertedValue);

		assertEquals(TEST_DATE_STRING, convertedString);
	}

	@Test
	void testInstantConversion() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, Instant.class);
		final var dateTimeFormat = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT).withZone(ZoneId.systemDefault());
		final var expectedValue = Instant.from(dateTimeFormat.parse(TEST_DATE_STRING));
		final Instant convertedValue = converter.convertToValue(TEST_DATE_STRING);

		assertEquals(expectedValue, convertedValue);

		final String convertedString = converter.convertToString(convertedValue);

		assertEquals(TEST_DATE_STRING, convertedString);
	}

	@Test
	void testLocalDateConversion() {
		final var converter = new ValueConverter<>(null, null, DATE_FORMAT, LocalDate.class);
		final var dateFormat = DateTimeFormatter.ofPattern(DATE_FORMAT).withZone(ZoneId.systemDefault());
		final var expectedValue = LocalDate.from(dateFormat.parse(TEST_LOCAL_DATE_STRING)).atStartOfDay(ZoneId.systemDefault())
				.toLocalDate();
		final LocalDate convertedValue = converter.convertToValue(TEST_LOCAL_DATE_STRING);

		assertEquals(expectedValue, convertedValue);

		final String convertedString = converter.convertToString(convertedValue);

		assertEquals(TEST_LOCAL_DATE_STRING, convertedString);
	}

	@Test
	void testGregorianCalendarConversion() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, GregorianCalendar.class);
		final var dateTimeFormat = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT).withZone(ZoneId.systemDefault());
		final TemporalAccessor temporalValue = dateTimeFormat.parse(TEST_DATE_STRING);
		final ZonedDateTime expectedValue = LocalDateTime.from(temporalValue).atZone(ZoneId.systemDefault());
		final GregorianCalendar convertedValue = converter.convertToValue(TEST_DATE_STRING);

		assertEquals(GregorianCalendar.from(expectedValue), convertedValue);

		final String convertedString = converter.convertToString(convertedValue);

		assertEquals(TEST_DATE_STRING, convertedString);
	}

	@Test
	void testUUIDConversion() {
		final var converter = new ValueConverter<>(null, null, null, UUID.class);
		final UUID convertedValue = converter.convertToValue(TEST_UUID_STRING);

		assertEquals(UUID.fromString(TEST_UUID_STRING), convertedValue);

		final String convertedString = converter.convertToString(convertedValue);

		assertEquals(TEST_UUID_STRING, convertedString);
	}

	@Test
	void testInvalidConversion() {
		final var converter = new ValueConverter<>(null, null, null, Integer.class);

		assertThrows(IllegalArgumentException.class, () -> converter.convertToValue(TEST_INVALID_INTEGER));
	}

	@Test
	void testConversionWithUnsupportedType() {
		final var converter = new ValueConverter<>(null, null, null, AtomicLong.class);

		assertThrows(IllegalArgumentException.class, () -> converter.convertToValue(TEST_LONG_STRING));
	}

	@Test
	void testNullConversion() {
		final var converter = new ValueConverter<>(null, null, null, Integer.class);
		final Integer convertedValue = converter.convertToValue(null);

		assertNull(convertedValue);

		final String convertedString = converter.convertToString(null);

		assertNull(convertedString);
	}

	@Test
	void testGetInitialDefaultDateValue() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, Date.class);
		final String convertedValue = converter.getInitialDefaultValue();

		assertNotNull(convertedValue);
	}

	@Test
	void testGetInitialDefaultGregorianCalendarValue() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, GregorianCalendar.class);
		final String convertedValue = converter.getInitialDefaultValue();

		assertNotNull(convertedValue);
	}

	@Test
	void testGetInitialDefaultLocalDateValue() {
		final var converter = new ValueConverter<>(null, null, DATE_FORMAT, LocalDate.class);
		final String convertedValue = converter.getInitialDefaultValue();

		assertNotNull(convertedValue);
	}

	@Test
	void testGetInitialDefaultInstantValue() {
		final var converter = new ValueConverter<>(null, DATE_TIME_FORMAT, null, Instant.class);
		final String convertedValue = converter.getInitialDefaultValue();

		assertNotNull(convertedValue);
	}

	@ParameterizedTest
	@ValueSource(strings = { "java.lang.Double", "java.lang.Float", "java.math.BigDecimal" })
	@SuppressWarnings("unchecked")
	void testGetInitialDefaulDecimalValue(String className) throws ClassNotFoundException {
		final var converter = ValueConverter.getNumberConverter(DECIMAL_FORMAT, (Class<Number>) Class.forName(className));
		final var decimalFormat = converter.getDecimalFormat();
		final String convertedValue = converter.getInitialDefaultValue();

		assertEquals(decimalFormat.format(0.0), convertedValue);
	}

	@Test
	void testGetInitialDefaultStringValue() {
		final var converter = new ValueConverter<>(null, null, null, String.class);
		final String convertedValue = converter.getInitialDefaultValue();

		assertEquals("", convertedValue);
	}
}