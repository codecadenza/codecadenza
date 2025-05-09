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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
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

/**
 * <p>
 * Utility class for converting a given value to a string and vice versa
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the value that should be converted
 */
public class ValueConverter<T> {
	private static final char DEFAULT_DECIMAL_SEPARATOR = '.';
	private static final char DEFAULT_GROUPING_SEPARATOR = ',';

	private final Class<T> type;
	private DecimalFormat decimalFormat;
	private DecimalFormat bigDecimalFormat;
	private DateTimeFormatter dateTimeFormat;
	private DateTimeFormatter dateFormat;

	/**
	 * Create a new {@link Double} value converter that uses a decimal format with a fixed decimal separator and a fixed grouping
	 * separator
	 * @param decimalFormat the decimal format string
	 * @return a {@link ValueConverter} for double values
	 */
	public static ValueConverter<Double> getDoubleConverter(String decimalFormat) {
		return getNumberConverter(decimalFormat, Double.class);
	}

	/**
	 * Create a new {@link Float} value converter that uses a decimal format with a fixed decimal separator and a fixed grouping
	 * separator
	 * @param decimalFormat the decimal format string
	 * @return a {@link ValueConverter} for float values
	 */
	public static ValueConverter<Float> getFloatConverter(String decimalFormat) {
		return getNumberConverter(decimalFormat, Float.class);
	}

	/**
	 * Create a new {@link BigDecimal} value converter that uses a decimal format with a fixed decimal separator and a fixed
	 * grouping separator
	 * @param decimalFormat the decimal format string
	 * @return a {@link ValueConverter} for {@link BigDecimal} values
	 */
	public static ValueConverter<BigDecimal> getBigDecimalConverter(String decimalFormat) {
		return getNumberConverter(decimalFormat, BigDecimal.class);
	}

	/**
	 * Create a new value converter that uses a decimal format with a fixed decimal separator and a fixed grouping separator
	 * @param decimalFormat the decimal format string
	 * @param numericType the numeric type of the value that should be converted
	 * @return a {@link ValueConverter} for decimal values
	 */
	public static <N extends Number> ValueConverter<N> getNumberConverter(String decimalFormat, Class<N> numericType) {
		return new ValueConverter<>(decimalFormat, DEFAULT_DECIMAL_SEPARATOR, DEFAULT_GROUPING_SEPARATOR, null, null, numericType);
	}

	/**
	 * Constructor that uses a decimal format with a fixed decimal separator and a fixed grouping separator
	 * @param decimalFormat
	 * @param dateTimeFormat
	 * @param dateFormat
	 * @param type
	 */
	public ValueConverter(String decimalFormat, String dateTimeFormat, String dateFormat, Class<T> type) {
		this(decimalFormat, DEFAULT_DECIMAL_SEPARATOR, DEFAULT_GROUPING_SEPARATOR, dateTimeFormat, dateFormat, type);
	}

	/**
	 * Constructor
	 * @param decimalFormat
	 * @param decimalSeparator
	 * @param groupingSeparator
	 * @param dateTimeFormat
	 * @param dateFormat
	 * @param type
	 */
	public ValueConverter(String decimalFormat, char decimalSeparator, char groupingSeparator, String dateTimeFormat,
			String dateFormat, Class<T> type) {
		this.type = type;

		if (decimalFormat != null && !decimalFormat.isEmpty()) {
			final var decimalSymbols = new DecimalFormatSymbols();
			decimalSymbols.setDecimalSeparator(decimalSeparator);
			decimalSymbols.setGroupingSeparator(groupingSeparator);

			this.decimalFormat = new DecimalFormat(decimalFormat);
			this.decimalFormat.setDecimalFormatSymbols(decimalSymbols);

			this.bigDecimalFormat = new DecimalFormat(decimalFormat);
			this.bigDecimalFormat.setDecimalFormatSymbols(decimalSymbols);
			this.bigDecimalFormat.setParseBigDecimal(true);
		}

		if (dateTimeFormat != null && !dateTimeFormat.isEmpty())
			this.dateTimeFormat = DateTimeFormatter.ofPattern(dateTimeFormat).withZone(ZoneId.systemDefault());

		if (dateFormat != null && !dateFormat.isEmpty())
			this.dateFormat = DateTimeFormatter.ofPattern(dateFormat).withZone(ZoneId.systemDefault());
	}

	/**
	 * Convert the given string to a value with the requested type
	 * @param string the string to be converted
	 * @return a new value
	 * @throws IllegalArgumentException if the value either could not be converted or if the requested type is not supported
	 */
	@SuppressWarnings("unchecked")
	public T convertToValue(String string) {
		Object value;

		if (string == null)
			return null;

		try {
			if (type.equals(String.class))
				value = string;
			else if (type.equals(Character.class)) {
				if (string.isEmpty() || string.length() > 1)
					throw new IllegalArgumentException("The string '" + string + "' cannot be converted to a character!");

				value = string.charAt(0);
			}
			else if (type.equals(BigDecimal.class))
				value = bigDecimalFormat.parse(string);
			else if (type.equals(Integer.class))
				value = Integer.parseInt(string);
			else if (type.equals(Long.class))
				value = Long.parseLong(string);
			else if (type.equals(Double.class))
				value = decimalFormat.parse(string).doubleValue();
			else if (type.equals(Float.class))
				value = decimalFormat.parse(string).floatValue();
			else if (type.equals(Boolean.class))
				value = Boolean.parseBoolean(string);
			else if (type.equals(UUID.class))
				value = UUID.fromString(string);
			else if (type.equals(Date.class)) {
				final TemporalAccessor temporalValue = dateTimeFormat.parse(string);
				final ZonedDateTime zonedDateTime = LocalDateTime.from(temporalValue).atZone(ZoneId.systemDefault());

				value = Date.from(zonedDateTime.toInstant());
			}
			else if (type.equals(GregorianCalendar.class)) {
				final TemporalAccessor temporalValue = dateTimeFormat.parse(string);
				final ZonedDateTime zonedDateTime = LocalDateTime.from(temporalValue).atZone(ZoneId.systemDefault());

				value = GregorianCalendar.from(zonedDateTime);
			}
			else if (type.equals(LocalDateTime.class)) {
				final TemporalAccessor temporalValue = dateTimeFormat.parse(string);

				value = LocalDateTime.from(temporalValue).atZone(ZoneId.systemDefault()).toLocalDateTime();
			}
			else if (type.equals(LocalDate.class)) {
				final TemporalAccessor temporalValue = dateFormat.parse(string);

				value = LocalDate.from(temporalValue).atStartOfDay(ZoneId.systemDefault()).toLocalDate();
			}
			else
				throw new IllegalArgumentException("The type '" + type.getName() + "' is not supported!");

			return (T) value;
		}
		catch (final IllegalArgumentException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new IllegalArgumentException("The string '" + string + "' could not be converted to a '" + type.getName() + "'!");
		}
	}

	/**
	 * Convert the given value to a string
	 * @param value
	 * @return the string representation of the value
	 */
	public String convertToString(T value) {
		if (value == null)
			return null;

		if (type.equals(String.class))
			return (String) value;
		else if (type.equals(Long.class))
			return Long.toString((long) value);
		else if (type.equals(Integer.class))
			return Integer.toString((int) value);
		else if (type.equals(Double.class))
			return decimalFormat.format((double) value);
		else if (type.equals(Float.class))
			return decimalFormat.format((float) value);
		else if (type.equals(BigDecimal.class))
			return decimalFormat.format(value);
		else if (type.equals(LocalDateTime.class))
			return dateTimeFormat.format((LocalDateTime) value);
		else if (type.equals(Date.class))
			return dateTimeFormat.format(Instant.ofEpochMilli(((Date) value).getTime()));
		else if (type.equals(GregorianCalendar.class))
			return dateTimeFormat.format(Instant.ofEpochMilli(((GregorianCalendar) value).getTimeInMillis()));
		else if (type.equals(LocalDate.class))
			return dateFormat.format((LocalDate) value);
		else
			return value.toString();
	}

	/**
	 * Create a default value to see the expected format of new elements to be added
	 * @return the initial default value
	 */
	public String getInitialDefaultValue() {
		if (type.equals(Date.class) || type.equals(GregorianCalendar.class) || type.equals(LocalDateTime.class))
			return dateTimeFormat.format(Instant.now());
		else if (type.equals(LocalDate.class))
			return dateFormat.format(Instant.now());
		else if (type.equals(Float.class) || type.equals(Double.class) || type.equals(BigDecimal.class))
			return decimalFormat.format(0.0);
		else if (type.equals(UUID.class))
			return UUID.randomUUID().toString();

		return "";
	}

	/**
	 * @return the decimal format used for {@link Double} and {@link Float} values
	 */
	public DecimalFormat getDecimalFormat() {
		return decimalFormat;
	}

	/**
	 * @return the decimal format used for {@link BigDecimal} values
	 */
	public DecimalFormat getBigDecimalFormat() {
		return bigDecimalFormat;
	}

}
