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
import java.text.ParseException;
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
	private final Class<T> type;
	private DecimalFormat decimalFormat;
	private DateTimeFormatter dateTimeFormat;
	private DateTimeFormatter dateFormat;

	/**
	 * Constructor
	 * @param decimalFormat
	 * @param dateTimeFormat
	 * @param dateFormat
	 * @param type
	 */
	public ValueConverter(String decimalFormat, String dateTimeFormat, String dateFormat, Class<T> type) {
		this.type = type;

		if (decimalFormat != null && !decimalFormat.isEmpty())
			this.decimalFormat = new DecimalFormat(decimalFormat);

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
				value = parseBigDecimal(string);
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
	 * Convert the given string to a {@link BigDecimal}
	 * @param string the string to convert
	 * @return the {@link BigDecimal} value
	 * @throws ParseException if the parsing has failed
	 */
	private synchronized Object parseBigDecimal(String string) throws ParseException {
		decimalFormat.setParseBigDecimal(true);

		final BigDecimal value = (BigDecimal) decimalFormat.parse(string);

		decimalFormat.setParseBigDecimal(false);

		return value;
	}

}
