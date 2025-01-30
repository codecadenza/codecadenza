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
package net.codecadenza.runtime.webclient.vaadin.validation.util;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_LENGTH_RANGE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_MAX_LENGTH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_MAX_VALUE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_MIN_LENGTH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_MIN_VALUE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_REGEX;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_FIELD_VALUE_RANGE;

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;

/**
 * <p>
 * Generator for validation error messages
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ValidationMessageBuilder {
	private static final String DATE_TIME_FORMAT_PATTERN = "dd.MM.yyyy HH:mm:ss";
	private static final String DATE_FORMAT_PATTERN = "dd.MM.yyyy";

	private final InternalI18NService i18n;

	/**
	 * Constructor
	 * @param locale
	 */
	public ValidationMessageBuilder(Locale locale) {
		this.i18n = new InternalI18NService(locale);
	}

	/**
	 * Generate a validation error message based on the provided numbers
	 * @param minValue
	 * @param maxValue
	 * @return the generated validation error message
	 */
	public String buildMessage(Number minValue, Number maxValue) {
		if (minValue != null && maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_VALUE_RANGE, minValue, maxValue);
		else if (minValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MIN_VALUE, minValue);
		else if (maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MAX_VALUE, maxValue);
		else
			return null;
	}

	/**
	 * Generate a validation error message based on the provided date objects
	 * @param minValue
	 * @param maxValue
	 * @return the generated validation error message
	 */
	public String buildMessage(Date minValue, Date maxValue) {
		final var dateFormat = new SimpleDateFormat(DATE_TIME_FORMAT_PATTERN);

		if (minValue != null && maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_VALUE_RANGE, dateFormat.format(minValue), dateFormat.format(maxValue));
		else if (minValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MIN_VALUE, dateFormat.format(minValue));
		else if (maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MAX_VALUE, dateFormat.format(maxValue));
		else
			return null;
	}

	/**
	 * Generate a validation error message based on the provided {@link LocalDate} objects
	 * @param minValue
	 * @param maxValue
	 * @return the generated validation error message
	 */
	public String buildMessage(LocalDate minValue, LocalDate maxValue) {
		final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(DATE_FORMAT_PATTERN).withZone(ZoneId.systemDefault());

		if (minValue != null && maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_VALUE_RANGE, dateFormat.format(minValue), dateFormat.format(maxValue));
		else if (minValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MIN_VALUE, dateFormat.format(minValue));
		else if (maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MAX_VALUE, dateFormat.format(maxValue));
		else
			return null;
	}

	/**
	 * Generate a validation error message based on the provided {@link LocalDateTime} objects
	 * @param minValue
	 * @param maxValue
	 * @return the generated validation error message
	 */
	public String buildMessage(LocalDateTime minValue, LocalDateTime maxValue) {
		final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT_PATTERN).withZone(ZoneId.systemDefault());

		if (minValue != null && maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_VALUE_RANGE, dateFormat.format(minValue), dateFormat.format(maxValue));
		else if (minValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MIN_VALUE, dateFormat.format(minValue));
		else if (maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MAX_VALUE, dateFormat.format(maxValue));
		else
			return null;
	}

	/**
	 * Generate a validation error message based on the provided calendar objects
	 * @param minValue
	 * @param maxValue
	 * @return the generated validation error message
	 */
	public String buildMessage(Calendar minValue, Calendar maxValue) {
		final var dateFormat = new SimpleDateFormat(DATE_TIME_FORMAT_PATTERN);

		if (minValue != null && maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_VALUE_RANGE, dateFormat.format(minValue.getTime()),
					dateFormat.format(maxValue.getTime()));
		else if (minValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MIN_VALUE, dateFormat.format(minValue.getTime()));
		else if (maxValue != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MAX_VALUE, dateFormat.format(maxValue.getTime()));
		else
			return null;
	}

	/**
	 * Generate a validation error message based on the provided length values
	 * @param minLength
	 * @param maxLength
	 * @return the generated validation error message
	 */
	public String buildMessage(Integer minLength, Integer maxLength) {
		if (minLength != null && maxLength != null)
			return i18n.getTranslation(MSG_ERR_FIELD_LENGTH_RANGE, minLength, maxLength);
		else if (minLength != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MIN_LENGTH, minLength);
		else if (maxLength != null)
			return i18n.getTranslation(MSG_ERR_FIELD_MAX_LENGTH, maxLength);
		else
			return null;
	}

	/**
	 * Generate a validation error message based on the provided regular expression
	 * @param regEx
	 * @return the generated validation error message
	 */
	public String buildMessage(String regEx) {
		return i18n.getTranslation(MSG_ERR_FIELD_REGEX, regEx);
	}

}
