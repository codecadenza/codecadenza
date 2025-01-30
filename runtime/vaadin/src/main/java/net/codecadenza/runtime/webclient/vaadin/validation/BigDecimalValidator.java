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
package net.codecadenza.runtime.webclient.vaadin.validation;

import com.vaadin.flow.data.validator.BigDecimalRangeValidator;
import java.math.BigDecimal;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.validation.util.ValidationMessageBuilder;

/**
 * <p>
 * Validator for checking if a {@link BigDecimal} is inside a given range
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BigDecimalValidator extends BigDecimalRangeValidator {
	private static final long serialVersionUID = -8776592364882163007L;

	/**
	 * Constructor
	 * @param minValue
	 * @param maxValue
	 * @param locale
	 */
	public BigDecimalValidator(BigDecimal minValue, BigDecimal maxValue, Locale locale) {
		super(new ValidationMessageBuilder(locale).buildMessage(minValue, maxValue), minValue, maxValue);
	}

	/**
	 * Constructor
	 * @param maxValue
	 * @param locale
	 */
	public BigDecimalValidator(BigDecimal maxValue, Locale locale) {
		this(null, maxValue, locale);
	}

}
