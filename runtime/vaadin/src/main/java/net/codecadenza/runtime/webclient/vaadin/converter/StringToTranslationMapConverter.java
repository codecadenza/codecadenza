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
package net.codecadenza.runtime.webclient.vaadin.converter;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_ERR_ITEM_NOT_FOUND;

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;

/**
 * <p>
 * {@link String} converter that uses a given translation map for determining the corresponding object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type (usually an enumeration) that should be converted
 */
public class StringToTranslationMapConverter<T> implements Converter<String, T> {
	private static final long serialVersionUID = -7106639878095621163L;

	private final transient Map<T, String> transMap;
	private final InternalI18NService i18n;
	private final boolean allowNullValue;

	/**
	 * Constructor
	 * @param transMap
	 * @param locale
	 * @param allowNullValue
	 */
	public StringToTranslationMapConverter(Map<T, String> transMap, Locale locale, boolean allowNullValue) {
		this.transMap = transMap;
		this.i18n = new InternalI18NService(locale);
		this.allowNullValue = allowNullValue;
	}

	/**
	 * Constructor
	 * @param transMap
	 * @param locale
	 */
	public StringToTranslationMapConverter(Map<T, String> transMap, Locale locale) {
		this(transMap, locale, false);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.Converter#convertToModel(java.lang.Object, com.vaadin.data.ValueContext)
	 */
	@Override
	public Result<T> convertToModel(String value, ValueContext context) {
		if (allowNullValue && value == null)
			return Result.ok(null);

		final Optional<Map.Entry<T, String>> entry = transMap.entrySet().stream().filter(e -> e.getValue().equals(value)).findFirst();

		if (entry.isPresent())
			return Result.ok(entry.get().getKey());

		return Result.error(i18n.getTranslation(MSG_ERR_ITEM_NOT_FOUND, value != null ? value : ""));
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.Converter#convertToPresentation(java.lang.Object, com.vaadin.data.ValueContext)
	 */
	@Override
	public String convertToPresentation(T value, ValueContext context) {
		return transMap.get(value);
	}

}
