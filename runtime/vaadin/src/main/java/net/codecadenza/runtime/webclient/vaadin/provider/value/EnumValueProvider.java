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
package net.codecadenza.runtime.webclient.vaadin.provider.value;

import com.vaadin.flow.function.ValueProvider;
import java.util.Map;

/**
 * <p>
 * Value provider for columns that are mapped to a {@link Enum} field. The values are converted by using the provided translation
 * map.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the object that contains the actual value
 */
public class EnumValueProvider<T> implements ValueProvider<T, String> {
	private static final long serialVersionUID = 4682337343423885602L;

	private final ValueProvider<T, Enum<?>> input;
	private final Map<String, String> translationMap;

	/**
	 * Constructor
	 * @param translationMap
	 * @param input
	 */
	public EnumValueProvider(Map<String, String> translationMap, ValueProvider<T, Enum<?>> input) {
		this.input = input;
		this.translationMap = translationMap;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.ValueProvider#apply(java.lang.Object)
	 */
	@Override
	public String apply(T source) {
		final Enum<?> value = input.apply(source);

		if (value == null || !translationMap.containsKey(value.name()))
			return "";

		return translationMap.get(value.name());
	}

}
