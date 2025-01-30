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
package net.codecadenza.runtime.webclient.vaadin.search;

import com.vaadin.flow.function.ValueProvider;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;

/**
 * <p>
 * Objects of this class represent search fields that can be bound to Vaadin components
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the source binding type
 */
public class BindingSearchField<T> extends SearchFieldDTO {
	private static final long serialVersionUID = 3305507194438402875L;

	private final ValueProvider<T, ?> binding;

	/**
	 * Constructor
	 * @param colDisplayOrder
	 * @param colName
	 * @param colLabel
	 * @param dataType
	 * @param width
	 * @param binding
	 */
	public BindingSearchField(int colDisplayOrder, String colName, String colLabel, SearchFieldDataTypeEnum dataType, int width,
			ValueProvider<T, ?> binding) {
		super(colDisplayOrder, colName, colLabel, dataType, width);

		this.binding = binding;
	}

	/**
	 * @return the binding
	 */
	public ValueProvider<T, ?> getBinding() {
		return binding;
	}

}
