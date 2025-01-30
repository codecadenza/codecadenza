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
package net.codecadenza.runtime.webclient.primefaces.search;

import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchInputField;

/**
 * <p>
 * Exception that is thrown if the validation of a {@link SearchInputField} has failed
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchInputFieldValidationException extends RuntimeException {
	private static final long serialVersionUID = 4802146201087687275L;

	private final String fieldName;

	/**
	 * Constructor
	 * @param searchField
	 * @param message
	 */
	public SearchInputFieldValidationException(SearchFieldDTO searchField, String message) {
		super(message);

		this.fieldName = searchField.getColLabel();
	}

	/**
	 * @return the name of the search field that has caused the exception
	 */
	public String getSearchFieldName() {
		return fieldName;
	}

}
