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
package net.codecadenza.runtime.search.util;

import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchInput;
import net.codecadenza.runtime.search.dto.SearchInputField;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;

/**
 * <p>
 * Utility class for converting a {@link SearchInput} into a {@link SearchDTO} object
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchObjectConverter {
	private final SearchDTO searchObj = new SearchDTO();
	private final SearchInput searchInput;

	/**
	 * Constructor
	 * @param searchInput
	 */
	public SearchObjectConverter(SearchInput searchInput) {
		this.searchInput = searchInput;

		// Initialize the internal search object with values provided by the search input object
		this.searchObj.setMaxResult(searchInput.getMaxResult());
		this.searchObj.setStartIndex(searchInput.getStartIndex());
		this.searchObj.setExactFilterMatch(searchInput.isExactFilterMatch());
		this.searchObj.setCaseSensitive(searchInput.isCaseSensitive());
		this.searchObj.setCount(false);
		this.searchObj.setDateFormat(searchInput.getDateFormat());
		this.searchObj.setDateTimeFormat(searchInput.getDateTimeFormat());
		this.searchObj.setNumberFormat(searchInput.getNumberFormat());
		this.searchObj.setDecimalSeparator(searchInput.getDecimalSeparator());
		this.searchObj.setGroupingSeparator(searchInput.getGroupingSeparator());
	}

	/**
	 * Add a field to the internal search object
	 * @param selectClause
	 * @param fieldName
	 * @param dataType
	 * @param dateTimeFormat
	 */
	public void addSearchField(String selectClause, String fieldName, SearchFieldDataTypeEnum dataType, boolean dateTimeFormat) {
		final var field = new SearchFieldDTO(searchObj.getSearchFields().size(), selectClause, fieldName, dataType, 0);
		field.setDateTimeFormat(dateTimeFormat);

		searchObj.getSearchFields().add(field);
	}

	/**
	 * Add a field to the internal search object
	 * @param selectClause
	 * @param fieldName
	 * @param dataType
	 */
	public void addSearchField(String selectClause, String fieldName, SearchFieldDataTypeEnum dataType) {
		addSearchField(selectClause, fieldName, dataType, true);
	}

	/**
	 * Convert the provided input into a {@link SearchDTO}
	 * @return the search object
	 */
	public SearchDTO convert() {
		for (final SearchInputField inputField : searchInput.getSearchFields())
			for (final SearchFieldDTO searchField : searchObj.getSearchFields()) {
				if (!inputField.getName().equals(searchField.getColLabel()))
					continue;

				searchField.setFilterCriteria(inputField.getFilterCriteria());
				searchField.setSortIndex(inputField.getSortIndex());
				searchField.setSortOrder(inputField.getSortOrder());
				searchField.setDateTimeFormat(inputField.isDateTimeFormat());

				if (inputField.getOperator() != null)
					for (final SearchOperatorDTO operator : SearchOperatorHelper.getOperatorsForField(searchField))
						if (operator.getValue().equals(inputField.getOperator().getValue())) {
							searchField.setOperator(operator);
							break;
						}

				break;
			}

		return searchObj;
	}

}
