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

import static net.codecadenza.runtime.search.SearchService.OPERATOR_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_LIKE;

import jakarta.faces.event.ValueChangeEvent;
import jakarta.faces.model.SelectItem;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;
import java.util.regex.Pattern;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import org.primefaces.model.DualListModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for searchable views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSearchableView {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final Pattern TOKEN_DELIMITER_PATTERN = Pattern.compile(SearchService.TOKEN_DELIMITER_IN);

	protected DualListModel<SearchFieldDTO> visibleFields;
	protected SearchDTO searchObj;

	/**
	 * @return a list model for selection of visible fields
	 */
	public DualListModel<SearchFieldDTO> getVisibleFields() {
		return visibleFields;
	}

	/**
	 * @param visibleFields
	 */
	public void setVisibleFields(DualListModel<SearchFieldDTO> visibleFields) {
		this.visibleFields = visibleFields;
	}

	/**
	 * @return the search object
	 */
	public SearchDTO getSearchObject() {
		return searchObj;
	}

	/**
	 * @param searchObject
	 */
	public void setSearchObject(SearchDTO searchObject) {
		this.searchObj = searchObject;
	}

	/**
	 * @return a collection of all valid operators for text fields
	 */
	public Collection<SearchOperatorDTO> getTextOperators() {
		return JSFSearchOperatorHelper.getTextOperators();
	}

	/**
	 * @return a collection of all valid operators for boolean fields
	 */
	public Collection<SearchOperatorDTO> getBoolOperators() {
		return JSFSearchOperatorHelper.getBoolOperators();
	}

	/**
	 * @return a collection of all valid operators for numeric fields
	 */
	public Collection<SearchOperatorDTO> getNumberOperators() {
		return JSFSearchOperatorHelper.getNumberOperators();
	}

	/**
	 * @return a collection of all valid operators for date fields
	 */
	public Collection<SearchOperatorDTO> getDateOperators() {
		return JSFSearchOperatorHelper.getDateOperators();
	}

	/**
	 * @return a collection of all valid operators for enumeration fields
	 */
	public Collection<SearchOperatorDTO> getEnumOperators() {
		return JSFSearchOperatorHelper.getEnumOperators();
	}

	/**
	 * @return a collection of all valid operators for UUID fields
	 */
	public Collection<SearchOperatorDTO> getUuIDOperators() {
		return JSFSearchOperatorHelper.getUUIDOperators();
	}

	/**
	 * @return a collection of all valid operators for UUID fields that support using a 'like'-operator
	 */
	public Collection<SearchOperatorDTO> getUuIDOperatorsWithLike() {
		return JSFSearchOperatorHelper.getUUIDOperatorsWithLike();
	}

	/**
	 * @param event
	 */
	public void onOperatorChanged(ValueChangeEvent event) {
		final String id = event.getComponent().getId();
		final Integer intId = Integer.parseInt(id.substring(id.indexOf('_') + 1));
		SearchOperatorDTO newOperator = null;

		try {
			if (event.getNewValue() != null)
				newOperator = (SearchOperatorDTO) event.getNewValue();
			else
				newOperator = JSFSearchOperatorHelper.getDefaultOperator();

			for (final SearchFieldDTO field : searchObj.getSearchFields()) {
				final var f = (JSFSearchFieldDTO) field;

				if (f.getColOrder() == intId)
					f.setBetween(newOperator.getValue().equals(SearchService.OPERATOR_BETWEEN));
			}
		}
		catch (final Exception e) {
			logger.error("Error while handling search operator change event!", e);
		}
	}

	/**
	 * @return an array of items
	 */
	public SelectItem[] getSortOrderList() {
		final var items = new SelectItem[SortDirectionEnum.values().length];
		int i = 0;

		for (final SortDirectionEnum item : SortDirectionEnum.values())
			items[i++] = new SelectItem(item, item.name());

		return items;
	}

	/**
	 * This method must to be invoked after a search object has been loaded from the persistence store!
	 */
	protected void prepareAfterLoad() {
		final var availableFields = new ArrayList<SearchFieldDTO>();
		final var visFields = new ArrayList<SearchFieldDTO>();

		searchObj.getSearchFields().forEach(f -> {
			if (f.getOperator() != null)
				f.setOperator(JSFSearchOperatorHelper.getOperator(f.getOperator().getValue()));

			if (f.isVisible())
				visFields.add(f);
			else
				availableFields.add(f);
		});

		visibleFields = new DualListModel<>();
		visibleFields.setSource(availableFields);
		visibleFields.setTarget(visFields);
	}

	/**
	 * This method must be called before the search operation is invoked
	 * @throws SearchInputFieldValidationException if the validation of at least one search input field has failed
	 */
	public void preSearch() {
		// Perform basic input validation
		validateSearchInput();

		// Hide invisible fields
		searchObj.getSearchFields().forEach(f -> f.setVisible(false));

		for (final SearchFieldDTO f : searchObj.getSearchFields())
			for (final SearchFieldDTO visibleField : visibleFields.getTarget())
				if (f.getColLabel().equals(visibleField.getColLabel())) {
					f.setVisible(true);
					break;
				}

		// Prepare further field properties depending on the visibility and the operator that is used
		searchObj.getSearchFields().stream().map(JSFSearchFieldDTO.class::cast).forEach(f -> {
			if (f.getOperator() != null && f.getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN))
				f.setBetween(f.getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN));

			if (f.getOperator() != null && f.getOperator().equals(JSFSearchOperatorHelper.getDefaultOperator()))
				f.setOperator(null);

			if (!f.isVisible()) {
				f.setSortOrder(SortDirectionEnum.NONE);
				f.setSortIndex(0);
			}
			else
				f.setSortIndex(Integer.MAX_VALUE - f.getColumnIndex());
		});
	}

	/**
	 * This method must be called after the respective search method has been invoked
	 */
	public void postSearch() {
		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.getOperator() == null)
				f.setOperator(JSFSearchOperatorHelper.getDefaultOperator());

		// Sort the fields in order to get the correct column order
		searchObj.getSearchFields().sort((f0, f1) -> f0.getColOrder() - f1.getColOrder());
	}

	/**
	 * Perform basic search input validation
	 * @throws SearchInputFieldValidationException if the validation of at least one search input field has failed
	 */
	private void validateSearchInput() {
		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			final var searchField = (JSFSearchFieldDTO) field;
			final SearchOperatorDTO operator = searchField.getOperator();
			final SearchFieldDataTypeEnum dataType = searchField.getDataType();

			if (operator != null && operator.getValue().equals(SearchService.OPERATOR_BETWEEN)
					&& ((field.hasTemporalDataType() && searchField.getDateBetweenCriterion() == null)
							|| (dataType == SearchFieldDataTypeEnum.STRING
									&& (searchField.getStringBetweenCriterion() == null || searchField.getStringBetweenCriterion().isEmpty()))
							|| ((dataType == SearchFieldDataTypeEnum.INTEGER || dataType == SearchFieldDataTypeEnum.LONG)
									&& searchField.getIntegerBetweenCriterion() == null)
							|| ((dataType == SearchFieldDataTypeEnum.DOUBLE || dataType == SearchFieldDataTypeEnum.FLOAT)
									&& searchField.getDoubleBetweenCriterion() == null)
							|| (dataType == SearchFieldDataTypeEnum.BIG_DECIMAL && searchField.getBigDecimalBetweenCriterion() == null)))
				throw new SearchInputFieldValidationException(searchField, "Missing upper bound for between operator!");

			validateSearchInputField(field);
		}
	}

	/**
	 * Check the entered filter data of a search field
	 * @param searchField
	 * @throws SearchInputFieldValidationException if the validation has failed
	 */
	private void validateSearchInputField(final SearchFieldDTO searchField) {
		final String[] values = splitFilterInput(searchField);

		for (final String value : values) {
			if (value == null || value.isEmpty())
				continue;

			if (searchField.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
					|| searchField.getDataType() == SearchFieldDataTypeEnum.UUID_STRING) {
				boolean checkInput = false;

				if (searchField.getOperator() == null && searchField.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY)
					checkInput = true;

				if (searchField.getOperator() != null && !searchField.getOperator().getValue().equals(OPERATOR_LIKE)
						&& !searchField.getOperator().getValue().equals(OPERATOR_NOT_LIKE))
					checkInput = true;

				if (checkInput)
					try {
						UUID.fromString(value);
					}
					catch (final IllegalArgumentException _) {
						throw new SearchInputFieldValidationException(searchField, "Error while converting field value to UUID!");
					}
			}
		}
	}

	/**
	 * Extract the dedicated filter values of a search field
	 * @param searchField
	 * @return an array of strings
	 * @throws SearchInputFieldValidationException if a filter value is either missing or not expected
	 */
	private String[] splitFilterInput(SearchFieldDTO searchField) {
		String[] values;

		try {
			if (searchField.getOperator() == null || searchField.getOperator().getValue().isEmpty()) {
				values = new String[1];
				values[0] = searchField.getFilterCriteria();

				return values;
			}

			if (!searchField.getOperator().isExpectsArgument() && searchField.getFilterCriteria() != null
					&& !searchField.getFilterCriteria().isEmpty())
				throw new SearchInputFieldValidationException(searchField, "Operator doesn't expect filter input!");

			if (searchField.getOperator().isExpectsArgument()
					&& (searchField.getFilterCriteria() == null || searchField.getFilterCriteria().isEmpty()))
				throw new SearchInputFieldValidationException(searchField, "Operator requires filter input!");

			if (searchField.getOperator().getValue().equals(OPERATOR_NOT_IN)
					|| searchField.getOperator().getValue().equals(OPERATOR_IN))
				values = TOKEN_DELIMITER_PATTERN.split(searchField.getFilterCriteria());
			else {
				values = new String[1];
				values[0] = searchField.getFilterCriteria();
			}
		}
		catch (final IllegalArgumentException _) {
			throw new SearchInputFieldValidationException(searchField,
					"Error while converting value of field '" + searchField.getColLabel() + "'!");
		}

		return values;
	}

}
