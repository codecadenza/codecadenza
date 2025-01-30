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
package net.codecadenza.runtime.search.dto;

import java.io.Serializable;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Data transfer object that holds all necessary data for building generic queries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchDTO implements Serializable {
	private static final long serialVersionUID = 1L;

	private int id;
	private int maxResult;
	private boolean caseSensitive;
	private String numberFormat;
	private String dateFormat;
	private boolean count;
	private int startIndex;
	private List<SearchFieldDTO> searchFields = new ArrayList<>();
	private String fromClause;
	private boolean fetchHidden = true;
	private String groupBy;
	private String dateTimeFormat;
	private boolean exactFilterMatch;
	private char decimalSeparator;
	private char groupingSeparator;

	/**
	 * Constructor
	 */
	public SearchDTO() {
		// Initialize the decimal and the grouping separator with the values of the default locale
		setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());
	}

	/**
	 * @return the date time format
	 */
	public String getDateTimeFormat() {
		return dateTimeFormat;
	}

	/**
	 * Set the date time format
	 * @param dateTimeFormat
	 */
	public void setDateTimeFormat(String dateTimeFormat) {
		this.dateTimeFormat = dateTimeFormat;
	}

	/**
	 * @return the group by statement
	 */
	public String getGroupBy() {
		return groupBy;
	}

	/**
	 * @param groupBy
	 */
	public void setGroupBy(String groupBy) {
		this.groupBy = groupBy;
	}

	/**
	 * @return the from clause
	 */
	public String getFromClause() {
		return fromClause;
	}

	/**
	 * Set the from clause of the query
	 * @param fromClause
	 */
	public void setFromClause(String fromClause) {
		this.fromClause = fromClause;
	}

	/**
	 * @return true if the search should be case sensitive
	 */
	public boolean isCaseSensitive() {
		return caseSensitive;
	}

	/**
	 * Set the flag to determine if a search should be case sensitive
	 * @param caseSensitive
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

	/**
	 * @return the date format that is used
	 */
	public String getDateFormat() {
		return dateFormat;
	}

	/**
	 * Set the date format
	 * @param dateFormat
	 */
	public void setDateFormat(String dateFormat) {
		this.dateFormat = dateFormat;
	}

	/**
	 * @return the max. number of results
	 */
	public int getMaxResult() {
		return maxResult;
	}

	/**
	 * Set the max. number of objects a query should return
	 * @param maxResult
	 */
	public void setMaxResult(int maxResult) {
		this.maxResult = maxResult;
	}

	/**
	 * @return the number format
	 */
	public String getNumberFormat() {
		return numberFormat;
	}

	/**
	 * Set the number format
	 * @param numberFormat
	 */
	public void setNumberFormat(String numberFormat) {
		this.numberFormat = numberFormat;
	}

	/**
	 * @return the search fields
	 */
	public List<SearchFieldDTO> getSearchFields() {
		return searchFields;
	}

	/**
	 * @param searchFields the search fields to set
	 */
	public void setSearchFields(List<SearchFieldDTO> searchFields) {
		this.searchFields = searchFields;
	}

	/**
	 * @return true if the service should perform a count operation
	 */
	public boolean isCount() {
		return count;
	}

	/**
	 * Set the flag that controls if a count operation should be performed
	 * @param count
	 */
	public void setCount(boolean count) {
		this.count = count;
	}

	/**
	 * @return true if hidden fields should be fetched
	 */
	public boolean isFetchHidden() {
		return fetchHidden;
	}

	/**
	 * Set the hidden fetch flag
	 * @param fetchHidden
	 */
	public void setFetchHidden(boolean fetchHidden) {
		this.fetchHidden = fetchHidden;
	}

	/**
	 * @return the start index
	 */
	public int getStartIndex() {
		return startIndex;
	}

	/**
	 * Set the start index
	 * @param startIndex
	 */
	public void setStartIndex(int startIndex) {
		this.startIndex = startIndex;
	}

	/**
	 * @return true if an exact filter match is expected
	 */
	public boolean isExactFilterMatch() {
		return exactFilterMatch;
	}

	/**
	 * @param exactFilterMatch
	 */
	public void setExactFilterMatch(boolean exactFilterMatch) {
		this.exactFilterMatch = exactFilterMatch;
	}

	/**
	 * @return the ID
	 */
	public int getId() {
		return id;
	}

	/**
	 * @param id
	 */
	public void setId(int id) {
		this.id = id;
	}

	/**
	 * @return the decimal separator character
	 */
	public char getDecimalSeparator() {
		return decimalSeparator;
	}

	/**
	 * @param decimalSeparator
	 */
	public void setDecimalSeparator(char decimalSeparator) {
		this.decimalSeparator = decimalSeparator;
	}

	/**
	 * @return the grouping separator character
	 */
	public char getGroupingSeparator() {
		return groupingSeparator;
	}

	/**
	 * @param groupingSeparator
	 */
	public void setGroupingSeparator(char groupingSeparator) {
		this.groupingSeparator = groupingSeparator;
	}

	/**
	 * Add a new search field
	 * @param colName the name of property which is used in the select clause of the query
	 * @param dataType the data type
	 * @return the new search field
	 */
	public SearchFieldDTO addSearchField(String colName, SearchFieldDataTypeEnum dataType) {
		final var colOrder = searchFields.stream().map(SearchFieldDTO::getColOrder).max(Integer::compare).orElse(-1) + 1;
		final var searchField = new SearchFieldDTO(colOrder, colName, null, dataType, 0);

		getSearchFields().add(searchField);

		return searchField;
	}

}
