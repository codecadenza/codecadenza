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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Data transfer object for generic search operations via integration services (e.g. REST or SOAP)
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class SearchInput implements Serializable {
	private static final long serialVersionUID = -5129142329127694576L;
	private static final int DEFAULT_FETCH_SIZE = 50;

	@XmlAttribute
	private int maxResult = DEFAULT_FETCH_SIZE;
	@XmlAttribute
	private boolean caseSensitive;
	@XmlAttribute
	private String numberFormat;
	@XmlAttribute
	private String dateFormat;
	@XmlAttribute
	private int startIndex;
	@XmlElement
	private List<SearchInputField> searchFields = new ArrayList<>();
	@XmlAttribute
	private String dateTimeFormat;
	@XmlAttribute
	private boolean exactFilterMatch = true;
	@XmlAttribute
	private char decimalSeparator;
	@XmlAttribute
	private char groupingSeparator;

	/**
	 * Constructor
	 */
	public SearchInput() {
		// Initialize the decimal and the grouping separator with the values of the default locale
		setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());
	}

	/**
	 * Add a search field
	 * @param name
	 * @param operator
	 */
	public void addSearchField(String name, FilterOperatorEnum operator) {
		searchFields.add(new SearchInputField(name, operator));
	}

	/**
	 * Add a search field
	 * @param name
	 * @param operator
	 * @param filterCriteria
	 */
	public void addSearchField(String name, FilterOperatorEnum operator, String filterCriteria) {
		searchFields.add(new SearchInputField(name, operator, filterCriteria));
	}

	/**
	 * Add a search field
	 * @param name
	 * @param operator
	 * @param filterCriteria
	 * @param sortOrder
	 */
	public void addSearchField(String name, FilterOperatorEnum operator, String filterCriteria, SortDirectionEnum sortOrder) {
		searchFields.add(new SearchInputField(name, operator, filterCriteria, sortOrder));
	}

	/**
	 * @return the date time format
	 */
	public String getDateTimeFormat() {
		return dateTimeFormat;
	}

	/**
	 * @param dateTimeFormat
	 */
	public void setDateTimeFormat(String dateTimeFormat) {
		this.dateTimeFormat = dateTimeFormat;
	}

	/**
	 * @return true if the search should be case sensitive
	 */
	public boolean isCaseSensitive() {
		return caseSensitive;
	}

	/**
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
	 * @param dateFormat
	 */
	public void setDateFormat(String dateFormat) {
		this.dateFormat = dateFormat;
	}

	/**
	 * @return the max. number of objects that should be returned
	 */
	public int getMaxResult() {
		return maxResult;
	}

	/**
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
	 * @param numberFormat
	 */
	public void setNumberFormat(String numberFormat) {
		this.numberFormat = numberFormat;
	}

	/**
	 * @return the search fields
	 */
	public List<SearchInputField> getSearchFields() {
		return searchFields;
	}

	/**
	 * @param searchFields
	 */
	public void setSearchFields(List<SearchInputField> searchFields) {
		this.searchFields = searchFields;
	}

	/**
	 * @return the start index
	 */
	public int getStartIndex() {
		return startIndex;
	}

	/**
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

}
