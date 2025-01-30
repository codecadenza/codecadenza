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
import java.util.Map;
import java.util.Objects;

/**
 * <p>
 * Data transfer object that represents all information for building query fragments on field level. There are three special
 * properties that must be mentioned: 1) fetchIndex: This index is used to ease reading of resultset data. 2) columnIndex: If a
 * field is visible the value is greater or equal 0. This index is equivalent to the table column index. 3) colOrder: This value
 * represents the column order. Note that this class has a natural ordering (see method compareTo()) that is inconsistent with
 * equals().
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchFieldDTO implements Serializable, Comparable<SearchFieldDTO> {
	private static final long serialVersionUID = 1L;

	private SearchOperatorDTO operator;
	private String filterCriteria;
	private int fetchIndex;
	private boolean visible = true;
	private SearchFieldTypeEnum type;
	private SearchFieldDataTypeEnum dataType;
	private String colLabel;
	private String colName;
	private int colWidth;
	private int colOrder;
	private SortDirectionEnum sortOrder;
	private String listOfValues;
	private int columnIndex = -1;
	private int sortIndex;
	private int originalColumnIndex;
	private Map<String, String> enumListValues;
	private String lovCommand;
	private boolean dateTimeFormat = true;

	/**
	 * Default constructor
	 */
	public SearchFieldDTO() {

	}

	/**
	 * Constructor
	 * @param colDisplayOrder the display order of the column
	 * @param colName the name of property which is used in the select clause of the query
	 * @param colLabel the displayed name of the column
	 * @param dataType the data type
	 * @param width the width of the column
	 */
	public SearchFieldDTO(int colDisplayOrder, String colName, String colLabel, SearchFieldDataTypeEnum dataType, int width) {
		this.colOrder = colDisplayOrder;
		this.colName = colName;
		this.colLabel = colLabel;
		this.dataType = dataType;
		this.type = SearchFieldTypeEnum.STANDARD;
		this.colWidth = width;
		this.setSortOrder(SortDirectionEnum.NONE);
		this.originalColumnIndex = this.colOrder;
	}

	/**
	 * Constructor
	 * @param type the field type
	 * @param colDisplayOrder the display order of the column
	 * @param colName the name of property which is used in the select clause of the query
	 * @param colLabel the displayed name of the column
	 * @param dataType the data type
	 * @param width the width of the column
	 */
	public SearchFieldDTO(SearchFieldTypeEnum type, int colDisplayOrder, String colName, String colLabel,
			SearchFieldDataTypeEnum dataType, int width) {
		this.colOrder = colDisplayOrder;
		this.colName = colName;
		this.colLabel = colLabel;
		this.dataType = dataType;
		this.type = type;
		this.colWidth = width;
		this.setSortOrder(SortDirectionEnum.NONE);
		this.originalColumnIndex = this.colOrder;
	}

	/**
	 * Constructor
	 * @param type the field type
	 * @param colDisplayOrder the display order of the column
	 * @param colName the name of property which is used in the select clause of the query
	 * @param colLabel the displayed name of the column
	 * @param dataType the data type
	 * @param width the width of the column
	 * @param lov the fully qualified class name of the list of values
	 */
	public SearchFieldDTO(SearchFieldTypeEnum type, int colDisplayOrder, String colName, String colLabel,
			SearchFieldDataTypeEnum dataType, int width, String lov) {
		this.colOrder = colDisplayOrder;
		this.colName = colName;
		this.colLabel = colLabel;
		this.dataType = dataType;
		this.type = type;
		this.colWidth = width;
		this.setSortOrder(SortDirectionEnum.NONE);
		this.setListOfValues(lov);
		this.originalColumnIndex = this.colOrder;
	}

	/**
	 * @return the fully qualified class name of the connected list-of-values dialog
	 */
	public String getListOfValues() {
		return listOfValues;
	}

	/**
	 * Set the fully qualified class name of the connected list-of-values dialog
	 * @param listOfValues
	 */
	public void setListOfValues(String listOfValues) {
		this.listOfValues = listOfValues;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(SearchFieldDTO o) {
		return this.colOrder - o.colOrder;
	}

	/**
	 * @return the column order
	 */
	public int getColOrder() {
		return colOrder;
	}

	/**
	 * Set the column order
	 * @param colDisplayOrder
	 */
	public void setColOrder(int colDisplayOrder) {
		this.colOrder = colDisplayOrder;
	}

	/**
	 * @return the width of the column
	 */
	public int getColWidth() {
		return colWidth;
	}

	/**
	 * Set the column width
	 * @param colWidth
	 */
	public void setColWidth(int colWidth) {
		this.colWidth = colWidth;
	}

	/**
	 * @return the column label
	 */
	public String getColLabel() {
		return colLabel;
	}

	/**
	 * Set the column label
	 * @param colLabel
	 */
	public void setColLabel(String colLabel) {
		this.colLabel = colLabel;
	}

	/**
	 * @return the column name
	 */
	public String getColName() {
		return colName;
	}

	/**
	 * Set the column name
	 * @param colName
	 */
	public void setColName(String colName) {
		this.colName = colName;
	}

	/**
	 * @return the column data type
	 */
	public SearchFieldDataTypeEnum getDataType() {
		return dataType;
	}

	/**
	 * Set the data type of the column
	 * @param type
	 */
	public void setDataType(SearchFieldDataTypeEnum type) {
		this.dataType = type;
	}

	/**
	 * @return true if the column should be visible
	 */
	public boolean isVisible() {
		return visible;
	}

	/**
	 * Set the flag that controls if a column is visible
	 * @param visible
	 */
	public void setVisible(boolean visible) {
		this.visible = visible;
	}

	/**
	 * @return the column fetch index
	 */
	public int getFetchIndex() {
		return fetchIndex;
	}

	/**
	 * Set the column fetch index
	 * @param colIndex
	 */
	public void setFetchIndex(int colIndex) {
		this.fetchIndex = colIndex;
	}

	/**
	 * @return the operator
	 */
	public SearchOperatorDTO getOperator() {
		return operator;
	}

	/**
	 * Set the search operator
	 * @param operator
	 */
	public void setOperator(SearchOperatorDTO operator) {
		this.operator = operator;
	}

	/**
	 * @return the filter criteria
	 */
	public String getFilterCriteria() {
		return filterCriteria;
	}

	/**
	 * Set the filter criteria
	 * @param filterCriteria
	 */
	public void setFilterCriteria(String filterCriteria) {
		this.filterCriteria = filterCriteria;
	}

	/**
	 * @return the sort order
	 */
	public SortDirectionEnum getSortOrder() {
		return sortOrder;
	}

	/**
	 * Set the sort order
	 * @param sortOrder
	 */
	public void setSortOrder(SortDirectionEnum sortOrder) {
		this.sortOrder = sortOrder;
	}

	/**
	 * @return the type of the search field
	 */
	public SearchFieldTypeEnum getType() {
		return type;
	}

	/**
	 * Set the search field type
	 * @param type
	 */
	public void setType(SearchFieldTypeEnum type) {
		this.type = type;
	}

	/**
	 * @return the column index
	 */
	public int getColumnIndex() {
		return columnIndex;
	}

	/**
	 * @param columnIndex set column index to set
	 */
	public void setColumnIndex(int columnIndex) {
		this.columnIndex = columnIndex;
	}

	/**
	 * @return the sort index
	 */
	public int getSortIndex() {
		return sortIndex;
	}

	/**
	 * @param sortIndex
	 */
	public void setSortIndex(int sortIndex) {
		this.sortIndex = sortIndex;
	}

	/**
	 * @return the original column index
	 */
	public int getOriginalColumnIndex() {
		return originalColumnIndex;
	}

	/**
	 * @param originalColumnIndex
	 */
	public void setOriginalColumnIndex(int originalColumnIndex) {
		this.originalColumnIndex = originalColumnIndex;
	}

	/**
	 * @return the hash map of enumeration values
	 */
	public Map<String, String> getEnumListValues() {
		return enumListValues;
	}

	/**
	 * @param enumListValues the hash map of enumeration values to set
	 */
	public void setEnumListValues(Map<String, String> enumListValues) {
		this.enumListValues = enumListValues;
	}

	/**
	 * @return the list-of-values command string
	 */
	public String getLovCommand() {
		return lovCommand;
	}

	/**
	 * @param lovCommand the list-of-values command string to set
	 */
	public void setLovCommand(String lovCommand) {
		this.lovCommand = lovCommand;
	}

	/**
	 * @return a flag that determines if a date field is of type date time or date. Fields supplied with a different type should
	 *         ignore the flag!
	 */
	public boolean isDateTimeFormat() {
		return dateTimeFormat;
	}

	/**
	 * @param dateTimeFormat the date time format flag to set
	 */
	public void setDateTimeFormat(boolean dateTimeFormat) {
		this.dateTimeFormat = dateTimeFormat;
	}

	/**
	 * @return true if the data type represents a date
	 */
	public boolean hasTemporalDataType() {
		return dataType == SearchFieldDataTypeEnum.DATE || dataType == SearchFieldDataTypeEnum.GREGORIAN_CALENDAR
				|| dataType == SearchFieldDataTypeEnum.LOCAL_DATE || dataType == SearchFieldDataTypeEnum.LOCAL_DATE_TIME;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return Objects.hash(colOrder);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null || getClass() != obj.getClass())
			return false;

		final SearchFieldDTO other = (SearchFieldDTO) obj;
		return colOrder == other.colOrder;
	}

}
