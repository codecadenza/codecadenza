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
import jakarta.xml.bind.annotation.XmlType;
import java.io.Serializable;

/**
 * <p>
 * Data transfer object that contains filter and sorting information in order to generate parts of a query upon field level
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlType
@XmlAccessorType(XmlAccessType.FIELD)
public class SearchInputField implements Serializable {
	private static final long serialVersionUID = -594862830004932025L;

	@XmlAttribute
	private String filterCriteria;
	@XmlAttribute
	private String name;
	@XmlAttribute
	private SortDirectionEnum sortOrder = SortDirectionEnum.NONE;
	@XmlAttribute
	private int sortIndex;
	@XmlAttribute
	private boolean dateTimeFormat = true;
	@XmlAttribute
	private FilterOperatorEnum operator;

	/**
	 * Default constructor
	 */
	public SearchInputField() {

	}

	/**
	 * Constructor
	 * @param name
	 * @param operator
	 */
	public SearchInputField(String name, FilterOperatorEnum operator) {
		this(name, operator, null, SortDirectionEnum.NONE);
	}

	/**
	 * Constructor
	 * @param name
	 * @param operator
	 * @param filterCriteria
	 */
	public SearchInputField(String name, FilterOperatorEnum operator, String filterCriteria) {
		this(name, operator, filterCriteria, SortDirectionEnum.NONE);
	}

	/**
	 * Constructor
	 * @param name
	 * @param operator
	 * @param filterCriteria
	 * @param sortOrder
	 */
	public SearchInputField(String name, FilterOperatorEnum operator, String filterCriteria, SortDirectionEnum sortOrder) {
		this.name = name;
		this.operator = operator;
		this.filterCriteria = filterCriteria;
		this.sortOrder = sortOrder;
	}

	/**
	 * @return the field's name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Set the field name
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the operator
	 */
	public FilterOperatorEnum getOperator() {
		return operator;
	}

	/**
	 * @param operator
	 */
	public void setOperator(FilterOperatorEnum operator) {
		this.operator = operator;
	}

	/**
	 * @return the filter criteria
	 */
	public String getFilterCriteria() {
		return filterCriteria;
	}

	/**
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
	 * @param sortOrder
	 */
	public void setSortOrder(SortDirectionEnum sortOrder) {
		this.sortOrder = sortOrder;
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
	 * @return a flag that determines if a date field is of type date time or date. Fields supplied with a different type should
	 *         ignore the flag!
	 */
	public boolean isDateTimeFormat() {
		return dateTimeFormat;
	}

	/**
	 * @param dateTimeFormat
	 */
	public void setDateTimeFormat(boolean dateTimeFormat) {
		this.dateTimeFormat = dateTimeFormat;
	}

}
