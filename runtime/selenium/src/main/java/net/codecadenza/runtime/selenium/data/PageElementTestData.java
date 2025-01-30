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
package net.codecadenza.runtime.selenium.data;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import java.io.Serializable;

/**
 * <p>
 * Objects of this class are responsible for providing test data of a single page element
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class PageElementTestData implements Serializable {
	public static final String ROW_COUNT_ID = "ROW_COUNT";
	public static final String PAGE_TITLE_ID = "PAGE_TITLE";
	public static final String CELL_VALUE_ID = "CELL_VALUE";
	public static final String ROW_INDEX_ID = "ROW_INDEX";
	private static final long serialVersionUID = 230778521304491479L;

	@XmlAttribute(name = "id", required = true)
	private String elementId;

	@XmlAttribute
	private String newValue;

	@XmlAttribute
	private String expectedValue;

	@XmlAttribute
	private String filterValue;

	/**
	 * @return the ID of the element
	 */
	public String getElementId() {
		return elementId;
	}

	/**
	 * @param elementId
	 */
	public void setElementId(String elementId) {
		this.elementId = elementId;
	}

	/**
	 * @return the new element value to be used while performing the test
	 */
	public String getNewValue() {
		return newValue;
	}

	/**
	 * @param newValue
	 */
	public void setNewValue(String newValue) {
		this.newValue = newValue;
	}

	/**
	 * @return the expected value an element should provide after initially loading a page
	 */
	public String getExpectedValue() {
		return expectedValue;
	}

	/**
	 * @param expectedValue
	 */
	public void setExpectedValue(String expectedValue) {
		this.expectedValue = expectedValue;
	}

	/**
	 * @return the value that should be used for filter operations
	 */
	public String getFilterValue() {
		return filterValue;
	}

	/**
	 * @param filterValue
	 */
	public void setFilterValue(String filterValue) {
		this.filterValue = filterValue;
	}

}
