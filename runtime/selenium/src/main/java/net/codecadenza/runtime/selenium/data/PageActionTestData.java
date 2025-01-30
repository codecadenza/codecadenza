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
import jakarta.xml.bind.annotation.XmlElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * <p>
 * Objects of this class represent test data for a single test action that belongs to a page object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class PageActionTestData implements Serializable {
	private static final long serialVersionUID = 5367954841650431770L;

	@XmlAttribute(required = true)
	private String pageClassName;

	@XmlAttribute
	private String objectId;

	@XmlAttribute
	private String navigationTarget;

	@XmlElement(name = "element")
	private final List<PageElementTestData> fields = new ArrayList<>();

	@XmlElement
	private PageActionResult result;

	/**
	 * @return the fully qualified page object class name this test action belongs to
	 */
	public String getPageClassName() {
		return pageClassName;
	}

	/**
	 * @param pageClassName
	 */
	public void setPageClassName(String pageClassName) {
		this.pageClassName = pageClassName;
	}

	/**
	 * @return the ID of the object that this test action refers to
	 */
	public String getObjectId() {
		return objectId;
	}

	/**
	 * @param objectId
	 */
	public void setObjectId(String objectId) {
		this.objectId = objectId;
	}

	/**
	 * @return the navigation target
	 */
	public String getNavigationTarget() {
		return navigationTarget;
	}

	/**
	 * @param navigationTarget
	 */
	public void setNavigationTarget(String navigationTarget) {
		this.navigationTarget = navigationTarget;
	}

	/**
	 * @return a list that contains test data for all elements of this test action
	 */
	public List<PageElementTestData> getElementTestData() {
		return fields;
	}

	/**
	 * @param id
	 * @return the test data object for a given element
	 * @throws RuntimeException if the element identified by the provided ID cannot be found!
	 */
	public PageElementTestData getElementTestDataById(String id) {
		final Optional<PageElementTestData> fieldData = fields.stream().filter(e -> e.getElementId().equals(id)).findFirst();

		if (!fieldData.isPresent())
			throw new RuntimeException("Element with ID '" + id + "' not found!");

		return fieldData.get();
	}

	/**
	 * @param id
	 * @return the new value for a given element
	 * @throws RuntimeException if the element identified by the provided ID cannot be found!
	 */
	public String getNewValue(String id) {
		return getElementTestDataById(id).getNewValue();
	}

	/**
	 * @param id
	 * @return the expected value of a given element
	 * @throws RuntimeException if the element identified by the provided ID cannot be found!
	 */
	public String getExpectedValue(String id) {
		return getElementTestDataById(id).getExpectedValue();
	}

	/**
	 * @return the expected result of the page after performing a test action
	 */
	public PageActionResult getResult() {
		return result;
	}

	/**
	 * @param result
	 */
	public void setResult(PageActionResult result) {
		this.result = result;
	}

}
