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
package net.codecadenza.runtime.ddt.model.xml;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.codecadenza.runtime.ddt.model.TestField;
import net.codecadenza.runtime.ddt.model.TestObject;

/**
 * <p>
 * XML mapping for {@link TestField} objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLTestField implements TestField {
	private UUID id;
	private UUID referencedId;
	private String name;
	private String value;
	private List<TestObject> testObjects = new ArrayList<>();
	private Integer expectedSize;

	/**
	 * Default constructor
	 */
	public XMLTestField() {
	}

	/**
	 * Constructor using fields
	 * @param name
	 */
	public XMLTestField(String name) {
		this.name = name;
	}

	/**
	 * Constructor using fields
	 * @param name
	 * @param value
	 */
	public XMLTestField(String name, String value) {
		this.name = name;
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#getId()
	 */
	@Override
	@XmlAttribute(name = "id")
	public UUID getId() {
		return id;
	}

	/**
	 * Set the ID
	 * @param id
	 */
	public void setId(UUID id) {
		this.id = id;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#getReferencedId()
	 */
	@Override
	@XmlAttribute(name = "ref_field_id")
	public UUID getReferencedId() {
		return referencedId;
	}

	/**
	 * Set the ID of the referenced field
	 * @param referencedId
	 */
	public void setReferencedId(UUID referencedId) {
		this.referencedId = referencedId;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#getName()
	 */
	@Override
	@XmlAttribute(name = "name")
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

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#getValue()
	 */
	@Override
	@XmlAttribute(name = "value")
	public String getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#setValue()
	 */
	@Override
	public void setValue(String value) {
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#getTestObjects()
	 */
	@Override
	@XmlElement(type = XMLTestObject.class, name = "object")
	public List<TestObject> getTestObjects() {
		return testObjects;
	}

	/**
	 * Set the test objects
	 * @param testObjects
	 */
	public void setTestObjects(List<TestObject> testObjects) {
		this.testObjects = testObjects;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestField#getExpectedSize()
	 */
	@Override
	@XmlAttribute(name = "expected_size")
	public Integer getExpectedSize() {
		return expectedSize;
	}

	/**
	 * Set the expected size
	 * @param expectedSize
	 */
	public void setExpectedSize(Integer expectedSize) {
		this.expectedSize = expectedSize;
	}

}
