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
 * XML mapping for {@link TestObject} objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLTestObject implements TestObject {
	private UUID id;
	private UUID referencedObjectId;
	private UUID referencedFieldId;
	private String value;
	private List<TestField> testFields = new ArrayList<>();

	/**
	 * Default constructor
	 */
	public XMLTestObject() {
	}

	/**
	 * Constructor for setting the value
	 * @param value
	 */
	public XMLTestObject(String value) {
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestObject#getId()
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
	 * @see net.codecadenza.runtime.ddt.model.TestObject#getReferencedObjectId()
	 */
	@Override
	@XmlAttribute(name = "ref_id")
	public UUID getReferencedObjectId() {
		return referencedObjectId;
	}

	/**
	 * Set the ID of the referenced object
	 * @param referencedObjectId
	 */
	public void setReferencedObjectId(UUID referencedObjectId) {
		this.referencedObjectId = referencedObjectId;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestObject#getReferencedFieldId()
	 */
	@Override
	@XmlAttribute(name = "ref_field_id")
	public UUID getReferencedFieldId() {
		return referencedFieldId;
	}

	/**
	 * Set the ID of the referenced field
	 * @param referencedFieldId
	 */
	public void setReferencedFieldId(UUID referencedFieldId) {
		this.referencedFieldId = referencedFieldId;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestObject#getValue()
	 */
	@Override
	@XmlAttribute(name = "value")
	public String getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestObject#setValue(java.lang.String)
	 */
	@Override
	public void setValue(String value) {
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestObject#getFields()
	 */
	@Override
	@XmlElement(type = XMLTestField.class, name = "field")
	public List<TestField> getFields() {
		return testFields;
	}

	/**
	 * Set the test fields
	 * @param fields
	 */
	public void setFields(List<TestField> fields) {
		this.testFields = fields;
	}

}
