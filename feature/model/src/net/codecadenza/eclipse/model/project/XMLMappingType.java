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
package net.codecadenza.eclipse.model.project;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>XML Mapping Type</b></em>', and utility methods for working with
 * them.
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getXMLMappingType()
 * @model
 * @generated
 */
public enum XMLMappingType implements Enumerator {
	/**
	 * The '<em><b>ATTRIBUTE</b></em>' literal object
	 * @see #ATTRIBUTE_VALUE
	 * @generated
	 * @ordered
	 */
	ATTRIBUTE(0, "ATTRIBUTE", "ATTRIBUTE"),

	/**
	 * The '<em><b>ELEMENT</b></em>' literal object
	 * @see #ELEMENT_VALUE
	 * @generated
	 * @ordered
	 */
	ELEMENT(1, "ELEMENT", "ELEMENT");

	/**
	 * The '<em><b>ATTRIBUTE</b></em>' literal value
	 * @see #ATTRIBUTE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ATTRIBUTE_VALUE = 0;

	/**
	 * The '<em><b>ELEMENT</b></em>' literal value
	 * @see #ELEMENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ELEMENT_VALUE = 1;

	/**
	 * An array of all the '<em><b>XML Mapping Type</b></em>' enumerators
	 * @generated
	 */
	private static final XMLMappingType[] VALUES_ARRAY = { ATTRIBUTE, ELEMENT };

	/**
	 * A public read-only list of all the '<em><b>XML Mapping Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<XMLMappingType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>XML Mapping Type</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static XMLMappingType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>XML Mapping Type</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static XMLMappingType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>XML Mapping Type</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static XMLMappingType get(int value) {
		switch (value) {
			case ATTRIBUTE_VALUE:
				return ATTRIBUTE;
			case ELEMENT_VALUE:
				return ELEMENT;
		}

		return null;
	}

	/**
	 * @generated
	 */
	private final int value;

	/**
	 * @generated
	 */
	private final String name;

	/**
	 * @generated
	 */
	private final String literal;

	/**
	 * Constructor
	 * @param value
	 * @param name
	 * @param literal
	 * @generated
	 */
	XMLMappingType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getValue()
	 * @generated
	 */
	@Override
	public int getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getLiteral()
	 * @generated
	 */
	@Override
	public String getLiteral() {
		return literal;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}

}
