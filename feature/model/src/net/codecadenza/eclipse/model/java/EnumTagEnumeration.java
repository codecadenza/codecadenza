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
package net.codecadenza.eclipse.model.java;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Enum Tag Enumeration</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumTagEnumeration()
 * @model
 * @generated
 */
public enum EnumTagEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>LOGGING LEVEL</b></em>' literal object
	 * @see #LOGGING_LEVEL_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_LEVEL(1, "LOGGING_LEVEL", "LOGGING_LEVEL");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>LOGGING LEVEL</b></em>' literal value
	 * @see #LOGGING_LEVEL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_LEVEL_VALUE = 1;

	/**
	 * An array of all the '<em><b>Enum Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final EnumTagEnumeration[] VALUES_ARRAY = { NONE, LOGGING_LEVEL };

	/**
	 * A public read-only list of all the '<em><b>Enum Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<EnumTagEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Enum Tag Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static EnumTagEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Enum Tag Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static EnumTagEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Enum Tag Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static EnumTagEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case LOGGING_LEVEL_VALUE:
				return LOGGING_LEVEL;
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
	EnumTagEnumeration(int value, String name, String literal) {
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
