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
 * A representation of the literals of the enumeration '<em><b>Enum Literal Tag Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteralTagEnumeration()
 * @model
 * @generated
 */
public enum EnumLiteralTagEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>LOGGING LEVEL WARN</b></em>' literal object
	 * @see #LOGGING_LEVEL_WARN_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_LEVEL_WARN(1, "LOGGING_LEVEL_WARN", "LOGGING_LEVEL_WARN"),

	/**
	 * The '<em><b>LOGGING LEVEL ERROR</b></em>' literal object
	 * @see #LOGGING_LEVEL_ERROR_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_LEVEL_ERROR(2, "LOGGING_LEVEL_ERROR", "LOGGING_LEVEL_ERROR"),

	/**
	 * The '<em><b>LOGGING LEVEL INFO</b></em>' literal object
	 * @see #LOGGING_LEVEL_INFO_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_LEVEL_INFO(3, "LOGGING_LEVEL_INFO", "LOGGING_LEVEL_INFO"),

	/**
	 * The '<em><b>LOGGING LEVEL DEBUG</b></em>' literal object
	 * @see #LOGGING_LEVEL_DEBUG_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_LEVEL_DEBUG(4, "LOGGING_LEVEL_DEBUG", "LOGGING_LEVEL_DEBUG");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>LOGGING LEVEL WARN</b></em>' literal value
	 * @see #LOGGING_LEVEL_WARN
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_LEVEL_WARN_VALUE = 1;

	/**
	 * The '<em><b>LOGGING LEVEL ERROR</b></em>' literal value
	 * @see #LOGGING_LEVEL_ERROR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_LEVEL_ERROR_VALUE = 2;

	/**
	 * The '<em><b>LOGGING LEVEL INFO</b></em>' literal value
	 * @see #LOGGING_LEVEL_INFO
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_LEVEL_INFO_VALUE = 3;

	/**
	 * The '<em><b>LOGGING LEVEL DEBUG</b></em>' literal value
	 * @see #LOGGING_LEVEL_DEBUG
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_LEVEL_DEBUG_VALUE = 4;

	/**
	 * An array of all the '<em><b>Enum Literal Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final EnumLiteralTagEnumeration[] VALUES_ARRAY = { NONE, LOGGING_LEVEL_WARN, LOGGING_LEVEL_ERROR,
			LOGGING_LEVEL_INFO, LOGGING_LEVEL_DEBUG };

	/**
	 * A public read-only list of all the '<em><b>Enum Literal Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<EnumLiteralTagEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Enum Literal Tag Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static EnumLiteralTagEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Enum Literal Tag Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static EnumLiteralTagEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Enum Literal Tag Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static EnumLiteralTagEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case LOGGING_LEVEL_WARN_VALUE:
				return LOGGING_LEVEL_WARN;
			case LOGGING_LEVEL_ERROR_VALUE:
				return LOGGING_LEVEL_ERROR;
			case LOGGING_LEVEL_INFO_VALUE:
				return LOGGING_LEVEL_INFO;
			case LOGGING_LEVEL_DEBUG_VALUE:
				return LOGGING_LEVEL_DEBUG;
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
	EnumLiteralTagEnumeration(int value, String name, String literal) {
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
