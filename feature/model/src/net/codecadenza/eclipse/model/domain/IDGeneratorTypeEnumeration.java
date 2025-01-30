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
package net.codecadenza.eclipse.model.domain;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>ID Generator Type Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGeneratorTypeEnumeration()
 * @model
 * @generated
 */
public enum IDGeneratorTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>SEQUENCE</b></em>' literal object
	 * @see #SEQUENCE_VALUE
	 * @generated
	 * @ordered
	 */
	SEQUENCE(1, "SEQUENCE", "SEQUENCE"),

	/**
	 * The '<em><b>IDENTITY</b></em>' literal object
	 * @see #IDENTITY_VALUE
	 * @generated
	 * @ordered
	 */
	IDENTITY(2, "IDENTITY", "IDENTITY"),

	/**
	 * The '<em><b>TABLE</b></em>' literal object
	 * @see #TABLE_VALUE
	 * @generated
	 * @ordered
	 */
	TABLE(3, "TABLE", "TABLE"),

	/**
	 * The '<em><b>UUID</b></em>' literal object
	 * @see #UUID_VALUE
	 * @generated
	 * @ordered
	 */
	UUID(4, "UUID", "UUID");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>SEQUENCE</b></em>' literal value
	 * @see #SEQUENCE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEQUENCE_VALUE = 1;

	/**
	 * The '<em><b>IDENTITY</b></em>' literal value
	 * @see #IDENTITY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int IDENTITY_VALUE = 2;

	/**
	 * The '<em><b>TABLE</b></em>' literal value
	 * @see #TABLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int TABLE_VALUE = 3;

	/**
	 * The '<em><b>UUID</b></em>' literal value
	 * @see #UUID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UUID_VALUE = 4;

	/**
	 * An array of all the '<em><b>ID Generator Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final IDGeneratorTypeEnumeration[] VALUES_ARRAY = { NONE, SEQUENCE, IDENTITY, TABLE, UUID };

	/**
	 * A public read-only list of all the '<em><b>ID Generator Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<IDGeneratorTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>ID Generator Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static IDGeneratorTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>ID Generator Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static IDGeneratorTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>ID Generator Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static IDGeneratorTypeEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case SEQUENCE_VALUE:
				return SEQUENCE;
			case IDENTITY_VALUE:
				return IDENTITY;
			case TABLE_VALUE:
				return TABLE;
			case UUID_VALUE:
				return UUID;
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
	IDGeneratorTypeEnumeration(int value, String name, String literal) {
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
