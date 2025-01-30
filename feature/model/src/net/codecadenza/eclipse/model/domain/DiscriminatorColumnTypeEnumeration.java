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
 * A representation of the literals of the enumeration '<em><b>Discriminator Column Type Enumeration</b></em>', and utility
 * methods for working with them.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDiscriminatorColumnTypeEnumeration()
 * @model
 * @generated
 */
public enum DiscriminatorColumnTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>STRING</b></em>' literal object
	 * @see #STRING_VALUE
	 * @generated
	 * @ordered
	 */
	STRING(0, "STRING", "STRING"),

	/**
	 * The '<em><b>INTEGER</b></em>' literal object
	 * @see #INTEGER_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGER(1, "INTEGER", "INTEGER"),

	/**
	 * The '<em><b>CHAR</b></em>' literal object
	 * @see #CHAR_VALUE
	 * @generated
	 * @ordered
	 */
	CHAR(2, "CHAR", "CHAR");

	/**
	 * The '<em><b>STRING</b></em>' literal value
	 * @see #STRING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int STRING_VALUE = 0;

	/**
	 * The '<em><b>INTEGER</b></em>' literal value
	 * @see #INTEGER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGER_VALUE = 1;

	/**
	 * The '<em><b>CHAR</b></em>' literal value
	 * @see #CHAR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CHAR_VALUE = 2;

	/**
	 * An array of all the '<em><b>Discriminator Column Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final DiscriminatorColumnTypeEnumeration[] VALUES_ARRAY = { STRING, INTEGER, CHAR };

	/**
	 * A public read-only list of all the '<em><b>Discriminator Column Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<DiscriminatorColumnTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Discriminator Column Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static DiscriminatorColumnTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Discriminator Column Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static DiscriminatorColumnTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Discriminator Column Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static DiscriminatorColumnTypeEnumeration get(int value) {
		switch (value) {
			case STRING_VALUE:
				return STRING;
			case INTEGER_VALUE:
				return INTEGER;
			case CHAR_VALUE:
				return CHAR;
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
	DiscriminatorColumnTypeEnumeration(int value, String name, String literal) {
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
