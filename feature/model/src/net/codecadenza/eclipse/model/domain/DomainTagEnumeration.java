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
 * A representation of the literals of the enumeration '<em><b>Tag Enumeration</b></em>', and utility methods for working with
 * them.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainTagEnumeration()
 * @model
 * @generated
 */
public enum DomainTagEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>USER</b></em>' literal object
	 * @see #USER_VALUE
	 * @generated
	 * @ordered
	 */
	USER(2, "USER", "USER"),

	/**
	 * The '<em><b>LOGGING</b></em>' literal object
	 * @see #LOGGING_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING(4, "LOGGING", "LOGGING"),

	/**
	 * The '<em><b>ROLE</b></em>' literal object
	 * @see #ROLE_VALUE
	 * @generated
	 * @ordered
	 */
	ROLE(5, "ROLE", "ROLE"),

	/**
	 * The '<em><b>DOCUMENT</b></em>' literal object
	 * @see #DOCUMENT_VALUE
	 * @generated
	 * @ordered
	 */
	DOCUMENT(1, "DOCUMENT", "DOCUMENT"),

	/**
	 * The '<em><b>SAVEDQUERY</b></em>' literal object
	 * @see #SAVEDQUERY_VALUE
	 * @generated
	 * @ordered
	 */
	SAVEDQUERY(3, "SAVEDQUERY", "SAVEDQUERY"),

	/**
	 * The '<em><b>CLIENT</b></em>' literal object
	 * @see #CLIENT_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT(6, "CLIENT", "CLIENT");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>USER</b></em>' literal value
	 * @see #USER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_VALUE = 2;

	/**
	 * The '<em><b>LOGGING</b></em>' literal value
	 * @see #LOGGING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_VALUE = 4;

	/**
	 * The '<em><b>ROLE</b></em>' literal value
	 * @see #ROLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ROLE_VALUE = 5;

	/**
	 * The '<em><b>DOCUMENT</b></em>' literal value
	 * @see #DOCUMENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOCUMENT_VALUE = 1;

	/**
	 * The '<em><b>SAVEDQUERY</b></em>' literal value
	 * @see #SAVEDQUERY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAVEDQUERY_VALUE = 3;

	/**
	 * The '<em><b>CLIENT</b></em>' literal value
	 * @see #CLIENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_VALUE = 6;

	/**
	 * An array of all the '<em><b>Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final DomainTagEnumeration[] VALUES_ARRAY = { NONE, USER, LOGGING, ROLE, DOCUMENT, SAVEDQUERY, CLIENT };

	/**
	 * A public read-only list of all the '<em><b>Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<DomainTagEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Tag Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static DomainTagEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Tag Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static DomainTagEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Tag Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static DomainTagEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case USER_VALUE:
				return USER;
			case LOGGING_VALUE:
				return LOGGING;
			case ROLE_VALUE:
				return ROLE;
			case DOCUMENT_VALUE:
				return DOCUMENT;
			case SAVEDQUERY_VALUE:
				return SAVEDQUERY;
			case CLIENT_VALUE:
				return CLIENT;
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
	DomainTagEnumeration(int value, String name, String literal) {
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
