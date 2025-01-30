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
 * A representation of the literals of the enumeration '<em><b>Association Tag Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAssociationTagEnumeration()
 * @model
 * @generated
 */
public enum AssociationTagEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>USER ROLE</b></em>' literal object
	 * @see #USER_ROLE_VALUE
	 * @generated
	 * @ordered
	 */
	USER_ROLE(1, "USER_ROLE", "USER_ROLE"),

	/**
	 * The '<em><b>LOGGING USER</b></em>' literal object
	 * @see #LOGGING_USER_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_USER(5, "LOGGING_USER", "LOGGING_USER"),

	/**
	 * The '<em><b>SAVEDQUERY OWNER</b></em>' literal object
	 * @see #SAVEDQUERY_OWNER_VALUE
	 * @generated
	 * @ordered
	 */
	SAVEDQUERY_OWNER(2, "SAVEDQUERY_OWNER", "SAVEDQUERY_OWNER"),

	/**
	 * The '<em><b>CLIENT REFERENCE</b></em>' literal object
	 * @see #CLIENT_REFERENCE_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT_REFERENCE(3, "CLIENT_REFERENCE", "CLIENT_REFERENCE");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>USER ROLE</b></em>' literal value
	 * @see #USER_ROLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_ROLE_VALUE = 1;

	/**
	 * The '<em><b>LOGGING USER</b></em>' literal value
	 * @see #LOGGING_USER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_USER_VALUE = 5;

	/**
	 * The '<em><b>SAVEDQUERY OWNER</b></em>' literal value
	 * @see #SAVEDQUERY_OWNER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAVEDQUERY_OWNER_VALUE = 2;

	/**
	 * The '<em><b>CLIENT REFERENCE</b></em>' literal value
	 * @see #CLIENT_REFERENCE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_REFERENCE_VALUE = 3;

	/**
	 * An array of all the '<em><b>Association Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final AssociationTagEnumeration[] VALUES_ARRAY = { NONE, USER_ROLE, LOGGING_USER, SAVEDQUERY_OWNER,
			CLIENT_REFERENCE };

	/**
	 * A public read-only list of all the '<em><b>Association Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<AssociationTagEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Association Tag Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static AssociationTagEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Association Tag Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static AssociationTagEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Association Tag Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static AssociationTagEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case USER_ROLE_VALUE:
				return USER_ROLE;
			case LOGGING_USER_VALUE:
				return LOGGING_USER;
			case SAVEDQUERY_OWNER_VALUE:
				return SAVEDQUERY_OWNER;
			case CLIENT_REFERENCE_VALUE:
				return CLIENT_REFERENCE;
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
	AssociationTagEnumeration(int value, String name, String literal) {
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
