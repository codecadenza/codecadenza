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
package net.codecadenza.eclipse.model.repository;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Permission Mode Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getPermissionModeEnumeration()
 * @model
 * @generated
 */
public enum PermissionModeEnumeration implements Enumerator {
	/**
	 * The '<em><b>DENY ALL</b></em>' literal object
	 * @see #DENY_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	DENY_ALL(0, "DENY_ALL", "DENY_ALL"),

	/**
	 * The '<em><b>PERMIT ALL</b></em>' literal object
	 * @see #PERMIT_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	PERMIT_ALL(1, "PERMIT_ALL", "PERMIT_ALL"),

	/**
	 * The '<em><b>DEDICATED ROLES</b></em>' literal object
	 * @see #DEDICATED_ROLES_VALUE
	 * @generated
	 * @ordered
	 */
	DEDICATED_ROLES(2, "DEDICATED_ROLES", "DEDICATED_ROLES");

	/**
	 * The '<em><b>DENY ALL</b></em>' literal value
	 * @see #DENY_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DENY_ALL_VALUE = 0;

	/**
	 * The '<em><b>PERMIT ALL</b></em>' literal value
	 * @see #PERMIT_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PERMIT_ALL_VALUE = 1;

	/**
	 * The '<em><b>DEDICATED ROLES</b></em>' literal value
	 * @see #DEDICATED_ROLES
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DEDICATED_ROLES_VALUE = 2;

	/**
	 * An array of all the '<em><b>Permission Mode Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final PermissionModeEnumeration[] VALUES_ARRAY = { DENY_ALL, PERMIT_ALL, DEDICATED_ROLES };

	/**
	 * A public read-only list of all the '<em><b>Permission Mode Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<PermissionModeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Permission Mode Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static PermissionModeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Permission Mode Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static PermissionModeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Permission Mode Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static PermissionModeEnumeration get(int value) {
		switch (value) {
			case DENY_ALL_VALUE:
				return DENY_ALL;
			case PERMIT_ALL_VALUE:
				return PERMIT_ALL;
			case DEDICATED_ROLES_VALUE:
				return DEDICATED_ROLES;
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
	PermissionModeEnumeration(int value, String name, String literal) {
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
