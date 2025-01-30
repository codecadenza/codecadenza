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
package net.codecadenza.eclipse.model.boundary;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Method Data Fetch Type</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethodDataFetchType()
 * @model
 * @generated
 */
public enum BoundaryMethodDataFetchType implements Enumerator {
	/**
	 * The '<em><b>DEFAULT</b></em>' literal object
	 * @see #DEFAULT_VALUE
	 * @generated
	 * @ordered
	 */
	DEFAULT(0, "DEFAULT", "DEFAULT"),

	/**
	 * The '<em><b>CLIENT</b></em>' literal object
	 * @see #CLIENT_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT(1, "CLIENT", "CLIENT"),

	/**
	 * The '<em><b>USER</b></em>' literal object
	 * @see #USER_VALUE
	 * @generated
	 * @ordered
	 */
	USER(2, "USER", "USER");

	/**
	 * The '<em><b>DEFAULT</b></em>' literal value
	 * @see #DEFAULT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DEFAULT_VALUE = 0;

	/**
	 * The '<em><b>CLIENT</b></em>' literal value
	 * @see #CLIENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_VALUE = 1;

	/**
	 * The '<em><b>USER</b></em>' literal value
	 * @see #USER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_VALUE = 2;

	/**
	 * An array of all the '<em><b>Method Data Fetch Type</b></em>' enumerators
	 * @generated
	 */
	private static final BoundaryMethodDataFetchType[] VALUES_ARRAY = { DEFAULT, CLIENT, USER };

	/**
	 * A public read-only list of all the '<em><b>Method Data Fetch Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<BoundaryMethodDataFetchType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Method Data Fetch Type</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static BoundaryMethodDataFetchType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Method Data Fetch Type</b></em>' literal with the specified name
	 * @generated
	 */
	public static BoundaryMethodDataFetchType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Method Data Fetch Type</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static BoundaryMethodDataFetchType get(int value) {
		switch (value) {
			case DEFAULT_VALUE:
				return DEFAULT;
			case CLIENT_VALUE:
				return CLIENT;
			case USER_VALUE:
				return USER;
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
	BoundaryMethodDataFetchType(int value, String name, String literal) {
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
