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
 * A representation of the literals of the enumeration '<em><b>Transaction Type Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getTransactionTypeEnumeration()
 * @model
 * @generated
 */
public enum TransactionTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>REQUIRES NEW</b></em>' literal object
	 * @see #REQUIRES_NEW_VALUE
	 * @generated
	 * @ordered
	 */
	REQUIRES_NEW(0, "REQUIRES_NEW", "REQUIRES_NEW"),

	/**
	 * The '<em><b>REQUIRED</b></em>' literal object
	 * @see #REQUIRED_VALUE
	 * @generated
	 * @ordered
	 */
	REQUIRED(1, "REQUIRED", "REQUIRED"),

	/**
	 * The '<em><b>SUPPORTS</b></em>' literal object
	 * @see #SUPPORTS_VALUE
	 * @generated
	 * @ordered
	 */
	SUPPORTS(2, "SUPPORTS", "SUPPORTS"),

	/**
	 * The '<em><b>NOT SUPPORTED</b></em>' literal object
	 * @see #NOT_SUPPORTED_VALUE
	 * @generated
	 * @ordered
	 */
	NOT_SUPPORTED(3, "NOT_SUPPORTED", "NOT_SUPPORTED"),

	/**
	 * The '<em><b>MANDATORY</b></em>' literal object
	 * @see #MANDATORY_VALUE
	 * @generated
	 * @ordered
	 */
	MANDATORY(4, "MANDATORY", "MANDATORY");

	/**
	 * The '<em><b>REQUIRES NEW</b></em>' literal value
	 * @see #REQUIRES_NEW
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int REQUIRES_NEW_VALUE = 0;

	/**
	 * The '<em><b>REQUIRED</b></em>' literal value
	 * @see #REQUIRED
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int REQUIRED_VALUE = 1;

	/**
	 * The '<em><b>SUPPORTS</b></em>' literal value
	 * @see #SUPPORTS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SUPPORTS_VALUE = 2;

	/**
	 * The '<em><b>NOT SUPPORTED</b></em>' literal value
	 * @see #NOT_SUPPORTED
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NOT_SUPPORTED_VALUE = 3;

	/**
	 * The '<em><b>MANDATORY</b></em>' literal value
	 * @see #MANDATORY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MANDATORY_VALUE = 4;

	/**
	 * An array of all the '<em><b>Transaction Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final TransactionTypeEnumeration[] VALUES_ARRAY = { REQUIRES_NEW, REQUIRED, SUPPORTS, NOT_SUPPORTED, MANDATORY };

	/**
	 * A public read-only list of all the '<em><b>Transaction Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<TransactionTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Transaction Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static TransactionTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Transaction Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static TransactionTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Transaction Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static TransactionTypeEnumeration get(int value) {
		switch (value) {
			case REQUIRES_NEW_VALUE:
				return REQUIRES_NEW;
			case REQUIRED_VALUE:
				return REQUIRED;
			case SUPPORTS_VALUE:
				return SUPPORTS;
			case NOT_SUPPORTED_VALUE:
				return NOT_SUPPORTED;
			case MANDATORY_VALUE:
				return MANDATORY;
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
	TransactionTypeEnumeration(int value, String name, String literal) {
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
