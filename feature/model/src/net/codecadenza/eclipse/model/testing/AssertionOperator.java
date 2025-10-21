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
package net.codecadenza.eclipse.model.testing;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Assertion Operator</b></em>', and utility methods for working with
 * them.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAssertionOperator()
 * @model
 * @generated
 */
public enum AssertionOperator implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>EQUAL</b></em>' literal object
	 * @see #EQUAL_VALUE
	 * @generated
	 * @ordered
	 */
	EQUAL(1, "EQUAL", "EQUAL"),

	/**
	 * The '<em><b>IS NULL</b></em>' literal object
	 * @see #IS_NULL_VALUE
	 * @generated
	 * @ordered
	 */
	IS_NULL(2, "IS_NULL", "IS_NULL"),

	/**
	 * The '<em><b>IS NOT NULL</b></em>' literal object
	 * @see #IS_NOT_NULL_VALUE
	 * @generated
	 * @ordered
	 */
	IS_NOT_NULL(3, "IS_NOT_NULL", "IS_NOT_NULL"),

	/**
	 * The '<em><b>GREATER OR EQUAL</b></em>' literal object
	 * @see #GREATER_OR_EQUAL_VALUE
	 * @generated
	 * @ordered
	 */
	GREATER_OR_EQUAL(4, "GREATER_OR_EQUAL", "GREATER_OR_EQUAL"),

	/**
	 * The '<em><b>GREATER</b></em>' literal object
	 * @see #GREATER_VALUE
	 * @generated
	 * @ordered
	 */
	GREATER(5, "GREATER", "GREATER"),

	/**
	 * The '<em><b>SMALLER OR EQUAL</b></em>' literal object
	 * @see #SMALLER_OR_EQUAL_VALUE
	 * @generated
	 * @ordered
	 */
	SMALLER_OR_EQUAL(6, "SMALLER_OR_EQUAL", "SMALLER_OR_EQUAL"),

	/**
	 * The '<em><b>SMALLER</b></em>' literal object
	 * @see #SMALLER_VALUE
	 * @generated
	 * @ordered
	 */
	SMALLER(7, "SMALLER", "SMALLER"),

	/**
	 * The '<em><b>STARTS WITH</b></em>' literal object
	 * @see #STARTS_WITH_VALUE
	 * @generated
	 * @ordered
	 */
	STARTS_WITH(8, "STARTS_WITH", "STARTS_WITH"),

	/**
	 * The '<em><b>ENDS WITH</b></em>' literal object
	 * @see #ENDS_WITH_VALUE
	 * @generated
	 * @ordered
	 */
	ENDS_WITH(9, "ENDS_WITH", "ENDS_WITH"),

	/**
	 * The '<em><b>CONTAINS</b></em>' literal object
	 * @see #CONTAINS_VALUE
	 * @generated
	 * @ordered
	 */
	CONTAINS(10, "CONTAINS", "CONTAINS"),

	/**
	 * The '<em><b>IS_EMPTY</b></em>' literal object
	 * @see #IS_EMPTY_VALUE
	 * @generated
	 * @ordered
	 */
	IS_EMPTY(11, "IS_EMPTY", "IS_EMPTY");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>EQUAL</b></em>' literal value
	 * @see #EQUAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EQUAL_VALUE = 1;

	/**
	 * The '<em><b>IS NULL</b></em>' literal value
	 * @see #IS_NULL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int IS_NULL_VALUE = 2;

	/**
	 * The '<em><b>IS NOT NULL</b></em>' literal value
	 * @see #IS_NOT_NULL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int IS_NOT_NULL_VALUE = 3;

	/**
	 * The '<em><b>GREATER OR EQUAL</b></em>' literal value
	 * @see #GREATER_OR_EQUAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GREATER_OR_EQUAL_VALUE = 4;

	/**
	 * The '<em><b>GREATER</b></em>' literal value
	 * @see #GREATER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GREATER_VALUE = 5;

	/**
	 * The '<em><b>SMALLER OR EQUAL</b></em>' literal value
	 * @see #SMALLER_OR_EQUAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SMALLER_OR_EQUAL_VALUE = 6;

	/**
	 * The '<em><b>SMALLER</b></em>' literal value
	 * @see #SMALLER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SMALLER_VALUE = 7;

	/**
	 * The '<em><b>STARTS WITH</b></em>' literal value
	 * @see #STARTS_WITH
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int STARTS_WITH_VALUE = 8;

	/**
	 * The '<em><b>ENDS WITH</b></em>' literal value
	 * @see #ENDS_WITH
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ENDS_WITH_VALUE = 9;

	/**
	 * The '<em><b>CONTAINS</b></em>' literal value
	 * @see #CONTAINS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CONTAINS_VALUE = 10;

	/**
	 * The '<em><b>IS_EMPTY</b></em>' literal value
	 * @see #IS_EMPTY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int IS_EMPTY_VALUE = 11;

	/**
	 * An array of all the '<em><b>Assertion Operator</b></em>' enumerators
	 * @generated
	 */
	private static final AssertionOperator[] VALUES_ARRAY = { NONE, EQUAL, IS_NULL, IS_NOT_NULL, GREATER_OR_EQUAL, GREATER,
			SMALLER_OR_EQUAL, SMALLER, STARTS_WITH, ENDS_WITH, CONTAINS, IS_EMPTY };

	/**
	 * A public read-only list of all the '<em><b>Assertion Operator</b></em>' enumerators
	 * @generated
	 */
	public static final List<AssertionOperator> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Return the '<em><b>Assertion Operator</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or <code>null</code>
	 * @generated
	 */
	public static AssertionOperator get(String literal) {
		for (final AssertionOperator result : VALUES_ARRAY)
			if (result.toString().equals(literal))
				return result;

		return null;
	}

	/**
	 * Return the '<em><b>Assertion Operator</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or <code>null</code>
	 * @generated
	 */
	public static AssertionOperator getByName(String name) {
		for (final AssertionOperator result : VALUES_ARRAY)
			if (result.getName().equals(name))
				return result;

		return null;
	}

	/**
	 * Return the '<em><b>Assertion Operator</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or <code>null</code>
	 * @generated
	 */
	public static AssertionOperator get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case EQUAL_VALUE:
				return EQUAL;
			case IS_NULL_VALUE:
				return IS_NULL;
			case IS_NOT_NULL_VALUE:
				return IS_NOT_NULL;
			case GREATER_OR_EQUAL_VALUE:
				return GREATER_OR_EQUAL;
			case GREATER_VALUE:
				return GREATER;
			case SMALLER_OR_EQUAL_VALUE:
				return SMALLER_OR_EQUAL;
			case SMALLER_VALUE:
				return SMALLER;
			case STARTS_WITH_VALUE:
				return STARTS_WITH;
			case ENDS_WITH_VALUE:
				return ENDS_WITH;
			case CONTAINS_VALUE:
				return CONTAINS;
			case IS_EMPTY_VALUE:
				return IS_EMPTY;
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
	AssertionOperator(int value, String name, String literal) {
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
