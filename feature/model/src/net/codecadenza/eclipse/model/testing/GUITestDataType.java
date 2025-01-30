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

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>GUI Test Data Type</b></em>', and utility methods for working with
 * them.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestDataType()
 * @model
 * @generated
 */
public enum GUITestDataType implements Enumerator {
	/**
	 * The '<em><b>FORM FIELD</b></em>' literal object
	 * @see #FORM_FIELD_VALUE
	 * @generated
	 * @ordered
	 */
	FORM_FIELD(0, "FORM_FIELD", "FORM_FIELD"),

	/**
	 * The '<em><b>PAGE TITLE</b></em>' literal object
	 * @see #PAGE_TITLE_VALUE
	 * @generated
	 * @ordered
	 */
	PAGE_TITLE(1, "PAGE_TITLE", "PAGE_TITLE"),

	/**
	 * The '<em><b>OBJECT ID</b></em>' literal object
	 * @see #OBJECT_ID_VALUE
	 * @generated
	 * @ordered
	 */
	OBJECT_ID(2, "OBJECT_ID", "OBJECT_ID"),

	/**
	 * The '<em><b>ROW INDEX</b></em>' literal object
	 * @see #ROW_INDEX_VALUE
	 * @generated
	 * @ordered
	 */
	ROW_INDEX(3, "ROW_INDEX", "ROW_INDEX"),

	/**
	 * The '<em><b>CELL VALUE</b></em>' literal object
	 * @see #CELL_VALUE_VALUE
	 * @generated
	 * @ordered
	 */
	CELL_VALUE(4, "CELL_VALUE", "CELL_VALUE"),

	/**
	 * The '<em><b>ROW COUNT</b></em>' literal object
	 * @see #ROW_COUNT_VALUE
	 * @generated
	 * @ordered
	 */
	ROW_COUNT(5, "ROW_COUNT", "ROW_COUNT"),

	/**
	 * The '<em><b>SEARCH FILTER</b></em>' literal object
	 * @see #SEARCH_FILTER_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_FILTER(6, "SEARCH_FILTER", "SEARCH_FILTER"),

	/**
	 * The '<em><b>SEARCH OPERATOR</b></em>' literal object
	 * @see #SEARCH_OPERATOR_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_OPERATOR(7, "SEARCH_OPERATOR", "SEARCH_OPERATOR"),

	/**
	 * The '<em><b>SEARCH SORT ORDER</b></em>' literal object
	 * @see #SEARCH_SORT_ORDER_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_SORT_ORDER(8, "SEARCH_SORT_ORDER", "SEARCH_SORT_ORDER");

	/**
	 * The '<em><b>FORM FIELD</b></em>' literal value
	 * @see #FORM_FIELD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FORM_FIELD_VALUE = 0;

	/**
	 * The '<em><b>PAGE TITLE</b></em>' literal value
	 * @see #PAGE_TITLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PAGE_TITLE_VALUE = 1;

	/**
	 * The '<em><b>OBJECT ID</b></em>' literal value
	 * @see #OBJECT_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int OBJECT_ID_VALUE = 2;

	/**
	 * The '<em><b>ROW INDEX</b></em>' literal value
	 * @see #ROW_INDEX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ROW_INDEX_VALUE = 3;

	/**
	 * The '<em><b>CELL VALUE</b></em>' literal value
	 * @see #CELL_VALUE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CELL_VALUE_VALUE = 4;

	/**
	 * The '<em><b>ROW COUNT</b></em>' literal value
	 * @see #ROW_COUNT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ROW_COUNT_VALUE = 5;

	/**
	 * The '<em><b>SEARCH FILTER</b></em>' literal value
	 * @see #SEARCH_FILTER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_FILTER_VALUE = 6;

	/**
	 * The '<em><b>SEARCH OPERATOR</b></em>' literal value
	 * @see #SEARCH_OPERATOR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_OPERATOR_VALUE = 7;

	/**
	 * The '<em><b>SEARCH SORT ORDER</b></em>' literal value
	 * @see #SEARCH_SORT_ORDER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_SORT_ORDER_VALUE = 8;

	/**
	 * An array of all the '<em><b>GUI Test Data Type</b></em>' enumerators
	 * @generated
	 */
	private static final GUITestDataType[] VALUES_ARRAY = { FORM_FIELD, PAGE_TITLE, OBJECT_ID, ROW_INDEX, CELL_VALUE, ROW_COUNT,
			SEARCH_FILTER, SEARCH_OPERATOR, SEARCH_SORT_ORDER };

	/**
	 * A public read-only list of all the '<em><b>GUI Test Data Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<GUITestDataType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>GUI Test Data Type</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestDataType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Data Type</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestDataType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Data Type</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestDataType get(int value) {
		switch (value) {
			case FORM_FIELD_VALUE:
				return FORM_FIELD;
			case PAGE_TITLE_VALUE:
				return PAGE_TITLE;
			case OBJECT_ID_VALUE:
				return OBJECT_ID;
			case ROW_INDEX_VALUE:
				return ROW_INDEX;
			case CELL_VALUE_VALUE:
				return CELL_VALUE;
			case ROW_COUNT_VALUE:
				return ROW_COUNT;
			case SEARCH_FILTER_VALUE:
				return SEARCH_FILTER;
			case SEARCH_OPERATOR_VALUE:
				return SEARCH_OPERATOR;
			case SEARCH_SORT_ORDER_VALUE:
				return SEARCH_SORT_ORDER;
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
	GUITestDataType(int value, String name, String literal) {
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
