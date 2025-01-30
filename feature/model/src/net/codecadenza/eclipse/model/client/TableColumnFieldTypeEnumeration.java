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
package net.codecadenza.eclipse.model.client;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Table Column Field Type Enumeration</b></em>', and utility methods
 * for working with them.
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnFieldTypeEnumeration()
 * @model
 * @generated
 */
public enum TableColumnFieldTypeEnumeration implements Enumerator {
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
	 * The '<em><b>DOUBLE</b></em>' literal object
	 * @see #DOUBLE_VALUE
	 * @generated
	 * @ordered
	 */
	DOUBLE(2, "DOUBLE", "DOUBLE"),

	/**
	 * The '<em><b>BOOLEAN</b></em>' literal object
	 * @see #BOOLEAN_VALUE
	 * @generated
	 * @ordered
	 */
	BOOLEAN(3, "BOOLEAN", "BOOLEAN"),

	/**
	 * The '<em><b>DATE</b></em>' literal object
	 * @see #DATE_VALUE
	 * @generated
	 * @ordered
	 */
	DATE(4, "DATE", "DATE"),

	/**
	 * The '<em><b>GREGORIAN CALENDAR</b></em>' literal object
	 * @see #GREGORIAN_CALENDAR_VALUE
	 * @generated
	 * @ordered
	 */
	GREGORIAN_CALENDAR(5, "GREGORIAN_CALENDAR", "GREGORIAN_CALENDAR"),

	/**
	 * The '<em><b>ENUM</b></em>' literal object
	 * @see #ENUM_VALUE
	 * @generated
	 * @ordered
	 */
	ENUM(6, "ENUM", "ENUM"),

	/**
	 * The '<em><b>FLOAT</b></em>' literal object
	 * @see #FLOAT_VALUE
	 * @generated
	 * @ordered
	 */
	FLOAT(7, "FLOAT", "FLOAT"),

	/**
	 * The '<em><b>LONG</b></em>' literal object
	 * @see #LONG_VALUE
	 * @generated
	 * @ordered
	 */
	LONG(8, "LONG", "LONG"),

	/**
	 * The '<em><b>CHAR</b></em>' literal object
	 * @see #CHAR_VALUE
	 * @generated
	 * @ordered
	 */
	CHAR(9, "CHAR", "CHAR"),

	/**
	 * The '<em><b>BIG DECIMAL</b></em>' literal object
	 * @see #BIG_DECIMAL_VALUE
	 * @generated
	 * @ordered
	 */
	BIG_DECIMAL(10, "BIG_DECIMAL", "BIG_DECIMAL"),

	/**
	 * The '<em><b>LOCAL DATE</b></em>' literal object
	 * @see #LOCAL_DATE_VALUE
	 * @generated
	 * @ordered
	 */
	LOCAL_DATE(11, "LOCAL_DATE", "LOCAL_DATE"),

	/**
	 * The '<em><b>LOCAL DATE TIME</b></em>' literal object
	 * @see #LOCAL_DATE_TIME_VALUE
	 * @generated
	 * @ordered
	 */
	LOCAL_DATE_TIME(12, "LOCAL_DATE_TIME", "LOCAL_DATE_TIME"),

	/**
	 * The '<em><b>UUID BINARY</b></em>' literal object
	 * @see #UUID_BINARY_VALUE
	 * @generated
	 * @ordered
	 */
	UUID_BINARY(13, "UUID_BINARY", "UUID_BINARY"),

	/**
	 * The '<em><b>UUID STRING</b></em>' literal object
	 * @see #UUID_STRING_VALUE
	 * @generated
	 * @ordered
	 */
	UUID_STRING(14, "UUID_STRING", "UUID_STRING");

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
	 * The '<em><b>DOUBLE</b></em>' literal value
	 * @see #DOUBLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOUBLE_VALUE = 2;

	/**
	 * The '<em><b>BOOLEAN</b></em>' literal value
	 * @see #BOOLEAN
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int BOOLEAN_VALUE = 3;

	/**
	 * The '<em><b>DATE</b></em>' literal value
	 * @see #DATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DATE_VALUE = 4;

	/**
	 * The '<em><b>GREGORIAN CALENDAR</b></em>' literal value
	 * @see #GREGORIAN_CALENDAR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GREGORIAN_CALENDAR_VALUE = 5;

	/**
	 * The '<em><b>ENUM</b></em>' literal value
	 * @see #ENUM
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ENUM_VALUE = 6;

	/**
	 * The '<em><b>FLOAT</b></em>' literal value
	 * @see #FLOAT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FLOAT_VALUE = 7;

	/**
	 * The '<em><b>LONG</b></em>' literal value
	 * @see #LONG
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LONG_VALUE = 8;

	/**
	 * The '<em><b>CHAR</b></em>' literal value
	 * @see #CHAR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CHAR_VALUE = 9;

	/**
	 * The '<em><b>BIG DECIMAL</b></em>' literal value
	 * @see #BIG_DECIMAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int BIG_DECIMAL_VALUE = 10;

	/**
	 * The '<em><b>LOCAL DATE</b></em>' literal value
	 * @see #LOCAL_DATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOCAL_DATE_VALUE = 11;

	/**
	 * The '<em><b>LOCAL DATE TIME</b></em>' literal value
	 * @see #LOCAL_DATE_TIME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOCAL_DATE_TIME_VALUE = 12;

	/**
	 * The '<em><b>UUID BINARY</b></em>' literal value
	 * @see #UUID_BINARY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UUID_BINARY_VALUE = 13;

	/**
	 * The '<em><b>UUID STRING</b></em>' literal value
	 * @see #UUID_STRING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UUID_STRING_VALUE = 14;

	/**
	 * An array of all the '<em><b>Table Column Field Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final TableColumnFieldTypeEnumeration[] VALUES_ARRAY = { STRING, INTEGER, DOUBLE, BOOLEAN, DATE,
			GREGORIAN_CALENDAR, ENUM, FLOAT, LONG, CHAR, BIG_DECIMAL, LOCAL_DATE, LOCAL_DATE_TIME, UUID_BINARY, UUID_STRING };

	/**
	 * A public read-only list of all the '<em><b>Table Column Field Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<TableColumnFieldTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Table Column Field Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static TableColumnFieldTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Table Column Field Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static TableColumnFieldTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Table Column Field Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static TableColumnFieldTypeEnumeration get(int value) {
		switch (value) {
			case STRING_VALUE:
				return STRING;
			case INTEGER_VALUE:
				return INTEGER;
			case DOUBLE_VALUE:
				return DOUBLE;
			case BOOLEAN_VALUE:
				return BOOLEAN;
			case DATE_VALUE:
				return DATE;
			case GREGORIAN_CALENDAR_VALUE:
				return GREGORIAN_CALENDAR;
			case ENUM_VALUE:
				return ENUM;
			case FLOAT_VALUE:
				return FLOAT;
			case LONG_VALUE:
				return LONG;
			case CHAR_VALUE:
				return CHAR;
			case BIG_DECIMAL_VALUE:
				return BIG_DECIMAL;
			case LOCAL_DATE_VALUE:
				return LOCAL_DATE;
			case LOCAL_DATE_TIME_VALUE:
				return LOCAL_DATE_TIME;
			case UUID_BINARY_VALUE:
				return UUID_BINARY;
			case UUID_STRING_VALUE:
				return UUID_STRING;
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
	TableColumnFieldTypeEnumeration(int value, String name, String literal) {
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
