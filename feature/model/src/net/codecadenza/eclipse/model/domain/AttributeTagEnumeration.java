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
 * A representation of the literals of the enumeration '<em><b>Attribute Tag Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAttributeTagEnumeration()
 * @model
 * @generated
 */
public enum AttributeTagEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>USER NAME</b></em>' literal object
	 * @see #USER_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	USER_NAME(1, "USER_NAME", "USER_NAME"),

	/**
	 * The '<em><b>USER PASSWORD</b></em>' literal object
	 * @see #USER_PASSWORD_VALUE
	 * @generated
	 * @ordered
	 */
	USER_PASSWORD(2, "USER_PASSWORD", "USER_PASSWORD"),

	/**
	 * The '<em><b>USER EMAIL</b></em>' literal object
	 * @see #USER_EMAIL_VALUE
	 * @generated
	 * @ordered
	 */
	USER_EMAIL(3, "USER_EMAIL", "USER_EMAIL"),

	/**
	 * The '<em><b>USER ACTIVE</b></em>' literal object
	 * @see #USER_ACTIVE_VALUE
	 * @generated
	 * @ordered
	 */
	USER_ACTIVE(4, "USER_ACTIVE", "USER_ACTIVE"),

	/**
	 * The '<em><b>LOGGING DURATION</b></em>' literal object
	 * @see #LOGGING_DURATION_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_DURATION(11, "LOGGING_DURATION", "LOGGING_DURATION"),

	/**
	 * The '<em><b>LOGGING STACK TRACE</b></em>' literal object
	 * @see #LOGGING_STACK_TRACE_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_STACK_TRACE(12, "LOGGING_STACK_TRACE", "LOGGING_STACK_TRACE"),

	/**
	 * The '<em><b>LOGGING CLASS NAME</b></em>' literal object
	 * @see #LOGGING_CLASS_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_CLASS_NAME(13, "LOGGING_CLASS_NAME", "LOGGING_CLASS_NAME"),

	/**
	 * The '<em><b>LOGGING METHOD NAME</b></em>' literal object
	 * @see #LOGGING_METHOD_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_METHOD_NAME(14, "LOGGING_METHOD_NAME", "LOGGING_METHOD_NAME"),

	/**
	 * The '<em><b>LOGGING DATE</b></em>' literal object
	 * @see #LOGGING_DATE_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_DATE(15, "LOGGING_DATE", "LOGGING_DATE"),

	/**
	 * The '<em><b>LOGGING MESSAGE</b></em>' literal object
	 * @see #LOGGING_MESSAGE_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_MESSAGE(16, "LOGGING_MESSAGE", "LOGGING_MESSAGE"),

	/**
	 * The '<em><b>LOGGING LEVEL</b></em>' literal object
	 * @see #LOGGING_LEVEL_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_LEVEL(17, "LOGGING_LEVEL", "LOGGING_LEVEL"),

	/**
	 * The '<em><b>LOGGING HOST</b></em>' literal object
	 * @see #LOGGING_HOST_VALUE
	 * @generated
	 * @ordered
	 */
	LOGGING_HOST(18, "LOGGING_HOST", "LOGGING_HOST"),

	/**
	 * The '<em><b>ROLE NAME</b></em>' literal object
	 * @see #ROLE_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	ROLE_NAME(22, "ROLE_NAME", "ROLE_NAME"),

	/**
	 * The '<em><b>DOCUMENT REF</b></em>' literal object
	 * @see #DOCUMENT_REF_VALUE
	 * @generated
	 * @ordered
	 */
	DOCUMENT_REF(5, "DOCUMENT_REF", "DOCUMENT_REF"),

	/**
	 * The '<em><b>DOCUMENT DATA</b></em>' literal object
	 * @see #DOCUMENT_DATA_VALUE
	 * @generated
	 * @ordered
	 */
	DOCUMENT_DATA(6, "DOCUMENT_DATA", "DOCUMENT_DATA"),

	/**
	 * The '<em><b>DOCUMENT NAME</b></em>' literal object
	 * @see #DOCUMENT_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	DOCUMENT_NAME(7, "DOCUMENT_NAME", "DOCUMENT_NAME"),

	/**
	 * The '<em><b>DOCUMENT SIZE</b></em>' literal object
	 * @see #DOCUMENT_SIZE_VALUE
	 * @generated
	 * @ordered
	 */
	DOCUMENT_SIZE(8, "DOCUMENT_SIZE", "DOCUMENT_SIZE"),

	/**
	 * The '<em><b>SAVEDQUERY TITLE</b></em>' literal object
	 * @see #SAVEDQUERY_TITLE_VALUE
	 * @generated
	 * @ordered
	 */
	SAVEDQUERY_TITLE(9, "SAVEDQUERY_TITLE", "SAVEDQUERY_TITLE"),

	/**
	 * The '<em><b>SAVEDQUERY VIEW NAME</b></em>' literal object
	 * @see #SAVEDQUERY_VIEW_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	SAVEDQUERY_VIEW_NAME(10, "SAVEDQUERY_VIEW_NAME", "SAVEDQUERY_VIEW_NAME"),

	/**
	 * The '<em><b>SAVEDQUERY DATA OBJ</b></em>' literal object
	 * @see #SAVEDQUERY_DATA_OBJ_VALUE
	 * @generated
	 * @ordered
	 */
	SAVEDQUERY_DATA_OBJ(19, "SAVEDQUERY_DATA_OBJ", "SAVEDQUERY_DATA_OBJ"),

	/**
	 * The '<em><b>CLIENT NAME</b></em>' literal object
	 * @see #CLIENT_NAME_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT_NAME(20, "CLIENT_NAME", "CLIENT_NAME"),

	/**
	 * The '<em><b>CLIENT ACTIVE</b></em>' literal object
	 * @see #CLIENT_ACTIVE_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT_ACTIVE(21, "CLIENT_ACTIVE", "CLIENT_ACTIVE"),

	/**
	 * The '<em><b>CLIENT DISPLAY</b></em>' literal object
	 * @see #CLIENT_DISPLAY_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT_DISPLAY(23, "CLIENT_DISPLAY", "CLIENT_DISPLAY");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>USER NAME</b></em>' literal value
	 * @see #USER_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_NAME_VALUE = 1;

	/**
	 * The '<em><b>USER PASSWORD</b></em>' literal value
	 * @see #USER_PASSWORD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_PASSWORD_VALUE = 2;

	/**
	 * The '<em><b>USER EMAIL</b></em>' literal value
	 * @see #USER_EMAIL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_EMAIL_VALUE = 3;

	/**
	 * The '<em><b>USER ACTIVE</b></em>' literal value
	 * @see #USER_ACTIVE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int USER_ACTIVE_VALUE = 4;

	/**
	 * The '<em><b>LOGGING DURATION</b></em>' literal value
	 * @see #LOGGING_DURATION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_DURATION_VALUE = 11;

	/**
	 * The '<em><b>LOGGING STACK TRACE</b></em>' literal value
	 * @see #LOGGING_STACK_TRACE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_STACK_TRACE_VALUE = 12;

	/**
	 * The '<em><b>LOGGING CLASS NAME</b></em>' literal value
	 * @see #LOGGING_CLASS_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_CLASS_NAME_VALUE = 13;

	/**
	 * The '<em><b>LOGGING METHOD NAME</b></em>' literal value
	 * @see #LOGGING_METHOD_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_METHOD_NAME_VALUE = 14;

	/**
	 * The '<em><b>LOGGING DATE</b></em>' literal value
	 * @see #LOGGING_DATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_DATE_VALUE = 15;

	/**
	 * The '<em><b>LOGGING MESSAGE</b></em>' literal value
	 * @see #LOGGING_MESSAGE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_MESSAGE_VALUE = 16;

	/**
	 * The '<em><b>LOGGING LEVEL</b></em>' literal value
	 * @see #LOGGING_LEVEL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_LEVEL_VALUE = 17;

	/**
	 * The '<em><b>LOGGING HOST</b></em>' literal value
	 * @see #LOGGING_HOST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOGGING_HOST_VALUE = 18;

	/**
	 * The '<em><b>ROLE NAME</b></em>' literal value
	 * @see #ROLE_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ROLE_NAME_VALUE = 22;

	/**
	 * The '<em><b>DOCUMENT REF</b></em>' literal value
	 * @see #DOCUMENT_REF
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOCUMENT_REF_VALUE = 5;

	/**
	 * The '<em><b>DOCUMENT DATA</b></em>' literal value
	 * @see #DOCUMENT_DATA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOCUMENT_DATA_VALUE = 6;

	/**
	 * The '<em><b>DOCUMENT NAME</b></em>' literal value
	 * @see #DOCUMENT_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOCUMENT_NAME_VALUE = 7;

	/**
	 * The '<em><b>DOCUMENT SIZE</b></em>' literal value
	 * @see #DOCUMENT_SIZE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOCUMENT_SIZE_VALUE = 8;

	/**
	 * The '<em><b>SAVEDQUERY TITLE</b></em>' literal value
	 * @see #SAVEDQUERY_TITLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAVEDQUERY_TITLE_VALUE = 9;

	/**
	 * The '<em><b>SAVEDQUERY VIEW NAME</b></em>' literal value
	 * @see #SAVEDQUERY_VIEW_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAVEDQUERY_VIEW_NAME_VALUE = 10;

	/**
	 * The '<em><b>SAVEDQUERY DATA OBJ</b></em>' literal value
	 * @see #SAVEDQUERY_DATA_OBJ
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAVEDQUERY_DATA_OBJ_VALUE = 19;

	/**
	 * The '<em><b>CLIENT NAME</b></em>' literal value
	 * @see #CLIENT_NAME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_NAME_VALUE = 20;

	/**
	 * The '<em><b>CLIENT ACTIVE</b></em>' literal value
	 * @see #CLIENT_ACTIVE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_ACTIVE_VALUE = 21;

	/**
	 * The '<em><b>CLIENT DISPLAY</b></em>' literal value
	 * @see #CLIENT_DISPLAY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_DISPLAY_VALUE = 23;

	/**
	 * An array of all the '<em><b>Attribute Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final AttributeTagEnumeration[] VALUES_ARRAY = { NONE, USER_NAME, USER_PASSWORD, USER_EMAIL, USER_ACTIVE,
			LOGGING_DURATION, LOGGING_STACK_TRACE, LOGGING_CLASS_NAME, LOGGING_METHOD_NAME, LOGGING_DATE, LOGGING_MESSAGE,
			LOGGING_LEVEL, LOGGING_HOST, ROLE_NAME, DOCUMENT_REF, DOCUMENT_DATA, DOCUMENT_NAME, DOCUMENT_SIZE, SAVEDQUERY_TITLE,
			SAVEDQUERY_VIEW_NAME, SAVEDQUERY_DATA_OBJ, CLIENT_NAME, CLIENT_ACTIVE, CLIENT_DISPLAY };

	/**
	 * A public read-only list of all the '<em><b>Attribute Tag Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<AttributeTagEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Attribute Tag Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static AttributeTagEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Attribute Tag Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static AttributeTagEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Attribute Tag Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static AttributeTagEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case USER_NAME_VALUE:
				return USER_NAME;
			case USER_PASSWORD_VALUE:
				return USER_PASSWORD;
			case USER_EMAIL_VALUE:
				return USER_EMAIL;
			case USER_ACTIVE_VALUE:
				return USER_ACTIVE;
			case LOGGING_DURATION_VALUE:
				return LOGGING_DURATION;
			case LOGGING_STACK_TRACE_VALUE:
				return LOGGING_STACK_TRACE;
			case LOGGING_CLASS_NAME_VALUE:
				return LOGGING_CLASS_NAME;
			case LOGGING_METHOD_NAME_VALUE:
				return LOGGING_METHOD_NAME;
			case LOGGING_DATE_VALUE:
				return LOGGING_DATE;
			case LOGGING_MESSAGE_VALUE:
				return LOGGING_MESSAGE;
			case LOGGING_LEVEL_VALUE:
				return LOGGING_LEVEL;
			case LOGGING_HOST_VALUE:
				return LOGGING_HOST;
			case ROLE_NAME_VALUE:
				return ROLE_NAME;
			case DOCUMENT_REF_VALUE:
				return DOCUMENT_REF;
			case DOCUMENT_DATA_VALUE:
				return DOCUMENT_DATA;
			case DOCUMENT_NAME_VALUE:
				return DOCUMENT_NAME;
			case DOCUMENT_SIZE_VALUE:
				return DOCUMENT_SIZE;
			case SAVEDQUERY_TITLE_VALUE:
				return SAVEDQUERY_TITLE;
			case SAVEDQUERY_VIEW_NAME_VALUE:
				return SAVEDQUERY_VIEW_NAME;
			case SAVEDQUERY_DATA_OBJ_VALUE:
				return SAVEDQUERY_DATA_OBJ;
			case CLIENT_NAME_VALUE:
				return CLIENT_NAME;
			case CLIENT_ACTIVE_VALUE:
				return CLIENT_ACTIVE;
			case CLIENT_DISPLAY_VALUE:
				return CLIENT_DISPLAY;
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
	AttributeTagEnumeration(int value, String name, String literal) {
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
