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
package net.codecadenza.eclipse.model.integration;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Http Method Enumeration</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getHttpMethodEnumeration()
 * @model
 * @generated
 */
public enum HttpMethodEnumeration implements Enumerator {
	/**
	 * The '<em><b>GET</b></em>' literal object
	 * @see #GET_VALUE
	 * @generated
	 * @ordered
	 */
	GET(0, "GET", "GET"),

	/**
	 * The '<em><b>PUT</b></em>' literal object
	 * @see #PUT_VALUE
	 * @generated
	 * @ordered
	 */
	PUT(1, "PUT", "PUT"),

	/**
	 * The '<em><b>POST</b></em>' literal object
	 * @see #POST_VALUE
	 * @generated
	 * @ordered
	 */
	POST(2, "POST", "POST"),

	/**
	 * The '<em><b>DELETE</b></em>' literal object
	 * @see #DELETE_VALUE
	 * @generated
	 * @ordered
	 */
	DELETE(3, "DELETE", "DELETE");

	/**
	 * The '<em><b>GET</b></em>' literal value
	 * @see #GET
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GET_VALUE = 0;

	/**
	 * The '<em><b>PUT</b></em>' literal value
	 * @see #PUT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PUT_VALUE = 1;

	/**
	 * The '<em><b>POST</b></em>' literal value
	 * @see #POST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int POST_VALUE = 2;

	/**
	 * The '<em><b>DELETE</b></em>' literal value
	 * @see #DELETE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DELETE_VALUE = 3;

	/**
	 * An array of all the '<em><b>Http Method Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final HttpMethodEnumeration[] VALUES_ARRAY = { GET, PUT, POST, DELETE };

	/**
	 * A public read-only list of all the '<em><b>Http Method Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<HttpMethodEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>Http Method Enumeration</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static HttpMethodEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>Http Method Enumeration</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static HttpMethodEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>Http Method Enumeration</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static HttpMethodEnumeration get(int value) {
		switch (value) {
			case GET_VALUE:
				return GET;
			case PUT_VALUE:
				return PUT;
			case POST_VALUE:
				return POST;
			case DELETE_VALUE:
				return DELETE;
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
	HttpMethodEnumeration(int value, String name, String literal) {
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
