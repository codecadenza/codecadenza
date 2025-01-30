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
package net.codecadenza.eclipse.model.exchange;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Parser Implementation Enumeration</b></em>', and utility methods
 * for working with them.
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getParserImplementationEnumeration()
 * @model
 * @generated
 */
public enum ParserImplementationEnumeration implements Enumerator {
	/**
	 * The '<em><b>POI</b></em>' literal object
	 * @see #POI_VALUE
	 * @generated
	 * @ordered
	 */
	POI(0, "POI", "POI"),

	/**
	 * The '<em><b>JAXB</b></em>' literal object
	 * @see #JAXB_VALUE
	 * @generated
	 * @ordered
	 */
	JAXB(1, "JAXB", "JAXB"),

	/**
	 * The '<em><b>APACHE COMMONS</b></em>' literal object
	 * @see #APACHE_COMMONS_VALUE
	 * @generated
	 * @ordered
	 */
	APACHE_COMMONS(2, "APACHE_COMMONS", "APACHE_COMMONS"),

	/**
	 * The '<em><b>JSONB</b></em>' literal object
	 * @see #JSONB_VALUE
	 * @generated
	 * @ordered
	 */
	JSONB(3, "JSONB", "JSONB");

	/**
	 * The '<em><b>POI</b></em>' literal value
	 * @see #POI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int POI_VALUE = 0;

	/**
	 * The '<em><b>JAXB</b></em>' literal value
	 * @see #JAXB
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JAXB_VALUE = 1;

	/**
	 * The '<em><b>APACHE COMMONS</b></em>' literal value
	 * @see #APACHE_COMMONS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int APACHE_COMMONS_VALUE = 2;

	/**
	 * The '<em><b>JSONB</b></em>' literal value
	 * @see #JSONB
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JSONB_VALUE = 3;

	/**
	 * An array of all the '<em><b>Parser Implementation Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final ParserImplementationEnumeration[] VALUES_ARRAY = { POI, JAXB, APACHE_COMMONS, JSONB };

	/**
	 * A public read-only list of all the '<em><b>Parser Implementation Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<ParserImplementationEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Parser Implementation Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static ParserImplementationEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Parser Implementation Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static ParserImplementationEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Parser Implementation Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static ParserImplementationEnumeration get(int value) {
		switch (value) {
			case POI_VALUE:
				return POI;
			case JAXB_VALUE:
				return JAXB;
			case APACHE_COMMONS_VALUE:
				return APACHE_COMMONS;
			case JSONB_VALUE:
				return JSONB;
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
	ParserImplementationEnumeration(int value, String name, String literal) {
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
