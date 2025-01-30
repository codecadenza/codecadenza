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
 * A representation of the literals of the enumeration '<em><b>Selenium Driver</b></em>', and utility methods for working with
 * them.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumDriver()
 * @model
 * @generated
 */
public enum SeleniumDriver implements Enumerator {
	/**
	 * The '<em><b>CHROME</b></em>' literal object
	 * @see #CHROME_VALUE
	 * @generated
	 * @ordered
	 */
	CHROME(0, "CHROME", "CHROME"),

	/**
	 * The '<em><b>EDGE</b></em>' literal object
	 * @see #EDGE_VALUE
	 * @generated
	 * @ordered
	 */
	EDGE(1, "EDGE", "EDGE"),

	/**
	 * The '<em><b>FIREFOX</b></em>' literal object
	 * @see #FIREFOX_VALUE
	 * @generated
	 * @ordered
	 */
	FIREFOX(2, "FIREFOX", "FIREFOX"),

	/**
	 * The '<em><b>INTERNET EXPLORER</b></em>' literal object
	 * @see #INTERNET_EXPLORER_VALUE
	 * @generated
	 * @ordered
	 */
	INTERNET_EXPLORER(3, "INTERNET_EXPLORER", "INTERNET_EXPLORER"),

	/**
	 * The '<em><b>OPERA</b></em>' literal object
	 * @see #OPERA_VALUE
	 * @generated
	 * @ordered
	 */
	OPERA(4, "OPERA", "OPERA"),

	/**
	 * The '<em><b>SAFARI</b></em>' literal object
	 * @see #SAFARI_VALUE
	 * @generated
	 * @ordered
	 */
	SAFARI(5, "SAFARI", "SAFARI");

	/**
	 * The '<em><b>CHROME</b></em>' literal value
	 * @see #CHROME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CHROME_VALUE = 0;

	/**
	 * The '<em><b>EDGE</b></em>' literal value
	 * @see #EDGE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EDGE_VALUE = 1;

	/**
	 * The '<em><b>FIREFOX</b></em>' literal value
	 * @see #FIREFOX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIREFOX_VALUE = 2;

	/**
	 * The '<em><b>INTERNET EXPLORER</b></em>' literal value
	 * @see #INTERNET_EXPLORER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTERNET_EXPLORER_VALUE = 3;

	/**
	 * The '<em><b>OPERA</b></em>' literal value
	 * @see #OPERA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int OPERA_VALUE = 4;

	/**
	 * The '<em><b>SAFARI</b></em>' literal value
	 * @see #SAFARI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAFARI_VALUE = 5;

	/**
	 * An array of all the '<em><b>Selenium Driver</b></em>' enumerators
	 * @generated
	 */
	private static final SeleniumDriver[] VALUES_ARRAY = { CHROME, EDGE, FIREFOX, INTERNET_EXPLORER, OPERA, SAFARI };

	/**
	 * A public read-only list of all the '<em><b>Selenium Driver</b></em>' enumerators
	 * @generated
	 */
	public static final List<SeleniumDriver> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>Selenium Driver</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static SeleniumDriver get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>Selenium Driver</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static SeleniumDriver getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>Selenium Driver</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static SeleniumDriver get(int value) {
		switch (value) {
			case CHROME_VALUE:
				return CHROME;
			case EDGE_VALUE:
				return EDGE;
			case FIREFOX_VALUE:
				return FIREFOX;
			case INTERNET_EXPLORER_VALUE:
				return INTERNET_EXPLORER;
			case OPERA_VALUE:
				return OPERA;
			case SAFARI_VALUE:
				return SAFARI;
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
	SeleniumDriver(int value, String name, String literal) {
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
