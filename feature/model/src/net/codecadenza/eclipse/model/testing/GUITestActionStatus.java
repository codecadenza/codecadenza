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
 * A representation of the literals of the enumeration '<em><b>GUI Test Action Status</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionStatus()
 * @model
 * @generated
 */
public enum GUITestActionStatus implements Enumerator {
	/**
	 * The '<em><b>INFO</b></em>' literal object
	 * @see #INFO_VALUE
	 * @generated
	 * @ordered
	 */
	INFO(0, "INFO", "INFO"),

	/**
	 * The '<em><b>WARNING</b></em>' literal object
	 * @see #WARNING_VALUE
	 * @generated
	 * @ordered
	 */
	WARNING(1, "WARNING", "WARNING"),

	/**
	 * The '<em><b>ERROR</b></em>' literal object
	 * @see #ERROR_VALUE
	 * @generated
	 * @ordered
	 */
	ERROR(2, "ERROR", "ERROR");

	/**
	 * The '<em><b>INFO</b></em>' literal value
	 * @see #INFO
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INFO_VALUE = 0;

	/**
	 * The '<em><b>WARNING</b></em>' literal value
	 * @see #WARNING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int WARNING_VALUE = 1;

	/**
	 * The '<em><b>ERROR</b></em>' literal value
	 * @see #ERROR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ERROR_VALUE = 2;

	/**
	 * An array of all the '<em><b>GUI Test Action Status</b></em>' enumerators
	 * @generated
	 */
	private static final GUITestActionStatus[] VALUES_ARRAY = { INFO, WARNING, ERROR };

	/**
	 * A public read-only list of all the '<em><b>GUI Test Action Status</b></em>' enumerators
	 * @generated
	 */
	public static final List<GUITestActionStatus> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>GUI Test Action Status</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionStatus get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Action Status</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionStatus getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Action Status</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionStatus get(int value) {
		switch (value) {
			case INFO_VALUE:
				return INFO;
			case WARNING_VALUE:
				return WARNING;
			case ERROR_VALUE:
				return ERROR;
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
	GUITestActionStatus(int value, String name, String literal) {
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
