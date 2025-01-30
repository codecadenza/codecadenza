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
 * A representation of the literals of the enumeration '<em><b>GUI Test Action Result Component Type</b></em>', and utility
 * methods for working with them.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResultComponentType()
 * @model
 * @generated
 */
public enum GUITestActionResultComponentType implements Enumerator {
	/**
	 * The '<em><b>DIALOG</b></em>' literal object
	 * @see #DIALOG_VALUE
	 * @generated
	 * @ordered
	 */
	DIALOG(0, "DIALOG", "DIALOG"),

	/**
	 * The '<em><b>NOTIFICATION</b></em>' literal object
	 * @see #NOTIFICATION_VALUE
	 * @generated
	 * @ordered
	 */
	NOTIFICATION(1, "NOTIFICATION", "NOTIFICATION");

	/**
	 * The '<em><b>DIALOG</b></em>' literal value
	 * @see #DIALOG
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DIALOG_VALUE = 0;

	/**
	 * The '<em><b>NOTIFICATION</b></em>' literal value
	 * @see #NOTIFICATION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NOTIFICATION_VALUE = 1;

	/**
	 * An array of all the '<em><b>GUI Test Action Result Component Type</b></em>' enumerators
	 * @generated
	 */
	private static final GUITestActionResultComponentType[] VALUES_ARRAY = { DIALOG, NOTIFICATION };

	/**
	 * A public read-only list of all the '<em><b>GUI Test Action Result Component Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<GUITestActionResultComponentType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>GUI Test Action Result Component Type</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionResultComponentType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Action Result Component Type</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionResultComponentType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Action Result Component Type</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionResultComponentType get(int value) {
		switch (value) {
			case DIALOG_VALUE:
				return DIALOG;
			case NOTIFICATION_VALUE:
				return NOTIFICATION;
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
	GUITestActionResultComponentType(int value, String name, String literal) {
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
