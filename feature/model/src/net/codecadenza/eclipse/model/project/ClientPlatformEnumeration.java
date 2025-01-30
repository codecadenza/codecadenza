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
package net.codecadenza.eclipse.model.project;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Client Platform Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getClientPlatformEnumeration()
 * @model
 * @generated
 */
public enum ClientPlatformEnumeration implements Enumerator {
	/**
	 * The '<em><b>NONE</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "NONE", "NONE"),

	/**
	 * The '<em><b>RCP</b></em>' literal object
	 * @see #RCP_VALUE
	 * @generated
	 * @ordered
	 */
	RCP(1, "RCP", "RCP"),

	/**
	 * The '<em><b>SWING</b></em>' literal object
	 * @see #SWING_VALUE
	 * @generated
	 * @ordered
	 */
	SWING(2, "SWING", "SWING"),

	/**
	 * The '<em><b>JSF PRIMEFACES</b></em>' literal object
	 * @see #JSF_PRIMEFACES_VALUE
	 * @generated
	 * @ordered
	 */
	JSF_PRIMEFACES(3, "JSF_PRIMEFACES", "JSF_PRIMEFACES"),

	/**
	 * The '<em><b>RAP</b></em>' literal object
	 * @see #RAP_VALUE
	 * @generated
	 * @ordered
	 */
	RAP(4, "RAP", "RAP"),

	/**
	 * The '<em><b>VAADIN</b></em>' literal object
	 * @see #VAADIN_VALUE
	 * @generated
	 * @ordered
	 */
	VAADIN(5, "VAADIN", "VAADIN"),

	/**
	 * The '<em><b>JAVAFX</b></em>' literal object
	 * @see #JAVAFX_VALUE
	 * @generated
	 * @ordered
	 */
	JAVAFX(6, "JAVAFX", "JAVAFX"),

	/**
	 * The '<em><b>ANGULAR</b></em>' literal object
	 * @see #ANGULAR_VALUE
	 * @generated
	 * @ordered
	 */
	ANGULAR(7, "ANGULAR", "ANGULAR");

	/**
	 * The '<em><b>NONE</b></em>' literal value
	 * @see #NONE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>RCP</b></em>' literal value
	 * @see #RCP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int RCP_VALUE = 1;

	/**
	 * The '<em><b>SWING</b></em>' literal value
	 * @see #SWING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SWING_VALUE = 2;

	/**
	 * The '<em><b>JSF PRIMEFACES</b></em>' literal value
	 * @see #JSF_PRIMEFACES
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JSF_PRIMEFACES_VALUE = 3;

	/**
	 * The '<em><b>RAP</b></em>' literal value
	 * @see #RAP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int RAP_VALUE = 4;

	/**
	 * The '<em><b>VAADIN</b></em>' literal value
	 * @see #VAADIN
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int VAADIN_VALUE = 5;

	/**
	 * The '<em><b>JAVAFX</b></em>' literal value
	 * @see #JAVAFX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JAVAFX_VALUE = 6;

	/**
	 * The '<em><b>ANGULAR</b></em>' literal value
	 * @see #ANGULAR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ANGULAR_VALUE = 7;

	/**
	 * An array of all the '<em><b>Client Platform Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final ClientPlatformEnumeration[] VALUES_ARRAY = { NONE, RCP, SWING, JSF_PRIMEFACES, RAP, VAADIN, JAVAFX,
			ANGULAR };

	/**
	 * A public read-only list of all the '<em><b>Client Platform Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<ClientPlatformEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Client Platform Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static ClientPlatformEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Client Platform Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static ClientPlatformEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Client Platform Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static ClientPlatformEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case RCP_VALUE:
				return RCP;
			case SWING_VALUE:
				return SWING;
			case JSF_PRIMEFACES_VALUE:
				return JSF_PRIMEFACES;
			case RAP_VALUE:
				return RAP;
			case VAADIN_VALUE:
				return VAADIN;
			case JAVAFX_VALUE:
				return JAVAFX;
			case ANGULAR_VALUE:
				return ANGULAR;
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
	ClientPlatformEnumeration(int value, String name, String literal) {
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
