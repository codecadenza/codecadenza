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
 * A representation of the literals of the enumeration '<em><b>Form Type Enumeration</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTypeEnumeration()
 * @model
 * @generated
 */
public enum FormTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>UPDATE</b></em>' literal object
	 * @see #UPDATE_VALUE
	 * @generated
	 * @ordered
	 */
	UPDATE(0, "UPDATE", "UPDATE"),

	/**
	 * The '<em><b>READONLY</b></em>' literal object
	 * @see #READONLY_VALUE
	 * @generated
	 * @ordered
	 */
	READONLY(1, "READONLY", "READONLY"),

	/**
	 * The '<em><b>CREATE</b></em>' literal object
	 * @see #CREATE_VALUE
	 * @generated
	 * @ordered
	 */
	CREATE(2, "CREATE", "CREATE"),

	/**
	 * The '<em><b>SEARCHABLE VIEW</b></em>' literal object
	 * @see #SEARCHABLE_VIEW_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCHABLE_VIEW(3, "SEARCHABLE_VIEW", "SEARCHABLE_VIEW"),

	/**
	 * The '<em><b>SIMPLE VIEW</b></em>' literal object
	 * @see #SIMPLE_VIEW_VALUE
	 * @generated
	 * @ordered
	 */
	SIMPLE_VIEW(4, "SIMPLE_VIEW", "SIMPLE_VIEW"),

	/**
	 * The '<em><b>ADD</b></em>' literal object
	 * @see #ADD_VALUE
	 * @generated
	 * @ordered
	 */
	ADD(5, "ADD", "ADD"),

	/**
	 * The '<em><b>LOV</b></em>' literal object
	 * @see #LOV_VALUE
	 * @generated
	 * @ordered
	 */
	LOV(6, "LOV", "LOV"),

	/**
	 * The '<em><b>GRID</b></em>' literal object
	 * @see #GRID_VALUE
	 * @generated
	 * @ordered
	 */
	GRID(7, "GRID", "GRID"),

	/**
	 * The '<em><b>TREE VIEW</b></em>' literal object
	 * @see #TREE_VIEW_VALUE
	 * @generated
	 * @ordered
	 */
	TREE_VIEW(8, "TREE_VIEW", "TREE_VIEW");

	/**
	 * The '<em><b>UPDATE</b></em>' literal value
	 * @see #UPDATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPDATE_VALUE = 0;

	/**
	 * The '<em><b>READONLY</b></em>' literal value
	 * @see #READONLY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int READONLY_VALUE = 1;

	/**
	 * The '<em><b>CREATE</b></em>' literal value
	 * @see #CREATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CREATE_VALUE = 2;

	/**
	 * The '<em><b>SEARCHABLE VIEW</b></em>' literal value
	 * @see #SEARCHABLE_VIEW
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCHABLE_VIEW_VALUE = 3;

	/**
	 * The '<em><b>SIMPLE VIEW</b></em>' literal value
	 * @see #SIMPLE_VIEW
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SIMPLE_VIEW_VALUE = 4;

	/**
	 * The '<em><b>ADD</b></em>' literal value
	 * @see #ADD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ADD_VALUE = 5;

	/**
	 * The '<em><b>LOV</b></em>' literal value
	 * @see #LOV
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOV_VALUE = 6;

	/**
	 * The '<em><b>GRID</b></em>' literal value
	 * @see #GRID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GRID_VALUE = 7;

	/**
	 * The '<em><b>TREE VIEW</b></em>' literal value
	 * @see #TREE_VIEW
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int TREE_VIEW_VALUE = 8;

	/**
	 * An array of all the '<em><b>Form Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final FormTypeEnumeration[] VALUES_ARRAY = { UPDATE, READONLY, CREATE, SEARCHABLE_VIEW, SIMPLE_VIEW, ADD, LOV,
			GRID, TREE_VIEW };

	/**
	 * A public read-only list of all the '<em><b>Form Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<FormTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Form Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static FormTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Form Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static FormTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Form Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static FormTypeEnumeration get(int value) {
		switch (value) {
			case UPDATE_VALUE:
				return UPDATE;
			case READONLY_VALUE:
				return READONLY;
			case CREATE_VALUE:
				return CREATE;
			case SEARCHABLE_VIEW_VALUE:
				return SEARCHABLE_VIEW;
			case SIMPLE_VIEW_VALUE:
				return SIMPLE_VIEW;
			case ADD_VALUE:
				return ADD;
			case LOV_VALUE:
				return LOV;
			case GRID_VALUE:
				return GRID;
			case TREE_VIEW_VALUE:
				return TREE_VIEW;
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
	FormTypeEnumeration(int value, String name, String literal) {
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
	 */
	@Override
	public String toString() {
		return literal;
	}

}
