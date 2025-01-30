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
package net.codecadenza.eclipse.model.java;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Type Modifier Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaTypeModifierEnumeration()
 * @model
 * @generated
 */
public enum JavaTypeModifierEnumeration implements Enumerator {
	/**
	 * The '<em><b>None</b></em>' literal object
	 * @see #NONE_VALUE
	 * @generated
	 * @ordered
	 */
	NONE(0, "None", "None"),

	/**
	 * The '<em><b>Collection</b></em>' literal object
	 * @see #COLLECTION_VALUE
	 * @generated
	 * @ordered
	 */
	COLLECTION(1, "Collection", "Collection"),

	/**
	 * The '<em><b>Array List</b></em>' literal object
	 * @see #ARRAY_LIST_VALUE
	 * @generated
	 * @ordered
	 */
	ARRAY_LIST(2, "ArrayList", "ArrayList"),

	/**
	 * The '<em><b>Vector</b></em>' literal object
	 * @see #VECTOR_VALUE
	 * @generated
	 * @ordered
	 */
	VECTOR(3, "Vector", "Vector"),

	/**
	 * The '<em><b>Hash Map</b></em>' literal object
	 * @see #HASH_MAP_VALUE
	 * @generated
	 * @ordered
	 */
	HASH_MAP(4, "HashMap", "HashMap"),

	/**
	 * The '<em><b>Hash Set</b></em>' literal object
	 * @see #HASH_SET_VALUE
	 * @generated
	 * @ordered
	 */
	HASH_SET(5, "HashSet", "HashSet"),

	/**
	 * The '<em><b>List</b></em>' literal object
	 * @see #LIST_VALUE
	 * @generated
	 * @ordered
	 */
	LIST(6, "List", "List");

	/**
	 * The '<em><b>None</b></em>' literal valu
	 * @see #NONE
	 * @model name="None"
	 * @generated
	 * @ordered
	 */
	public static final int NONE_VALUE = 0;

	/**
	 * The '<em><b>Collection</b></em>' literal value
	 * @see #COLLECTION
	 * @model name="Collection"
	 * @generated
	 * @ordered
	 */
	public static final int COLLECTION_VALUE = 1;

	/**
	 * The '<em><b>Array List</b></em>' literal value
	 * @see #ARRAY_LIST
	 * @model name="ArrayList"
	 * @generated
	 * @ordered
	 */
	public static final int ARRAY_LIST_VALUE = 2;

	/**
	 * The '<em><b>Vector</b></em>' literal value
	 * @see #VECTOR
	 * @model name="Vector"
	 * @generated
	 * @ordered
	 */
	public static final int VECTOR_VALUE = 3;

	/**
	 * The '<em><b>Hash Map</b></em>' literal value
	 * @see #HASH_MAP
	 * @model name="HashMap"
	 * @generated
	 * @ordered
	 */
	public static final int HASH_MAP_VALUE = 4;

	/**
	 * The '<em><b>Hash Set</b></em>' literal value
	 * @see #HASH_SET
	 * @model name="HashSet"
	 * @generated
	 * @ordered
	 */
	public static final int HASH_SET_VALUE = 5;

	/**
	 * The '<em><b>List</b></em>' literal value
	 * @see #LIST
	 * @model name="List"
	 * @generated
	 * @ordered
	 */
	public static final int LIST_VALUE = 6;

	/**
	 * An array of all the '<em><b>Type Modifier Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final JavaTypeModifierEnumeration[] VALUES_ARRAY = { NONE, COLLECTION, ARRAY_LIST, VECTOR, HASH_MAP, HASH_SET,
			LIST };

	/**
	 * A public read-only list of all the '<em><b>Type Modifier Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<JavaTypeModifierEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Type Modifier Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static JavaTypeModifierEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Type Modifier Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static JavaTypeModifierEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Type Modifier Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static JavaTypeModifierEnumeration get(int value) {
		switch (value) {
			case NONE_VALUE:
				return NONE;
			case COLLECTION_VALUE:
				return COLLECTION;
			case ARRAY_LIST_VALUE:
				return ARRAY_LIST;
			case VECTOR_VALUE:
				return VECTOR;
			case HASH_MAP_VALUE:
				return HASH_MAP;
			case HASH_SET_VALUE:
				return HASH_SET;
			case LIST_VALUE:
				return LIST;
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
	JavaTypeModifierEnumeration(int value, String name, String literal) {
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
