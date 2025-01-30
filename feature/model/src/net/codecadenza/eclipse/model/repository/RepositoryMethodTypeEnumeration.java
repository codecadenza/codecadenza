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
package net.codecadenza.eclipse.model.repository;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Repository Method Type Enumeration</b></em>', and utility methods
 * for working with them.
 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethodTypeEnumeration()
 * @model
 * @generated
 */
public enum RepositoryMethodTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>FIND ALL</b></em>' literal object
	 * @see #FIND_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_ALL(0, "FIND_ALL", "FIND_ALL"),

	/**
	 * The '<em><b>FIND BY ID</b></em>' literal object
	 * @see #FIND_BY_ID_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_ID(1, "FIND_BY_ID", "FIND_BY_ID"),

	/**
	 * The '<em><b>EXISTS BY ID</b></em>' literal object
	 * @see #EXISTS_BY_ID_VALUE
	 * @generated
	 * @ordered
	 */
	EXISTS_BY_ID(2, "EXISTS_BY_ID", "EXISTS_BY_ID"),

	/**
	 * The '<em><b>DELETE ALL</b></em>' literal object
	 * @see #DELETE_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	DELETE_ALL(3, "DELETE_ALL", "DELETE_ALL"),

	/**
	 * The '<em><b>GET ASSOCIATION</b></em>' literal object
	 * @see #GET_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	GET_ASSOCIATION(4, "GET_ASSOCIATION", "GET_ASSOCIATION"),

	/**
	 * The '<em><b>FIND BY OBJECT</b></em>' literal object
	 * @see #FIND_BY_OBJECT_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_OBJECT(5, "FIND_BY_OBJECT", "FIND_BY_OBJECT"),

	/**
	 * The '<em><b>SEARCH BY UNIQUE KEY</b></em>' literal object
	 * @see #SEARCH_BY_UNIQUE_KEY_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_BY_UNIQUE_KEY(6, "SEARCH_BY_UNIQUE_KEY", "SEARCH_BY_UNIQUE_KEY"),

	/**
	 * The '<em><b>FIND BY UNIQUE KEY</b></em>' literal object
	 * @see #FIND_BY_UNIQUE_KEY_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_UNIQUE_KEY(7, "FIND_BY_UNIQUE_KEY", "FIND_BY_UNIQUE_KEY"),

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY</b></em>' literal object
	 * @see #EXISTS_BY_UNIQUE_KEY_VALUE
	 * @generated
	 * @ordered
	 */
	EXISTS_BY_UNIQUE_KEY(8, "EXISTS_BY_UNIQUE_KEY", "EXISTS_BY_UNIQUE_KEY"),

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY WITH ID</b></em>' literal object
	 * @see #EXISTS_BY_UNIQUE_KEY_WITH_ID_VALUE
	 * @generated
	 * @ordered
	 */
	EXISTS_BY_UNIQUE_KEY_WITH_ID(9, "EXISTS_BY_UNIQUE_KEY_WITH_ID", "EXISTS_BY_UNIQUE_KEY_WITH_ID"),

	/**
	 * The '<em><b>COUNT ALL</b></em>' literal object
	 * @see #COUNT_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	COUNT_ALL(10, "COUNT_ALL", "COUNT_ALL"),

	/**
	 * The '<em><b>DELETE</b></em>' literal object
	 * @see #DELETE_VALUE
	 * @generated
	 * @ordered
	 */
	DELETE(11, "DELETE", "DELETE"),

	/**
	 * The '<em><b>MERGE</b></em>' literal object
	 * @see #MERGE_VALUE
	 * @generated
	 * @ordered
	 */
	MERGE(12, "MERGE", "MERGE"),

	/**
	 * The '<em><b>PERSIST</b></em>' literal object
	 * @see #PERSIST_VALUE
	 * @generated
	 * @ordered
	 */
	PERSIST(13, "PERSIST", "PERSIST"),

	/**
	 * The '<em><b>FIND EXISTING</b></em>' literal object
	 * @see #FIND_EXISTING_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_EXISTING(14, "FIND_EXISTING", "FIND_EXISTING"),

	/**
	 * The '<em><b>SEARCH</b></em>' literal object
	 * @see #SEARCH_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH(15, "SEARCH", "SEARCH"),

	/**
	 * The '<em><b>COUNT</b></em>' literal object
	 * @see #COUNT_VALUE
	 * @generated
	 * @ordered
	 */
	COUNT(16, "COUNT", "COUNT"),

	/**
	 * The '<em><b>CHANGE PARENT</b></em>' literal object
	 * @see #CHANGE_PARENT_VALUE
	 * @generated
	 * @ordered
	 */
	CHANGE_PARENT(17, "CHANGE_PARENT", "CHANGE_PARENT"),

	/**
	 * The '<em><b>ADD TO ASSOCIATION</b></em>' literal object
	 * @see #ADD_TO_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	ADD_TO_ASSOCIATION(18, "ADD_TO_ASSOCIATION", "ADD_TO_ASSOCIATION"),

	/**
	 * The '<em><b>REMOVE FROM ASSOCIATION</b></em>' literal object
	 * @see #REMOVE_FROM_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	REMOVE_FROM_ASSOCIATION(19, "REMOVE_FROM_ASSOCIATION", "REMOVE_FROM_ASSOCIATION"),

	/**
	 * The '<em><b>COPY</b></em>' literal object
	 * @see #COPY_VALUE
	 * @generated
	 * @ordered
	 */
	COPY(20, "COPY", "COPY"),

	/**
	 * The '<em><b>SAVE</b></em>' literal object
	 * @see #SAVE_VALUE
	 * @generated
	 * @ordered
	 */
	SAVE(21, "SAVE", "SAVE");

	/**
	 * The '<em><b>FIND ALL</b></em>' literal value
	 * @see #FIND_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_ALL_VALUE = 0;

	/**
	 * The '<em><b>FIND BY ID</b></em>' literal value
	 * @see #FIND_BY_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_ID_VALUE = 2;

	/**
	 * The '<em><b>EXISTS BY ID</b></em>' literal value
	 * @see #EXISTS_BY_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXISTS_BY_ID_VALUE = 3;

	/**
	 * The '<em><b>DELETE ALL</b></em>' literal value
	 * @see #DELETE_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DELETE_ALL_VALUE = 4;

	/**
	 * The '<em><b>GET ASSOCIATION</b></em>' literal value
	 * @see #GET_ASSOCIATION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GET_ASSOCIATION_VALUE = 5;

	/**
	 * The '<em><b>FIND BY OBJECT</b></em>' literal value
	 * @see #FIND_BY_OBJECT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_OBJECT_VALUE = 6;

	/**
	 * The '<em><b>SEARCH BY UNIQUE KEY</b></em>' literal value
	 * @see #SEARCH_BY_UNIQUE_KEY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_BY_UNIQUE_KEY_VALUE = 7;

	/**
	 * The '<em><b>FIND BY UNIQUE KEY</b></em>' literal value
	 * @see #FIND_BY_UNIQUE_KEY_VALUE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_UNIQUE_KEY_VALUE = 8;

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY</b></em>' literal value
	 * @see #EXISTS_BY_UNIQUE_KEY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXISTS_BY_UNIQUE_KEY_VALUE = 9;

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY WITH ID</b></em>' literal value
	 * @see #EXISTS_BY_UNIQUE_KEY_WITH_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXISTS_BY_UNIQUE_KEY_WITH_ID_VALUE = 10;

	/**
	 * The '<em><b>COUNT ALL</b></em>' literal value
	 * @see #COUNT_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COUNT_ALL_VALUE = 11;

	/**
	 * The '<em><b>DELETE</b></em>' literal value
	 * @see #DELETE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DELETE_VALUE = 12;

	/**
	 * The '<em><b>MERGE</b></em>' literal value
	 * @see #MERGE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MERGE_VALUE = 13;

	/**
	 * The '<em><b>PERSIST</b></em>' literal value
	 * @see #PERSIST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PERSIST_VALUE = 14;

	/**
	 * The '<em><b>FIND EXISTING</b></em>' literal value
	 * @see #FIND_EXISTING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_EXISTING_VALUE = 15;

	/**
	 * The '<em><b>SEARCH</b></em>' literal value
	 * @see #SEARCH
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_VALUE = 16;

	/**
	 * The '<em><b>COUNT</b></em>' literal value
	 * @see #COUNT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COUNT_VALUE = 17;

	/**
	 * The '<em><b>CHANGE PARENT</b></em>' literal value
	 * @see #CHANGE_PARENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CHANGE_PARENT_VALUE = 18;

	/**
	 * The '<em><b>ADD TO ASSOCIATION</b></em>' literal value
	 * @see #ADD_TO_ASSOCIATION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ADD_TO_ASSOCIATION_VALUE = 19;

	/**
	 * The '<em><b>REMOVE FROM ASSOCIATION</b></em>' literal value
	 * @see #REMOVE_FROM_ASSOCIATION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int REMOVE_FROM_ASSOCIATION_VALUE = 20;

	/**
	 * The '<em><b>COPY</b></em>' literal value
	 * @see #COPY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COPY_VALUE = 21;

	/**
	 * The '<em><b>SAVE</b></em>' literal value
	 * @see #SAVE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SAVE_VALUE = 22;

	/**
	 * An array of all the '<em><b>Repository Method Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final RepositoryMethodTypeEnumeration[] VALUES_ARRAY = { FIND_ALL, FIND_BY_ID, EXISTS_BY_ID, DELETE_ALL,
			GET_ASSOCIATION, FIND_BY_OBJECT, SEARCH_BY_UNIQUE_KEY, FIND_BY_UNIQUE_KEY, EXISTS_BY_UNIQUE_KEY,
			EXISTS_BY_UNIQUE_KEY_WITH_ID, COUNT_ALL, DELETE, MERGE, PERSIST, FIND_EXISTING, SEARCH, COUNT, CHANGE_PARENT,
			ADD_TO_ASSOCIATION, REMOVE_FROM_ASSOCIATION, COPY, SAVE };

	/**
	 * A public read-only list of all the '<em><b>Repository Method Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<RepositoryMethodTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Repository Method Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static RepositoryMethodTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Repository Method Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static RepositoryMethodTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Repository Method Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static RepositoryMethodTypeEnumeration get(int value) {
		switch (value) {
			case FIND_ALL_VALUE:
				return FIND_ALL;
			case FIND_BY_ID_VALUE:
				return FIND_BY_ID;
			case EXISTS_BY_ID_VALUE:
				return EXISTS_BY_ID;
			case DELETE_ALL_VALUE:
				return DELETE_ALL;
			case GET_ASSOCIATION_VALUE:
				return GET_ASSOCIATION;
			case FIND_BY_OBJECT_VALUE:
				return FIND_BY_OBJECT;
			case SEARCH_BY_UNIQUE_KEY_VALUE:
				return SEARCH_BY_UNIQUE_KEY;
			case FIND_BY_UNIQUE_KEY_VALUE:
				return FIND_BY_UNIQUE_KEY;
			case EXISTS_BY_UNIQUE_KEY_VALUE:
				return EXISTS_BY_UNIQUE_KEY;
			case EXISTS_BY_UNIQUE_KEY_WITH_ID_VALUE:
				return EXISTS_BY_UNIQUE_KEY_WITH_ID;
			case COUNT_ALL_VALUE:
				return COUNT_ALL;
			case DELETE_VALUE:
				return DELETE;
			case MERGE_VALUE:
				return MERGE;
			case PERSIST_VALUE:
				return PERSIST;
			case FIND_EXISTING_VALUE:
				return FIND_EXISTING;
			case SEARCH_VALUE:
				return SEARCH;
			case COUNT_VALUE:
				return COUNT;
			case CHANGE_PARENT_VALUE:
				return CHANGE_PARENT;
			case ADD_TO_ASSOCIATION_VALUE:
				return ADD_TO_ASSOCIATION;
			case REMOVE_FROM_ASSOCIATION_VALUE:
				return REMOVE_FROM_ASSOCIATION;
			case COPY_VALUE:
				return COPY;
			case SAVE_VALUE:
				return SAVE;
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
	RepositoryMethodTypeEnumeration(int value, String name, String literal) {
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
