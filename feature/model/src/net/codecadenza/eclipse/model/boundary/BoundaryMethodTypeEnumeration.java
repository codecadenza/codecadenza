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
package net.codecadenza.eclipse.model.boundary;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Method Type Enumeration</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethodTypeEnumeration()
 * @model
 * @generated
 */
public enum BoundaryMethodTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>DELETE</b></em>' literal object
	 * @see #DELETE_VALUE
	 * @generated
	 * @ordered
	 */
	DELETE(0, "DELETE", "DELETE"),

	/**
	 * The '<em><b>UPDATE</b></em>' literal object
	 * @see #UPDATE_VALUE
	 * @generated
	 * @ordered
	 */
	UPDATE(1, "UPDATE", "UPDATE"),

	/**
	 * The '<em><b>FIND ALL</b></em>' literal object
	 * @see #FIND_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_ALL(2, "FIND_ALL", "FIND_ALL"),

	/**
	 * The '<em><b>CREATE</b></em>' literal object
	 * @see #CREATE_VALUE
	 * @generated
	 * @ordered
	 */
	CREATE(3, "CREATE", "CREATE"),

	/**
	 * The '<em><b>SEARCH</b></em>' literal object
	 * @see #SEARCH_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH(4, "SEARCH", "SEARCH"),

	/**
	 * The '<em><b>FIND BY PARENT</b></em>' literal object
	 * @see #FIND_BY_PARENT_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_PARENT(5, "FIND_BY_PARENT", "FIND_BY_PARENT"),

	/**
	 * The '<em><b>FIND BY ID</b></em>' literal object
	 * @see #FIND_BY_ID_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_ID(6, "FIND_BY_ID", "FIND_BY_ID"),

	/**
	 * The '<em><b>EXISTS BY ID</b></em>' literal object
	 * @see #EXISTS_BY_ID_VALUE
	 * @generated
	 * @ordered
	 */
	EXISTS_BY_ID(7, "EXISTS_BY_ID", "EXISTS_BY_ID"),

	/**
	 * The '<em><b>DELETE ALL</b></em>' literal object
	 * @see #DELETE_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	DELETE_ALL(8, "DELETE_ALL", "DELETE_ALL"),

	/**
	 * The '<em><b>GET ASSOCIATION</b></em>' literal object
	 * @see #GET_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	GET_ASSOCIATION(9, "GET_ASSOCIATION", "GET_ASSOCIATION"),

	/**
	 * The '<em><b>FIND BY OBJECT</b></em>' literal object
	 * @see #FIND_BY_OBJECT_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_OBJECT(10, "FIND_BY_OBJECT", "FIND_BY_OBJECT"),

	/**
	 * The '<em><b>SEARCH BY UNIQUE KEY</b></em>' literal object
	 * @see #SEARCH_BY_UNIQUE_KEY_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_BY_UNIQUE_KEY(11, "SEARCH_BY_UNIQUE_KEY", "SEARCH_BY_UNIQUE_KEY"),

	/**
	 * The '<em><b>FIND BY UNIQUE KEY</b></em>' literal object
	 * @see #FIND_BY_UNIQUE_KEY_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_BY_UNIQUE_KEY(12, "FIND_BY_UNIQUE_KEY", "FIND_BY_UNIQUE_KEY"),

	/**
	 * The '<em><b>COUNT ALL</b></em>' literal object
	 * @see #COUNT_ALL_VALUE
	 * @generated
	 * @ordered
	 */
	COUNT_ALL(13, "COUNT_ALL", "COUNT_ALL"),

	/**
	 * The '<em><b>FIND EXISTING</b></em>' literal object
	 * @see #FIND_EXISTING_VALUE
	 * @generated
	 * @ordered
	 */
	FIND_EXISTING(14, "FIND_EXISTING", "FIND_EXISTING"),

	/**
	 * The '<em><b>SEARCH BY FILTER</b></em>' literal object
	 * @see #SEARCH_BY_FILTER_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_BY_FILTER(15, "SEARCH_BY_FILTER", "SEARCH_BY_FILTER"),

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY</b></em>' literal object
	 * @see #EXISTS_BY_UNIQUE_KEY_VALUE
	 * @generated
	 * @ordered
	 */
	EXISTS_BY_UNIQUE_KEY(16, "EXISTS_BY_UNIQUE_KEY", "EXISTS_BY_UNIQUE_KEY"),

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY WITH ID</b></em>' literal object
	 * @see #EXISTS_BY_UNIQUE_KEY_WITH_ID_VALUE
	 * @generated
	 * @ordered
	 */
	EXISTS_BY_UNIQUE_KEY_WITH_ID(17, "EXISTS_BY_UNIQUE_KEY_WITH_ID", "EXISTS_BY_UNIQUE_KEY_WITH_ID"),

	/**
	 * The '<em><b>CHANGE PARENT</b></em>' literal object
	 * @see #CHANGE_PARENT_VALUE
	 * @generated
	 * @ordered
	 */
	CHANGE_PARENT(18, "CHANGE_PARENT", "CHANGE_PARENT"),

	/**
	 * The '<em><b>ADD TO ASSOCIATION</b></em>' literal object
	 * @see #ADD_TO_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	ADD_TO_ASSOCIATION(19, "ADD_TO_ASSOCIATION", "ADD_TO_ASSOCIATION"),

	/**
	 * The '<em><b>REMOVE FROM ASSOCIATION</b></em>' literal object
	 * @see #REMOVE_FROM_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	REMOVE_FROM_ASSOCIATION(20, "REMOVE_FROM_ASSOCIATION", "REMOVE_FROM_ASSOCIATION"),

	/**
	 * The '<em><b>GET LIST OF VALUES</b></em>' literal object
	 * @see #GET_LIST_OF_VALUES_VALUE
	 * @generated
	 * @ordered
	 */
	GET_LIST_OF_VALUES(21, "GET_LIST_OF_VALUES", "GET_LIST_OF_VALUES"),

	/**
	 * The '<em><b>COUNT</b></em>' literal object
	 * @see #COUNT_VALUE
	 * @generated
	 * @ordered
	 */
	COUNT(22, "COUNT", "COUNT"),

	/**
	 * The '<em><b>LOG ON</b></em>' literal object
	 * @see #LOG_ON_VALUE
	 * @generated
	 * @ordered
	 */
	LOG_ON(23, "LOG_ON", "LOG_ON"),

	/**
	 * The '<em><b>DOWNLOAD</b></em>' literal object
	 * @see #DOWNLOAD_VALUE
	 * @generated
	 * @ordered
	 */
	DOWNLOAD(24, "DOWNLOAD", "DOWNLOAD"),

	/**
	 * The '<em><b>UPLOAD</b></em>' literal object
	 * @see #UPLOAD_VALUE
	 * @generated
	 * @ordered
	 */
	UPLOAD(25, "UPLOAD", "UPLOAD"),

	/**
	 * The '<em><b>CHANGE PASSWORD</b></em>' literal object
	 * @see #CHANGE_PASSWORD_VALUE
	 * @generated
	 * @ordered
	 */
	CHANGE_PASSWORD(26, "CHANGE_PASSWORD", "CHANGE_PASSWORD"),

	/**
	 * The '<em><b>SERVICE CALL</b></em>' literal object
	 * @see #SERVICE_CALL_VALUE
	 * @generated
	 * @ordered
	 */
	SERVICE_CALL(27, "SERVICE_CALL", "SERVICE_CALL"),

	/**
	 * The '<em><b>DOWNLOAD EXPORT</b></em>' literal object
	 * @see #DOWNLOAD_EXPORT_VALUE
	 * @generated
	 * @ordered
	 */
	DOWNLOAD_EXPORT(28, "DOWNLOAD_EXPORT", "DOWNLOAD_EXPORT"),

	/**
	 * The '<em><b>UPLOAD IMPORT</b></em>' literal object
	 * @see #UPLOAD_IMPORT_VALUE
	 * @generated
	 * @ordered
	 */
	UPLOAD_IMPORT(29, "UPLOAD_IMPORT", "UPLOAD_IMPORT"),

	/**
	 * The '<em><b>COPY</b></em>' literal object
	 * @see #COPY_VALUE
	 * @generated
	 * @ordered
	 */
	COPY(30, "COPY", "COPY"),

	/**
	 * The '<em><b>CHANGE ASSOCIATION</b></em>' literal object
	 * @see #CHANGE_ASSOCIATION_VALUE
	 * @generated
	 * @ordered
	 */
	CHANGE_ASSOCIATION(31, "CHANGE_ASSOCIATION", "CHANGE_ASSOCIATION"),

	/**
	 * The '<em><b>SAVE</b></em>' literal object
	 * @see #SAVE_VALUE
	 * @generated
	 * @ordered
	 */
	SAVE(32, "SAVE", "SAVE");

	/**
	 * The '<em><b>DELETE</b></em>' literal value
	 * @see #DELETE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DELETE_VALUE = 0;

	/**
	 * The '<em><b>UPDATE</b></em>' literal value
	 * @see #UPDATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPDATE_VALUE = 1;

	/**
	 * The '<em><b>FIND ALL</b></em>' literal value
	 * @see #FIND_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_ALL_VALUE = 2;

	/**
	 * The '<em><b>CREATE</b></em>' literal value
	 * @see #CREATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CREATE_VALUE = 3;

	/**
	 * The '<em><b>SEARCH</b></em>' literal value
	 * @see #SEARCH
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_VALUE = 4;

	/**
	 * The '<em><b>FIND BY PARENT</b></em>' literal value
	 * @see #FIND_BY_PARENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_PARENT_VALUE = 5;

	/**
	 * The '<em><b>FIND BY ID</b></em>' literal value
	 * @see #FIND_BY_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_ID_VALUE = 6;

	/**
	 * The '<em><b>EXISTS BY ID</b></em>' literal value
	 * @see #EXISTS_BY_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXISTS_BY_ID_VALUE = 7;

	/**
	 * The '<em><b>DELETE ALL</b></em>' literal value
	 * @see #DELETE_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DELETE_ALL_VALUE = 8;

	/**
	 * The '<em><b>GET ASSOCIATION</b></em>' literal value
	 * @see #GET_ASSOCIATION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GET_ASSOCIATION_VALUE = 9;

	/**
	 * The '<em><b>FIND BY OBJECT</b></em>' literal value
	 * @see #FIND_BY_OBJECT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_OBJECT_VALUE = 10;

	/**
	 * The '<em><b>SEARCH BY UNIQUE KEY</b></em>' literal value
	 * @see #SEARCH_BY_UNIQUE_KEY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_BY_UNIQUE_KEY_VALUE = 11;

	/**
	 * The '<em><b>FIND BY UNIQUE KEY</b></em>' literal value
	 * @see #FIND_BY_UNIQUE_KEY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_BY_UNIQUE_KEY_VALUE = 12;

	/**
	 * The '<em><b>COUNT ALL</b></em>' literal value
	 * @see #COUNT_ALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COUNT_ALL_VALUE = 13;

	/**
	 * The '<em><b>FIND EXISTING</b></em>' literal value
	 * @see #FIND_EXISTING
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FIND_EXISTING_VALUE = 14;

	/**
	 * The '<em><b>SEARCH BY FILTER</b></em>' literal value
	 * @see #SEARCH_BY_FILTER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_BY_FILTER_VALUE = 15;

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY</b></em>' literal value
	 * @see #EXISTS_BY_UNIQUE_KEY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXISTS_BY_UNIQUE_KEY_VALUE = 16;

	/**
	 * The '<em><b>EXISTS BY UNIQUE KEY WITH ID</b></em>' literal value
	 * @see #EXISTS_BY_UNIQUE_KEY_WITH_ID
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXISTS_BY_UNIQUE_KEY_WITH_ID_VALUE = 17;

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
	 * The '<em><b>GET LIST OF VALUES</b></em>' literal value
	 * @see #GET_LIST_OF_VALUES
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GET_LIST_OF_VALUES_VALUE = 21;

	/**
	 * The '<em><b>COUNT</b></em>' literal value
	 * @see #COUNT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COUNT_VALUE = 22;

	/**
	 * The '<em><b>LOG ON</b></em>' literal value
	 * @see #LOG_ON
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOG_ON_VALUE = 23;

	/**
	 * The '<em><b>DOWNLOAD</b></em>' literal value
	 * @see #DOWNLOAD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOWNLOAD_VALUE = 24;

	/**
	 * The '<em><b>UPLOAD</b></em>' literal value
	 * @see #UPLOAD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPLOAD_VALUE = 25;

	/**
	 * The '<em><b>CHANGE PASSWORD</b></em>' literal value
	 * @see #CHANGE_PASSWORD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CHANGE_PASSWORD_VALUE = 26;

	/**
	 * The '<em><b>SERVICE CALL</b></em>' literal value
	 * @see #SERVICE_CALL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SERVICE_CALL_VALUE = 27;

	/**
	 * The '<em><b>DOWNLOAD EXPORT</b></em>' literal value
	 * @see #DOWNLOAD_EXPORT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOWNLOAD_EXPORT_VALUE = 28;

	/**
	 * The '<em><b>UPLOAD IMPORT</b></em>' literal value
	 * @see #UPLOAD_IMPORT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPLOAD_IMPORT_VALUE = 29;

	/**
	 * The '<em><b>COPY</b></em>' literal value
	 * @see #COPY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COPY_VALUE = 30;

	/**
	 * The '<em><b>CHANGE ASSOCIATION</b></em>' literal value
	 * @see #CHANGE_ASSOCIATION
	 * @generated
	 * @ordered
	 */
	public static final int CHANGE_ASSOCIATION_VALUE = 31;

	/**
	 * The '<em><b>SAVE</b></em>' literal value
	 * @see #SAVE
	 * @generated
	 * @ordered
	 */
	public static final int SAVE_VALUE = 32;

	/**
	 * An array of all the '<em><b>Method Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final BoundaryMethodTypeEnumeration[] VALUES_ARRAY = { DELETE, UPDATE, FIND_ALL, CREATE, SEARCH, FIND_BY_PARENT,
			FIND_BY_ID, EXISTS_BY_ID, DELETE_ALL, GET_ASSOCIATION, FIND_BY_OBJECT, SEARCH_BY_UNIQUE_KEY, FIND_BY_UNIQUE_KEY, COUNT_ALL,
			FIND_EXISTING, SEARCH_BY_FILTER, EXISTS_BY_UNIQUE_KEY, EXISTS_BY_UNIQUE_KEY_WITH_ID, CHANGE_PARENT, ADD_TO_ASSOCIATION,
			REMOVE_FROM_ASSOCIATION, GET_LIST_OF_VALUES, COUNT, LOG_ON, DOWNLOAD, UPLOAD, CHANGE_PASSWORD, SERVICE_CALL,
			DOWNLOAD_EXPORT, UPLOAD_IMPORT, COPY, CHANGE_ASSOCIATION, SAVE };

	/**
	 * A public read-only list of all the '<em><b>Method Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<BoundaryMethodTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Method Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static BoundaryMethodTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Method Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static BoundaryMethodTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Method Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static BoundaryMethodTypeEnumeration get(int value) {
		switch (value) {
			case DELETE_VALUE:
				return DELETE;
			case UPDATE_VALUE:
				return UPDATE;
			case FIND_ALL_VALUE:
				return FIND_ALL;
			case CREATE_VALUE:
				return CREATE;
			case SEARCH_VALUE:
				return SEARCH;
			case FIND_BY_PARENT_VALUE:
				return FIND_BY_PARENT;
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
			case COUNT_ALL_VALUE:
				return COUNT_ALL;
			case FIND_EXISTING_VALUE:
				return FIND_EXISTING;
			case SEARCH_BY_FILTER_VALUE:
				return SEARCH_BY_FILTER;
			case EXISTS_BY_UNIQUE_KEY_VALUE:
				return EXISTS_BY_UNIQUE_KEY;
			case EXISTS_BY_UNIQUE_KEY_WITH_ID_VALUE:
				return EXISTS_BY_UNIQUE_KEY_WITH_ID;
			case CHANGE_PARENT_VALUE:
				return CHANGE_PARENT;
			case ADD_TO_ASSOCIATION_VALUE:
				return ADD_TO_ASSOCIATION;
			case REMOVE_FROM_ASSOCIATION_VALUE:
				return REMOVE_FROM_ASSOCIATION;
			case GET_LIST_OF_VALUES_VALUE:
				return GET_LIST_OF_VALUES;
			case COUNT_VALUE:
				return COUNT;
			case LOG_ON_VALUE:
				return LOG_ON;
			case DOWNLOAD_VALUE:
				return DOWNLOAD;
			case UPLOAD_VALUE:
				return UPLOAD;
			case CHANGE_PASSWORD_VALUE:
				return CHANGE_PASSWORD;
			case SERVICE_CALL_VALUE:
				return SERVICE_CALL;
			case DOWNLOAD_EXPORT_VALUE:
				return DOWNLOAD_EXPORT;
			case UPLOAD_IMPORT_VALUE:
				return UPLOAD_IMPORT;
			case COPY_VALUE:
				return COPY;
			case CHANGE_ASSOCIATION_VALUE:
				return CHANGE_ASSOCIATION;
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
	BoundaryMethodTypeEnumeration(int value, String name, String literal) {
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
