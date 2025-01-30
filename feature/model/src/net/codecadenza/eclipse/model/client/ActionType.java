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
 * A representation of the literals of the enumeration '<em><b>Action Type</b></em>', and utility methods for working with them.
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getActionType()
 * @model
 * @generated
 */
public enum ActionType implements Enumerator {
	/**
	 * The '<em><b>CREATE</b></em>' literal object
	 * @see #CREATE_VALUE
	 * @generated
	 * @ordered
	 */
	CREATE(0, "CREATE", "CREATE"),

	/**
	 * The '<em><b>DIRECT UPLOAD</b></em>' literal object
	 * @see #DIRECT_UPLOAD_VALUE
	 * @generated
	 * @ordered
	 */
	DIRECT_UPLOAD(1, "DIRECT_UPLOAD", "DIRECT_UPLOAD"),

	/**
	 * The '<em><b>INDIRECT UPLOAD</b></em>' literal object
	 * @see #INDIRECT_UPLOAD_VALUE
	 * @generated
	 * @ordered
	 */
	INDIRECT_UPLOAD(2, "INDIRECT_UPLOAD", "INDIRECT_UPLOAD"),

	/**
	 * The '<em><b>DOWNLOAD</b></em>' literal object
	 * @see #DOWNLOAD_VALUE
	 * @generated
	 * @ordered
	 */
	DOWNLOAD(3, "DOWNLOAD", "DOWNLOAD"),

	/**
	 * The '<em><b>POSITVE DECISION</b></em>' literal object
	 * @see #POSITVE_DECISION_VALUE
	 * @generated
	 * @ordered
	 */
	POSITVE_DECISION(4, "POSITVE_DECISION", "POSITVE_DECISION"),

	/**
	 * The '<em><b>NEGATIVE DECISION</b></em>' literal object
	 * @see #NEGATIVE_DECISION_VALUE
	 * @generated
	 * @ordered
	 */
	NEGATIVE_DECISION(5, "NEGATIVE_DECISION", "NEGATIVE_DECISION"),

	/**
	 * The '<em><b>UNDEF DECISION</b></em>' literal object
	 * @see #UNDEF_DECISION_VALUE
	 * @generated
	 * @ordered
	 */
	UNDEF_DECISION(6, "UNDEF_DECISION", "UNDEF_DECISION"),

	/**
	 * The '<em><b>UPDATE</b></em>' literal object
	 * @see #UPDATE_VALUE
	 * @generated
	 * @ordered
	 */
	UPDATE(7, "UPDATE", "UPDATE"),

	/**
	 * The '<em><b>READ</b></em>' literal object
	 * @see #READ_VALUE
	 * @generated
	 * @ordered
	 */
	READ(8, "READ", "READ"),

	/**
	 * The '<em><b>DELETE</b></em>' literal object
	 * @see #DELETE_VALUE
	 * @generated
	 * @ordered
	 */
	DELETE(9, "DELETE", "DELETE"),

	/**
	 * The '<em><b>DOWNLOAD EXPORT</b></em>' literal object
	 * @see #DOWNLOAD_EXPORT_VALUE
	 * @generated
	 * @ordered
	 */
	DOWNLOAD_EXPORT(10, "DOWNLOAD_EXPORT", "DOWNLOAD_EXPORT"),

	/**
	 * The '<em><b>UPLOAD IMPORT</b></em>' literal object
	 * @see #UPLOAD_IMPORT_VALUE
	 * @generated
	 * @ordered
	 */
	UPLOAD_IMPORT(11, "UPLOAD_IMPORT", "UPLOAD_IMPORT"),

	/**
	 * The '<em><b>COPY</b></em>' literal object
	 * @see #COPY_VALUE
	 * @generated
	 * @ordered
	 */
	COPY(12, "COPY", "COPY");

	/**
	 * The '<em><b>CREATE</b></em>' literal value
	 * @see #CREATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CREATE_VALUE = 0;

	/**
	 * The '<em><b>DIRECT UPLOAD</b></em>' literal value
	 * @see #DIRECT_UPLOAD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DIRECT_UPLOAD_VALUE = 1;

	/**
	 * The '<em><b>INDIRECT UPLOAD</b></em>' literal value
	 * @see #INDIRECT_UPLOAD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INDIRECT_UPLOAD_VALUE = 2;

	/**
	 * The '<em><b>DOWNLOAD</b></em>' literal value
	 * @see #DOWNLOAD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOWNLOAD_VALUE = 3;

	/**
	 * The '<em><b>POSITVE DECISION</b></em>' literal value
	 * @see #POSITVE_DECISION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int POSITVE_DECISION_VALUE = 4;

	/**
	 * The '<em><b>NEGATIVE DECISION</b></em>' literal value
	 * @see #NEGATIVE_DECISION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int NEGATIVE_DECISION_VALUE = 5;

	/**
	 * The '<em><b>UNDEF DECISION</b></em>' literal value
	 * @see #UNDEF_DECISION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UNDEF_DECISION_VALUE = 6;

	/**
	 * The '<em><b>UPDATE</b></em>' literal value
	 * @see #UPDATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPDATE_VALUE = 7;

	/**
	 * The '<em><b>READ</b></em>' literal value
	 * @see #READ
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int READ_VALUE = 8;

	/**
	 * The '<em><b>DELETE</b></em>' literal value
	 * @see #DELETE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DELETE_VALUE = 9;

	/**
	 * The '<em><b>DOWNLOAD EXPORT</b></em>' literal value
	 * @see #DOWNLOAD_EXPORT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOWNLOAD_EXPORT_VALUE = 10;

	/**
	 * The '<em><b>UPLOAD IMPORT</b></em>' literal value
	 * @see #UPLOAD_IMPORT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPLOAD_IMPORT_VALUE = 11;

	/**
	 * The '<em><b>COPY</b></em>' literal value
	 * @see #COPY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COPY_VALUE = 12;

	/**
	 * An array of all the '<em><b>Action Type</b></em>' enumerators
	 * @generated
	 */
	private static final ActionType[] VALUES_ARRAY = { CREATE, DIRECT_UPLOAD, INDIRECT_UPLOAD, DOWNLOAD, POSITVE_DECISION,
			NEGATIVE_DECISION, UNDEF_DECISION, UPDATE, READ, DELETE, DOWNLOAD_EXPORT, UPLOAD_IMPORT, COPY };

	/**
	 * A public read-only list of all the '<em><b>Action Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<ActionType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Action Type</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static ActionType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Action Type</b></em>' literal with the specified name
	 * @generated
	 */
	public static ActionType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Action Type</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static ActionType get(int value) {
		switch (value) {
			case CREATE_VALUE:
				return CREATE;
			case DIRECT_UPLOAD_VALUE:
				return DIRECT_UPLOAD;
			case INDIRECT_UPLOAD_VALUE:
				return INDIRECT_UPLOAD;
			case DOWNLOAD_VALUE:
				return DOWNLOAD;
			case POSITVE_DECISION_VALUE:
				return POSITVE_DECISION;
			case NEGATIVE_DECISION_VALUE:
				return NEGATIVE_DECISION;
			case UNDEF_DECISION_VALUE:
				return UNDEF_DECISION;
			case UPDATE_VALUE:
				return UPDATE;
			case READ_VALUE:
				return READ;
			case DELETE_VALUE:
				return DELETE;
			case DOWNLOAD_EXPORT_VALUE:
				return DOWNLOAD_EXPORT;
			case UPLOAD_IMPORT_VALUE:
				return UPLOAD_IMPORT;
			case COPY_VALUE:
				return COPY;
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
	ActionType(int value, String name, String literal) {
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
