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
 * A representation of the literals of the enumeration '<em><b>Form Field Type Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormFieldTypeEnumeration()
 * @model
 * @generated
 */
public enum FormFieldTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>SIMPLE TEXT</b></em>' literal object
	 * @see #SIMPLE_TEXT_VALUE
	 * @generated
	 * @ordered
	 */
	SIMPLE_TEXT(0, "SIMPLE_TEXT", "SIMPLE_TEXT"),

	/**
	 * The '<em><b>LABEL</b></em>' literal object
	 * @see #LABEL_VALUE
	 * @generated
	 * @ordered
	 */
	LABEL(1, "LABEL", "LABEL"),

	/**
	 * The '<em><b>CHECKBOX</b></em>' literal object
	 * @see #CHECKBOX_VALUE
	 * @generated
	 * @ordered
	 */
	CHECKBOX(2, "CHECKBOX", "CHECKBOX"),

	/**
	 * The '<em><b>DATE TIME</b></em>' literal object
	 * @see #DATE_TIME_VALUE
	 * @generated
	 * @ordered
	 */
	DATE_TIME(3, "DATE_TIME", "DATE_TIME"),

	/**
	 * The '<em><b>MULTI LINE TEXT</b></em>' literal object
	 * @see #MULTI_LINE_TEXT_VALUE
	 * @generated
	 * @ordered
	 */
	MULTI_LINE_TEXT(4, "MULTI_LINE_TEXT", "MULTI_LINE_TEXT"),

	/**
	 * The '<em><b>COMBOBOX</b></em>' literal object
	 * @see #COMBOBOX_VALUE
	 * @generated
	 * @ordered
	 */
	COMBOBOX(5, "COMBOBOX", "COMBOBOX"),

	/**
	 * The '<em><b>LOV</b></em>' literal object
	 * @see #LOV_VALUE
	 * @generated
	 * @ordered
	 */
	LOV(6, "LOV", "LOV"),

	/**
	 * The '<em><b>DATE</b></em>' literal object
	 * @see #DATE_VALUE
	 * @generated
	 * @ordered
	 */
	DATE(7, "DATE", "DATE"),

	/**
	 * The '<em><b>LIST</b></em>' literal object
	 * @see #LIST_VALUE
	 * @generated
	 * @ordered
	 */
	LIST(8, "LIST", "LIST"),

	/**
	 * The '<em><b>SEARCHABLE LIST</b></em>' literal object
	 * @see #SEARCHABLE_LIST_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCHABLE_LIST(9, "SEARCHABLE_LIST", "SEARCHABLE_LIST"),

	/**
	 * The '<em><b>SELECTION BY PARENT FORM</b></em>' literal object
	 * @see #SELECTION_BY_PARENT_FORM_VALUE
	 * @generated
	 * @ordered
	 */
	SELECTION_BY_PARENT_FORM(10, "SELECTION_BY_PARENT_FORM", "SELECTION_BY_PARENT_FORM"),

	/**
	 * The '<em><b>SELECTION BY SECURITY DTO</b></em>' literal object
	 * @see #SELECTION_BY_SECURITY_DTO_VALUE
	 * @generated
	 * @ordered
	 */
	SELECTION_BY_SECURITY_DTO(11, "SELECTION_BY_SECURITY_DTO", "SELECTION_BY_SECURITY_DTO"),

	/**
	 * The '<em><b>PROPOSAL TEXT</b></em>' literal object
	 * @see #PROPOSAL_TEXT_VALUE
	 * @generated
	 * @ordered
	 */
	PROPOSAL_TEXT(12, "PROPOSAL_TEXT", "PROPOSAL_TEXT"),

	/**
	 * The '<em><b>ENUM COMBOBOX</b></em>' literal object
	 * @see #ENUM_COMBOBOX_VALUE
	 * @generated
	 * @ordered
	 */
	ENUM_COMBOBOX(13, "ENUM_COMBOBOX", "ENUM_COMBOBOX"),

	/**
	 * The '<em><b>DOCUMENT SIZE FIELD</b></em>' literal object
	 * @see #DOCUMENT_SIZE_FIELD_VALUE
	 * @generated
	 * @ordered
	 */
	DOCUMENT_SIZE_FIELD(14, "DOCUMENT_SIZE_FIELD", "DOCUMENT_SIZE_FIELD"),

	/**
	 * The '<em><b>MULTI LINE LABEL</b></em>' literal object
	 * @see #MULTI_LINE_LABEL_VALUE
	 * @generated
	 * @ordered
	 */
	MULTI_LINE_LABEL(15, "MULTI_LINE_LABEL", "MULTI_LINE_LABEL"),

	/**
	 * The '<em><b>WEB LINK</b></em>' literal object
	 * @see #WEB_LINK_VALUE
	 * @generated
	 * @ordered
	 */
	WEB_LINK(16, "WEB_LINK", "WEB_LINK"),

	/**
	 * The '<em><b>MAIL LINK</b></em>' literal object
	 * @see #MAIL_LINK_VALUE
	 * @generated
	 * @ordered
	 */
	MAIL_LINK(17, "MAIL_LINK", "MAIL_LINK"),

	/**
	 * The '<em><b>FORM LINK</b></em>' literal object
	 * @see #FORM_LINK_VALUE
	 * @generated
	 * @ordered
	 */
	FORM_LINK(18, "FORM_LINK", "FORM_LINK"),

	/**
	 * The '<em><b>SELECTION BY CLIENT</b></em>' literal object
	 * @see #SELECTION_BY_CLIENT_VALUE
	 * @generated
	 * @ordered
	 */
	SELECTION_BY_CLIENT(19, "SELECTION_BY_CLIENT", "SELECTION_BY_CLIENT"),

	/**
	 * The '<em><b>ELEMENT COLLECTION EDITOR</b></em>' literal object
	 * @see #ELEMENT_COLLECTION_EDITOR_VALUE
	 * @generated
	 * @ordered
	 */
	ELEMENT_COLLECTION_EDITOR(20, "ELEMENT_COLLECTION_EDITOR", "ELEMENT_COLLECTION_EDITOR");

	/**
	 * The '<em><b>SIMPLE TEXT</b></em>' literal value
	 * @see #SIMPLE_TEXT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SIMPLE_TEXT_VALUE = 0;

	/**
	 * The '<em><b>LABEL</b></em>' literal value
	 * @see #LABEL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LABEL_VALUE = 1;

	/**
	 * The '<em><b>CHECKBOX</b></em>' literal value
	 * @see #CHECKBOX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CHECKBOX_VALUE = 2;

	/**
	 * The '<em><b>DATE TIME</b></em>' literal value
	 * @see #DATE_TIME
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DATE_TIME_VALUE = 3;

	/**
	 * The '<em><b>MULTI LINE TEXT</b></em>' literal value
	 * @see #MULTI_LINE_TEXT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MULTI_LINE_TEXT_VALUE = 4;

	/**
	 * The '<em><b>COMBOBOX</b></em>' literal value
	 * @see #COMBOBOX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COMBOBOX_VALUE = 5;

	/**
	 * The '<em><b>LOV</b></em>' literal value
	 * @see #LOV
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LOV_VALUE = 6;

	/**
	 * The '<em><b>DATE</b></em>' literal value
	 * @see #DATE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DATE_VALUE = 7;

	/**
	 * The '<em><b>LIST</b></em>' literal value
	 * @see #LIST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int LIST_VALUE = 8;

	/**
	 * The '<em><b>SEARCHABLE LIST</b></em>' literal value
	 * @see #SEARCHABLE_LIST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCHABLE_LIST_VALUE = 9;

	/**
	 * The '<em><b>SELECTION BY PARENT FORM</b></em>' literal value
	 * @see #SELECTION_BY_PARENT_FORM
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SELECTION_BY_PARENT_FORM_VALUE = 10;

	/**
	 * The '<em><b>SELECTION BY SECURITY DTO</b></em>' literal value
	 * @see #SELECTION_BY_SECURITY_DTO
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SELECTION_BY_SECURITY_DTO_VALUE = 11;

	/**
	 * The '<em><b>PROPOSAL TEXT</b></em>' literal value
	 * @see #PROPOSAL_TEXT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PROPOSAL_TEXT_VALUE = 12;

	/**
	 * The '<em><b>ENUM COMBOBOX</b></em>' literal value
	 * @see #ENUM_COMBOBOX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ENUM_COMBOBOX_VALUE = 13;

	/**
	 * The '<em><b>DOCUMENT SIZE FIELD</b></em>' literal value
	 * @see #DOCUMENT_SIZE_FIELD
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOCUMENT_SIZE_FIELD_VALUE = 14;

	/**
	 * The '<em><b>MULTI LINE LABEL</b></em>' literal value
	 * @see #MULTI_LINE_LABEL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MULTI_LINE_LABEL_VALUE = 15;

	/**
	 * The '<em><b>WEB LINK</b></em>' literal value
	 * @see #WEB_LINK
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int WEB_LINK_VALUE = 16;

	/**
	 * The '<em><b>MAIL LINK</b></em>' literal value
	 * @see #MAIL_LINK
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MAIL_LINK_VALUE = 17;

	/**
	 * The '<em><b>FORM LINK</b></em>' literal value
	 * @see #FORM_LINK
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FORM_LINK_VALUE = 18;

	/**
	 * The '<em><b>SELECTION BY CLIENT</b></em>' literal value
	 * @see #SELECTION_BY_CLIENT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SELECTION_BY_CLIENT_VALUE = 19;

	/**
	 * The '<em><b>ELEMENT COLLECTION EDITOR</b></em>' literal value
	 * @see #ELEMENT_COLLECTION_EDITOR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ELEMENT_COLLECTION_EDITOR_VALUE = 20;

	/**
	 * An array of all the '<em><b>Form Field Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final FormFieldTypeEnumeration[] VALUES_ARRAY = { SIMPLE_TEXT, LABEL, CHECKBOX, DATE_TIME, MULTI_LINE_TEXT,
			COMBOBOX, LOV, DATE, LIST, SEARCHABLE_LIST, SELECTION_BY_PARENT_FORM, SELECTION_BY_SECURITY_DTO, PROPOSAL_TEXT,
			ENUM_COMBOBOX, DOCUMENT_SIZE_FIELD, MULTI_LINE_LABEL, WEB_LINK, MAIL_LINK, FORM_LINK, SELECTION_BY_CLIENT,
			ELEMENT_COLLECTION_EDITOR };

	/**
	 * A public read-only list of all the '<em><b>Form Field Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<FormFieldTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Form Field Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static FormFieldTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Form Field Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static FormFieldTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Form Field Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static FormFieldTypeEnumeration get(int value) {
		switch (value) {
			case SIMPLE_TEXT_VALUE:
				return SIMPLE_TEXT;
			case LABEL_VALUE:
				return LABEL;
			case CHECKBOX_VALUE:
				return CHECKBOX;
			case DATE_TIME_VALUE:
				return DATE_TIME;
			case MULTI_LINE_TEXT_VALUE:
				return MULTI_LINE_TEXT;
			case COMBOBOX_VALUE:
				return COMBOBOX;
			case LOV_VALUE:
				return LOV;
			case DATE_VALUE:
				return DATE;
			case LIST_VALUE:
				return LIST;
			case SEARCHABLE_LIST_VALUE:
				return SEARCHABLE_LIST;
			case SELECTION_BY_PARENT_FORM_VALUE:
				return SELECTION_BY_PARENT_FORM;
			case SELECTION_BY_SECURITY_DTO_VALUE:
				return SELECTION_BY_SECURITY_DTO;
			case PROPOSAL_TEXT_VALUE:
				return PROPOSAL_TEXT;
			case ENUM_COMBOBOX_VALUE:
				return ENUM_COMBOBOX;
			case DOCUMENT_SIZE_FIELD_VALUE:
				return DOCUMENT_SIZE_FIELD;
			case MULTI_LINE_LABEL_VALUE:
				return MULTI_LINE_LABEL;
			case WEB_LINK_VALUE:
				return WEB_LINK;
			case MAIL_LINK_VALUE:
				return MAIL_LINK;
			case FORM_LINK_VALUE:
				return FORM_LINK;
			case SELECTION_BY_CLIENT_VALUE:
				return SELECTION_BY_CLIENT;
			case ELEMENT_COLLECTION_EDITOR_VALUE:
				return ELEMENT_COLLECTION_EDITOR;
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
	FormFieldTypeEnumeration(int value, String name, String literal) {
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
