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
 * A representation of the literals of the enumeration '<em><b>GUI Test Action Type</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionType()
 * @model
 * @generated
 */
public enum GUITestActionType implements Enumerator {
	/**
	 * The '<em><b>EXECUTE FORM ACTION</b></em>' literal object
	 * @see #EXECUTE_FORM_ACTION_VALUE
	 * @generated
	 * @ordered
	 */
	EXECUTE_FORM_ACTION(0, "EXECUTE_FORM_ACTION", "EXECUTE_FORM_ACTION"),

	/**
	 * The '<em><b>EXECUTE REFRESH ACTION</b></em>' literal object
	 * @see #EXECUTE_REFRESH_ACTION_VALUE
	 * @generated
	 * @ordered
	 */
	EXECUTE_REFRESH_ACTION(1, "EXECUTE_REFRESH_ACTION", "EXECUTE_REFRESH_ACTION"),

	/**
	 * The '<em><b>OPEN PAGE DIRECT</b></em>' literal object
	 * @see #OPEN_PAGE_DIRECT_VALUE
	 * @generated
	 * @ordered
	 */
	OPEN_PAGE_DIRECT(2, "OPEN_PAGE_DIRECT", "OPEN_PAGE_DIRECT"),

	/**
	 * The '<em><b>OPEN PAGE BY NAVIGATOR</b></em>' literal object
	 * @see #OPEN_PAGE_BY_NAVIGATOR_VALUE
	 * @generated
	 * @ordered
	 */
	OPEN_PAGE_BY_NAVIGATOR(3, "OPEN_PAGE_BY_NAVIGATOR", "OPEN_PAGE_BY_NAVIGATOR"),

	/**
	 * The '<em><b>PERFORM LOGOUT</b></em>' literal object
	 * @see #PERFORM_LOGOUT_VALUE
	 * @generated
	 * @ordered
	 */
	PERFORM_LOGOUT(4, "PERFORM_LOGOUT", "PERFORM_LOGOUT"),

	/**
	 * The '<em><b>SEARCH ROW CURRENT PAGE</b></em>' literal object
	 * @see #SEARCH_ROW_CURRENT_PAGE_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_ROW_CURRENT_PAGE(5, "SEARCH_ROW_CURRENT_PAGE", "SEARCH_ROW_CURRENT_PAGE"),

	/**
	 * The '<em><b>SEARCH ROW ALL PAGES</b></em>' literal object
	 * @see #SEARCH_ROW_ALL_PAGES_VALUE
	 * @generated
	 * @ordered
	 */
	SEARCH_ROW_ALL_PAGES(6, "SEARCH_ROW_ALL_PAGES", "SEARCH_ROW_ALL_PAGES"),

	/**
	 * The '<em><b>ENTER SEARCH DATA</b></em>' literal object
	 * @see #ENTER_SEARCH_DATA_VALUE
	 * @generated
	 * @ordered
	 */
	ENTER_SEARCH_DATA(7, "ENTER_SEARCH_DATA", "ENTER_SEARCH_DATA"),

	/**
	 * The '<em><b>COUNT RECORDS</b></em>' literal object
	 * @see #COUNT_RECORDS_VALUE
	 * @generated
	 * @ordered
	 */
	COUNT_RECORDS(8, "COUNT_RECORDS", "COUNT_RECORDS"),

	/**
	 * The '<em><b>RESET SEARCH DATA</b></em>' literal object
	 * @see #RESET_SEARCH_DATA_VALUE
	 * @generated
	 * @ordered
	 */
	RESET_SEARCH_DATA(9, "RESET_SEARCH_DATA", "RESET_SEARCH_DATA"),

	/**
	 * The '<em><b>ENTER FORM DATA</b></em>' literal object
	 * @see #ENTER_FORM_DATA_VALUE
	 * @generated
	 * @ordered
	 */
	ENTER_FORM_DATA(10, "ENTER_FORM_DATA", "ENTER_FORM_DATA"),

	/**
	 * The '<em><b>VALIDATE FORM DATA</b></em>' literal object
	 * @see #VALIDATE_FORM_DATA_VALUE
	 * @generated
	 * @ordered
	 */
	VALIDATE_FORM_DATA(11, "VALIDATE_FORM_DATA", "VALIDATE_FORM_DATA"),

	/**
	 * The '<em><b>DOUBLE CLICK ROW</b></em>' literal object
	 * @see #DOUBLE_CLICK_ROW_VALUE
	 * @generated
	 * @ordered
	 */
	DOUBLE_CLICK_ROW(12, "DOUBLE_CLICK_ROW", "DOUBLE_CLICK_ROW"),

	/**
	 * The '<em><b>PRESS OK BUTTON</b></em>' literal object
	 * @see #PRESS_OK_BUTTON_VALUE
	 * @generated
	 * @ordered
	 */
	PRESS_OK_BUTTON(13, "PRESS_OK_BUTTON", "PRESS_OK_BUTTON"),

	/**
	 * The '<em><b>PRESS CANCEL BUTTON</b></em>' literal object
	 * @see #PRESS_CANCEL_BUTTON_VALUE
	 * @generated
	 * @ordered
	 */
	PRESS_CANCEL_BUTTON(14, "PRESS_CANCEL_BUTTON", "PRESS_CANCEL_BUTTON"),

	/**
	 * The '<em><b>OPEN LOGIN PAGE</b></em>' literal object
	 * @see #OPEN_LOGIN_PAGE_VALUE
	 * @generated
	 * @ordered
	 */
	OPEN_LOGIN_PAGE(15, "OPEN_LOGIN_PAGE", "OPEN_LOGIN_PAGE"),

	/**
	 * The '<em><b>UPLOAD FILE</b></em>' literal object
	 * @see #UPLOAD_FILE_VALUE
	 * @generated
	 * @ordered
	 */
	UPLOAD_FILE(16, "UPLOAD_FILE", "UPLOAD_FILE"),

	/**
	 * The '<em><b>PRESS DOWNLOAD BUTTON</b></em>' literal object
	 * @see #PRESS_DOWNLOAD_BUTTON_VALUE
	 * @generated
	 * @ordered
	 */
	PRESS_DOWNLOAD_BUTTON(17, "PRESS_DOWNLOAD_BUTTON", "PRESS_DOWNLOAD_BUTTON"),

	/**
	 * The '<em><b>VALIDATE ROW COUNT EQUAL</b></em>' literal object
	 * @see #VALIDATE_ROW_COUNT_EQUAL_VALUE
	 * @generated
	 * @ordered
	 */
	VALIDATE_ROW_COUNT_EQUAL(18, "VALIDATE_ROW_COUNT_EQUAL", "VALIDATE_ROW_COUNT_EQUAL"),

	/**
	 * The '<em><b>VALIDATE ROW COUNT GREATER</b></em>' literal object
	 * @see #VALIDATE_ROW_COUNT_GREATER_VALUE
	 * @generated
	 * @ordered
	 */
	VALIDATE_ROW_COUNT_GREATER(19, "VALIDATE_ROW_COUNT_GREATER", "VALIDATE_ROW_COUNT_GREATER"),

	/**
	 * The '<em><b>VALIDATE ROW COUNT SMALLER</b></em>' literal object
	 * @see #VALIDATE_ROW_COUNT_SMALLER_VALUE
	 * @generated
	 * @ordered
	 */
	VALIDATE_ROW_COUNT_SMALLER(20, "VALIDATE_ROW_COUNT_SMALLER", "VALIDATE_ROW_COUNT_SMALLER");

	/**
	 * The '<em><b>EXECUTE FORM ACTION</b></em>' literal value
	 * @see #EXECUTE_FORM_ACTION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXECUTE_FORM_ACTION_VALUE = 0;

	/**
	 * The '<em><b>EXECUTE REFRESH ACTION</b></em>' literal value
	 * @see #EXECUTE_REFRESH_ACTION
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXECUTE_REFRESH_ACTION_VALUE = 1;

	/**
	 * The '<em><b>OPEN PAGE DIRECT</b></em>' literal value
	 * @see #OPEN_PAGE_DIRECT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int OPEN_PAGE_DIRECT_VALUE = 2;

	/**
	 * The '<em><b>OPEN PAGE BY NAVIGATOR</b></em>' literal value
	 * @see #OPEN_PAGE_BY_NAVIGATOR
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int OPEN_PAGE_BY_NAVIGATOR_VALUE = 3;

	/**
	 * The '<em><b>PERFORM LOGOUT</b></em>' literal value
	 * @see #PERFORM_LOGOUT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PERFORM_LOGOUT_VALUE = 4;

	/**
	 * The '<em><b>SEARCH ROW CURRENT PAGE</b></em>' literal value
	 * @see #SEARCH_ROW_CURRENT_PAGE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_ROW_CURRENT_PAGE_VALUE = 5;

	/**
	 * The '<em><b>SEARCH ROW ALL PAGES</b></em>' literal value
	 * @see #SEARCH_ROW_ALL_PAGES
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SEARCH_ROW_ALL_PAGES_VALUE = 6;

	/**
	 * The '<em><b>ENTER SEARCH DATA</b></em>' literal value
	 * @see #ENTER_SEARCH_DATA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ENTER_SEARCH_DATA_VALUE = 7;

	/**
	 * The '<em><b>COUNT RECORDS</b></em>' literal value
	 * @see #COUNT_RECORDS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COUNT_RECORDS_VALUE = 8;

	/**
	 * The '<em><b>RESET SEARCH DATA</b></em>' literal value
	 * @see #RESET_SEARCH_DATA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int RESET_SEARCH_DATA_VALUE = 9;

	/**
	 * The '<em><b>ENTER FORM DATA</b></em>' literal value
	 * @see #ENTER_FORM_DATA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ENTER_FORM_DATA_VALUE = 10;

	/**
	 * The '<em><b>VALIDATE FORM DATA</b></em>' literal value
	 * @see #VALIDATE_FORM_DATA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int VALIDATE_FORM_DATA_VALUE = 11;

	/**
	 * The '<em><b>DOUBLE CLICK ROW</b></em>' literal value
	 * @see #DOUBLE_CLICK_ROW
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOUBLE_CLICK_ROW_VALUE = 12;

	/**
	 * The '<em><b>PRESS OK BUTTON</b></em>' literal value
	 * @see #PRESS_OK_BUTTON
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PRESS_OK_BUTTON_VALUE = 13;

	/**
	 * The '<em><b>PRESS CANCEL BUTTON</b></em>' literal value
	 * @see #PRESS_CANCEL_BUTTON
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PRESS_CANCEL_BUTTON_VALUE = 14;

	/**
	 * The '<em><b>OPEN LOGIN PAGE</b></em>' literal value
	 * @see #OPEN_LOGIN_PAGE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int OPEN_LOGIN_PAGE_VALUE = 15;

	/**
	 * The '<em><b>UPLOAD FILE</b></em>' literal value
	 * @see #UPLOAD_FILE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int UPLOAD_FILE_VALUE = 16;

	/**
	 * The '<em><b>PRESS DOWNLOAD BUTTON</b></em>' literal value
	 * @see #PRESS_DOWNLOAD_BUTTON_VALUE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int PRESS_DOWNLOAD_BUTTON_VALUE = 17;

	/**
	 * The '<em><b>VALIDATE ROW COUNT EQUAL</b></em>' literal value
	 * @see #VALIDATE_ROW_COUNT_EQUAL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int VALIDATE_ROW_COUNT_EQUAL_VALUE = 18;

	/**
	 * The '<em><b>VALIDATE ROW COUNT GREATER</b></em>' literal value
	 * @see #VALIDATE_ROW_COUNT_GREATER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int VALIDATE_ROW_COUNT_GREATER_VALUE = 19;

	/**
	 * The '<em><b>VALIDATE ROW COUNT SMALLER</b></em>' literal value
	 * @see #VALIDATE_ROW_COUNT_SMALLER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int VALIDATE_ROW_COUNT_SMALLER_VALUE = 20;

	/**
	 * An array of all the '<em><b>GUI Test Action Type</b></em>' enumerators
	 * @generated
	 */
	private static final GUITestActionType[] VALUES_ARRAY = { EXECUTE_FORM_ACTION, EXECUTE_REFRESH_ACTION, OPEN_PAGE_DIRECT,
			OPEN_PAGE_BY_NAVIGATOR, PERFORM_LOGOUT, SEARCH_ROW_CURRENT_PAGE, SEARCH_ROW_ALL_PAGES, ENTER_SEARCH_DATA, COUNT_RECORDS,
			RESET_SEARCH_DATA, ENTER_FORM_DATA, VALIDATE_FORM_DATA, DOUBLE_CLICK_ROW, PRESS_OK_BUTTON, PRESS_CANCEL_BUTTON,
			OPEN_LOGIN_PAGE, VALIDATE_ROW_COUNT_EQUAL, VALIDATE_ROW_COUNT_GREATER, VALIDATE_ROW_COUNT_SMALLER, UPLOAD_FILE,
			PRESS_DOWNLOAD_BUTTON };

	/**
	 * A public read-only list of all the '<em><b>GUI Test Action Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<GUITestActionType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>GUI Test Action Type</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Action Type</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>GUI Test Action Type</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static GUITestActionType get(int value) {
		switch (value) {
			case EXECUTE_FORM_ACTION_VALUE:
				return EXECUTE_FORM_ACTION;
			case EXECUTE_REFRESH_ACTION_VALUE:
				return EXECUTE_REFRESH_ACTION;
			case OPEN_PAGE_DIRECT_VALUE:
				return OPEN_PAGE_DIRECT;
			case OPEN_PAGE_BY_NAVIGATOR_VALUE:
				return OPEN_PAGE_BY_NAVIGATOR;
			case PERFORM_LOGOUT_VALUE:
				return PERFORM_LOGOUT;
			case SEARCH_ROW_CURRENT_PAGE_VALUE:
				return SEARCH_ROW_CURRENT_PAGE;
			case SEARCH_ROW_ALL_PAGES_VALUE:
				return SEARCH_ROW_ALL_PAGES;
			case ENTER_SEARCH_DATA_VALUE:
				return ENTER_SEARCH_DATA;
			case COUNT_RECORDS_VALUE:
				return COUNT_RECORDS;
			case RESET_SEARCH_DATA_VALUE:
				return RESET_SEARCH_DATA;
			case ENTER_FORM_DATA_VALUE:
				return ENTER_FORM_DATA;
			case VALIDATE_FORM_DATA_VALUE:
				return VALIDATE_FORM_DATA;
			case DOUBLE_CLICK_ROW_VALUE:
				return DOUBLE_CLICK_ROW;
			case PRESS_OK_BUTTON_VALUE:
				return PRESS_OK_BUTTON;
			case PRESS_CANCEL_BUTTON_VALUE:
				return PRESS_CANCEL_BUTTON;
			case OPEN_LOGIN_PAGE_VALUE:
				return OPEN_LOGIN_PAGE;
			case UPLOAD_FILE_VALUE:
				return UPLOAD_FILE;
			case PRESS_DOWNLOAD_BUTTON_VALUE:
				return PRESS_DOWNLOAD_BUTTON;
			case VALIDATE_ROW_COUNT_EQUAL_VALUE:
				return VALIDATE_ROW_COUNT_EQUAL;
			case VALIDATE_ROW_COUNT_GREATER_VALUE:
				return VALIDATE_ROW_COUNT_GREATER;
			case VALIDATE_ROW_COUNT_SMALLER_VALUE:
				return VALIDATE_ROW_COUNT_SMALLER;
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
	GUITestActionType(int value, String name, String literal) {
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
