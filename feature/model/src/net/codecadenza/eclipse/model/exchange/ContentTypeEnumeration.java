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
package net.codecadenza.eclipse.model.exchange;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Content Type Enumeration</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getContentTypeEnumeration()
 * @model
 * @generated
 */
public enum ContentTypeEnumeration implements Enumerator {
	/**
	 * The '<em><b>XML</b></em>' literal object
	 * @see #XML_VALUE
	 * @generated
	 * @ordered
	 */
	XML(0, "XML", "XML"),

	/**
	 * The '<em><b>EXCEL97</b></em>' literal object
	 * @see #EXCEL97_VALUE
	 * @generated
	 * @ordered
	 */
	EXCEL97(1, "EXCEL97", "EXCEL97"),

	/**
	 * The '<em><b>EXCEL2007</b></em>' literal object
	 * @see #EXCEL2007_VALUE
	 * @generated
	 * @ordered
	 */
	EXCEL2007(2, "EXCEL2007", "EXCEL2007"),

	/**
	 * The '<em><b>CSV</b></em>' literal object
	 * @see #CSV_VALUE
	 * @generated
	 * @ordered
	 */
	CSV(3, "CSV", "CSV"),

	/**
	 * The '<em><b>JSON</b></em>' literal object
	 * @see #JSON_VALUE
	 * @generated
	 * @ordered
	 */
	JSON(4, "JSON", "JSON");

	/**
	 * The '<em><b>XML</b></em>' literal value
	 * @see #XML
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int XML_VALUE = 0;

	/**
	 * The '<em><b>EXCEL97</b></em>' literal value
	 * @see #EXCEL97
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXCEL97_VALUE = 1;

	/**
	 * The '<em><b>EXCEL2007</b></em>' literal value
	 * @see #EXCEL2007
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int EXCEL2007_VALUE = 2;

	/**
	 * The '<em><b>CSV</b></em>' literal value
	 * @see #CSV
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CSV_VALUE = 3;

	/**
	 * The '<em><b>JSON</b></em>' literal value
	 * @see #JSON
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JSON_VALUE = 4;

	/**
	 * An array of all the '<em><b>Content Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final ContentTypeEnumeration[] VALUES_ARRAY = { XML, EXCEL97, EXCEL2007, CSV, JSON };

	/**
	 * A public read-only list of all the '<em><b>Content Type Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<ContentTypeEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Content Type Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static ContentTypeEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Content Type Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static ContentTypeEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Content Type Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static ContentTypeEnumeration get(int value) {
		switch (value) {
			case XML_VALUE:
				return XML;
			case EXCEL97_VALUE:
				return EXCEL97;
			case EXCEL2007_VALUE:
				return EXCEL2007;
			case CSV_VALUE:
				return CSV;
			case JSON_VALUE:
				return JSON;
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
	ContentTypeEnumeration(int value, String name, String literal) {
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

	/**
	 * @return the default file extension for this content type
	 * @generated not
	 */
	public String getDefaultFileExtension() {
		if (name.equals(ContentTypeEnumeration.EXCEL97.name))
			return "xls";
		else if (name.equals(ContentTypeEnumeration.EXCEL2007.name))
			return "xlsx";
		else
			return name.toLowerCase();
	}

	/**
	 * @return the default file description for this content type
	 * @generated not
	 */
	public String getDefaultFileDescription() {
		if (name.equals(ContentTypeEnumeration.EXCEL97.name) || name.equals(ContentTypeEnumeration.EXCEL2007.name))
			return "Microsoft Excel files";

		return name.toUpperCase() + " files";
	}

}
