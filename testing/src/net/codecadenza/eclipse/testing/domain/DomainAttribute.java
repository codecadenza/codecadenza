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
package net.codecadenza.eclipse.testing.domain;

/**
 * <p>
 * Objects of this class represent domain attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAttribute {
	public static final String TYPE_BIG_DECIMAL = "BigDecimal";
	public static final String TYPE_BOOLEAN = "boolean";
	public static final String TYPE_CALENDAR = "GregorianCalendar";
	public static final String TYPE_DATE = "Date";
	public static final String TYPE_DOUBLE = "double";
	public static final String TYPE_LOCAL_DATE = "LocalDate";
	public static final String TYPE_LOCAL_DATE_TIME = "LocalDateTime";
	public static final String TYPE_LONG = "long";
	public static final String TYPE_STRING = "String";
	public static final String TYPE_UUID = "UUID";

	private final String name;
	private final String type;
	private final boolean primaryKey;
	private final boolean displayAttribute;

	/**
	 * Constructor
	 * @param name
	 * @param type
	 * @param primaryKey
	 * @param displayAttribute
	 */
	public DomainAttribute(String name, String type, boolean primaryKey, boolean displayAttribute) {
		this.name = name;
		this.type = type;
		this.primaryKey = primaryKey;
		this.displayAttribute = displayAttribute;
	}

	/**
	 * @return the domain attribute name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the Java type name of the domain attribute
	 */
	public String getType() {
		return type;
	}

	/**
	 * @return true if the domain attribute represents a primary key
	 */
	public boolean isPrimaryKey() {
		return primaryKey;
	}

	/**
	 * @return true if the domain attribute represents a display attribute
	 */
	public boolean isDisplayAttribute() {
		return displayAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return type + " " + name;
	}

}
