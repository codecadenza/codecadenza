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
package net.codecadenza.eclipse.model.db;

import static net.codecadenza.eclipse.shared.Constants.QUOTE;

/**
 * <p>
 * Utility class for converting database identifiers
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBNamingUtil {
	private static final String QUOTE_ESC = "\\\"";

	/**
	 * Prevent instantiation
	 */
	private DBNamingUtil() {

	}

	/**
	 * Converts a database identifier to a format that should be used for DDL operations
	 * @param name
	 * @param db
	 * @return the converted identifier
	 */
	public static String convertToDatabase(String name, Database db) {
		if (db == null)
			return name;

		final DBVendorGroupEnumeration dbVendor = db.getVendorGroup();

		if (db.getIdentifierStyle() == IdentifierStyleEnumeration.CASE_SENSITIVE) {
			if (dbVendor == DBVendorGroupEnumeration.ORACLE || dbVendor == DBVendorGroupEnumeration.POSTGRESQL
					|| dbVendor == DBVendorGroupEnumeration.H2 || dbVendor == DBVendorGroupEnumeration.H2_EMBEDDED)
				return QUOTE + name + QUOTE;
		}
		else if (db.getIdentifierStyle() == IdentifierStyleEnumeration.UPPERCASE) {
			if (dbVendor == DBVendorGroupEnumeration.POSTGRESQL)
				return QUOTE + name.toUpperCase() + QUOTE;

			return name.toUpperCase();
		}
		else if (db.getIdentifierStyle() == IdentifierStyleEnumeration.LOWERCASE) {
			if (dbVendor == DBVendorGroupEnumeration.ORACLE || dbVendor == DBVendorGroupEnumeration.H2
					|| dbVendor == DBVendorGroupEnumeration.H2_EMBEDDED)
				return QUOTE + name.toLowerCase() + QUOTE;

			return name.toLowerCase();
		}

		return name;
	}

	/**
	 * Converts a database identifier to common style which is driven by respective database property and removes all quote
	 * characters
	 * @param name
	 * @param db
	 * @return the converted identifier
	 */
	public static final String convertToStyle(String name, Database db) {
		String convertedName = name;

		if (db == null)
			return convertedName;

		if (db.getIdentifierStyle() == IdentifierStyleEnumeration.UPPERCASE)
			convertedName = name.toUpperCase();
		else if (db.getIdentifierStyle() == IdentifierStyleEnumeration.LOWERCASE)
			convertedName = name.toLowerCase();

		return convertedName.replace(QUOTE, "");
	}

	/**
	 * Converts a database identifier to a format that should be used for JPA annotations
	 * @param name
	 * @param db
	 * @return the converted identifier
	 */
	public static String convertToMapping(String name, Database db) {
		return convertToDatabase(name, db).replace(QUOTE, QUOTE_ESC);
	}

}
