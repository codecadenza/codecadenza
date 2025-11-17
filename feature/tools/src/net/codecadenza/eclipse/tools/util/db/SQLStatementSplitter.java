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
package net.codecadenza.eclipse.tools.util.db;

import java.util.Collections;
import java.util.List;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;

/**
 * <p>
 * Utility class for splitting a string that contains multiple SQL statements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLStatementSplitter {

	/**
	 * Prevent instantiation
	 */
	private SQLStatementSplitter() {

	}

	/**
	 * Split the given SQL statements using the ';' character
	 * @param statements a string that contains any number of SQL statements
	 * @return a list with all SQL statements that are contained in the given input string
	 */
	public static List<String> splitSQLStatements(String statements) {
		if (statements == null || statements.isEmpty())
			return Collections.emptyList();

		try {
			return CCJSqlParserUtil.parseStatements(statements).stream().map(Object::toString).toList();
		}
		catch (final Exception e) {
			throw new IllegalStateException("Error while parsing SQL statements! Message: " + e.getMessage());
		}
	}

}
