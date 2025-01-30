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
package net.codecadenza.eclipse.tools.sqleditor.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.tools.sqleditor.viewer.SQLSyntax;

/**
 * <p>
 * SQL syntax initializer
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLSyntaxInitializer {
	/**
	 * Prevent instantiation
	 */
	private SQLSyntaxInitializer() {

	}

	/**
	 * Initialize the SQL syntax object
	 * @param database
	 * @return the SQL syntax depending on the database
	 */
	public static synchronized SQLSyntax initializeSQLSyntax(Database database) {
		final var syntax = new SQLSyntax();
		final String[] reservedWords = database.getReservedWords().split(" ");
		final var wordMap = new HashSet<String>();

		for (final String word : reservedWords)
			wordMap.add(word.trim());

		syntax.setReservedWords(wordMap);

		// Add all tables including their columns to the syntax object
		database.getDatabaseTables().forEach(table -> {
			final ArrayList<String> columnList = table.getColumns().stream().map(DBColumn::getDatabaseName)
					.collect(Collectors.toCollection(ArrayList::new));

			syntax.getTableMap().put(table.getDatabaseName(), columnList);
		});

		return syntax;
	}

}
