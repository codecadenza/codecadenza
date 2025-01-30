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
package net.codecadenza.eclipse.tools.dbsync.service;

import java.util.HashSet;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.tools.util.db.DBManager;

/**
 * <p>
 * Interface for generating DDL statements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IDDLService {
	/**
	 * The JDBC meta-data interface doesn't provide information about sequences! A given implementation must search them manually!
	 * @param database
	 * @param dbManager
	 * @param catalogName
	 * @param schemaName
	 * @return a set containing all sequences
	 */
	HashSet<String> getAllSequences(Database database, DBManager dbManager, String catalogName, String schemaName);

	/**
	 * Create a sequence
	 * @param name
	 * @param startIndex
	 * @param blockSize
	 * @return the DDL script
	 */
	String createSequence(String name, int startIndex, int blockSize);

	/**
	 * Create a primary key
	 * @param table
	 * @return the DDL script
	 */
	String createPrimaryKey(DBTable table);

	/**
	 * Drop the primary key
	 * @param table
	 * @return the DDL script
	 */
	String dropPrimaryKey(DBTable table);

	/**
	 * Create a table
	 * @param table the table to be created
	 * @return the DDL script
	 */
	String createTable(DBTable table);

	/**
	 * Add a foreign key
	 * @param key
	 * @return the DDL script
	 */
	String addForeignKey(ForeignKey key);

	/**
	 * Remove the foreign key
	 * @param key
	 * @return the DDL script
	 */
	String removeForeignKey(ForeignKey key);

	/**
	 * Add a column to the table
	 * @param column
	 * @return the DDL script
	 */
	String addColumn(DBColumn column);

	/**
	 * Modifiy a column
	 * @param column
	 * @return the DDL script
	 */
	String modifyColumn(DBColumn column);

	/**
	 * Remove a column
	 * @param column
	 * @return the DDL script
	 */
	String removeColumn(DBColumn column);

	/**
	 * Add a unique key
	 * @param key the unique key to be created
	 * @return the DDL script
	 */
	String addUniqueKey(DBIndex key);

	/**
	 * Remove a unique key
	 * @param index the index to be removed
	 * @return the DDL script
	 */
	String removeUniqueKey(DBIndex index);

	/**
	 * Create an index
	 * @param index
	 * @return the DDL script
	 */
	String createIndex(DBIndex index);

	/**
	 * Remove an index
	 * @param index
	 * @return the DDL script
	 */
	String removeIndex(DBIndex index);

	/**
	 * Drop a table
	 * @param table the table to be dropped
	 * @return the DDL script
	 */
	String dropTable(DBTable table);

	/**
	 * Rename a column
	 * @param newName the new name of the column
	 * @param col the column to be renamed
	 * @return the DDL script
	 */
	String renameColumn(String newName, DBColumn col);

	/**
	 * Rename a table
	 * @param oldName the old name of the table
	 * @param table
	 * @return the DDL script
	 */
	String renameTable(String oldName, DBTable table);

}
