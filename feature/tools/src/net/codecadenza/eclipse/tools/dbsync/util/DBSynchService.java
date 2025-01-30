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
package net.codecadenza.eclipse.tools.dbsync.util;

import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN_EDIT;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_EDIT;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE_NEW;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE_REPLACE;
import static net.codecadenza.eclipse.shared.Constants.QUOTE;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.dbsync.model.DDLTransaction;
import net.codecadenza.eclipse.tools.dbsync.model.DDLTransactionType;
import net.codecadenza.eclipse.tools.dbsync.service.DDLServiceFactory;
import net.codecadenza.eclipse.tools.dbsync.service.IDDLService;
import net.codecadenza.eclipse.tools.util.db.DBManager;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Helper class that provides services concerning database synchronization
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBSynchService {
	private static final String DERBY_SYS_INDEX_PREFIX = "sql";

	private Datasource ds;
	private int transCount;
	private final Database db;
	private ArrayList<DDLTransaction> addKeysTransactions;
	private ArrayList<DDLTransaction> addPKsTransactions;
	private ArrayList<DDLTransaction> removePKsTransactions;
	private ArrayList<DDLTransaction> removeIndexTransactions;
	private ArrayList<DDLTransaction> addColsTransactions;
	private ArrayList<DDLTransaction> removeColsTransactions;
	private ArrayList<DDLTransaction> addSequenceTransactions;
	private IDDLService ddlService;

	/**
	 * Constructor
	 * @param db
	 */
	public DBSynchService(Database db) {
		this.db = db;
	}

	/**
	 * Constructor
	 * @param db
	 * @param ds
	 */
	public DBSynchService(Database db, Datasource ds) {
		this(db);

		this.ds = ds;
	}

	/**
	 * Check if the provided string represents a valid database identifier
	 * @param identifier
	 * @throws DBObjectValidationException if the identifier is illegal
	 */
	public void validateIdentifier(String identifier) throws DBObjectValidationException {
		final var reservedWords = new HashMap<String, String>();

		// Get all reserved words of this database vendor
		if (db.getReservedWords() != null)
			for (final String word : db.getReservedWords().split(" "))
				reservedWords.put(word, "");

		if (identifier == null || identifier.isEmpty())
			throw new DBObjectValidationException("The database identifier must not be empty!");

		if (!identifier.matches(db.getIdentifierRegEx())) {
			final var msg = "The database identifier '" + identifier + "' is illegal as it doesn't match the regular expression '"
					+ db.getIdentifierRegEx() + "'!";

			throw new DBObjectValidationException(msg);
		}

		// Check the length of the column name
		if (identifier.length() > db.getMaxIdentifierLength()) {
			final var msg = "The database identifier '" + identifier + "' exceeds the maximum length of " + db.getMaxIdentifierLength()
					+ " characters!";

			throw new DBObjectValidationException(msg);
		}

		// Check if the column name is a reserved word
		if (reservedWords.containsKey(identifier.toUpperCase()))
			throw new DBObjectValidationException("The database identifier '" + identifier + "' must not be a reserved word!");
	}

	/**
	 * Validate the database column name
	 * @param table
	 * @param colName the name of the column
	 * @throws DBObjectValidationException if the validation has failed
	 */
	public void validateColumnName(DBTable table, String colName) throws DBObjectValidationException {
		validateIdentifier(colName);

		for (final DBColumn col : table.getColumns()) {
			final String convertedName = col.getConvertedName();
			final String orginalName = col.getName();

			// Change the column name temporarily
			col.setName(colName);

			final String newName = col.getConvertedName();

			col.setName(orginalName);

			if (convertedName.equals(newName))
				throw new DBObjectValidationException(
						"A column with the name '" + colName + "' already exists in table '" + table.getConvertedName() + "'!");
		}
	}

	/**
	 * Validate the database table name
	 * @param schemaName
	 * @param catalogName
	 * @param tableName the name of the table
	 * @throws DBObjectValidationException if the name is invalid
	 */
	public void validateTableName(String schemaName, String catalogName, String tableName) throws DBObjectValidationException {
		validateIdentifier(tableName);

		// Create the table temporarily in order to use getFullDatabaseName() properly!
		final DBTable tmpTable = DbFactory.eINSTANCE.createDBTable();
		tmpTable.setDatabase(db);
		tmpTable.setName(tableName);
		tmpTable.setSchemaName(schemaName);
		tmpTable.setCatalogName(catalogName);

		for (final DBTable table : db.getDatabaseTables())
			if (!table.equals(tmpTable) && table.getFullDatabaseName().equals(tmpTable.getFullDatabaseName())) {
				tmpTable.setDatabase(null);
				final String displayName = table.getFullDatabaseName().replace(QUOTE, "");

				throw new DBObjectValidationException("A database table with the name '" + displayName + "' already exists!");
			}

		tmpTable.setDatabase(null);
	}

	/**
	 * Rename the given database column
	 * @param newName the new name of the column
	 * @param col the column object
	 * @throws DBObjectValidationException if the name of the column is invalid
	 */
	public void renameColumn(String newName, DBColumn col) throws DBObjectValidationException {
		// Check if the new name is valid
		validateColumnName(col.getDatabaseTable(), newName);

		// Initialize the service for creating DDL statements
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		final String oldName = col.getName();

		col.setName(newName);

		// Reset the column name temporarily!
		final String newDatabaseColName = col.getDatabaseName();

		col.setName(oldName);

		// Build the DDL statement and execute it
		executeDDL(ddlService.renameColumn(newDatabaseColName, col));
	}

	/**
	 * Rename the database table. Note that the caller is responsible for providing a valid table name!
	 * @param table
	 * @param oldName
	 * @param newName
	 */
	public void renameTable(DBTable table, String oldName, String newName) {
		// Initialize the service for creating DDL statements
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		// Reset the name temporarily!
		table.setName(oldName);
		final String oldTableName = table.getFullDatabaseName();

		table.setName(newName);

		// Build the DDL statement and execute it
		executeDDL(ddlService.renameTable(oldTableName, table));
	}

	/**
	 * Create the given database table
	 * @param table the table to be created
	 */
	public void createTable(DBTable table) {
		// Initialize the service for creating DDL statements
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		// Create the DDL script for creating the table and try to create the table in the target database
		executeDDL(ddlService.createTable(table));

		// Create the unique keys
		table.getIndexes().forEach(key -> executeDDL(ddlService.addUniqueKey(key)));

		// Create the foreign keys
		table.getForeignKeys().forEach(key -> executeDDL(ddlService.addForeignKey(key)));
	}

	/**
	 * Recreate the given database table in the target database
	 * @param table
	 */
	public void recreateTable(DBTable table) {
		// Initialize the service for creating DDL statements
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		// Drop the existing table
		executeDDL(ddlService.dropTable(table));

		// Create the DDL script for creating the table and try to create table in the target database
		executeDDL(ddlService.createTable(table));

		// Create the unique keys
		table.getIndexes().forEach(key -> executeDDL(ddlService.addUniqueKey(key)));

		// Create the foreign keys
		table.getForeignKeys().forEach(key -> executeDDL(ddlService.addForeignKey(key)));
	}

	/**
	 * Drop the given table in the target database
	 * @param table
	 */
	public void dropTable(DBTable table) {
		// Initialize the service for creating DDL statements
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		// Create the DDL script for dropping the table and execute the statement
		executeDDL(ddlService.dropTable(table));
	}

	/**
	 * Test if the schema and the catalog of a given database table match with the current schema and the current catalog
	 * @param table
	 * @param schemaName
	 * @param catalogName
	 * @return true if the given schema and the catalog match
	 */
	private boolean compareSchemaAndCatalog(DBTable table, String schemaName, String catalogName) {
		final String tableSchema = table.getSchemaName() == null || table.getSchemaName().isEmpty() ? null
				: DBNamingUtil.convertToStyle(table.getSchemaName(), db);
		final String tableCatalog = table.getCatalogName() == null || table.getCatalogName().isEmpty() ? null
				: DBNamingUtil.convertToStyle(table.getCatalogName(), db);

		if (schemaName == null && tableSchema != null)
			return false;

		if (schemaName != null && !schemaName.equals(tableSchema))
			return false;

		if (catalogName == null && tableCatalog != null)
			return false;

		return catalogName == null || catalogName.equals(tableCatalog);
	}

	/**
	 * Add indexes to the given table
	 * @param databaseMetaData
	 * @param table
	 */
	private void addIndexesToTable(DatabaseMetaData databaseMetaData, DBTable table) {
		final var indexMap = new HashMap<String, DBIndex>();
		final DBVendorGroupEnumeration dbVendor = db.getVendorGroup();

		for (final String catalogName : db.getAllCatalogs())
			for (final String schemaName : db.getAllSchemas()) {
				// When searching for database meta-data JDBC drivers expect non-empty String parameters for the default catalog and the
				// default schema name!
				final String catalogFilter = catalogName.isEmpty() ? null : catalogName;
				final String schemaFilter = schemaName.isEmpty() ? null : schemaName;

				// Get all indexes and add them to the table
				try (ResultSet rs = databaseMetaData.getIndexInfo(catalogFilter, schemaFilter, table.getConvertedName(), false, true)) {
					if (!compareSchemaAndCatalog(table, schemaFilter, catalogFilter))
						continue;

					while (rs.next()) {
						final String indexName = rs.getString("INDEX_NAME");

						if (indexName == null || indexName.isEmpty())
							continue;

						// If we are using Derby the JDBC meta-data interface will also return system-generated indexes that we should skip!
						if ((dbVendor == DBVendorGroupEnumeration.DERBY || dbVendor == DBVendorGroupEnumeration.DERBY_EMBEDDED)
								&& indexName.toLowerCase().startsWith(DERBY_SYS_INDEX_PREFIX))
							continue;

						final String keyName = DBNamingUtil.convertToStyle(indexName, db);
						final boolean fkFound = table.getForeignKeys().stream().anyMatch(fk -> fk.getConvertedName().equals(keyName));

						// Prevent the creation of the index if it refers to a foreign key!
						if (fkFound)
							continue;

						// Prevent the creation of the index if it represents a primary key!
						if (table.getPrimaryKey() != null && keyName.equals(table.getPrimaryKey().getConvertedName()))
							continue;

						// Check if the index already exists
						DBIndex index = indexMap.get(keyName);

						if (index == null) {
							index = DbFactory.eINSTANCE.createDBIndex();
							index.setUnique(!rs.getBoolean("NON_UNIQUE"));
							index.setName(keyName);
							index.setTable(table);

							table.getIndexes().add(index);
							indexMap.put(keyName, index);
						}

						// Add the column to the index
						for (final DBColumn col : table.getColumns()) {
							final String colName = DBNamingUtil.convertToStyle(rs.getString("COLUMN_NAME"), db);

							if (col.getConvertedName().equals(colName)) {
								index.getColumns().add(col);
								break;
							}
						}
					}
				}
				catch (final SQLException sqlException) {
					CodeCadenzaToolsPlugin.getInstance().logError(sqlException);
				}
			}
	}

	/**
	 * Add a primary key to the given table
	 * @param databaseMetaData
	 * @param table
	 */
	private void addPrimaryKeyToTable(DatabaseMetaData databaseMetaData, DBTable table) {
		for (final String catalogName : db.getAllCatalogs())
			for (final String schemaName : db.getAllSchemas()) {
				// When searching for database meta-data JDBC drivers expect non-empty String parameters for the default catalog and the
				// default schema name!
				final String catalogFilter = catalogName.isEmpty() ? null : catalogName;
				final String schemaFilter = schemaName.isEmpty() ? null : schemaName;

				// Add the primary key to the table
				try (ResultSet rs = databaseMetaData.getPrimaryKeys(catalogFilter, schemaFilter, table.getConvertedName())) {
					if (!compareSchemaAndCatalog(table, schemaFilter, catalogFilter))
						continue;

					String pkColName = null;
					String pkName = null;
					int pkColCount = 0;

					while (rs.next()) {
						pkColName = DBNamingUtil.convertToStyle(rs.getString("COLUMN_NAME"), db);
						pkName = rs.getString("PK_NAME");
						pkColCount++;
					}

					// A primary key is only added if it contains exactly one column!
					if (pkColCount == 1 && pkName != null) {
						final PrimaryKey pk = DbFactory.eINSTANCE.createPrimaryKey();
						pk.setName(DBNamingUtil.convertToStyle(pkName, db));
						pk.setTable(table);

						for (final DBColumn col : table.getColumns())
							if (col.getConvertedName().equals(pkColName)) {
								pk.setColumn(col);
								break;
							}

						table.setPrimaryKey(pk);
					}
				}
				catch (final SQLException sqlException) {
					CodeCadenzaToolsPlugin.getInstance().logError(sqlException);
				}
			}
	}

	/**
	 * Add the foreign keys to the table
	 * @param databaseMetaData
	 * @param table
	 * @param targetDB
	 */
	private void addForeignKeysToTable(DatabaseMetaData databaseMetaData, DBTable table, Database targetDB) {
		for (final String catalogName : db.getAllCatalogs())
			for (final String schemaName : db.getAllSchemas()) {
				// When searching for database meta-data JDBC drivers expect non-empty String parameters for the default catalog and the
				// default schema name!
				final String catalogFilter = catalogName.isEmpty() ? null : catalogName;
				final String schemaFilter = schemaName.isEmpty() ? null : schemaName;

				try (ResultSet rs = databaseMetaData.getImportedKeys(catalogFilter, schemaFilter, table.getConvertedName())) {
					if (!compareSchemaAndCatalog(table, schemaFilter, catalogFilter))
						continue;

					while (rs.next()) {
						final ForeignKey foreignKey = DbFactory.eINSTANCE.createForeignKey();
						foreignKey.setName(DBNamingUtil.convertToStyle(rs.getString("FK_NAME"), db));
						foreignKey.setTable(table);

						for (final DBColumn col : table.getColumns()) {
							final String colName = DBNamingUtil.convertToStyle(rs.getString("FKCOLUMN_NAME"), db);

							if (col.getConvertedName().equals(colName)) {
								foreignKey.setColumn(col);
								break;
							}
						}

						for (final DBTable refTable : targetDB.getDatabaseTables()) {
							final String tableName = DBNamingUtil.convertToStyle(rs.getString("PKTABLE_NAME"), db);

							if (refTable.getConvertedName().equals(tableName)) {
								for (final DBColumn refCol : refTable.getColumns()) {
									final String colName = DBNamingUtil.convertToStyle(rs.getString("PKCOLUMN_NAME"), db);

									if (refCol.getConvertedName().equals(colName)) {
										foreignKey.setReferencedColumn(refCol);
										break;
									}
								}
							}
						}

						table.getForeignKeys().add(foreignKey);
					}
				}
				catch (final SQLException sqlException) {
					CodeCadenzaToolsPlugin.getInstance().logError(sqlException);
				}
			}
	}

	/**
	 * @return a list containing all sequences
	 * @throws RuntimeException if the operation has failed
	 */
	public Set<String> getAllSequences() {
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		try (var dbManager = new DBManager(ds)) {
			return ddlService.getAllSequences(db, dbManager, "", "");
		}
		catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Get a snap-shot of the target database
	 * @return the database
	 * @throws RuntimeException if the operation has failed
	 */
	public Database getDatabaseSnapShot() {
		DatabaseMetaData databaseMetaData = null;
		Map<String, DBTable> tableMap = new HashMap<>();
		Connection con = null;

		final Database targetDB = DbFactory.eINSTANCE.createDatabase();
		targetDB.setCatalogName(db.getCatalogName());
		targetDB.setSchemaName(db.getSchemaName());
		targetDB.setIdentifierStyle(db.getIdentifierStyle());
		targetDB.setVendorGroup(db.getVendorGroup());

		try (var dbManager = new DBManager(ds)) {
			con = dbManager.getConnection();

			// Get the JDBC meta-data
			databaseMetaData = con.getMetaData();

			for (final String catalogName : db.getAllCatalogs())
				for (final String schemaName : db.getAllSchemas())
					tableMap = getDatabaseTables(databaseMetaData, targetDB, catalogName, schemaName);

			// Add the primary key, all foreign keys and all indexes to the table
			for (final DBTable table : tableMap.values()) {
				// Note: It is absolutely important to call this method before calling addIndexesToTable() in order to prevent adding the
				// primary key and the foreign keys as indexes
				addPrimaryKeyToTable(databaseMetaData, table);
				addForeignKeysToTable(databaseMetaData, table, targetDB);
				addIndexesToTable(databaseMetaData, table);
			}
		}
		catch (final Exception e) {
			throw new RuntimeException(e);
		}

		return targetDB;
	}

	/**
	 * Get the meta-data of all tables for the given catalog and schema
	 * @param databaseMetaData
	 * @param targetDB
	 * @param catalogName
	 * @param schemaName
	 * @return a {@link java.util.Map} that contains all table names and their respective meta-data objects
	 * @throws SQLException if the meta-data for the database tables could not be determined
	 */
	private Map<String, DBTable> getDatabaseTables(DatabaseMetaData databaseMetaData, final Database targetDB, String catalogName,
			String schemaName) throws SQLException {
		final var tableMap = new HashMap<String, DBTable>();

		// When searching for database meta-data JDBC drivers expect non-empty String parameters for the default catalog and the
		// default schema name!
		final String catalogFilter = catalogName.isEmpty() ? null : catalogName;
		final String schemaFilter = schemaName.isEmpty() ? null : schemaName;

		// Search for all tables!
		try (ResultSet rs = databaseMetaData.getColumns(catalogFilter, schemaFilter, "%", null)) {
			while (rs.next()) {
				final String tableName = DBNamingUtil.convertToStyle(rs.getString("TABLE_NAME"), db);

				final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
				col.setName(DBNamingUtil.convertToStyle(rs.getString("COLUMN_NAME"), db));
				col.setLength(rs.getInt("COLUMN_SIZE"));

				final int nullable = rs.getInt("NULLABLE");

				if (nullable == DatabaseMetaData.columnNullable)
					col.setNullable(true);

				final DBColumnType type = DbFactory.eINSTANCE.createDBColumnType();
				type.setName(rs.getString("TYPE_NAME"));

				col.setColumnType(type);

				if (tableMap.containsKey(tableName)) {
					final DBTable table = tableMap.get(tableName);
					table.getColumns().add(col);

					col.setDatabaseTable(table);
				}
				else {
					final DBTable table = DbFactory.eINSTANCE.createDBTable();
					table.setName(tableName);
					table.setCatalogName(catalogName);
					table.setSchemaName(schemaName);
					table.getColumns().add(col);
					table.setDatabase(targetDB);

					col.setDatabaseTable(table);
					tableMap.put(tableName, table);

					targetDB.getDatabaseTables().add(table);
				}
			}
		}

		return tableMap;
	}

	/**
	 * Compare the indexes
	 * @param itemTable
	 * @param targetTable
	 * @param metaTable
	 * @return true if no differences were found
	 */
	private boolean compareIndexes(TreeItem itemTable, DBTable targetTable, DBTable metaTable) {
		boolean clean = true;

		for (final DBIndex metaIndex : metaTable.getIndexes()) {
			final var indexItem = new TreeItem(itemTable, SWT.NONE);

			if (metaIndex.isUnique())
				indexItem.setText("Unique key: " + metaIndex.getConvertedName());
			else
				indexItem.setText("Index: " + metaIndex.getConvertedName());

			boolean keyExists = false;

			// Do not search for existing indexes if the target table is not set by the caller
			if (targetTable != null) {
				for (final DBIndex targetIndex : targetTable.getIndexes()) {
					if (targetIndex.getConvertedName().equals(metaIndex.getConvertedName())) {
						keyExists = true;

						// Check if both indexes have the same columns
						int counter = 0;

						for (final DBColumn metaCol : metaIndex.getColumns()) {
							for (final DBColumn targetCol : targetIndex.getColumns()) {
								if (metaCol.getConvertedName().equals(targetCol.getConvertedName())) {
									counter++;
									break;
								}
							}
						}

						if (counter == metaIndex.getColumns().size() && counter == targetIndex.getColumns().size()) {
							if (metaIndex.isUnique())
								indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY));
							else
								indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX));
						}
						else {
							if (metaIndex.isUnique()) {
								// Add a transaction to remove a unique key
								removeIndexTransactions
										.add(new DDLTransaction(transCount++, ddlService.removeUniqueKey(targetIndex), DDLTransactionType.REMOVE_UK));

								// Add a transaction to add a unique key
								addKeysTransactions
										.add(new DDLTransaction(transCount++, ddlService.addUniqueKey(metaIndex), DDLTransactionType.ADD_UK));

								indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_EDIT));
							}
							else {
								// Add a transaction to remove an index
								removeIndexTransactions
										.add(new DDLTransaction(transCount++, ddlService.removeIndex(targetIndex), DDLTransactionType.REMOVE_INDEX));

								// Add a transaction to add an index
								addKeysTransactions
										.add(new DDLTransaction(transCount++, ddlService.createIndex(metaIndex), DDLTransactionType.ADD_INDEX));

								indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX_ADD));
							}

							clean = false;
						}

						break;
					}
				}
			}

			if (!keyExists) {
				clean = false;

				if (metaIndex.isUnique()) {
					// Add a transaction to add a unique key
					addKeysTransactions
							.add(new DDLTransaction(transCount++, ddlService.addUniqueKey(metaIndex), DDLTransactionType.ADD_UK));

					// The unique key does not exist
					indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD));
				}
				else {
					// Add a transaction to add an index
					addKeysTransactions
							.add(new DDLTransaction(transCount++, ddlService.createIndex(metaIndex), DDLTransactionType.ADD_INDEX));

					// The index does not exist
					indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX_ADD));
				}
			}
		}

		// Do not search for indexes that should be removed if the target table is undefined
		if (targetTable == null)
			return clean;

		// Search for indexes to be removed
		for (final DBIndex targetIndex : targetTable.getIndexes()) {
			final boolean removeIndex = metaTable.getIndexes().stream()
					.noneMatch(metaIndex -> targetIndex.getConvertedName().equals(metaIndex.getConvertedName()));

			if (removeIndex) {
				clean = false;
				final var indexItem = new TreeItem(itemTable, SWT.NONE);

				if (targetIndex.isUnique()) {
					// Add a transaction to remove a unique key
					removeIndexTransactions
							.add(new DDLTransaction(transCount++, ddlService.removeUniqueKey(targetIndex), DDLTransactionType.REMOVE_UK));

					indexItem.setText("Unique key: " + targetIndex.getConvertedName());
					indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_DELETE));
				}
				else {
					// Add a transaction to remove an index
					removeIndexTransactions
							.add(new DDLTransaction(transCount++, ddlService.removeIndex(targetIndex), DDLTransactionType.REMOVE_INDEX));

					indexItem.setText("Index: " + targetIndex.getConvertedName());
					indexItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX_DELETE));
				}
			}
		}

		return clean;
	}

	/**
	 * Compare the foreign keys
	 * @param itemTable
	 * @param targetTable
	 * @param metaTable
	 * @return true if no differences were found
	 */
	private boolean compareForeignKeys(TreeItem itemTable, DBTable targetTable, DBTable metaTable) {
		boolean clean = true;

		for (final ForeignKey metaKey : metaTable.getForeignKeys()) {
			final var fkItem = new TreeItem(itemTable, SWT.NONE);
			fkItem.setText("Foreign key: " + metaKey.getConvertedName());

			boolean keyExists = false;

			// Do not search for existing keys if the target table is not set by the caller
			if (targetTable != null) {
				for (final ForeignKey targetKey : targetTable.getForeignKeys()) {
					if (targetKey.getConvertedName().equals(metaKey.getConvertedName())) {
						keyExists = true;

						// Check if both keys have the same columns
						if (!targetKey.getColumn().getConvertedName().equals(metaKey.getColumn().getConvertedName())) {
							// Add a transaction to remove a unique key
							removeIndexTransactions
									.add(new DDLTransaction(transCount++, ddlService.removeForeignKey(targetKey), DDLTransactionType.REMOVE_FK));

							// Add a transaction to add a unique key
							addKeysTransactions
									.add(new DDLTransaction(transCount++, ddlService.addForeignKey(metaKey), DDLTransactionType.ADD_FK));

							fkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_EDIT));
							clean = false;
						}
						else
							fkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY));

						break;
					}
				}
			}

			if (!keyExists) {
				// Add a transaction to add a foreign key
				addKeysTransactions.add(new DDLTransaction(transCount++, ddlService.addForeignKey(metaKey), DDLTransactionType.ADD_FK));

				// The foreign key does not exist
				fkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD));
				clean = false;
			}
		}

		// Do not search for foreign keys that should be removed if the target table is undefined
		if (targetTable == null)
			return clean;

		// Search for foreign keys to be removed
		for (final ForeignKey targetKey : targetTable.getForeignKeys()) {
			final boolean removeKey = metaTable.getForeignKeys().stream()
					.noneMatch(metaKey -> targetKey.getConvertedName().equals(metaKey.getConvertedName()));

			if (removeKey) {
				// Add a transaction to remove a unique key
				removeIndexTransactions
						.add(new DDLTransaction(transCount++, ddlService.removeForeignKey(targetKey), DDLTransactionType.REMOVE_FK));

				final var fkItem = new TreeItem(itemTable, SWT.NONE);
				fkItem.setText("Foreign key: " + targetKey.getConvertedName());
				fkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_DELETE));

				clean = false;
			}
		}

		return clean;
	}

	/**
	 * Compare the primary keys
	 * @param itemTable
	 * @param targetTable
	 * @param metaTable
	 * @return true if no DDL transaction was added
	 */
	private boolean comparePrimaryKey(TreeItem itemTable, DBTable targetTable, DBTable metaTable) {
		if (targetTable == null) {
			if (metaTable.getPrimaryKey() == null)
				return true;

			final String ddl = ddlService.createPrimaryKey(metaTable);

			if (ddl != null)
				addPKsTransactions.add(new DDLTransaction(transCount++, ddl, DDLTransactionType.ADD_PK));

			// Add the primary key
			final var pkItem = new TreeItem(itemTable, SWT.NONE);
			pkItem.setText("Primary key: " + metaTable.getPrimaryKey().getConvertedName());
			pkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD));

			return false;
		}

		if (targetTable.getPrimaryKey() == null) {
			if (metaTable.getPrimaryKey() == null)
				return true;

			final String ddl = ddlService.createPrimaryKey(metaTable);

			if (ddl != null)
				addPKsTransactions.add(new DDLTransaction(transCount++, ddl, DDLTransactionType.ADD_PK));

			// Add the primary key
			final var pkItem = new TreeItem(itemTable, SWT.NONE);
			pkItem.setText("Primary key: " + metaTable.getPrimaryKey().getConvertedName());
			pkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD));

			return false;
		}

		if (metaTable.getPrimaryKey() != null) {
			// Check if both primary keys have the same column!
			if (targetTable.getPrimaryKey().getColumn().getConvertedName()
					.equals(metaTable.getPrimaryKey().getColumn().getConvertedName())) {
				final var pkItem = new TreeItem(itemTable, SWT.NONE);
				pkItem.setText("Primary key: " + metaTable.getPrimaryKey().getConvertedName());
				pkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY));

				return true;
			}

			String ddl = ddlService.dropPrimaryKey(targetTable);

			if (ddl != null)
				removePKsTransactions.add(new DDLTransaction(transCount++, ddl, DDLTransactionType.DROP_PK));

			ddl = ddlService.createPrimaryKey(metaTable);

			if (ddl != null)
				addPKsTransactions.add(new DDLTransaction(transCount++, ddl, DDLTransactionType.ADD_PK));

			// Add the primary key
			final var pkItem = new TreeItem(itemTable, SWT.NONE);
			pkItem.setText("Primary key: " + targetTable.getPrimaryKey().getConvertedName());
			pkItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_DELETE));

			return false;
		}

		return true;
	}

	/**
	 * Compare the database sequences
	 * @param metaTable
	 * @param existingSequences
	 * @return true if no DDL transaction was added
	 */
	private boolean compareSequences(DBTable metaTable, Set<String> existingSequences) {
		if (metaTable == null)
			return true;

		final Collection<DomainObject> allDomainObjects = db.getProject().getAllDomainObjectsOfProject(true, true);
		final DomainObject domainObject = allDomainObjects.stream()
				.filter(e -> e.getDatabaseTable() != null && e.getDatabaseTable().getConvertedName().equals(metaTable.getConvertedName()))
				.findFirst().orElse(null);

		if (domainObject == null || domainObject.getParent() != null)
			return true;

		if (domainObject.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.SEQUENCE) {
			if (existingSequences.contains(DBNamingUtil.convertToStyle(domainObject.getIDGenerator().getName(), db)))
				return true;

			String sequenceName = DBNamingUtil.convertToDatabase(domainObject.getIDGenerator().getName(), db);
			final String schemaName = db.getSchemaName() == null || db.getSchemaName().isEmpty() ? ""
					: DBNamingUtil.convertToDatabase(db.getSchemaName(), db);
			final String catalogName = db.getCatalogName() == null || db.getCatalogName().isEmpty() ? ""
					: DBNamingUtil.convertToDatabase(db.getSchemaName(), db);
			var tableSchemaName = "";
			var tableCatalogName = "";

			if (domainObject.getDatabaseTable() != null) {
				// We assume that a sequence is in the same schema and catalog as the corresponding table!
				final DBTable table = domainObject.getDatabaseTable();

				tableSchemaName = table.getSchemaName() == null || table.getSchemaName().isEmpty() ? ""
						: DBNamingUtil.convertToDatabase(table.getSchemaName(), db);
				tableCatalogName = table.getCatalogName() == null || table.getCatalogName().isEmpty() ? ""
						: DBNamingUtil.convertToDatabase(table.getCatalogName(), db);
			}

			if (!tableSchemaName.isEmpty() || !tableCatalogName.isEmpty()) {
				if (!tableSchemaName.isEmpty())
					sequenceName = tableSchemaName + "." + sequenceName;
				else
					sequenceName = tableCatalogName + "." + sequenceName;
			}
			else {
				if (!catalogName.isEmpty())
					sequenceName = catalogName + "." + sequenceName;

				if (!schemaName.isEmpty())
					sequenceName = schemaName + "." + sequenceName;
			}

			final int startIndex = domainObject.getIDGenerator().getInitialValue();
			final int blockSize = domainObject.getIDGenerator().getBlockSize();

			final String ddl = ddlService.createSequence(sequenceName, startIndex, blockSize);

			if (ddl != null) {
				addSequenceTransactions.add(new DDLTransaction(transCount++, ddl, DDLTransactionType.ADD_SEQUENCE));
				return false;
			}
		}

		return true;
	}

	/**
	 * Compare the columns of the target and the meta-data table
	 * @param itemTable the table tree item
	 * @param targetTable the target table
	 * @param metaTable the meta-data table
	 * @return true if no differences were found
	 */
	private boolean compareColumns(TreeItem itemTable, DBTable targetTable, DBTable metaTable) {
		boolean clean = true;

		for (final DBColumn metaCol : metaTable.getColumns()) {
			boolean colFoundInDatabase = false;

			for (final DBColumn targetCol : targetTable.getColumns()) {
				if (metaCol.getConvertedName().equals(targetCol.getConvertedName())) {
					colFoundInDatabase = true;
					final var colItem = new TreeItem(itemTable, SWT.NONE);

					// We don't check the column type!
					if (metaCol.getLength() == 0) {
						colItem.setText("Column: " + metaCol.getConvertedName() + " " + metaCol.getColumnType().getName());

						if (metaCol.isNullable() == targetCol.isNullable())
							colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN));
						else {
							// Add a transaction to modify the column
							addColsTransactions
									.add(new DDLTransaction(transCount++, ddlService.modifyColumn(metaCol), DDLTransactionType.MOD_COLUMN));

							clean = false;
							colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_EDIT));
						}
					}
					else {
						colItem.setText("Column: " + metaCol.getConvertedName() + " " + metaCol.getColumnType().getName() + "("
								+ metaCol.getLength() + ")");

						if (metaCol.getLength() == targetCol.getLength() && metaCol.isNullable() == targetCol.isNullable())
							colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN));
						else {
							// Add a transaction to modify the column
							addColsTransactions
									.add(new DDLTransaction(transCount++, ddlService.modifyColumn(metaCol), DDLTransactionType.MOD_COLUMN));

							clean = false;
							colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_EDIT));
						}
					}
				}
			}

			if (!colFoundInDatabase) {
				// Add a transaction to add a column
				addColsTransactions.add(new DDLTransaction(transCount++, ddlService.addColumn(metaCol), DDLTransactionType.ADD_COLUMN));

				clean = false;
				final var colItem = new TreeItem(itemTable, SWT.NONE);

				if (metaCol.getLength() == 0)
					colItem.setText("Column: " + metaCol.getConvertedName() + " " + metaCol.getColumnType().getName());
				else
					colItem.setText("Column: " + metaCol.getConvertedName() + " " + metaCol.getColumnType().getName() + "("
							+ metaCol.getLength() + ")");

				colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_ADD));
			}
		}

		// Search for columns that should be removed
		for (final DBColumn targetCol : targetTable.getColumns()) {
			final boolean colExistsInDatabase = metaTable.getColumns().stream()
					.anyMatch(metaCol -> metaCol.getConvertedName().equals(targetCol.getConvertedName()));

			if (!colExistsInDatabase) {
				// Add a transaction to remove the column
				removeColsTransactions
						.add(new DDLTransaction(transCount++, ddlService.removeColumn(targetCol), DDLTransactionType.REMOVE_COLUMN));

				clean = false;

				// Mark the column to be removed from the database table
				final var colItem = new TreeItem(itemTable, SWT.NONE);

				if (targetCol.getLength() == 0)
					colItem.setText("Column: " + targetCol.getConvertedName() + " " + targetCol.getColumnType().getName());
				else
					colItem.setText("Column: " + targetCol.getConvertedName() + " " + targetCol.getColumnType().getName() + "("
							+ targetCol.getLength() + ")");

				colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_DELETE));
			}
		}

		return clean;
	}

	/**
	 * Run the given DDL statement
	 * @param ddl
	 * @throw RuntimeException if the operation has failed
	 */
	public void executeDDL(String ddl) {
		try (var dbManager = new DBManager(ds); Connection con = dbManager.getConnection(); Statement st = con.createStatement()) {
			// Execute the SQL statement
			st.executeUpdate(ddl);
		}
		catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Compare the meta-data and the target database
	 * @param shell
	 * @param monitor the progress monitor
	 * @param tree the tree view
	 * @return a list with all DDL transactions to synchronize the meta-data model with the target database
	 */
	public List<DDLTransaction> compareDataBase(final Shell shell, final IProgressMonitor monitor, final Tree tree) {
		final Set<String> existingSequences;
		final Database targetDB;

		// Initialize the transaction counter
		transCount = 1000;

		// Initialize lists the transactions should be added to
		final var allTransactions = new ArrayList<DDLTransaction>();
		addKeysTransactions = new ArrayList<>();
		removeIndexTransactions = new ArrayList<>();
		addColsTransactions = new ArrayList<>();
		removeColsTransactions = new ArrayList<>();
		addPKsTransactions = new ArrayList<>();
		removePKsTransactions = new ArrayList<>();
		addSequenceTransactions = new ArrayList<>();

		// Initialize the service for creating DDL statements
		ddlService = DDLServiceFactory.getService(db.getVendorGroup());

		// Create the database service and take a snap-shot
		try {
			targetDB = getDatabaseSnapShot();

			existingSequences = getAllSequences();
		}
		catch (final Exception e) {
			shell.getDisplay().syncExec(() -> CodeCadenzaToolsPlugin.getInstance().handleInternalError(e));

			return Collections.emptyList();
		}

		// Begin new task. It makes no sense to define the number of work steps as most of the time is consumed by
		// getDatabaseSnapShot()!
		monitor.beginTask("Analyse table structure...", IProgressMonitor.UNKNOWN);

		// Search for differences
		for (final DBTable metaTable : db.getDatabaseTables()) {
			final Collection<DomainObject> allDomainObjects = db.getProject().getAllDomainObjectsOfProject(true, true);

			shell.getDisplay().syncExec(() -> {
				boolean tableFoundInDatabase = false;
				boolean analyzeTable = true;
				boolean columnsClean = false;
				boolean pkClean = false;
				boolean ukClean = false;
				boolean fkClean = false;

				for (final DBTable targetTable : targetDB.getDatabaseTables()) {
					if (!targetTable.getConvertedName().equals(metaTable.getConvertedName()))
						continue;

					// Check if the table belongs to a mapped superclass
					for (final DomainObject domainObj : allDomainObjects)
						if (domainObj.isMappedSuperClass() && domainObj.getDatabaseTable().equals(metaTable)) {
							// If the table belongs to a mapped superclass we must omit further table operations!
							analyzeTable = false;
							break;
						}

					try {
						// Sequences must be tested in any case!
						compareSequences(metaTable, existingSequences);

						if (!analyzeTable)
							break;

						final var tableItem = new TreeItem(tree, SWT.NONE);
						tableItem.setText(metaTable.getConvertedName());

						tableFoundInDatabase = true;

						// Compare the database columns
						columnsClean = compareColumns(tableItem, targetTable, metaTable);

						// Compare the primary keys
						pkClean = comparePrimaryKey(tableItem, targetTable, metaTable);

						// Compare the indexes
						ukClean = compareIndexes(tableItem, targetTable, metaTable);

						// Compare the foreign keys
						fkClean = compareForeignKeys(tableItem, targetTable, metaTable);

						if (columnsClean && pkClean && ukClean && fkClean)
							tableItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE));
						else
							tableItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE_REPLACE));
					}
					catch (final Exception e) {
						CodeCadenzaToolsPlugin.getInstance().logError(e);
					}
				}

				if (tableFoundInDatabase)
					return;

				try {
					// Sequences must be tested in any case!
					compareSequences(metaTable, existingSequences);

					// Check if the table belongs to a mapped superclass
					for (final DomainObject domainObj : allDomainObjects)
						if (domainObj.isMappedSuperClass() && domainObj.getDatabaseTable().equals(metaTable))
							return;

					// Add a transaction to create a table
					allTransactions
							.add(new DDLTransaction(transCount++, ddlService.createTable(metaTable), DDLTransactionType.CREATE_TABLE));

					// Add a table to indicate that it does not exist in the target database
					final var tableNewItem = new TreeItem(tree, SWT.NONE);
					tableNewItem.setText(metaTable.getConvertedName());
					tableNewItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE_NEW));

					// Compare the primary keys
					comparePrimaryKey(tableNewItem, null, metaTable);

					// Compare the indexes
					compareIndexes(tableNewItem, null, metaTable);

					// Compare the foreign keys
					compareForeignKeys(tableNewItem, null, metaTable);
				}
				catch (final Exception e) {
					CodeCadenzaToolsPlugin.getInstance().logError(e);
				}
			});
		}

		// Add all transactions
		allTransactions.addAll(removeIndexTransactions);
		allTransactions.addAll(removePKsTransactions);
		allTransactions.addAll(addColsTransactions);
		allTransactions.addAll(addPKsTransactions);
		allTransactions.addAll(addKeysTransactions);
		allTransactions.addAll(removeColsTransactions);
		allTransactions.addAll(addSequenceTransactions);

		return allTransactions;
	}

}
