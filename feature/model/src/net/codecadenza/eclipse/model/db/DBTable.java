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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>DB Table</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getDatabase <em>Database</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getColumns <em>Columns</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey <em>Primary Key</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getForeignKeys <em>Foreign Keys</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getIndexes <em>Indexes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getSchemaName <em>Schema Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBTable#getCatalogName <em>Catalog Name</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable()
 * @model
 * @generated
 */
public interface DBTable extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBTable#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Database</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.Database#getDatabaseTables <em>Database Tables</em>} '.
	 * @return the value of the '<em>Database</em>' reference
	 * @see #setDatabase(Database)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Database()
	 * @see net.codecadenza.eclipse.model.db.Database#getDatabaseTables
	 * @model opposite="databaseTables"
	 * @generated
	 */
	Database getDatabase();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBTable#getDatabase <em>Database</em>}' reference
	 * @param value the new value of the '<em>Database</em>' reference
	 * @see #getDatabase()
	 * @generated
	 */
	void setDatabase(Database value);

	/**
	 * Return the value of the '<em><b>Columns</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.db.DBColumn}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable <em>Database Table</em>}'.
	 * @return the value of the '<em>Columns</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Columns()
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable
	 * @model opposite="databaseTable" containment="true"
	 * @generated
	 */
	EList<DBColumn> getColumns();

	/**
	 * Return the value of the '<em><b>Primary Key</b></em>' containment reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getTable <em>Table</em>}'.
	 * @return the value of the '<em>Primary Key</em>' containment reference
	 * @see #setPrimaryKey(PrimaryKey)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_PrimaryKey()
	 * @see net.codecadenza.eclipse.model.db.PrimaryKey#getTable
	 * @model opposite="table" containment="true"
	 * @generated
	 */
	PrimaryKey getPrimaryKey();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey <em>Primary Key</em>}' containment
	 * reference
	 * @param value the new value of the '<em>Primary Key</em>' containment reference
	 * @see #getPrimaryKey()
	 * @generated
	 */
	void setPrimaryKey(PrimaryKey value);

	/**
	 * Return the value of the '<em><b>Foreign Keys</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.db.ForeignKey}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.ForeignKey#getTable <em>Table</em>}'.
	 * @return the value of the '<em>Foreign Keys</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_ForeignKeys()
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getTable
	 * @model opposite="table" containment="true"
	 * @generated
	 */
	EList<ForeignKey> getForeignKeys();

	/**
	 * Return the value of the '<em><b>Indexes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.db.DBIndex}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.DBIndex#getTable <em>Table</em>}'.
	 * @return the value of the '<em>Indexes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Indexes()
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getTable
	 * @model opposite="table" containment="true"
	 * @generated
	 */
	EList<DBIndex> getIndexes();

	/**
	 * Return the value of the '<em><b>Schema Name</b></em>' attribute
	 * @return the value of the '<em>Schema Name</em>' attribute
	 * @see #setSchemaName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_SchemaName()
	 * @model
	 * @generated
	 */
	String getSchemaName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBTable#getSchemaName <em>Schema Name</em>}' attribute
	 * @param value the new value of the '<em>Schema Name</em>' attribute
	 * @see #getSchemaName()
	 * @generated
	 */
	void setSchemaName(String value);

	/**
	 * Return the value of the '<em><b>Catalog Name</b></em>' attribute
	 * @return the value of the '<em>Catalog Name</em>' attribute
	 * @see #setCatalogName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_CatalogName()
	 * @model
	 * @generated
	 */
	String getCatalogName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBTable#getCatalogName <em>Catalog Name</em>}' attribute
	 * @param value the new value of the '<em>Catalog Name</em>' attribute
	 * @see #getCatalogName()
	 * @generated
	 */
	void setCatalogName(String value);

	/**
	 * @model transient="true" changeable="false"
	 * @return the short table name
	 */
	String getShortTableName();

	/**
	 * @return the converted name due to the selected identifier style of the target database
	 * @generated not
	 */
	String getConvertedName();

	/**
	 * @return the name used for DDL operations
	 * @generated not
	 */
	String getDatabaseName();

	/**
	 * @return the name used for DDL operations including the specific schema or catalog name
	 * @generated not
	 */
	String getFullDatabaseName();

	/**
	 * @return the name used for JPA mappings
	 * @generated not
	 */
	String getMappingName();

	/**
	 * Create a copy of this table and add it to the given database
	 * @param database
	 * @return a copy of the original table
	 * @generated not
	 */
	DBTable copyTableToDatabase(Database database);

	/**
	 * Add a copy of the column to this table
	 * @param column
	 * @return the new column or null if the table already contains a column with the same name
	 * @generated not
	 */
	DBColumn addColumnCopy(DBColumn column);

	/**
	 * Copy all foreign keys of the source table to this table
	 * @param sourceTable
	 * @generated not
	 */
	void addForeignKeyCopies(DBTable sourceTable);

	/**
	 * Copy all foreign keys of the source table to this table
	 * @param sourceTable
	 * @param strictMode
	 * @generated not
	 */
	void addForeignKeyCopies(DBTable sourceTable, boolean strictMode);

	/**
	 * Add a copy of all indexes of the source table to this table
	 * @param sourceTable
	 * @generated not
	 */
	void addIndexCopies(DBTable sourceTable);

	/**
	 * @param convertedName
	 * @return the column or null if the table doesn't contain a column with the specified name
	 * @generated not
	 */
	DBColumn getColumnByConvertedName(String convertedName);
}
