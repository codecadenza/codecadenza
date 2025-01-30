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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>DB Column</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#getLength <em>Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#isNullable <em>Nullable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#getPrecision <em>Precision</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#getScale <em>Scale</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable <em>Database Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumn#getColumnType <em>Column Type</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn()
 * @model
 * @generated
 */
public interface DBColumn extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Length</b></em>' attribute
	 * @return the value of the '<em>Length</em>' attribute
	 * @see #setLength(int)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Length()
	 * @model
	 * @generated
	 */
	int getLength();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#getLength <em>Length</em>}' attribute
	 * @param value the new value of the '<em>Length</em>' attribute
	 * @see #getLength()
	 * @generated
	 */
	void setLength(int value);

	/**
	 * Return the value of the '<em><b>Nullable</b></em>' attribute
	 * @return the value of the '<em>Nullable</em>' attribute
	 * @see #setNullable(boolean)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Nullable()
	 * @model
	 * @generated
	 */
	boolean isNullable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#isNullable <em>Nullable</em>}' attribute
	 * @param value the new value of the '<em>Nullable</em>' attribute
	 * @see #isNullable()
	 * @generated
	 */
	void setNullable(boolean value);

	/**
	 * Return the value of the '<em><b>Precision</b></em>' attribute
	 * @return the value of the '<em>Precision</em>' attribute
	 * @see #setPrecision(int)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Precision()
	 * @model
	 * @generated
	 */
	int getPrecision();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#getPrecision <em>Precision</em>}' attribute
	 * @param value the new value of the '<em>Precision</em>' attribute
	 * @see #getPrecision()
	 * @generated
	 */
	void setPrecision(int value);

	/**
	 * Return the value of the '<em><b>Scale</b></em>' attribute
	 * @return the value of the '<em>Scale</em>' attribute
	 * @see #setScale(int)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Scale()
	 * @model
	 * @generated
	 */
	int getScale();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#getScale <em>Scale</em>}' attribute
	 * @param value the new value of the '<em>Scale</em>' attribute
	 * @see #getScale()
	 * @generated
	 */
	void setScale(int value);

	/**
	 * Return the value of the '<em><b>Database Table</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.DBTable#getColumns <em>Columns</em>} '.
	 * @return the value of the '<em>Database Table</em>' container reference
	 * @see #setDatabaseTable(DBTable)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_DatabaseTable()
	 * @see net.codecadenza.eclipse.model.db.DBTable#getColumns
	 * @model opposite="columns"
	 * @generated
	 */
	DBTable getDatabaseTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable <em>Database Table</em>}' container
	 * reference
	 * @param value the new value of the '<em>Database Table</em>' container reference
	 * @see #getDatabaseTable()
	 * @generated
	 */
	void setDatabaseTable(DBTable value);

	/**
	 * Return the value of the '<em><b>Column Type</b></em>' reference
	 * @return the value of the '<em>Column Type</em>' reference
	 * @see #setColumnType(DBColumnType)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_ColumnType()
	 * @model
	 * @generated
	 */
	DBColumnType getColumnType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumn#getColumnType <em>Column Type</em>}' reference
	 * @param value the new value of the '<em>Column Type</em>' reference
	 * @see #getColumnType()
	 * @generated
	 */
	void setColumnType(DBColumnType value);

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
	 * @return the name used for JPA mappings
	 * @generated not
	 */
	String getMappingName();

	/**
	 * Add a foreign key to this column and add an index if the database doesn't create a backing index automatically
	 * @param targetColumn the target column of the foreign key
	 * @param skipBackingIndex a flag that controls whether to skip creation of a backing index
	 * @generated not
	 */
	void addForeignKey(DBColumn targetColumn, boolean skipBackingIndex);

}
