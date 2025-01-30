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
 * A representation of the model object '<em><b>Primary Key</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.PrimaryKey#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.PrimaryKey#getTable <em>Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.PrimaryKey#getColumn <em>Column</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey()
 * @model
 * @generated
 */
public interface PrimaryKey extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Table</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey <em>Primary Key</em>}'.
	 * @return the value of the '<em>Table</em>' container reference
	 * @see #setTable(DBTable)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey_Table()
	 * @see net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey
	 * @model opposite="primaryKey"
	 * @generated
	 */
	DBTable getTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getTable <em>Table</em>}' container reference
	 * @param value the new value of the '<em>Table</em>' container reference
	 * @see #getTable()
	 * @generated
	 */
	void setTable(DBTable value);

	/**
	 * Return the value of the '<em><b>Column</b></em>' reference
	 * @return the value of the '<em>Column</em>' reference
	 * @see #setColumn(DBColumn)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey_Column()
	 * @model
	 * @generated
	 */
	DBColumn getColumn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getColumn <em>Column</em>}' reference
	 * @param value the new value of the '<em>Column</em>' reference
	 * @see #getColumn()
	 * @generated
	 */
	void setColumn(DBColumn value);

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

}
