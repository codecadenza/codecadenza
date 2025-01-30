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
 * A representation of the model object '<em><b>DB Index</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.DBIndex#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBIndex#getTable <em>Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBIndex#isUnique <em>Unique</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBIndex#getColumns <em>Columns</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex()
 * @model
 * @generated
 */
public interface DBIndex extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBIndex#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Table</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.DBTable#getIndexes <em>Indexes</em>}'.
	 * @return the value of the '<em>Table</em>' container reference
	 * @see #setTable(DBTable)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Table()
	 * @see net.codecadenza.eclipse.model.db.DBTable#getIndexes
	 * @model opposite="indexes"
	 * @generated
	 */
	DBTable getTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBIndex#getTable <em>Table</em>}' container reference
	 * @param value the new value of the '<em>Table</em>' container reference
	 * @see #getTable()
	 * @generated
	 */
	void setTable(DBTable value);

	/**
	 * Return the value of the '<em><b>Unique</b></em>' attribute
	 * @return the value of the '<em>Unique</em>' attribute
	 * @see #setUnique(boolean)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Unique()
	 * @model
	 * @generated
	 */
	boolean isUnique();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBIndex#isUnique <em>Unique</em>}' attribute
	 * @param value the new value of the '<em>Unique</em>' attribute
	 * @see #isUnique()
	 * @generated
	 */
	void setUnique(boolean value);

	/**
	 * Return the value of the '<em><b>Columns</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.db.DBColumn}.
	 * @return the value of the '<em>Columns</em>' reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Columns()
	 * @model
	 * @generated
	 */
	EList<DBColumn> getColumns();

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

}
