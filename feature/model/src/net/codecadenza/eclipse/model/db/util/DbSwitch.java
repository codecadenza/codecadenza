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
package net.codecadenza.eclipse.model.db.util;

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.db.DbPackage
 * @generated
 */
public class DbSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static DbPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public DbSwitch() {
		if (modelPackage == null)
			modelPackage = DbPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#isSwitchFor(org.eclipse.emf.ecore.EPackage)
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#doSwitch(int, org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case DbPackage.DB_COLUMN: {
				final var dbColumn = (DBColumn) theEObject;
				T result = caseDBColumn(dbColumn);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DbPackage.DB_COLUMN_TYPE: {
				final var dbColumnType = (DBColumnType) theEObject;
				T result = caseDBColumnType(dbColumnType);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DbPackage.DB_INDEX: {
				final var dbIndex = (DBIndex) theEObject;
				T result = caseDBIndex(dbIndex);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DbPackage.DB_TABLE: {
				final var dbTable = (DBTable) theEObject;
				T result = caseDBTable(dbTable);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DbPackage.DATABASE: {
				final var database = (Database) theEObject;
				T result = caseDatabase(database);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DbPackage.FOREIGN_KEY: {
				final var foreignKey = (ForeignKey) theEObject;
				T result = caseForeignKey(foreignKey);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DbPackage.PRIMARY_KEY: {
				final var primaryKey = (PrimaryKey) theEObject;
				T result = casePrimaryKey(primaryKey);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>DB Column</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>DB Column</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDBColumn(DBColumn object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>DB Column Type</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>DB Column Type</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDBColumnType(DBColumnType object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>DB Index</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>DB Index</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDBIndex(DBIndex object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>DB Table</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>DB Table</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDBTable(DBTable object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Database</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Database</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDatabase(Database object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Foreign Key</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Foreign Key</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseForeignKey(ForeignKey object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Primary Key</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Primary Key</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T casePrimaryKey(PrimaryKey object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#defaultCase(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

}
