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
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.db.DbPackage
 * @generated
 */
public class DbAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static DbPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public DbAdapterFactory() {
		if (modelPackage == null)
			modelPackage = DbPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#isFactoryForType(java.lang.Object)
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage)
			return true;

		if (object instanceof final EObject eObject)
			return eObject.eClass().getEPackage() == modelPackage;

		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * @generated
	 */
	protected DbSwitch<Adapter> modelSwitch = new DbSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#caseDBColumn(net.codecadenza.eclipse.model.db.DBColumn)
		 */
		@Override
		public Adapter caseDBColumn(DBColumn object) {
			return createDBColumnAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#caseDBColumnType(net.codecadenza.eclipse.model.db.DBColumnType)
		 */
		@Override
		public Adapter caseDBColumnType(DBColumnType object) {
			return createDBColumnTypeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#caseDBIndex(net.codecadenza.eclipse.model.db.DBIndex)
		 */
		@Override
		public Adapter caseDBIndex(DBIndex object) {
			return createDBIndexAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#caseDBTable(net.codecadenza.eclipse.model.db.DBTable)
		 */
		@Override
		public Adapter caseDBTable(DBTable object) {
			return createDBTableAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#caseDatabase(net.codecadenza.eclipse.model.db.Database)
		 */
		@Override
		public Adapter caseDatabase(Database object) {
			return createDatabaseAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#caseForeignKey(net.codecadenza.eclipse.model.db.ForeignKey)
		 */
		@Override
		public Adapter caseForeignKey(ForeignKey object) {
			return createForeignKeyAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#casePrimaryKey(net.codecadenza.eclipse.model.db.PrimaryKey)
		 */
		@Override
		public Adapter casePrimaryKey(PrimaryKey object) {
			return createPrimaryKeyAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.db.util.DbSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public Adapter defaultCase(EObject object) {
			return createEObjectAdapter();
		}
	};

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#createAdapter(org.eclipse.emf.common.notify.Notifier)
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.DBColumn <em>DB Column</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.DBColumn
	 * @generated
	 */
	public Adapter createDBColumnAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.DBColumnType <em>DB Column Type</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.DBColumnType
	 * @generated
	 */
	public Adapter createDBColumnTypeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.DBIndex <em>DB Index</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.DBIndex
	 * @generated
	 */
	public Adapter createDBIndexAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.DBTable <em>DB Table</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.DBTable
	 * @generated
	 */
	public Adapter createDBTableAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.Database <em>Database</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.Database
	 * @generated
	 */
	public Adapter createDatabaseAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.ForeignKey <em>Foreign Key</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.ForeignKey
	 * @generated
	 */
	public Adapter createForeignKeyAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.db.PrimaryKey <em>Primary Key</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.db.PrimaryKey
	 * @generated
	 */
	public Adapter createPrimaryKeyAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for the default case. This default implementation returns null.
	 * @return the new adapter
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

}
