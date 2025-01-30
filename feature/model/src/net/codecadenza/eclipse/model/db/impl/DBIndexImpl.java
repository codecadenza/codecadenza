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
package net.codecadenza.eclipse.model.db.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>DB Index</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBIndexImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBIndexImpl#getTable <em>Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBIndexImpl#isUnique <em>Unique</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBIndexImpl#getColumns <em>Columns</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DBIndexImpl extends EObjectImpl implements DBIndex {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #isUnique() <em>Unique</em>}' attribute
	 * @see #isUnique()
	 * @generated
	 * @ordered
	 */
	protected static final boolean UNIQUE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUnique() <em>Unique</em>}' attribute
	 * @see #isUnique()
	 * @generated
	 * @ordered
	 */
	protected boolean unique = UNIQUE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getColumns() <em>Columns</em>}' reference list
	 * @see #getColumns()
	 * @generated
	 * @ordered
	 */
	protected EList<DBColumn> columns;

	/**
	 * @generated
	 */
	protected DBIndexImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DbPackage.Literals.DB_INDEX;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_INDEX__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getTable()
	 * @generated
	 */
	@Override
	public DBTable getTable() {
		if (eContainerFeatureID() != DbPackage.DB_INDEX__TABLE)
			return null;

		return (DBTable) eInternalContainer();
	}

	/**
	 * @param newTable
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetTable(DBTable newTable, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newTable, DbPackage.DB_INDEX__TABLE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#setTable(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated
	 */
	@Override
	public void setTable(DBTable newTable) {
		if (newTable != eInternalContainer() || (eContainerFeatureID() != DbPackage.DB_INDEX__TABLE && newTable != null)) {
			if (EcoreUtil.isAncestor(this, newTable))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newTable != null)
				msgs = ((InternalEObject) newTable).eInverseAdd(this, DbPackage.DB_TABLE__INDEXES, DBTable.class, msgs);

			msgs = basicSetTable(newTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_INDEX__TABLE, newTable, newTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#isUnique()
	 * @generated
	 */
	@Override
	public boolean isUnique() {
		return unique;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#setUnique(boolean)
	 * @generated
	 */
	@Override
	public void setUnique(boolean newUnique) {
		final boolean oldUnique = unique;
		unique = newUnique;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_INDEX__UNIQUE, oldUnique, unique));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getColumns()
	 * @generated
	 */
	@Override
	public EList<DBColumn> getColumns() {
		if (columns == null)
			columns = new EObjectResolvingEList<>(DBColumn.class, this, DbPackage.DB_INDEX__COLUMNS);

		return columns;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DbPackage.DB_INDEX__TABLE:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetTable((DBTable) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DbPackage.DB_INDEX__TABLE:
				return basicSetTable(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case DbPackage.DB_INDEX__TABLE:
				return eInternalContainer().eInverseRemove(this, DbPackage.DB_TABLE__INDEXES, DBTable.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DbPackage.DB_INDEX__NAME:
				return getName();
			case DbPackage.DB_INDEX__TABLE:
				return getTable();
			case DbPackage.DB_INDEX__UNIQUE:
				return isUnique();
			case DbPackage.DB_INDEX__COLUMNS:
				return getColumns();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DbPackage.DB_INDEX__NAME:
				setName((String) newValue);
				return;
			case DbPackage.DB_INDEX__TABLE:
				setTable((DBTable) newValue);
				return;
			case DbPackage.DB_INDEX__UNIQUE:
				setUnique((Boolean) newValue);
				return;
			case DbPackage.DB_INDEX__COLUMNS:
				getColumns().clear();
				getColumns().addAll((Collection<? extends DBColumn>) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DbPackage.DB_INDEX__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DbPackage.DB_INDEX__TABLE:
				setTable((DBTable) null);
				return;
			case DbPackage.DB_INDEX__UNIQUE:
				setUnique(UNIQUE_EDEFAULT);
				return;
			case DbPackage.DB_INDEX__COLUMNS:
				getColumns().clear();
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DbPackage.DB_INDEX__NAME:
				return name != null;
			case DbPackage.DB_INDEX__TABLE:
				return getTable() != null;
			case DbPackage.DB_INDEX__UNIQUE:
				return unique != UNIQUE_EDEFAULT;
			case DbPackage.DB_INDEX__COLUMNS:
				return columns != null && !columns.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", unique: ");
		result.append(unique);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getConvertedName()
	 * @generated not
	 */
	@Override
	public String getConvertedName() {
		Database database = null;

		if (getTable() != null)
			database = getTable().getDatabase();

		return DBNamingUtil.convertToStyle(name, database);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getDatabaseName()
	 * @generated not
	 */
	@Override
	public String getDatabaseName() {
		Database database = null;

		if (getTable() != null)
			database = getTable().getDatabase();

		return DBNamingUtil.convertToDatabase(name, database);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getFullDatabaseName()
	 * @generated not
	 */
	@Override
	public String getFullDatabaseName() {
		if (getTable() == null || getTable().getDatabase() == null)
			return name;

		final DBTable table = getTable();
		final Database database = table.getDatabase();
		final var defaultSchema = database.getSchemaName() == null ? "" : database.getSchemaName();
		final var defaultCatalog = database.getCatalogName() == null ? "" : database.getCatalogName();
		final var tableSchema = table.getSchemaName() == null ? "" : table.getSchemaName();
		final var tableCatalog = table.getCatalogName() == null ? "" : table.getCatalogName();

		// We don't cover cases where schema and catalog are specified!
		if (!tableSchema.isEmpty())
			return DBNamingUtil.convertToDatabase(tableSchema, database) + "." + getDatabaseName();

		if (!tableCatalog.isEmpty())
			return DBNamingUtil.convertToDatabase(tableCatalog, database) + "." + getDatabaseName();

		if (!defaultSchema.isEmpty())
			return DBNamingUtil.convertToDatabase(defaultSchema, database) + "." + getDatabaseName();

		if (!defaultCatalog.isEmpty())
			return DBNamingUtil.convertToDatabase(defaultCatalog, database) + "." + getDatabaseName();

		return getDatabaseName();
	}

}
