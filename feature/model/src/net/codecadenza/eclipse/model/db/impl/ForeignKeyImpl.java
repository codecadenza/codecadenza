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

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.ForeignKey;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Foreign Key</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl#getTable <em>Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl#getColumn <em>Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl#getReferencedColumn <em>Referenced Column</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class ForeignKeyImpl extends EObjectImpl implements ForeignKey {
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
	 * The cached value of the '{@link #getColumn() <em>Column</em>}' reference
	 * @see #getColumn()
	 * @generated
	 * @ordered
	 */
	protected DBColumn column;

	/**
	 * The cached value of the '{@link #getReferencedColumn() <em>Referenced Column</em>}' reference
	 * @see #getReferencedColumn()
	 * @generated
	 * @ordered
	 */
	protected DBColumn referencedColumn;

	/**
	 * @generated
	 */
	protected ForeignKeyImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DbPackage.Literals.FOREIGN_KEY;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.FOREIGN_KEY__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getTable()
	 * @generated
	 */
	@Override
	public DBTable getTable() {
		if (eContainerFeatureID() != DbPackage.FOREIGN_KEY__TABLE)
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
		msgs = eBasicSetContainer((InternalEObject) newTable, DbPackage.FOREIGN_KEY__TABLE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#setTable(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated
	 */
	@Override
	public void setTable(DBTable newTable) {
		if (newTable != eInternalContainer() || (eContainerFeatureID() != DbPackage.FOREIGN_KEY__TABLE && newTable != null)) {
			if (EcoreUtil.isAncestor(this, newTable))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newTable != null)
				msgs = ((InternalEObject) newTable).eInverseAdd(this, DbPackage.DB_TABLE__FOREIGN_KEYS, DBTable.class, msgs);

			msgs = basicSetTable(newTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.FOREIGN_KEY__TABLE, newTable, newTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getColumn()
	 * @generated
	 */
	@Override
	public DBColumn getColumn() {
		if (column != null && column.eIsProxy()) {
			final var oldColumn = (InternalEObject) column;
			column = (DBColumn) eResolveProxy(oldColumn);

			if (column != oldColumn && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DbPackage.FOREIGN_KEY__COLUMN, oldColumn, column));
		}

		return column;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DBColumn basicGetColumn() {
		return column;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#setColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated
	 */
	@Override
	public void setColumn(DBColumn newColumn) {
		final DBColumn oldColumn = column;
		column = newColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.FOREIGN_KEY__COLUMN, oldColumn, column));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getReferencedColumn()
	 * @generated
	 */
	@Override
	public DBColumn getReferencedColumn() {
		if (referencedColumn != null && referencedColumn.eIsProxy()) {
			final var oldReferencedColumn = (InternalEObject) referencedColumn;
			referencedColumn = (DBColumn) eResolveProxy(oldReferencedColumn);

			if (referencedColumn != oldReferencedColumn && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DbPackage.FOREIGN_KEY__REFERENCED_COLUMN, oldReferencedColumn,
						referencedColumn));
		}

		return referencedColumn;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DBColumn basicGetReferencedColumn() {
		return referencedColumn;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#setReferencedColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated
	 */
	@Override
	public void setReferencedColumn(DBColumn newReferencedColumn) {
		final DBColumn oldReferencedColumn = referencedColumn;
		referencedColumn = newReferencedColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.FOREIGN_KEY__REFERENCED_COLUMN, oldReferencedColumn,
					referencedColumn));
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
			case DbPackage.FOREIGN_KEY__TABLE:
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
			case DbPackage.FOREIGN_KEY__TABLE:
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
			case DbPackage.FOREIGN_KEY__TABLE:
				return eInternalContainer().eInverseRemove(this, DbPackage.DB_TABLE__FOREIGN_KEYS, DBTable.class, msgs);
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
			case DbPackage.FOREIGN_KEY__NAME:
				return getName();
			case DbPackage.FOREIGN_KEY__TABLE:
				return getTable();
			case DbPackage.FOREIGN_KEY__COLUMN:
				if (resolve)
					return getColumn();

				return basicGetColumn();
			case DbPackage.FOREIGN_KEY__REFERENCED_COLUMN:
				if (resolve)
					return getReferencedColumn();

				return basicGetReferencedColumn();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DbPackage.FOREIGN_KEY__NAME:
				setName((String) newValue);
				return;
			case DbPackage.FOREIGN_KEY__TABLE:
				setTable((DBTable) newValue);
				return;
			case DbPackage.FOREIGN_KEY__COLUMN:
				setColumn((DBColumn) newValue);
				return;
			case DbPackage.FOREIGN_KEY__REFERENCED_COLUMN:
				setReferencedColumn((DBColumn) newValue);
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
			case DbPackage.FOREIGN_KEY__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DbPackage.FOREIGN_KEY__TABLE:
				setTable((DBTable) null);
				return;
			case DbPackage.FOREIGN_KEY__COLUMN:
				setColumn((DBColumn) null);
				return;
			case DbPackage.FOREIGN_KEY__REFERENCED_COLUMN:
				setReferencedColumn((DBColumn) null);
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
			case DbPackage.FOREIGN_KEY__NAME:
				return name != null;
			case DbPackage.FOREIGN_KEY__TABLE:
				return getTable() != null;
			case DbPackage.FOREIGN_KEY__COLUMN:
				return column != null;
			case DbPackage.FOREIGN_KEY__REFERENCED_COLUMN:
				return referencedColumn != null;
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
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getConvertedName()
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
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getDatabaseName()
	 * @generated not
	 */
	@Override
	public String getDatabaseName() {
		Database database = null;

		if (getTable() != null)
			database = getTable().getDatabase();

		return DBNamingUtil.convertToDatabase(name, database);
	}

}
