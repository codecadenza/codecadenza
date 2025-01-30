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

import static net.codecadenza.eclipse.shared.Constants.DB_FOREIGN_KEY_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.DB_INDEX_PREFIX;

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
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
 * An implementation of the model object '<em><b>DB Column</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#getLength <em>Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#isNullable <em>Nullable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#getPrecision <em>Precision</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#getScale <em>Scale</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#getDatabaseTable <em>Database Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl#getColumnType <em>Column Type</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DBColumnImpl extends EObjectImpl implements DBColumn {
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
	 * The default value of the '{@link #getLength() <em>Length</em>}' attribute
	 * @see #getLength()
	 * @generated
	 * @ordered
	 */
	protected static final int LENGTH_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getLength() <em>Length</em>}' attribute
	 * @see #getLength()
	 * @generated
	 * @ordered
	 */
	protected int length = LENGTH_EDEFAULT;

	/**
	 * The default value of the '{@link #isNullable() <em>Nullable</em>}' attribute
	 * @see #isNullable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean NULLABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isNullable() <em>Nullable</em>}' attribute
	 * @see #isNullable()
	 * @generated
	 * @ordered
	 */
	protected boolean nullable = NULLABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #getPrecision() <em>Precision</em>}' attribute
	 * @see #getPrecision()
	 * @generated
	 * @ordered
	 */
	protected static final int PRECISION_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getPrecision() <em>Precision</em>}' attribute
	 * @see #getPrecision()
	 * @generated
	 * @ordered
	 */
	protected int precision = PRECISION_EDEFAULT;

	/**
	 * The default value of the '{@link #getScale() <em>Scale</em>}' attribute
	 * @see #getScale()
	 * @generated
	 * @ordered
	 */
	protected static final int SCALE_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getScale() <em>Scale</em>}' attribute
	 * @see #getScale()
	 * @generated
	 * @ordered
	 */
	protected int scale = SCALE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getColumnType() <em>Column Type</em>}' reference
	 * @see #getColumnType()
	 * @generated
	 * @ordered
	 */
	protected DBColumnType columnType;

	/**
	 * @generated
	 */
	protected DBColumnImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DbPackage.Literals.DB_COLUMN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getLength()
	 * @generated
	 */
	@Override
	public int getLength() {
		return length;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setLength(int)
	 * @generated
	 */
	@Override
	public void setLength(int newLength) {
		final int oldLength = length;
		length = newLength;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__LENGTH, oldLength, length));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#isNullable()
	 * @generated
	 */
	@Override
	public boolean isNullable() {
		return nullable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setNullable(boolean)
	 * @generated
	 */
	@Override
	public void setNullable(boolean newNullable) {
		final boolean oldNullable = nullable;
		nullable = newNullable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__NULLABLE, oldNullable, nullable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getPrecision()
	 * @generated
	 */
	@Override
	public int getPrecision() {
		return precision;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setPrecision(int)
	 * @generated
	 */
	@Override
	public void setPrecision(int newPrecision) {
		final int oldPrecision = precision;
		precision = newPrecision;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__PRECISION, oldPrecision, precision));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getScale()
	 * @generated
	 */
	@Override
	public int getScale() {
		return scale;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setScale(int)
	 * @generated
	 */
	@Override
	public void setScale(int newScale) {
		final int oldScale = scale;
		scale = newScale;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__SCALE, oldScale, scale));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable()
	 * @generated
	 */
	@Override
	public DBTable getDatabaseTable() {
		if (eContainerFeatureID() != DbPackage.DB_COLUMN__DATABASE_TABLE)
			return null;

		return (DBTable) eInternalContainer();
	}

	/**
	 * @param newDatabaseTable
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDatabaseTable(DBTable newDatabaseTable, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newDatabaseTable, DbPackage.DB_COLUMN__DATABASE_TABLE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setDatabaseTable(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated
	 */
	@Override
	public void setDatabaseTable(DBTable newDatabaseTable) {
		if (newDatabaseTable != eInternalContainer()
				|| (eContainerFeatureID() != DbPackage.DB_COLUMN__DATABASE_TABLE && newDatabaseTable != null)) {
			if (EcoreUtil.isAncestor(this, newDatabaseTable))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newDatabaseTable != null)
				msgs = ((InternalEObject) newDatabaseTable).eInverseAdd(this, DbPackage.DB_TABLE__COLUMNS, DBTable.class, msgs);

			msgs = basicSetDatabaseTable(newDatabaseTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__DATABASE_TABLE, newDatabaseTable, newDatabaseTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getColumnType()
	 * @generated
	 */
	@Override
	public DBColumnType getColumnType() {
		if (columnType != null && columnType.eIsProxy()) {
			final var oldColumnType = (InternalEObject) columnType;
			columnType = (DBColumnType) eResolveProxy(oldColumnType);

			if (columnType != oldColumnType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DbPackage.DB_COLUMN__COLUMN_TYPE, oldColumnType, columnType));
		}

		return columnType;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DBColumnType basicGetColumnType() {
		return columnType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#setColumnType(net.codecadenza.eclipse.model.db.DBColumnType)
	 * @generated
	 */
	@Override
	public void setColumnType(DBColumnType newColumnType) {
		final DBColumnType oldColumnType = columnType;
		columnType = newColumnType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN__COLUMN_TYPE, oldColumnType, columnType));
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
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetDatabaseTable((DBTable) otherEnd, msgs);
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
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				return basicSetDatabaseTable(null, msgs);
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
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				return eInternalContainer().eInverseRemove(this, DbPackage.DB_TABLE__COLUMNS, DBTable.class, msgs);
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
			case DbPackage.DB_COLUMN__NAME:
				return getName();
			case DbPackage.DB_COLUMN__LENGTH:
				return getLength();
			case DbPackage.DB_COLUMN__NULLABLE:
				return isNullable();
			case DbPackage.DB_COLUMN__PRECISION:
				return getPrecision();
			case DbPackage.DB_COLUMN__SCALE:
				return getScale();
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				return getDatabaseTable();
			case DbPackage.DB_COLUMN__COLUMN_TYPE:
				if (resolve)
					return getColumnType();

				return basicGetColumnType();
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
			case DbPackage.DB_COLUMN__NAME:
				setName((String) newValue);
				return;
			case DbPackage.DB_COLUMN__LENGTH:
				setLength((Integer) newValue);
				return;
			case DbPackage.DB_COLUMN__NULLABLE:
				setNullable((Boolean) newValue);
				return;
			case DbPackage.DB_COLUMN__PRECISION:
				setPrecision((Integer) newValue);
				return;
			case DbPackage.DB_COLUMN__SCALE:
				setScale((Integer) newValue);
				return;
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				setDatabaseTable((DBTable) newValue);
				return;
			case DbPackage.DB_COLUMN__COLUMN_TYPE:
				setColumnType((DBColumnType) newValue);
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
			case DbPackage.DB_COLUMN__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DbPackage.DB_COLUMN__LENGTH:
				setLength(LENGTH_EDEFAULT);
				return;
			case DbPackage.DB_COLUMN__NULLABLE:
				setNullable(NULLABLE_EDEFAULT);
				return;
			case DbPackage.DB_COLUMN__PRECISION:
				setPrecision(PRECISION_EDEFAULT);
				return;
			case DbPackage.DB_COLUMN__SCALE:
				setScale(SCALE_EDEFAULT);
				return;
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				setDatabaseTable((DBTable) null);
				return;
			case DbPackage.DB_COLUMN__COLUMN_TYPE:
				setColumnType((DBColumnType) null);
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
			case DbPackage.DB_COLUMN__NAME:
				return name != null;
			case DbPackage.DB_COLUMN__LENGTH:
				return length != LENGTH_EDEFAULT;
			case DbPackage.DB_COLUMN__NULLABLE:
				return nullable != NULLABLE_EDEFAULT;
			case DbPackage.DB_COLUMN__PRECISION:
				return precision != PRECISION_EDEFAULT;
			case DbPackage.DB_COLUMN__SCALE:
				return scale != SCALE_EDEFAULT;
			case DbPackage.DB_COLUMN__DATABASE_TABLE:
				return getDatabaseTable() != null;
			case DbPackage.DB_COLUMN__COLUMN_TYPE:
				return columnType != null;
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
		result.append(", length: ");
		result.append(length);
		result.append(", nullable: ");
		result.append(nullable);
		result.append(", precision: ");
		result.append(precision);
		result.append(", scale: ");
		result.append(scale);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getConvertedName()
	 * @generated not
	 */
	@Override
	public String getConvertedName() {
		Database database = null;

		if (getDatabaseTable() != null)
			database = getDatabaseTable().getDatabase();

		return DBNamingUtil.convertToStyle(name, database);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getDatabaseName()
	 * @generated not
	 */
	@Override
	public String getDatabaseName() {
		Database database = null;

		if (getDatabaseTable() != null)
			database = getDatabaseTable().getDatabase();

		return DBNamingUtil.convertToDatabase(name, database);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getMappingName()
	 * @generated not
	 */
	@Override
	public String getMappingName() {
		Database database = null;

		if (getDatabaseTable() != null)
			database = getDatabaseTable().getDatabase();

		return DBNamingUtil.convertToMapping(name, database);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumn#addForeignKey(net.codecadenza.eclipse.model.db.DBColumn, boolean)
	 * @generated not
	 */
	@Override
	public void addForeignKey(DBColumn targetColumn, boolean skipBackingIndex) {
		final DBVendorGroupEnumeration dbVendor = getDatabaseTable().getDatabase().getVendorGroup();

		final ForeignKey fk = DbFactory.eINSTANCE.createForeignKey();
		fk.setColumn(this);
		fk.setName(createForeignKeyName());
		fk.setReferencedColumn(targetColumn);
		fk.setTable(getDatabaseTable());

		getDatabaseTable().getForeignKeys().add(fk);

		if (!skipBackingIndex && (dbVendor == DBVendorGroupEnumeration.ORACLE || dbVendor == DBVendorGroupEnumeration.POSTGRESQL
				|| dbVendor == DBVendorGroupEnumeration.MSSQL)) {
			final DBIndex dbIndex = DbFactory.eINSTANCE.createDBIndex();
			dbIndex.setName(createIndexName());
			dbIndex.getColumns().add(this);
			dbIndex.setTable(getDatabaseTable());

			getDatabaseTable().getIndexes().add(dbIndex);
		}
	}

	/**
	 * @return a unique name for a new foreign key
	 * @generated not
	 */
	private String createForeignKeyName() {
		final String initialName = DB_FOREIGN_KEY_PREFIX + getDatabaseTable().getShortTableName() + "_" + getName();
		int counter = 0;

		while (true) {
			final String newName = counter > 0 ? initialName + counter : initialName;

			final boolean nameExists = getDatabaseTable().getForeignKeys().stream().map(ForeignKey::getName)
					.anyMatch(fkName -> fkName.equals(newName));

			if (!nameExists)
				return newName;

			counter++;
		}
	}

	/**
	 * @return a unique name for a new column index
	 * @generated not
	 */
	private String createIndexName() {
		final String initialName = DB_INDEX_PREFIX + getDatabaseTable().getShortTableName() + "_" + getName();
		int counter = 0;

		while (true) {
			final String newName = counter > 0 ? initialName + counter : initialName;

			final boolean nameExists = getDatabaseTable().getIndexes().stream().map(DBIndex::getName)
					.anyMatch(indexName -> indexName.equals(newName));

			if (!nameExists)
				return newName;

			counter++;
		}
	}

}
