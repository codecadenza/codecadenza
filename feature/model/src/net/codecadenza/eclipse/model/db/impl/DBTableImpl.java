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

import static net.codecadenza.eclipse.shared.Constants.DB_TABLE_SUFFIX;

import java.util.Collection;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>DB Table</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getDatabase <em>Database</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getColumns <em>Columns</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getPrimaryKey <em>Primary Key</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getForeignKeys <em>Foreign Keys</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getIndexes <em>Indexes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getSchemaName <em>Schema Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl#getCatalogName <em>Catalog Name</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DBTableImpl extends EObjectImpl implements DBTable {
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
	 * The cached value of the '{@link #getDatabase() <em>Database</em>}' reference
	 * @see #getDatabase()
	 * @generated
	 * @ordered
	 */
	protected Database database;

	/**
	 * The cached value of the '{@link #getColumns() <em>Columns</em>}' containment reference list
	 * @see #getColumns()
	 * @generated
	 * @ordered
	 */
	protected EList<DBColumn> columns;

	/**
	 * The cached value of the '{@link #getPrimaryKey() <em>Primary Key</em>}' containment reference
	 * @see #getPrimaryKey()
	 * @generated
	 * @ordered
	 */
	protected PrimaryKey primaryKey;

	/**
	 * The cached value of the '{@link #getForeignKeys() <em>Foreign Keys</em>}' containment reference list
	 * @see #getForeignKeys()
	 * @generated
	 * @ordered
	 */
	protected EList<ForeignKey> foreignKeys;

	/**
	 * The cached value of the '{@link #getIndexes() <em>Indexes</em>}' containment reference list
	 * @see #getIndexes()
	 * @generated
	 * @ordered
	 */
	protected EList<DBIndex> indexes;

	/**
	 * The default value of the '{@link #getSchemaName() <em>Schema Name</em>}' attribute
	 * @see #getSchemaName()
	 * @generated
	 * @ordered
	 */
	protected static final String SCHEMA_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSchemaName() <em>Schema Name</em>}' attribute
	 * @see #getSchemaName()
	 * @generated
	 * @ordered
	 */
	protected String schemaName = SCHEMA_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getCatalogName() <em>Catalog Name</em>}' attribute
	 * @see #getCatalogName()
	 * @generated
	 * @ordered
	 */
	protected static final String CATALOG_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getCatalogName() <em>Catalog Name</em>}' attribute
	 * @see #getCatalogName()
	 * @generated
	 * @ordered
	 */
	protected String catalogName = CATALOG_NAME_EDEFAULT;

	/**
	 * @generated
	 */
	protected DBTableImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DbPackage.Literals.DB_TABLE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getDatabase()
	 * @generated
	 */
	@Override
	public Database getDatabase() {
		if (database != null && database.eIsProxy()) {
			final var oldDatabase = (InternalEObject) database;
			database = (Database) eResolveProxy(oldDatabase);

			if (database != oldDatabase && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DbPackage.DB_TABLE__DATABASE, oldDatabase, database));
		}

		return database;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Database basicGetDatabase() {
		return database;
	}

	/**
	 * @param newDatabase
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDatabase(Database newDatabase, NotificationChain msgs) {
		final Database oldDatabase = database;
		database = newDatabase;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__DATABASE, oldDatabase,
					newDatabase);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#setDatabase(net.codecadenza.eclipse.model.db.Database)
	 * @generated
	 */
	@Override
	public void setDatabase(Database newDatabase) {
		if (newDatabase != database) {
			NotificationChain msgs = null;

			if (database != null)
				msgs = ((InternalEObject) database).eInverseRemove(this, DbPackage.DATABASE__DATABASE_TABLES, Database.class, msgs);

			if (newDatabase != null)
				msgs = ((InternalEObject) newDatabase).eInverseAdd(this, DbPackage.DATABASE__DATABASE_TABLES, Database.class, msgs);

			msgs = basicSetDatabase(newDatabase, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__DATABASE, newDatabase, newDatabase));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getColumns()
	 * @generated
	 */
	@Override
	public EList<DBColumn> getColumns() {
		if (columns == null)
			columns = new EObjectContainmentWithInverseEList<>(DBColumn.class, this, DbPackage.DB_TABLE__COLUMNS,
					DbPackage.DB_COLUMN__DATABASE_TABLE);

		return columns;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey()
	 * @generated
	 */
	@Override
	public PrimaryKey getPrimaryKey() {
		return primaryKey;
	}

	/**
	 * @param newPrimaryKey
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetPrimaryKey(PrimaryKey newPrimaryKey, NotificationChain msgs) {
		final PrimaryKey oldPrimaryKey = primaryKey;
		primaryKey = newPrimaryKey;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__PRIMARY_KEY, oldPrimaryKey,
					newPrimaryKey);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#setPrimaryKey(net.codecadenza.eclipse.model.db.PrimaryKey)
	 * @generated
	 */
	@Override
	public void setPrimaryKey(PrimaryKey newPrimaryKey) {
		if (newPrimaryKey != primaryKey) {
			NotificationChain msgs = null;

			if (primaryKey != null)
				msgs = ((InternalEObject) primaryKey).eInverseRemove(this, DbPackage.PRIMARY_KEY__TABLE, PrimaryKey.class, msgs);

			if (newPrimaryKey != null)
				msgs = ((InternalEObject) newPrimaryKey).eInverseAdd(this, DbPackage.PRIMARY_KEY__TABLE, PrimaryKey.class, msgs);

			msgs = basicSetPrimaryKey(newPrimaryKey, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__PRIMARY_KEY, newPrimaryKey, newPrimaryKey));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getForeignKeys()
	 * @generated
	 */
	@Override
	public EList<ForeignKey> getForeignKeys() {
		if (foreignKeys == null)
			foreignKeys = new EObjectContainmentWithInverseEList<>(ForeignKey.class, this, DbPackage.DB_TABLE__FOREIGN_KEYS,
					DbPackage.FOREIGN_KEY__TABLE);

		return foreignKeys;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getIndexes()
	 * @generated
	 */
	@Override
	public EList<DBIndex> getIndexes() {
		if (indexes == null)
			indexes = new EObjectContainmentWithInverseEList<>(DBIndex.class, this, DbPackage.DB_TABLE__INDEXES,
					DbPackage.DB_INDEX__TABLE);

		return indexes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getSchemaName()
	 * @generated
	 */
	@Override
	public String getSchemaName() {
		return schemaName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#setSchemaName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setSchemaName(String newSchemaName) {
		final String oldSchemaName = schemaName;
		schemaName = newSchemaName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__SCHEMA_NAME, oldSchemaName, schemaName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getCatalogName()
	 * @generated
	 */
	@Override
	public String getCatalogName() {
		return catalogName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#setCatalogName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setCatalogName(String newCatalogName) {
		final String oldCatalogName = catalogName;
		catalogName = newCatalogName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_TABLE__CATALOG_NAME, oldCatalogName, catalogName));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DbPackage.DB_TABLE__DATABASE:
				if (database != null)
					msgs = ((InternalEObject) database).eInverseRemove(this, DbPackage.DATABASE__DATABASE_TABLES, Database.class, msgs);

				return basicSetDatabase((Database) otherEnd, msgs);
			case DbPackage.DB_TABLE__COLUMNS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getColumns()).basicAdd(otherEnd, msgs);
			case DbPackage.DB_TABLE__PRIMARY_KEY:
				if (primaryKey != null)
					msgs = ((InternalEObject) primaryKey).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - DbPackage.DB_TABLE__PRIMARY_KEY,
							null, msgs);

				return basicSetPrimaryKey((PrimaryKey) otherEnd, msgs);
			case DbPackage.DB_TABLE__FOREIGN_KEYS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getForeignKeys()).basicAdd(otherEnd, msgs);
			case DbPackage.DB_TABLE__INDEXES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getIndexes()).basicAdd(otherEnd, msgs);
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
			case DbPackage.DB_TABLE__DATABASE:
				return basicSetDatabase(null, msgs);
			case DbPackage.DB_TABLE__COLUMNS:
				return ((InternalEList<?>) getColumns()).basicRemove(otherEnd, msgs);
			case DbPackage.DB_TABLE__PRIMARY_KEY:
				return basicSetPrimaryKey(null, msgs);
			case DbPackage.DB_TABLE__FOREIGN_KEYS:
				return ((InternalEList<?>) getForeignKeys()).basicRemove(otherEnd, msgs);
			case DbPackage.DB_TABLE__INDEXES:
				return ((InternalEList<?>) getIndexes()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DbPackage.DB_TABLE__NAME:
				return getName();
			case DbPackage.DB_TABLE__DATABASE:
				if (resolve)
					return getDatabase();

				return basicGetDatabase();
			case DbPackage.DB_TABLE__COLUMNS:
				return getColumns();
			case DbPackage.DB_TABLE__PRIMARY_KEY:
				return getPrimaryKey();
			case DbPackage.DB_TABLE__FOREIGN_KEYS:
				return getForeignKeys();
			case DbPackage.DB_TABLE__INDEXES:
				return getIndexes();
			case DbPackage.DB_TABLE__SCHEMA_NAME:
				return getSchemaName();
			case DbPackage.DB_TABLE__CATALOG_NAME:
				return getCatalogName();
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
			case DbPackage.DB_TABLE__NAME:
				setName((String) newValue);
				return;
			case DbPackage.DB_TABLE__DATABASE:
				setDatabase((Database) newValue);
				return;
			case DbPackage.DB_TABLE__COLUMNS:
				getColumns().clear();
				getColumns().addAll((Collection<? extends DBColumn>) newValue);
				return;
			case DbPackage.DB_TABLE__PRIMARY_KEY:
				setPrimaryKey((PrimaryKey) newValue);
				return;
			case DbPackage.DB_TABLE__FOREIGN_KEYS:
				getForeignKeys().clear();
				getForeignKeys().addAll((Collection<? extends ForeignKey>) newValue);
				return;
			case DbPackage.DB_TABLE__INDEXES:
				getIndexes().clear();
				getIndexes().addAll((Collection<? extends DBIndex>) newValue);
				return;
			case DbPackage.DB_TABLE__SCHEMA_NAME:
				setSchemaName((String) newValue);
				return;
			case DbPackage.DB_TABLE__CATALOG_NAME:
				setCatalogName((String) newValue);
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
			case DbPackage.DB_TABLE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DbPackage.DB_TABLE__DATABASE:
				setDatabase((Database) null);
				return;
			case DbPackage.DB_TABLE__COLUMNS:
				getColumns().clear();
				return;
			case DbPackage.DB_TABLE__PRIMARY_KEY:
				setPrimaryKey((PrimaryKey) null);
				return;
			case DbPackage.DB_TABLE__FOREIGN_KEYS:
				getForeignKeys().clear();
				return;
			case DbPackage.DB_TABLE__INDEXES:
				getIndexes().clear();
				return;
			case DbPackage.DB_TABLE__SCHEMA_NAME:
				setSchemaName(SCHEMA_NAME_EDEFAULT);
				return;
			case DbPackage.DB_TABLE__CATALOG_NAME:
				setCatalogName(CATALOG_NAME_EDEFAULT);
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
			case DbPackage.DB_TABLE__NAME:
				return name != null;
			case DbPackage.DB_TABLE__DATABASE:
				return database != null;
			case DbPackage.DB_TABLE__COLUMNS:
				return columns != null && !columns.isEmpty();
			case DbPackage.DB_TABLE__PRIMARY_KEY:
				return primaryKey != null;
			case DbPackage.DB_TABLE__FOREIGN_KEYS:
				return foreignKeys != null && !foreignKeys.isEmpty();
			case DbPackage.DB_TABLE__INDEXES:
				return indexes != null && !indexes.isEmpty();
			case DbPackage.DB_TABLE__SCHEMA_NAME:
				return schemaName != null;
			case DbPackage.DB_TABLE__CATALOG_NAME:
				return catalogName != null;
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
		result.append(", schemaName: ");
		result.append(schemaName);
		result.append(", catalogName: ");
		result.append(catalogName);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getShortTableName()
	 * @generated not
	 */
	@Override
	public String getShortTableName() {
		if (this.name == null || this.name.isEmpty())
			return "";

		if (!name.contains(DB_TABLE_SUFFIX))
			return name;

		return name.substring(0, name.indexOf(DB_TABLE_SUFFIX));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getConvertedName()
	 * @generated not
	 */
	@Override
	public String getConvertedName() {
		return DBNamingUtil.convertToStyle(name, getDatabase());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getDatabaseName()
	 * @generated not
	 */
	@Override
	public String getDatabaseName() {
		return DBNamingUtil.convertToDatabase(name, getDatabase());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getFullDatabaseName()
	 * @generated not
	 */
	@Override
	public String getFullDatabaseName() {
		final Database db = getDatabase();
		final var defaultSchema = db.getSchemaName() == null ? "" : db.getSchemaName();
		final var defaultCatalog = db.getCatalogName() == null ? "" : db.getCatalogName();
		final var tableSchema = getSchemaName() == null ? "" : getSchemaName();
		final var tableCatalog = getCatalogName() == null ? "" : getCatalogName();

		// We don't cover cases where schema and catalog are specified!
		if (!tableSchema.isEmpty())
			return DBNamingUtil.convertToDatabase(tableSchema, db) + "." + getDatabaseName();

		if (!tableCatalog.isEmpty())
			return DBNamingUtil.convertToDatabase(tableCatalog, db) + "." + getDatabaseName();

		if (!defaultSchema.isEmpty())
			return DBNamingUtil.convertToDatabase(defaultSchema, db) + "." + getDatabaseName();

		if (!defaultCatalog.isEmpty())
			return DBNamingUtil.convertToDatabase(defaultCatalog, db) + "." + getDatabaseName();

		return getDatabaseName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getMappingName()
	 * @generated not
	 */
	@Override
	public String getMappingName() {
		return DBNamingUtil.convertToMapping(name, getDatabase());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#copyTableToDatabase(net.codecadenza.eclipse.model.db.Database)
	 * @generated not
	 */
	@Override
	public DBTable copyTableToDatabase(Database database) {
		final DBTable targetTable = DbFactory.eINSTANCE.createDBTable();
		targetTable.setCatalogName(getCatalogName());
		targetTable.setDatabase(database);
		targetTable.setName(getName());
		targetTable.setSchemaName(getSchemaName());

		// Copy all columns of this table to the target table
		getColumns().forEach(targetTable::addColumnCopy);

		if (getPrimaryKey() != null && getPrimaryKey().getColumn() != null) {
			final PrimaryKey pk = DbFactory.eINSTANCE.createPrimaryKey();
			pk.setName(getPrimaryKey().getName());
			pk.setTable(targetTable);

			for (final DBColumn col : targetTable.getColumns())
				if (col.getConvertedName().equals(getPrimaryKey().getColumn().getConvertedName())) {
					pk.setColumn(col);
					break;
				}

			targetTable.setPrimaryKey(pk);
		}

		// Copy all foreign keys. Even if the referenced column isn't set the foreign key should be added!
		targetTable.addForeignKeyCopies(this, false);

		// Copy all indexes
		targetTable.addIndexCopies(this);

		// Add table to database
		database.getDatabaseTables().add(targetTable);

		return targetTable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#addColumnCopy(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated not
	 */
	@Override
	public DBColumn addColumnCopy(DBColumn sourceColumn) {
		// Test if a column with the same name already exists
		for (final DBColumn existingColumn : getColumns())
			if (sourceColumn.getConvertedName().equals(existingColumn.getConvertedName()))
				return null;

		final DBColumn newCol = DbFactory.eINSTANCE.createDBColumn();
		newCol.setDatabaseTable(this);
		newCol.setName(sourceColumn.getName());
		newCol.setLength(sourceColumn.getLength());
		newCol.setNullable(sourceColumn.isNullable());
		newCol.setPrecision(sourceColumn.getPrecision());
		newCol.setScale(sourceColumn.getScale());

		for (final DBColumnType colType : getDatabase().getAllSupportedColumnTypes())
			if (colType.getName().equals(sourceColumn.getColumnType().getName())) {
				newCol.setColumnType(colType);
				break;
			}

		this.getColumns().add(newCol);

		return newCol;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#addForeignKeyCopies(net.codecadenza.eclipse.model.db.DBTable, boolean)
	 * @generated not
	 */
	@Override
	public void addForeignKeyCopies(DBTable sourceTable, boolean strictMode) {
		for (final ForeignKey sourceKey : sourceTable.getForeignKeys()) {
			final boolean keyExists = getForeignKeys().stream()
					.anyMatch(existingKey -> existingKey.getConvertedName().equals(sourceKey.getConvertedName()));

			if (keyExists)
				continue;

			final ForeignKey newForeignKey = DbFactory.eINSTANCE.createForeignKey();
			newForeignKey.setName(sourceKey.getName());

			for (final DBColumn col : getColumns())
				if (col.getConvertedName().equals(sourceKey.getColumn().getConvertedName())) {
					newForeignKey.setColumn(col);
					break;
				}

			// A foreign key shouldn't be added if the column field is not initialized!
			if (newForeignKey.getColumn() == null)
				continue;

			final DBColumn refColumn = sourceKey.getReferencedColumn();

			if (refColumn != null)
				for (final DBTable refTable : getDatabase().getDatabaseTables()) {
					if (!refColumn.getDatabaseTable().getConvertedName().equals(refTable.getConvertedName()))
						continue;

					boolean columnFound = false;

					for (final DBColumn col : refTable.getColumns())
						if (col.getConvertedName().equals(refColumn.getConvertedName())) {
							newForeignKey.setReferencedColumn(col);
							columnFound = true;
							break;
						}

					if (columnFound)
						break;
				}

			// Don't add an inappropriately initialized foreign key if the strict mode is enabled!
			if (newForeignKey.getReferencedColumn() == null && strictMode)
				continue;

			newForeignKey.setTable(this);
			getForeignKeys().add(newForeignKey);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#addForeignKeyCopies(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated not
	 */
	@Override
	public void addForeignKeyCopies(DBTable sourceTable) {
		// Copy foreign keys in strict mode. That means that a foreign key isn't added if the referenced column is not set!
		addForeignKeyCopies(sourceTable, true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#addIndexCopies(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated not
	 */
	@Override
	public void addIndexCopies(DBTable sourceTable) {
		for (final DBIndex sourceIndex : sourceTable.getIndexes()) {
			final boolean indexFound = getIndexes().stream()
					.anyMatch(existingIndex -> sourceIndex.getConvertedName().equals(existingIndex.getConvertedName()));

			if (indexFound)
				continue;

			final DBIndex newIndex = DbFactory.eINSTANCE.createDBIndex();
			newIndex.setName(sourceIndex.getName());
			newIndex.setUnique(sourceIndex.isUnique());

			for (final DBColumn colIndex : sourceIndex.getColumns())
				for (final DBColumn col : getColumns())
					if (col.getConvertedName().equals(colIndex.getConvertedName())) {
						newIndex.getColumns().add(col);
						break;
					}

			// Don't add an inappropriately initialized index!
			if (sourceIndex.getColumns().size() != newIndex.getColumns().size())
				continue;

			newIndex.setTable(this);
			getIndexes().add(newIndex);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBTable#getColumnByConvertedName(java.lang.String)
	 * @generated not
	 */
	@Override
	public DBColumn getColumnByConvertedName(String convertedName) {
		return getColumns().stream().filter(col -> col.getConvertedName().equals(convertedName)).findFirst().orElse(null);
	}

}
