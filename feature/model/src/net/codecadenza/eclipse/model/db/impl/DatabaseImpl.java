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
import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectWithInverseResolvingEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Database</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getDatabaseTables <em>Database Tables</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getProject <em>Project</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getSchemaName <em>Schema Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getCatalogName <em>Catalog Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getIdentifierRegEx <em>Identifier Reg Ex</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getIdentifierStyle <em>Identifier Style</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getMaxIdentifierLength <em>Max Identifier Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getHibernateDialect <em>Hibernate Dialect</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getEclipseLinkTargetDBName <em>Eclipse Link Target DB
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getReservedWords <em>Reserved Words</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#isSupportsIdentityColumn <em>Supports Identity Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#isSupportsSequence <em>Supports Sequence</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getVendorGroup <em>Vendor Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl#getAllSupportedColumnTypes <em>All Supported Column
 * Types</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DatabaseImpl extends EObjectImpl implements Database {
	/**
	 * The cached value of the '{@link #getDatabaseTables() <em>Database Tables</em>}' reference list
	 * @see #getDatabaseTables()
	 * @generated
	 * @ordered
	 */
	protected EList<DBTable> databaseTables;

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
	 * The default value of the '{@link #getIdentifierRegEx() <em>Identifier Reg Ex</em>}' attribute
	 * @see #getIdentifierRegEx()
	 * @generated
	 * @ordered
	 */
	protected static final String IDENTIFIER_REG_EX_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getIdentifierRegEx() <em>Identifier Reg Ex</em>}' attribute
	 * @see #getIdentifierRegEx()
	 * @generated
	 * @ordered
	 */
	protected String identifierRegEx = IDENTIFIER_REG_EX_EDEFAULT;

	/**
	 * The default value of the '{@link #getIdentifierStyle() <em>Identifier Style</em>}' attribute
	 * @see #getIdentifierStyle()
	 * @generated
	 * @ordered
	 */
	protected static final IdentifierStyleEnumeration IDENTIFIER_STYLE_EDEFAULT = IdentifierStyleEnumeration.UPPERCASE;

	/**
	 * The cached value of the '{@link #getIdentifierStyle() <em>Identifier Style</em>}' attribute
	 * @see #getIdentifierStyle()
	 * @generated
	 * @ordered
	 */
	protected IdentifierStyleEnumeration identifierStyle = IDENTIFIER_STYLE_EDEFAULT;

	/**
	 * The default value of the '{@link #getMaxIdentifierLength() <em>Max Identifier Length</em>}' attribute
	 * @see #getMaxIdentifierLength()
	 * @generated
	 * @ordered
	 */
	protected static final int MAX_IDENTIFIER_LENGTH_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getMaxIdentifierLength() <em>Max Identifier Length</em>}' attribute
	 * @see #getMaxIdentifierLength()
	 * @generated
	 * @ordered
	 */
	protected int maxIdentifierLength = MAX_IDENTIFIER_LENGTH_EDEFAULT;

	/**
	 * The default value of the '{@link #getHibernateDialect() <em>Hibernate Dialect</em>}' attribute
	 * @see #getHibernateDialect()
	 * @generated
	 * @ordered
	 */
	protected static final String HIBERNATE_DIALECT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getHibernateDialect() <em>Hibernate Dialect</em>}' attribute
	 * @see #getHibernateDialect()
	 * @generated
	 * @ordered
	 */
	protected String hibernateDialect = HIBERNATE_DIALECT_EDEFAULT;

	/**
	 * The default value of the '{@link #getEclipseLinkTargetDBName() <em>Eclipse Link Target DB Name</em>}' attribute
	 * @see #getEclipseLinkTargetDBName()
	 * @generated
	 * @ordered
	 */
	protected static final String ECLIPSE_LINK_TARGET_DB_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEclipseLinkTargetDBName() <em>Eclipse Link Target DB Name</em>}' attribute
	 * @see #getEclipseLinkTargetDBName()
	 * @generated
	 * @ordered
	 */
	protected String eclipseLinkTargetDBName = ECLIPSE_LINK_TARGET_DB_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getReservedWords() <em>Reserved Words</em>}' attribute
	 * @see #getReservedWords()
	 * @generated
	 * @ordered
	 */
	protected static final String RESERVED_WORDS_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getReservedWords() <em>Reserved Words</em>}' attribute
	 * @see #getReservedWords()
	 * @generated
	 * @ordered
	 */
	protected String reservedWords = RESERVED_WORDS_EDEFAULT;

	/**
	 * The default value of the '{@link #isSupportsIdentityColumn() <em>Supports Identity Column</em>}' attribute
	 * @see #isSupportsIdentityColumn()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SUPPORTS_IDENTITY_COLUMN_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSupportsIdentityColumn() <em>Supports Identity Column</em>}' attribute
	 * @see #isSupportsIdentityColumn()
	 * @generated
	 * @ordered
	 */
	protected boolean supportsIdentityColumn = SUPPORTS_IDENTITY_COLUMN_EDEFAULT;

	/**
	 * The default value of the '{@link #isSupportsSequence() <em>Supports Sequence</em>}' attribute
	 * @see #isSupportsSequence()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SUPPORTS_SEQUENCE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSupportsSequence() <em>Supports Sequence</em>}' attribute
	 * @see #isSupportsSequence()
	 * @generated
	 * @ordered
	 */
	protected boolean supportsSequence = SUPPORTS_SEQUENCE_EDEFAULT;

	/**
	 * The default value of the '{@link #getVendorGroup() <em>Vendor Group</em>}' attribute
	 * @see #getVendorGroup()
	 * @generated
	 * @ordered
	 */
	protected static final DBVendorGroupEnumeration VENDOR_GROUP_EDEFAULT = DBVendorGroupEnumeration.MYSQL;

	/**
	 * The cached value of the '{@link #getVendorGroup() <em>Vendor Group</em>}' attribute
	 * @see #getVendorGroup()
	 * @generated
	 * @ordered
	 */
	protected DBVendorGroupEnumeration vendorGroup = VENDOR_GROUP_EDEFAULT;

	/**
	 * The cached value of the '{@link #getAllSupportedColumnTypes() <em>All Supported Column Types</em>}' containment reference
	 * list
	 * @see #getAllSupportedColumnTypes()
	 * @generated
	 * @ordered
	 */
	protected EList<DBColumnType> allSupportedColumnTypes;

	/**
	 * @generated
	 */
	protected DatabaseImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DbPackage.Literals.DATABASE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getDatabaseTables()
	 * @generated
	 */
	@Override
	public EList<DBTable> getDatabaseTables() {
		if (databaseTables == null)
			databaseTables = new EObjectWithInverseResolvingEList<>(DBTable.class, this, DbPackage.DATABASE__DATABASE_TABLES,
					DbPackage.DB_TABLE__DATABASE);

		return databaseTables;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getProject()
	 * @generated
	 */
	@Override
	public Project getProject() {
		if (eContainerFeatureID() != DbPackage.DATABASE__PROJECT)
			return null;

		return (Project) eInternalContainer();
	}

	/**
	 * @param newProject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetProject(Project newProject, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newProject, DbPackage.DATABASE__PROJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setProject(net.codecadenza.eclipse.model.project.Project)
	 * @generated
	 */
	@Override
	public void setProject(Project newProject) {
		if (newProject != eInternalContainer() || (eContainerFeatureID() != DbPackage.DATABASE__PROJECT && newProject != null)) {
			if (EcoreUtil.isAncestor(this, newProject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newProject != null)
				msgs = ((InternalEObject) newProject).eInverseAdd(this, ProjectPackage.PROJECT__DATABASE, Project.class, msgs);

			msgs = basicSetProject(newProject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__PROJECT, newProject, newProject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getSchemaName()
	 * @generated
	 */
	@Override
	public String getSchemaName() {
		return schemaName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setSchemaName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setSchemaName(String newSchemaName) {
		final String oldSchemaName = schemaName;
		schemaName = newSchemaName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__SCHEMA_NAME, oldSchemaName, schemaName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getCatalogName()
	 * @generated
	 */
	@Override
	public String getCatalogName() {
		return catalogName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setCatalogName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setCatalogName(String newCatalogName) {
		final String oldCatalogName = catalogName;
		catalogName = newCatalogName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__CATALOG_NAME, oldCatalogName, catalogName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getIdentifierRegEx()
	 * @generated
	 */
	@Override
	public String getIdentifierRegEx() {
		return identifierRegEx;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setIdentifierRegEx(java.lang.String)
	 * @generated
	 */
	@Override
	public void setIdentifierRegEx(String newIdentifierRegEx) {
		final String oldIdentifierRegEx = identifierRegEx;
		identifierRegEx = newIdentifierRegEx;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__IDENTIFIER_REG_EX, oldIdentifierRegEx,
					identifierRegEx));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getIdentifierStyle()
	 * @generated
	 */
	@Override
	public IdentifierStyleEnumeration getIdentifierStyle() {
		return identifierStyle;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setIdentifierStyle(net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration)
	 * @generated
	 */
	@Override
	public void setIdentifierStyle(IdentifierStyleEnumeration newIdentifierStyle) {
		final IdentifierStyleEnumeration oldIdentifierStyle = identifierStyle;
		identifierStyle = newIdentifierStyle == null ? IDENTIFIER_STYLE_EDEFAULT : newIdentifierStyle;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__IDENTIFIER_STYLE, oldIdentifierStyle,
					identifierStyle));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getMaxIdentifierLength()
	 * @generated
	 */
	@Override
	public int getMaxIdentifierLength() {
		return maxIdentifierLength;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setMaxIdentifierLength(int)
	 * @generated
	 */
	@Override
	public void setMaxIdentifierLength(int newMaxIdentifierLength) {
		final int oldMaxIdentifierLength = maxIdentifierLength;
		maxIdentifierLength = newMaxIdentifierLength;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__MAX_IDENTIFIER_LENGTH, oldMaxIdentifierLength,
					maxIdentifierLength));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getHibernateDialect()
	 * @generated
	 */
	@Override
	public String getHibernateDialect() {
		return hibernateDialect;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setHibernateDialect(java.lang.String)
	 * @generated
	 */
	@Override
	public void setHibernateDialect(String newHibernateDialect) {
		final String oldHibernateDialect = hibernateDialect;
		hibernateDialect = newHibernateDialect;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__HIBERNATE_DIALECT, oldHibernateDialect,
					hibernateDialect));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getEclipseLinkTargetDBName()
	 * @generated
	 */
	@Override
	public String getEclipseLinkTargetDBName() {
		return eclipseLinkTargetDBName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setEclipseLinkTargetDBName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setEclipseLinkTargetDBName(String newEclipseLinkTargetDBName) {
		final String oldEclipseLinkTargetDBName = eclipseLinkTargetDBName;
		eclipseLinkTargetDBName = newEclipseLinkTargetDBName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__ECLIPSE_LINK_TARGET_DB_NAME,
					oldEclipseLinkTargetDBName, eclipseLinkTargetDBName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getReservedWords()
	 * @generated
	 */
	@Override
	public String getReservedWords() {
		return reservedWords;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setReservedWords(java.lang.String)
	 * @generated
	 */
	@Override
	public void setReservedWords(String newReservedWords) {
		final String oldReservedWords = reservedWords;
		reservedWords = newReservedWords;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__RESERVED_WORDS, oldReservedWords, reservedWords));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#isSupportsIdentityColumn()
	 * @generated
	 */
	@Override
	public boolean isSupportsIdentityColumn() {
		return supportsIdentityColumn;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setSupportsIdentityColumn(boolean)
	 * @generated
	 */
	@Override
	public void setSupportsIdentityColumn(boolean newSupportsIdentityColumn) {
		final boolean oldSupportsIdentityColumn = supportsIdentityColumn;
		supportsIdentityColumn = newSupportsIdentityColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__SUPPORTS_IDENTITY_COLUMN,
					oldSupportsIdentityColumn, supportsIdentityColumn));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#isSupportsSequence()
	 * @generated
	 */
	@Override
	public boolean isSupportsSequence() {
		return supportsSequence;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setSupportsSequence(boolean)
	 * @generated
	 */
	@Override
	public void setSupportsSequence(boolean newSupportsSequence) {
		final boolean oldSupportsSequence = supportsSequence;
		supportsSequence = newSupportsSequence;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__SUPPORTS_SEQUENCE, oldSupportsSequence,
					supportsSequence));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getVendorGroup()
	 * @generated
	 */
	@Override
	public DBVendorGroupEnumeration getVendorGroup() {
		return vendorGroup;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#setVendorGroup(net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration)
	 * @generated
	 */
	@Override
	public void setVendorGroup(DBVendorGroupEnumeration newVendorGroup) {
		final DBVendorGroupEnumeration oldVendorGroup = vendorGroup;
		vendorGroup = newVendorGroup == null ? VENDOR_GROUP_EDEFAULT : newVendorGroup;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DATABASE__VENDOR_GROUP, oldVendorGroup, vendorGroup));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getAllSupportedColumnTypes()
	 * @generated
	 */
	@Override
	public EList<DBColumnType> getAllSupportedColumnTypes() {
		if (allSupportedColumnTypes == null)
			allSupportedColumnTypes = new EObjectContainmentEList<>(DBColumnType.class, this,
					DbPackage.DATABASE__ALL_SUPPORTED_COLUMN_TYPES);

		return allSupportedColumnTypes;
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
			case DbPackage.DATABASE__DATABASE_TABLES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getDatabaseTables()).basicAdd(otherEnd, msgs);
			case DbPackage.DATABASE__PROJECT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetProject((Project) otherEnd, msgs);
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
			case DbPackage.DATABASE__DATABASE_TABLES:
				return ((InternalEList<?>) getDatabaseTables()).basicRemove(otherEnd, msgs);
			case DbPackage.DATABASE__PROJECT:
				return basicSetProject(null, msgs);
			case DbPackage.DATABASE__ALL_SUPPORTED_COLUMN_TYPES:
				return ((InternalEList<?>) getAllSupportedColumnTypes()).basicRemove(otherEnd, msgs);
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
			case DbPackage.DATABASE__PROJECT:
				return eInternalContainer().eInverseRemove(this, ProjectPackage.PROJECT__DATABASE, Project.class, msgs);
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
			case DbPackage.DATABASE__DATABASE_TABLES:
				return getDatabaseTables();
			case DbPackage.DATABASE__PROJECT:
				return getProject();
			case DbPackage.DATABASE__SCHEMA_NAME:
				return getSchemaName();
			case DbPackage.DATABASE__CATALOG_NAME:
				return getCatalogName();
			case DbPackage.DATABASE__IDENTIFIER_REG_EX:
				return getIdentifierRegEx();
			case DbPackage.DATABASE__IDENTIFIER_STYLE:
				return getIdentifierStyle();
			case DbPackage.DATABASE__MAX_IDENTIFIER_LENGTH:
				return getMaxIdentifierLength();
			case DbPackage.DATABASE__HIBERNATE_DIALECT:
				return getHibernateDialect();
			case DbPackage.DATABASE__ECLIPSE_LINK_TARGET_DB_NAME:
				return getEclipseLinkTargetDBName();
			case DbPackage.DATABASE__RESERVED_WORDS:
				return getReservedWords();
			case DbPackage.DATABASE__SUPPORTS_IDENTITY_COLUMN:
				return isSupportsIdentityColumn();
			case DbPackage.DATABASE__SUPPORTS_SEQUENCE:
				return isSupportsSequence();
			case DbPackage.DATABASE__VENDOR_GROUP:
				return getVendorGroup();
			case DbPackage.DATABASE__ALL_SUPPORTED_COLUMN_TYPES:
				return getAllSupportedColumnTypes();
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
			case DbPackage.DATABASE__DATABASE_TABLES:
				getDatabaseTables().clear();
				getDatabaseTables().addAll((Collection<? extends DBTable>) newValue);
				return;
			case DbPackage.DATABASE__PROJECT:
				setProject((Project) newValue);
				return;
			case DbPackage.DATABASE__SCHEMA_NAME:
				setSchemaName((String) newValue);
				return;
			case DbPackage.DATABASE__CATALOG_NAME:
				setCatalogName((String) newValue);
				return;
			case DbPackage.DATABASE__IDENTIFIER_REG_EX:
				setIdentifierRegEx((String) newValue);
				return;
			case DbPackage.DATABASE__IDENTIFIER_STYLE:
				setIdentifierStyle((IdentifierStyleEnumeration) newValue);
				return;
			case DbPackage.DATABASE__MAX_IDENTIFIER_LENGTH:
				setMaxIdentifierLength((Integer) newValue);
				return;
			case DbPackage.DATABASE__HIBERNATE_DIALECT:
				setHibernateDialect((String) newValue);
				return;
			case DbPackage.DATABASE__ECLIPSE_LINK_TARGET_DB_NAME:
				setEclipseLinkTargetDBName((String) newValue);
				return;
			case DbPackage.DATABASE__RESERVED_WORDS:
				setReservedWords((String) newValue);
				return;
			case DbPackage.DATABASE__SUPPORTS_IDENTITY_COLUMN:
				setSupportsIdentityColumn((Boolean) newValue);
				return;
			case DbPackage.DATABASE__SUPPORTS_SEQUENCE:
				setSupportsSequence((Boolean) newValue);
				return;
			case DbPackage.DATABASE__VENDOR_GROUP:
				setVendorGroup((DBVendorGroupEnumeration) newValue);
				return;
			case DbPackage.DATABASE__ALL_SUPPORTED_COLUMN_TYPES:
				getAllSupportedColumnTypes().clear();
				getAllSupportedColumnTypes().addAll((Collection<? extends DBColumnType>) newValue);
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
			case DbPackage.DATABASE__DATABASE_TABLES:
				getDatabaseTables().clear();
				return;
			case DbPackage.DATABASE__PROJECT:
				setProject((Project) null);
				return;
			case DbPackage.DATABASE__SCHEMA_NAME:
				setSchemaName(SCHEMA_NAME_EDEFAULT);
				return;
			case DbPackage.DATABASE__CATALOG_NAME:
				setCatalogName(CATALOG_NAME_EDEFAULT);
				return;
			case DbPackage.DATABASE__IDENTIFIER_REG_EX:
				setIdentifierRegEx(IDENTIFIER_REG_EX_EDEFAULT);
				return;
			case DbPackage.DATABASE__IDENTIFIER_STYLE:
				setIdentifierStyle(IDENTIFIER_STYLE_EDEFAULT);
				return;
			case DbPackage.DATABASE__MAX_IDENTIFIER_LENGTH:
				setMaxIdentifierLength(MAX_IDENTIFIER_LENGTH_EDEFAULT);
				return;
			case DbPackage.DATABASE__HIBERNATE_DIALECT:
				setHibernateDialect(HIBERNATE_DIALECT_EDEFAULT);
				return;
			case DbPackage.DATABASE__ECLIPSE_LINK_TARGET_DB_NAME:
				setEclipseLinkTargetDBName(ECLIPSE_LINK_TARGET_DB_NAME_EDEFAULT);
				return;
			case DbPackage.DATABASE__RESERVED_WORDS:
				setReservedWords(RESERVED_WORDS_EDEFAULT);
				return;
			case DbPackage.DATABASE__SUPPORTS_IDENTITY_COLUMN:
				setSupportsIdentityColumn(SUPPORTS_IDENTITY_COLUMN_EDEFAULT);
				return;
			case DbPackage.DATABASE__SUPPORTS_SEQUENCE:
				setSupportsSequence(SUPPORTS_SEQUENCE_EDEFAULT);
				return;
			case DbPackage.DATABASE__VENDOR_GROUP:
				setVendorGroup(VENDOR_GROUP_EDEFAULT);
				return;
			case DbPackage.DATABASE__ALL_SUPPORTED_COLUMN_TYPES:
				getAllSupportedColumnTypes().clear();
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
			case DbPackage.DATABASE__DATABASE_TABLES:
				return databaseTables != null && !databaseTables.isEmpty();
			case DbPackage.DATABASE__PROJECT:
				return getProject() != null;
			case DbPackage.DATABASE__SCHEMA_NAME:
				return schemaName != null;
			case DbPackage.DATABASE__CATALOG_NAME:
				return catalogName != null;
			case DbPackage.DATABASE__IDENTIFIER_REG_EX:
				return identifierRegEx != null;
			case DbPackage.DATABASE__IDENTIFIER_STYLE:
				return identifierStyle != IDENTIFIER_STYLE_EDEFAULT;
			case DbPackage.DATABASE__MAX_IDENTIFIER_LENGTH:
				return maxIdentifierLength != MAX_IDENTIFIER_LENGTH_EDEFAULT;
			case DbPackage.DATABASE__HIBERNATE_DIALECT:
				return hibernateDialect != null;
			case DbPackage.DATABASE__ECLIPSE_LINK_TARGET_DB_NAME:
				return eclipseLinkTargetDBName != null;
			case DbPackage.DATABASE__RESERVED_WORDS:
				return reservedWords != null;
			case DbPackage.DATABASE__SUPPORTS_IDENTITY_COLUMN:
				return supportsIdentityColumn != SUPPORTS_IDENTITY_COLUMN_EDEFAULT;
			case DbPackage.DATABASE__SUPPORTS_SEQUENCE:
				return supportsSequence != SUPPORTS_SEQUENCE_EDEFAULT;
			case DbPackage.DATABASE__VENDOR_GROUP:
				return vendorGroup != VENDOR_GROUP_EDEFAULT;
			case DbPackage.DATABASE__ALL_SUPPORTED_COLUMN_TYPES:
				return allSupportedColumnTypes != null && !allSupportedColumnTypes.isEmpty();
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
		result.append(" (schemaName: ");
		result.append(schemaName);
		result.append(", catalogName: ");
		result.append(catalogName);
		result.append(", identifierRegEx: ");
		result.append(identifierRegEx);
		result.append(", identifierStyle: ");
		result.append(identifierStyle);
		result.append(", maxIdentifierLength: ");
		result.append(maxIdentifierLength);
		result.append(", hibernateDialect: ");
		result.append(hibernateDialect);
		result.append(", eclipseLinkTargetDBName: ");
		result.append(eclipseLinkTargetDBName);
		result.append(", reservedWords: ");
		result.append(reservedWords);
		result.append(", supportsIdentityColumn: ");
		result.append(supportsIdentityColumn);
		result.append(", supportsSequence: ");
		result.append(supportsSequence);
		result.append(", vendorGroup: ");
		result.append(vendorGroup);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getAllSchemas()
	 * @generated not
	 */
	@Override
	public Set<String> getAllSchemas() {
		final var schemaSet = new HashSet<String>();
		var defaultSchema = "";

		if (getSchemaName() != null)
			defaultSchema = getSchemaName();

		schemaSet.add(DBNamingUtil.convertToStyle(defaultSchema, this));

		for (final DBTable table : getDatabaseTables())
			if (table.getSchemaName() != null && !table.getSchemaName().isEmpty())
				schemaSet.add(DBNamingUtil.convertToStyle(table.getSchemaName(), this));

		return schemaSet;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.Database#getAllCatalogs()
	 * @generated not
	 */
	@Override
	public Set<String> getAllCatalogs() {
		final var catalogSet = new HashSet<String>();
		var defaultCatalog = "";

		if (getCatalogName() != null)
			defaultCatalog = getCatalogName();

		catalogSet.add(DBNamingUtil.convertToStyle(defaultCatalog, this));

		for (final DBTable table : getDatabaseTables())
			if (table.getCatalogName() != null && !table.getCatalogName().isEmpty())
				catalogSet.add(DBNamingUtil.convertToStyle(table.getCatalogName(), this));

		return catalogSet;
	}

}
