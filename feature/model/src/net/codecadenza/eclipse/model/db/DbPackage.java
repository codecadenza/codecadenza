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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * The <b>Package</b> for the model. It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.db.DbFactory
 * @model kind="package"
 * @generated
 */
public interface DbPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "db";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/db.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.db";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	DbPackage eINSTANCE = net.codecadenza.eclipse.model.db.impl.DbPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl <em>DB Column</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.DBColumnImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBColumn()
	 * @generated
	 */
	int DB_COLUMN = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Length</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__LENGTH = 1;

	/**
	 * The feature ID for the '<em><b>Nullable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__NULLABLE = 2;

	/**
	 * The feature ID for the '<em><b>Precision</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__PRECISION = 3;

	/**
	 * The feature ID for the '<em><b>Scale</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__SCALE = 4;

	/**
	 * The feature ID for the '<em><b>Database Table</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__DATABASE_TABLE = 5;

	/**
	 * The feature ID for the '<em><b>Column Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN__COLUMN_TYPE = 6;

	/**
	 * The number of structural features of the '<em>DB Column</em>' class
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN_FEATURE_COUNT = 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl <em>DB Column Type</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBColumnType()
	 * @generated
	 */
	int DB_COLUMN_TYPE = 1;

	/**
	 * The feature ID for the '<em><b>Java Types</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN_TYPE__JAVA_TYPES = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN_TYPE__NAME = 1;

	/**
	 * The feature ID for the '<em><b>Omit Size Information</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION = 2;

	/**
	 * The number of structural features of the '<em>DB Column Type</em>' class
	 * @generated
	 * @ordered
	 */
	int DB_COLUMN_TYPE_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.DBIndexImpl <em>DB Index</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.DBIndexImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBIndex()
	 * @generated
	 */
	int DB_INDEX = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_INDEX__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Table</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DB_INDEX__TABLE = 1;

	/**
	 * The feature ID for the '<em><b>Unique</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_INDEX__UNIQUE = 2;

	/**
	 * The feature ID for the '<em><b>Columns</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DB_INDEX__COLUMNS = 3;

	/**
	 * The number of structural features of the '<em>DB Index</em>' class
	 * @generated
	 * @ordered
	 */
	int DB_INDEX_FEATURE_COUNT = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl <em>DB Table</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.DBTableImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBTable()
	 * @generated
	 */
	int DB_TABLE = 3;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Database</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__DATABASE = 1;

	/**
	 * The feature ID for the '<em><b>Columns</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__COLUMNS = 2;

	/**
	 * The feature ID for the '<em><b>Primary Key</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__PRIMARY_KEY = 3;

	/**
	 * The feature ID for the '<em><b>Foreign Keys</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__FOREIGN_KEYS = 4;

	/**
	 * The feature ID for the '<em><b>Indexes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__INDEXES = 5;

	/**
	 * The feature ID for the '<em><b>Schema Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__SCHEMA_NAME = 6;

	/**
	 * The feature ID for the '<em><b>Catalog Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DB_TABLE__CATALOG_NAME = 7;

	/**
	 * The number of structural features of the '<em>DB Table</em>' class
	 * @generated
	 * @ordered
	 */
	int DB_TABLE_FEATURE_COUNT = 8;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl <em>Database</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.DatabaseImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDatabase()
	 * @generated
	 */
	int DATABASE = 4;

	/**
	 * The feature ID for the '<em><b>Database Tables</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DATABASE__DATABASE_TABLES = 0;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DATABASE__PROJECT = 1;

	/**
	 * The feature ID for the '<em><b>Schema Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__SCHEMA_NAME = 2;

	/**
	 * The feature ID for the '<em><b>Catalog Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__CATALOG_NAME = 3;

	/**
	 * The feature ID for the '<em><b>Identifier Reg Ex</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__IDENTIFIER_REG_EX = 4;

	/**
	 * The feature ID for the '<em><b>Identifier Style</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__IDENTIFIER_STYLE = 5;

	/**
	 * The feature ID for the '<em><b>Max Identifier Length</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__MAX_IDENTIFIER_LENGTH = 6;

	/**
	 * The feature ID for the '<em><b>Hibernate Dialect</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__HIBERNATE_DIALECT = 7;

	/**
	 * The feature ID for the '<em><b>Eclipse Link Target DB Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__ECLIPSE_LINK_TARGET_DB_NAME = 8;

	/**
	 * The feature ID for the '<em><b>Reserved Words</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__RESERVED_WORDS = 9;

	/**
	 * The feature ID for the '<em><b>Supports Identity Column</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__SUPPORTS_IDENTITY_COLUMN = 10;

	/**
	 * The feature ID for the '<em><b>Supports Sequence</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__SUPPORTS_SEQUENCE = 11;

	/**
	 * The feature ID for the '<em><b>Vendor Group</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATABASE__VENDOR_GROUP = 12;

	/**
	 * The feature ID for the '<em><b>All Supported Column Types</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATABASE__ALL_SUPPORTED_COLUMN_TYPES = 13;

	/**
	 * The number of structural features of the '<em>Database</em>' class
	 * @generated
	 * @ordered
	 */
	int DATABASE_FEATURE_COUNT = 14;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl <em>Foreign Key</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getForeignKey()
	 * @generated
	 */
	int FOREIGN_KEY = 5;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FOREIGN_KEY__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Table</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int FOREIGN_KEY__TABLE = 1;

	/**
	 * The feature ID for the '<em><b>Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FOREIGN_KEY__COLUMN = 2;

	/**
	 * The feature ID for the '<em><b>Referenced Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FOREIGN_KEY__REFERENCED_COLUMN = 3;

	/**
	 * The number of structural features of the '<em>Foreign Key</em>' class
	 * @generated
	 * @ordered
	 */
	int FOREIGN_KEY_FEATURE_COUNT = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.impl.PrimaryKeyImpl <em>Primary Key</em>}' class
	 * @see net.codecadenza.eclipse.model.db.impl.PrimaryKeyImpl
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getPrimaryKey()
	 * @generated
	 */
	int PRIMARY_KEY = 6;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PRIMARY_KEY__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Table</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int PRIMARY_KEY__TABLE = 1;

	/**
	 * The feature ID for the '<em><b>Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int PRIMARY_KEY__COLUMN = 2;

	/**
	 * The number of structural features of the '<em>Primary Key</em>' class
	 * @generated
	 * @ordered
	 */
	int PRIMARY_KEY_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration <em>DB Vendor Group
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBVendorGroupEnumeration()
	 * @generated
	 */
	int DB_VENDOR_GROUP_ENUMERATION = 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration <em>Identifier Style
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration
	 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getIdentifierStyleEnumeration()
	 * @generated
	 */
	int IDENTIFIER_STYLE_ENUMERATION = 8;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.DBColumn <em>DB Column</em>}'
	 * @return the meta object for class '<em>DB Column</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn
	 * @generated
	 */
	EClass getDBColumn();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumn#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getName()
	 * @see #getDBColumn()
	 * @generated
	 */
	EAttribute getDBColumn_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumn#getLength <em>Length</em>}'
	 * @return the meta object for the attribute '<em>Length</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getLength()
	 * @see #getDBColumn()
	 * @generated
	 */
	EAttribute getDBColumn_Length();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumn#isNullable <em>Nullable</em>}'
	 * @return the meta object for the attribute '<em>Nullable</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#isNullable()
	 * @see #getDBColumn()
	 * @generated
	 */
	EAttribute getDBColumn_Nullable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumn#getPrecision <em>Precision</em>}'
	 * @return the meta object for the attribute '<em>Precision</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getPrecision()
	 * @see #getDBColumn()
	 * @generated
	 */
	EAttribute getDBColumn_Precision();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumn#getScale <em>Scale</em>}'
	 * @return the meta object for the attribute '<em>Scale</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getScale()
	 * @see #getDBColumn()
	 * @generated
	 */
	EAttribute getDBColumn_Scale();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable
	 * <em>Database Table</em>}'
	 * @return the meta object for the container reference '<em>Database Table</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getDatabaseTable()
	 * @see #getDBColumn()
	 * @generated
	 */
	EReference getDBColumn_DatabaseTable();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.db.DBColumn#getColumnType <em>Column
	 * Type</em>}'
	 * @return the meta object for the reference '<em>Column Type</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumn#getColumnType()
	 * @see #getDBColumn()
	 * @generated
	 */
	EReference getDBColumn_ColumnType();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.DBColumnType <em>DB Column Type</em>}'
	 * @return the meta object for class '<em>DB Column Type</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumnType
	 * @generated
	 */
	EClass getDBColumnType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumnType#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#getName()
	 * @see #getDBColumnType()
	 * @generated
	 */
	EAttribute getDBColumnType_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBColumnType#isOmitSizeInformation <em>Omit
	 * Size Information</em>}'
	 * @return the meta object for the attribute '<em>Omit Size Information</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#isOmitSizeInformation()
	 * @see #getDBColumnType()
	 * @generated
	 */
	EAttribute getDBColumnType_OmitSizeInformation();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.db.DBColumnType#getJavaTypes <em>Java
	 * Types</em>}'
	 * @return the meta object for the reference list '<em>Java Types</em>'
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#getJavaTypes()
	 * @see #getDBColumnType()
	 * @generated
	 */
	EReference getDBColumnType_JavaTypes();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.DBIndex <em>DB Index</em>}'
	 * @return the meta object for class '<em>DB Index</em>'
	 * @see net.codecadenza.eclipse.model.db.DBIndex
	 * @generated
	 */
	EClass getDBIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBIndex#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getName()
	 * @see #getDBIndex()
	 * @generated
	 */
	EAttribute getDBIndex_Name();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.db.DBIndex#getTable <em>Table</em>}'
	 * @return the meta object for the container reference '<em>Table</em>'
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getTable()
	 * @see #getDBIndex()
	 * @generated
	 */
	EReference getDBIndex_Table();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBIndex#isUnique <em>Unique</em>}'
	 * @return the meta object for the attribute '<em>Unique</em>'
	 * @see net.codecadenza.eclipse.model.db.DBIndex#isUnique()
	 * @see #getDBIndex()
	 * @generated
	 */
	EAttribute getDBIndex_Unique();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.db.DBIndex#getColumns <em>Columns</em>}'
	 * @return the meta object for the reference list '<em>Columns</em>'
	 * @see net.codecadenza.eclipse.model.db.DBIndex#getColumns()
	 * @see #getDBIndex()
	 * @generated
	 */
	EReference getDBIndex_Columns();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.DBTable <em>DB Table</em>}'
	 * @return the meta object for class '<em>DB Table</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable
	 * @generated
	 */
	EClass getDBTable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBTable#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getName()
	 * @see #getDBTable()
	 * @generated
	 */
	EAttribute getDBTable_Name();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.db.DBTable#getDatabase <em>Database</em>}'
	 * @return the meta object for the reference '<em>Database</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getDatabase()
	 * @see #getDBTable()
	 * @generated
	 */
	EReference getDBTable_Database();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.db.DBTable#getColumns
	 * <em>Columns</em>}'
	 * @return the meta object for the containment reference list '<em>Columns</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getColumns()
	 * @see #getDBTable()
	 * @generated
	 */
	EReference getDBTable_Columns();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey
	 * <em>Primary Key</em>}'
	 * @return the meta object for the containment reference '<em>Primary Key</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getPrimaryKey()
	 * @see #getDBTable()
	 * @generated
	 */
	EReference getDBTable_PrimaryKey();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.db.DBTable#getForeignKeys
	 * <em>Foreign Keys</em>}'
	 * @return the meta object for the containment reference list '<em>Foreign Keys</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getForeignKeys()
	 * @see #getDBTable()
	 * @generated
	 */
	EReference getDBTable_ForeignKeys();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.db.DBTable#getIndexes
	 * <em>Indexes</em>}'
	 * @return the meta object for the containment reference list '<em>Indexes</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getIndexes()
	 * @see #getDBTable()
	 * @generated
	 */
	EReference getDBTable_Indexes();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBTable#getSchemaName <em>Schema Name</em>}'
	 * @return the meta object for the attribute '<em>Schema Name</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getSchemaName()
	 * @see #getDBTable()
	 * @generated
	 */
	EAttribute getDBTable_SchemaName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.DBTable#getCatalogName <em>Catalog
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>Catalog Name</em>'
	 * @see net.codecadenza.eclipse.model.db.DBTable#getCatalogName()
	 * @see #getDBTable()
	 * @generated
	 */
	EAttribute getDBTable_CatalogName();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.Database <em>Database</em>}'
	 * @return the meta object for class '<em>Database</em>'
	 * @see net.codecadenza.eclipse.model.db.Database
	 * @generated
	 */
	EClass getDatabase();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.db.Database#getDatabaseTables <em>Database
	 * Tables</em>}'
	 * @return the meta object for the reference list '<em>Database Tables</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getDatabaseTables()
	 * @see #getDatabase()
	 * @generated
	 */
	EReference getDatabase_DatabaseTables();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.db.Database#getProject
	 * <em>Project</em>}'
	 * @return the meta object for the container reference '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getProject()
	 * @see #getDatabase()
	 * @generated
	 */
	EReference getDatabase_Project();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getSchemaName <em>Schema
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>Schema Name</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getSchemaName()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_SchemaName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getCatalogName <em>Catalog
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>Catalog Name</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getCatalogName()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_CatalogName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getIdentifierRegEx <em>Identifier
	 * Reg Ex</em>}'
	 * @return the meta object for the attribute '<em>Identifier Reg Ex</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getIdentifierRegEx()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_IdentifierRegEx();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getIdentifierStyle <em>Identifier
	 * Style</em>}'
	 * @return the meta object for the attribute '<em>Identifier Style</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getIdentifierStyle()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_IdentifierStyle();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getMaxIdentifierLength <em>Max
	 * Identifier Length</em>}'
	 * @return the meta object for the attribute '<em>Max Identifier Length</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getMaxIdentifierLength()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_MaxIdentifierLength();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getHibernateDialect <em>Hibernate
	 * Dialect</em>}'
	 * @return the meta object for the attribute '<em>Hibernate Dialect</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getHibernateDialect()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_HibernateDialect();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getEclipseLinkTargetDBName
	 * <em>Eclipse Link Target DB Name</em>}'
	 * @return the meta object for the attribute '<em>Eclipse Link Target DB Name</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getEclipseLinkTargetDBName()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_EclipseLinkTargetDBName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getReservedWords <em>Reserved
	 * Words</em>}'
	 * @return the meta object for the attribute '<em>Reserved Words</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getReservedWords()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_ReservedWords();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#isSupportsIdentityColumn
	 * <em>Supports Identity Column</em>}'
	 * @return the meta object for the attribute '<em>Supports Identity Column</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#isSupportsIdentityColumn()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_SupportsIdentityColumn();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#isSupportsSequence <em>Supports
	 * Sequence</em>}'
	 * @return the meta object for the attribute '<em>Supports Sequence</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#isSupportsSequence()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_SupportsSequence();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.Database#getVendorGroup <em>Vendor
	 * Group</em>}'
	 * @return the meta object for the attribute '<em>Vendor Group</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getVendorGroup()
	 * @see #getDatabase()
	 * @generated
	 */
	EAttribute getDatabase_VendorGroup();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.db.Database#getAllSupportedColumnTypes <em>All Supported Column Types</em>}'
	 * @return the meta object for the containment reference list '<em>All Supported Column Types</em>'
	 * @see net.codecadenza.eclipse.model.db.Database#getAllSupportedColumnTypes()
	 * @see #getDatabase()
	 * @generated
	 */
	EReference getDatabase_AllSupportedColumnTypes();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.ForeignKey <em>Foreign Key</em>}'
	 * @return the meta object for class '<em>Foreign Key</em>'
	 * @see net.codecadenza.eclipse.model.db.ForeignKey
	 * @generated
	 */
	EClass getForeignKey();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.ForeignKey#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getName()
	 * @see #getForeignKey()
	 * @generated
	 */
	EAttribute getForeignKey_Name();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.db.ForeignKey#getTable
	 * <em>Table</em>}'
	 * @return the meta object for the container reference '<em>Table</em>'
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getTable()
	 * @see #getForeignKey()
	 * @generated
	 */
	EReference getForeignKey_Table();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.db.ForeignKey#getColumn <em>Column</em>}'
	 * @return the meta object for the reference '<em>Column</em>'
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getColumn()
	 * @see #getForeignKey()
	 * @generated
	 */
	EReference getForeignKey_Column();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.db.ForeignKey#getReferencedColumn
	 * <em>Referenced Column</em>}'
	 * @return the meta object for the reference '<em>Referenced Column</em>'
	 * @see net.codecadenza.eclipse.model.db.ForeignKey#getReferencedColumn()
	 * @see #getForeignKey()
	 * @generated
	 */
	EReference getForeignKey_ReferencedColumn();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.db.PrimaryKey <em>Primary Key</em>}'
	 * @return the meta object for class '<em>Primary Key</em>'
	 * @see net.codecadenza.eclipse.model.db.PrimaryKey
	 * @generated
	 */
	EClass getPrimaryKey();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.db.PrimaryKey#getName()
	 * @see #getPrimaryKey()
	 * @generated
	 */
	EAttribute getPrimaryKey_Name();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getTable
	 * <em>Table</em>}'
	 * @return the meta object for the container reference '<em>Table</em>'
	 * @see net.codecadenza.eclipse.model.db.PrimaryKey#getTable()
	 * @see #getPrimaryKey()
	 * @generated
	 */
	EReference getPrimaryKey_Table();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.db.PrimaryKey#getColumn <em>Column</em>}'
	 * @return the meta object for the reference '<em>Column</em>'
	 * @see net.codecadenza.eclipse.model.db.PrimaryKey#getColumn()
	 * @see #getPrimaryKey()
	 * @generated
	 */
	EReference getPrimaryKey_Column();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration <em>DB Vendor Group
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>DB Vendor Group Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration
	 * @generated
	 */
	EEnum getDBVendorGroupEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration <em>Identifier Style
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Identifier Style Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration
	 * @generated
	 */
	EEnum getIdentifierStyleEnumeration();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	DbFactory getDbFactory();

	/**
	 * Defines literals for the meta objects that represent
	 * <ul>
	 * <li>each class,</li>
	 * <li>each feature of each class,</li>
	 * <li>each enum,</li>
	 * <li>and each data type</li>
	 * </ul>
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.DBColumnImpl <em>DB Column</em>}' class
		 * @see net.codecadenza.eclipse.model.db.impl.DBColumnImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBColumn()
		 * @generated
		 */
		EClass DB_COLUMN = eINSTANCE.getDBColumn();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN__NAME = eINSTANCE.getDBColumn_Name();

		/**
		 * The meta object literal for the '<em><b>Length</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN__LENGTH = eINSTANCE.getDBColumn_Length();

		/**
		 * The meta object literal for the '<em><b>Nullable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN__NULLABLE = eINSTANCE.getDBColumn_Nullable();

		/**
		 * The meta object literal for the '<em><b>Precision</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN__PRECISION = eINSTANCE.getDBColumn_Precision();

		/**
		 * The meta object literal for the '<em><b>Scale</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN__SCALE = eINSTANCE.getDBColumn_Scale();

		/**
		 * The meta object literal for the '<em><b>Database Table</b></em>' container reference feature
		 * @generated
		 */
		EReference DB_COLUMN__DATABASE_TABLE = eINSTANCE.getDBColumn_DatabaseTable();

		/**
		 * The meta object literal for the '<em><b>Column Type</b></em>' reference feature
		 * @generated
		 */
		EReference DB_COLUMN__COLUMN_TYPE = eINSTANCE.getDBColumn_ColumnType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl <em>DB Column Type</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBColumnType()
		 * @generated
		 */
		EClass DB_COLUMN_TYPE = eINSTANCE.getDBColumnType();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN_TYPE__NAME = eINSTANCE.getDBColumnType_Name();

		/**
		 * The meta object literal for the '<em><b>Omit Size Information</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION = eINSTANCE.getDBColumnType_OmitSizeInformation();

		/**
		 * The meta object literal for the '<em><b>Java Types</b></em>' reference list feature
		 * @generated
		 */
		EReference DB_COLUMN_TYPE__JAVA_TYPES = eINSTANCE.getDBColumnType_JavaTypes();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.DBIndexImpl <em>DB Index</em>}' class
		 * @see net.codecadenza.eclipse.model.db.impl.DBIndexImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBIndex()
		 * @generated
		 */
		EClass DB_INDEX = eINSTANCE.getDBIndex();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_INDEX__NAME = eINSTANCE.getDBIndex_Name();

		/**
		 * The meta object literal for the '<em><b>Table</b></em>' container reference feature
		 * @generated
		 */
		EReference DB_INDEX__TABLE = eINSTANCE.getDBIndex_Table();

		/**
		 * The meta object literal for the '<em><b>Unique</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_INDEX__UNIQUE = eINSTANCE.getDBIndex_Unique();

		/**
		 * The meta object literal for the '<em><b>Columns</b></em>' reference list feature
		 * @generated
		 */
		EReference DB_INDEX__COLUMNS = eINSTANCE.getDBIndex_Columns();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.DBTableImpl <em>DB Table</em>}' class
		 * @see net.codecadenza.eclipse.model.db.impl.DBTableImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBTable()
		 * @generated
		 */
		EClass DB_TABLE = eINSTANCE.getDBTable();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_TABLE__NAME = eINSTANCE.getDBTable_Name();

		/**
		 * The meta object literal for the '<em><b>Database</b></em>' reference feature
		 * @generated
		 */
		EReference DB_TABLE__DATABASE = eINSTANCE.getDBTable_Database();

		/**
		 * The meta object literal for the '<em><b>Columns</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DB_TABLE__COLUMNS = eINSTANCE.getDBTable_Columns();

		/**
		 * The meta object literal for the '<em><b>Primary Key</b></em>' containment reference feature
		 * @generated
		 */
		EReference DB_TABLE__PRIMARY_KEY = eINSTANCE.getDBTable_PrimaryKey();

		/**
		 * The meta object literal for the '<em><b>Foreign Keys</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DB_TABLE__FOREIGN_KEYS = eINSTANCE.getDBTable_ForeignKeys();

		/**
		 * The meta object literal for the '<em><b>Indexes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DB_TABLE__INDEXES = eINSTANCE.getDBTable_Indexes();

		/**
		 * The meta object literal for the '<em><b>Schema Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_TABLE__SCHEMA_NAME = eINSTANCE.getDBTable_SchemaName();

		/**
		 * The meta object literal for the '<em><b>Catalog Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DB_TABLE__CATALOG_NAME = eINSTANCE.getDBTable_CatalogName();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.DatabaseImpl <em>Database</em>}' class
		 * @see net.codecadenza.eclipse.model.db.impl.DatabaseImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDatabase()
		 * @generated
		 */
		EClass DATABASE = eINSTANCE.getDatabase();

		/**
		 * The meta object literal for the '<em><b>Database Tables</b></em>' reference list feature
		 * @generated
		 */
		EReference DATABASE__DATABASE_TABLES = eINSTANCE.getDatabase_DatabaseTables();

		/**
		 * The meta object literal for the '<em><b>Project</b></em>' container reference feature
		 * @generated
		 */
		EReference DATABASE__PROJECT = eINSTANCE.getDatabase_Project();

		/**
		 * The meta object literal for the '<em><b>Schema Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__SCHEMA_NAME = eINSTANCE.getDatabase_SchemaName();

		/**
		 * The meta object literal for the '<em><b>Catalog Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__CATALOG_NAME = eINSTANCE.getDatabase_CatalogName();

		/**
		 * The meta object literal for the '<em><b>Identifier Reg Ex</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__IDENTIFIER_REG_EX = eINSTANCE.getDatabase_IdentifierRegEx();

		/**
		 * The meta object literal for the '<em><b>Identifier Style</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__IDENTIFIER_STYLE = eINSTANCE.getDatabase_IdentifierStyle();

		/**
		 * The meta object literal for the '<em><b>Max Identifier Length</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__MAX_IDENTIFIER_LENGTH = eINSTANCE.getDatabase_MaxIdentifierLength();

		/**
		 * The meta object literal for the '<em><b>Hibernate Dialect</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__HIBERNATE_DIALECT = eINSTANCE.getDatabase_HibernateDialect();

		/**
		 * The meta object literal for the '<em><b>Eclipse Link Target DB Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__ECLIPSE_LINK_TARGET_DB_NAME = eINSTANCE.getDatabase_EclipseLinkTargetDBName();

		/**
		 * The meta object literal for the '<em><b>Reserved Words</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__RESERVED_WORDS = eINSTANCE.getDatabase_ReservedWords();

		/**
		 * The meta object literal for the '<em><b>Supports Identity Column</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__SUPPORTS_IDENTITY_COLUMN = eINSTANCE.getDatabase_SupportsIdentityColumn();

		/**
		 * The meta object literal for the '<em><b>Supports Sequence</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__SUPPORTS_SEQUENCE = eINSTANCE.getDatabase_SupportsSequence();

		/**
		 * The meta object literal for the '<em><b>Vendor Group</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATABASE__VENDOR_GROUP = eINSTANCE.getDatabase_VendorGroup();

		/**
		 * The meta object literal for the '<em><b>All Supported Column Types</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATABASE__ALL_SUPPORTED_COLUMN_TYPES = eINSTANCE.getDatabase_AllSupportedColumnTypes();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl <em>Foreign Key</em>}' class
		 * @see net.codecadenza.eclipse.model.db.impl.ForeignKeyImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getForeignKey()
		 * @generated
		 */
		EClass FOREIGN_KEY = eINSTANCE.getForeignKey();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FOREIGN_KEY__NAME = eINSTANCE.getForeignKey_Name();

		/**
		 * The meta object literal for the '<em><b>Table</b></em>' container reference feature
		 * @generated
		 */
		EReference FOREIGN_KEY__TABLE = eINSTANCE.getForeignKey_Table();

		/**
		 * The meta object literal for the '<em><b>Column</b></em>' reference feature
		 * @generated
		 */
		EReference FOREIGN_KEY__COLUMN = eINSTANCE.getForeignKey_Column();

		/**
		 * The meta object literal for the '<em><b>Referenced Column</b></em>' reference feature
		 * @generated
		 */
		EReference FOREIGN_KEY__REFERENCED_COLUMN = eINSTANCE.getForeignKey_ReferencedColumn();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.impl.PrimaryKeyImpl <em>Primary Key</em>}' class
		 * @see net.codecadenza.eclipse.model.db.impl.PrimaryKeyImpl
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getPrimaryKey()
		 * @generated
		 */
		EClass PRIMARY_KEY = eINSTANCE.getPrimaryKey();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PRIMARY_KEY__NAME = eINSTANCE.getPrimaryKey_Name();

		/**
		 * The meta object literal for the '<em><b>Table</b></em>' container reference feature
		 * @generated
		 */
		EReference PRIMARY_KEY__TABLE = eINSTANCE.getPrimaryKey_Table();

		/**
		 * The meta object literal for the '<em><b>Column</b></em>' reference feature
		 * @generated
		 */
		EReference PRIMARY_KEY__COLUMN = eINSTANCE.getPrimaryKey_Column();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration <em>DB Vendor Group
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getDBVendorGroupEnumeration()
		 * @generated
		 */
		EEnum DB_VENDOR_GROUP_ENUMERATION = eINSTANCE.getDBVendorGroupEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration <em>Identifier Style
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration
		 * @see net.codecadenza.eclipse.model.db.impl.DbPackageImpl#getIdentifierStyleEnumeration()
		 * @generated
		 */
		EEnum IDENTIFIER_STYLE_ENUMERATION = eINSTANCE.getIdentifierStyleEnumeration();

	}

}
