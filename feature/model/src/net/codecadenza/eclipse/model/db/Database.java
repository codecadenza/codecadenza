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

import java.util.Set;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Database</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getDatabaseTables <em>Database Tables</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getProject <em>Project</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getSchemaName <em>Schema Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getCatalogName <em>Catalog Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getIdentifierRegEx <em>Identifier Reg Ex</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getIdentifierStyle <em>Identifier Style</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getMaxIdentifierLength <em>Max Identifier Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getHibernateDialect <em>Hibernate Dialect</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getEclipseLinkTargetDBName <em>Eclipse Link Target DB Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getReservedWords <em>Reserved Words</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#isSupportsIdentityColumn <em>Supports Identity Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#isSupportsSequence <em>Supports Sequence</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getVendorGroup <em>Vendor Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.Database#getAllSupportedColumnTypes <em>All Supported Column Types</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase()
 * @model
 * @generated
 */
public interface Database extends EObject {
	/**
	 * Return the value of the '<em><b>Database Tables</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.db.DBTable}. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.db.DBTable#getDatabase <em>Database</em>}'.
	 * @return the value of the '<em>Database Tables</em>' reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_DatabaseTables()
	 * @see net.codecadenza.eclipse.model.db.DBTable#getDatabase
	 * @model opposite="database"
	 * @generated
	 */
	EList<DBTable> getDatabaseTables();

	/**
	 * Return the value of the '<em><b>Project</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.Project#getDatabase <em>Database</em>} '.
	 * @return the value of the '<em>Project</em>' container reference
	 * @see #setProject(Project)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_Project()
	 * @see net.codecadenza.eclipse.model.project.Project#getDatabase
	 * @model opposite="database" transient="false"
	 * @generated
	 */
	Project getProject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getProject <em>Project</em>}' container reference
	 * @param value the new value of the '<em>Project</em>' container reference
	 * @see #getProject()
	 * @generated
	 */
	void setProject(Project value);

	/**
	 * Return the value of the '<em><b>Schema Name</b></em>' attribute
	 * @return the value of the '<em>Schema Name</em>' attribute
	 * @see #setSchemaName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_SchemaName()
	 * @model
	 * @generated
	 */
	String getSchemaName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getSchemaName <em>Schema Name</em>}' attribute
	 * @param value the new value of the '<em>Schema Name</em>' attribute
	 * @see #getSchemaName()
	 * @generated
	 */
	void setSchemaName(String value);

	/**
	 * Return the value of the '<em><b>Catalog Name</b></em>' attribute
	 * @return the value of the '<em>Catalog Name</em>' attribute
	 * @see #setCatalogName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_CatalogName()
	 * @model
	 * @generated
	 */
	String getCatalogName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getCatalogName <em>Catalog Name</em>}' attribute
	 * @param value the new value of the '<em>Catalog Name</em>' attribute
	 * @see #getCatalogName()
	 * @generated
	 */
	void setCatalogName(String value);

	/**
	 * Return the value of the '<em><b>Identifier Reg Ex</b></em>' attribute
	 * @return the value of the '<em>Identifier Reg Ex</em>' attribute
	 * @see #setIdentifierRegEx(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_IdentifierRegEx()
	 * @model
	 * @generated
	 */
	String getIdentifierRegEx();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getIdentifierRegEx <em>Identifier Reg Ex</em>}'
	 * attribute
	 * @param value the new value of the '<em>Identifier Reg Ex</em>' attribute
	 * @see #getIdentifierRegEx()
	 * @generated
	 */
	void setIdentifierRegEx(String value);

	/**
	 * Return the value of the '<em><b>Identifier Style</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration}.
	 * @return the value of the '<em>Identifier Style</em>' attribute
	 * @see net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration
	 * @see #setIdentifierStyle(IdentifierStyleEnumeration)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_IdentifierStyle()
	 * @model
	 * @generated
	 */
	IdentifierStyleEnumeration getIdentifierStyle();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getIdentifierStyle <em>Identifier Style</em>}'
	 * attribute
	 * @param value the new value of the '<em>Identifier Style</em>' attribute
	 * @see net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration
	 * @see #getIdentifierStyle()
	 * @generated
	 */
	void setIdentifierStyle(IdentifierStyleEnumeration value);

	/**
	 * Return the value of the '<em><b>Max Identifier Length</b></em>' attribute
	 * @return the value of the '<em>Max Identifier Length</em>' attribute
	 * @see #setMaxIdentifierLength(int)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_MaxIdentifierLength()
	 * @model
	 * @generated
	 */
	int getMaxIdentifierLength();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getMaxIdentifierLength <em>Max Identifier Length</em>}'
	 * attribute
	 * @param value the new value of the '<em>Max Identifier Length</em>' attribute
	 * @see #getMaxIdentifierLength()
	 * @generated
	 */
	void setMaxIdentifierLength(int value);

	/**
	 * Return the value of the '<em><b>Hibernate Dialect</b></em>' attribute
	 * @return the value of the '<em>Hibernate Dialect</em>' attribute
	 * @see #setHibernateDialect(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_HibernateDialect()
	 * @model
	 * @generated
	 */
	String getHibernateDialect();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getHibernateDialect <em>Hibernate Dialect</em>}'
	 * attribute
	 * @param value the new value of the '<em>Hibernate Dialect</em>' attribute
	 * @see #getHibernateDialect()
	 * @generated
	 */
	void setHibernateDialect(String value);

	/**
	 * Return the value of the '<em><b>Eclipse Link Target DB Name</b></em>' attribute
	 * @return the value of the '<em>Eclipse Link Target DB Name</em>' attribute
	 * @see #setEclipseLinkTargetDBName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_EclipseLinkTargetDBName()
	 * @model
	 * @generated
	 */
	String getEclipseLinkTargetDBName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getEclipseLinkTargetDBName <em>Eclipse Link Target DB
	 * Name</em>}' attribute
	 * @param value the new value of the '<em>Eclipse Link Target DB Name</em>' attribute
	 * @see #getEclipseLinkTargetDBName()
	 * @generated
	 */
	void setEclipseLinkTargetDBName(String value);

	/**
	 * Return the value of the '<em><b>Reserved Words</b></em>' attribute
	 * @return the value of the '<em>Reserved Words</em>' attribute
	 * @see #setReservedWords(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_ReservedWords()
	 * @model
	 * @generated
	 */
	String getReservedWords();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getReservedWords <em>Reserved Words</em>}' attribute
	 * @param value the new value of the '<em>Reserved Words</em>' attribute
	 * @see #getReservedWords()
	 * @generated
	 */
	void setReservedWords(String value);

	/**
	 * Return the value of the '<em><b>Supports Identity Column</b></em>' attribute
	 * @return the value of the '<em>Supports Identity Column</em>' attribute
	 * @see #setSupportsIdentityColumn(boolean)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_SupportsIdentityColumn()
	 * @model
	 * @generated
	 */
	boolean isSupportsIdentityColumn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#isSupportsIdentityColumn <em>Supports Identity
	 * Column</em>}' attribute
	 * @param value the new value of the '<em>Supports Identity Column</em>' attribute
	 * @see #isSupportsIdentityColumn()
	 * @generated
	 */
	void setSupportsIdentityColumn(boolean value);

	/**
	 * Return the value of the '<em><b>Supports Sequence</b></em>' attribute
	 * @return the value of the '<em>Supports Sequence</em>' attribute
	 * @see #setSupportsSequence(boolean)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_SupportsSequence()
	 * @model
	 * @generated
	 */
	boolean isSupportsSequence();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#isSupportsSequence <em>Supports Sequence</em>}'
	 * attribute
	 * @param value the new value of the '<em>Supports Sequence</em>' attribute
	 * @see #isSupportsSequence()
	 * @generated
	 */
	void setSupportsSequence(boolean value);

	/**
	 * Return the value of the '<em><b>Vendor Group</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration}.
	 * @return the value of the '<em>Vendor Group</em>' attribute
	 * @see net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration
	 * @see #setVendorGroup(DBVendorGroupEnumeration)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_VendorGroup()
	 * @model
	 * @generated
	 */
	DBVendorGroupEnumeration getVendorGroup();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.Database#getVendorGroup <em>Vendor Group</em>}' attribute
	 * @param value the new value of the '<em>Vendor Group</em>' attribute
	 * @see net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration
	 * @see #getVendorGroup()
	 * @generated
	 */
	void setVendorGroup(DBVendorGroupEnumeration value);

	/**
	 * Return the value of the '<em><b>All Supported Column Types</b></em>' containment reference list. The list contents are of
	 * type {@link net.codecadenza.eclipse.model.db.DBColumnType}.
	 * @return the value of the '<em>All Supported Column Types</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_AllSupportedColumnTypes()
	 * @model containment="true"
	 * @generated
	 */
	EList<DBColumnType> getAllSupportedColumnTypes();

	/**
	 * @return a set containing all schemas used in this database
	 * @generated not
	 */
	Set<String> getAllSchemas();

	/**
	 * @return a set containing all catalogs used in this database
	 * @generated not
	 */
	Set<String> getAllCatalogs();

}
