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
package net.codecadenza.eclipse.model.project;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Project</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getCode <em>Code</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getServerPlatform <em>Server Platform</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getClientPlatform <em>Client Platform</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getPersistenceUnitProperties <em>Persistence Unit
 * Properties</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getDataSource <em>Data Source</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getRootNamespace <em>Root Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getRepositoryNamespace <em>Repository Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getDTONamespace <em>DTO Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getBoundaryNamespace <em>Boundary Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getDomainNamespace <em>Domain Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getClientNamespace <em>Client Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getSupportedStandardNamespaces <em>Supported Standard
 * Namespaces</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getDatabase <em>Database</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getAllSupportedTypes <em>All Supported Types</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getFormGroups <em>Form Groups</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getPersistenceProvider <em>Persistence Provider</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getJpaVersion <em>Jpa Version</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getTechnology <em>Technology</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getValidationType <em>Validation Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#isBoundaryMode <em>Boundary Mode</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getExchangeNamespace <em>Exchange Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getBuildTool <em>Build Tool</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getBuildConfiguration <em>Build Configuration</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getXmlNamespace() <em>Xml Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getIntegrationModules() <em>Integration Modules</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getMappingStrategy() <em>Mapping Strategy</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getDefaultXMLMappingType() <em>Default XML Mapping Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getXmlNamespacePrefix() <em>Xml Namespace Prefix</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Project#getTestModules() <em>Test Modules</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject()
 * @model
 * @generated
 */
public interface Project extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Code</b></em>' attribute
	 * @return the value of the '<em>Code</em>' attribute
	 * @see #setCode(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Code()
	 * @model
	 * @generated
	 */
	String getCode();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getCode <em>Code</em>}' attribute
	 * @param value the new value of the '<em>Code</em>' attribute
	 * @see #getCode()
	 * @generated
	 */
	void setCode(String value);

	/**
	 * Return the value of the '<em><b>Server Platform</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.ServerPlatformEnumeration}.
	 * @return the value of the '<em>Server Platform</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.ServerPlatformEnumeration
	 * @see #setServerPlatform(ServerPlatformEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ServerPlatform()
	 * @model
	 * @generated
	 */
	ServerPlatformEnumeration getServerPlatform();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getServerPlatform <em>Server Platform</em>}'
	 * attribute
	 * @param value the new value of the '<em>Server Platform</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.ServerPlatformEnumeration
	 * @see #getServerPlatform()
	 * @generated
	 */
	void setServerPlatform(ServerPlatformEnumeration value);

	/**
	 * Return the value of the '<em><b>Client Platform</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.ClientPlatformEnumeration}.
	 * @return the value of the '<em>Client Platform</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.ClientPlatformEnumeration
	 * @see #setClientPlatform(ClientPlatformEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ClientPlatform()
	 * @model
	 * @generated
	 */
	ClientPlatformEnumeration getClientPlatform();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getClientPlatform <em>Client Platform</em>}'
	 * attribute
	 * @param value the new value of the '<em>Client Platform</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.ClientPlatformEnumeration
	 * @see #getClientPlatform()
	 * @generated
	 */
	void setClientPlatform(ClientPlatformEnumeration value);

	/**
	 * Return the value of the '<em><b>Roles</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.Role}.
	 * @return the value of the '<em>Roles</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Roles()
	 * @model containment="true"
	 * @generated
	 */
	EList<Role> getRoles();

	/**
	 * Return the value of the '<em><b>Persistence Unit Properties</b></em>' containment reference list. The list contents are of
	 * type {@link net.codecadenza.eclipse.model.project.PersistenceUnitProperty}.
	 * @return the value of the '<em>Persistence Unit Properties</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_PersistenceUnitProperties()
	 * @model containment="true"
	 * @generated
	 */
	EList<PersistenceUnitProperty> getPersistenceUnitProperties();

	/**
	 * Return the value of the '<em><b>Data Source</b></em>' containment reference
	 * @return the value of the '<em>Data Source</em>' containment reference
	 * @see #setDataSource(Datasource)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DataSource()
	 * @model containment="true"
	 * @generated
	 */
	Datasource getDataSource();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getDataSource <em>Data Source</em>}' containment
	 * reference
	 * @param value the new value of the '<em>Data Source</em>' containment reference
	 * @see #getDataSource()
	 * @generated
	 */
	void setDataSource(Datasource value);

	/**
	 * Return the value of the '<em><b>Root Namespace</b></em>' containment reference
	 * @return the value of the '<em>Root Namespace</em>' containment reference
	 * @see #setRootNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_RootNamespace()
	 * @model containment="true"
	 * @generated
	 */
	Namespace getRootNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getRootNamespace <em>Root Namespace</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Root Namespace</em>' containment reference
	 * @see #getRootNamespace()
	 * @generated
	 */
	void setRootNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Repository Namespace</b></em>' reference
	 * @return the value of the '<em>Repository Namespace</em>' reference
	 * @see #setRepositoryNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_RepositoryNamespace()
	 * @model
	 * @generated
	 */
	Namespace getRepositoryNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getRepositoryNamespace <em>Repository
	 * Namespace</em>}' reference
	 * @param value the new value of the '<em>Repository Namespace</em>' reference
	 * @see #getRepositoryNamespace()
	 * @generated
	 */
	void setRepositoryNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>DTO Namespace</b></em>' reference
	 * @return the value of the '<em>DTO Namespace</em>' reference
	 * @see #setDTONamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DTONamespace()
	 * @model
	 * @generated
	 */
	Namespace getDTONamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getDTONamespace <em>DTO Namespace</em>}' reference
	 * @param value the new value of the '<em>DTO Namespace</em>' reference
	 * @see #getDTONamespace()
	 * @generated
	 */
	void setDTONamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Boundary Namespace</b></em>' reference
	 * @return the value of the '<em>Boundary Namespace</em>' reference
	 * @see #setBoundaryNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BoundaryNamespace()
	 * @model
	 * @generated
	 */
	Namespace getBoundaryNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getBoundaryNamespace <em>Boundary Namespace</em>}'
	 * reference
	 * @param value the new value of the '<em>Boundary Namespace</em>' reference
	 * @see #getBoundaryNamespace()
	 * @generated
	 */
	void setBoundaryNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Domain Namespace</b></em>' containment reference
	 * @return the value of the '<em>Domain Namespace</em>' containment reference
	 * @see #setDomainNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DomainNamespace()
	 * @model containment="true"
	 * @generated
	 */
	Namespace getDomainNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getDomainNamespace <em>Domain Namespace</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Domain Namespace</em>' containment reference
	 * @see #getDomainNamespace()
	 * @generated
	 */
	void setDomainNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Client Namespace</b></em>' containment reference
	 * @return the value of the '<em>Client Namespace</em>' containment reference
	 * @see #setClientNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ClientNamespace()
	 * @model containment="true"
	 * @generated
	 */
	Namespace getClientNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getClientNamespace <em>Client Namespace</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Client Namespace</em>' containment reference
	 * @see #getClientNamespace()
	 * @generated
	 */
	void setClientNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Supported Standard Namespaces</b></em>' containment reference list. The list contents are of
	 * type {@link net.codecadenza.eclipse.model.java.Namespace}.
	 * @return the value of the '<em>Supported Standard Namespaces</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_SupportedStandardNamespaces()
	 * @model containment="true"
	 * @generated
	 */
	EList<Namespace> getSupportedStandardNamespaces();

	/**
	 * Return the value of the '<em><b>Database</b></em>' containment reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.db.Database#getProject <em>Project</em>}'.
	 * @return the value of the '<em>Database</em>' containment reference
	 * @see #setDatabase(Database)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Database()
	 * @see net.codecadenza.eclipse.model.db.Database#getProject
	 * @model opposite="project" containment="true"
	 * @generated
	 */
	Database getDatabase();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getDatabase <em>Database</em>}' containment
	 * reference
	 * @param value the new value of the '<em>Database</em>' containment reference
	 * @see #getDatabase()
	 * @generated
	 */
	void setDatabase(Database value);

	/**
	 * Return the value of the '<em><b>All Supported Types</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.JavaType}.
	 * @return the value of the '<em>All Supported Types</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_AllSupportedTypes()
	 * @model containment="true"
	 * @generated
	 */
	EList<JavaType> getAllSupportedTypes();

	/**
	 * Return the value of the '<em><b>Form Groups</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormGroup}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormGroup#getProject <em>Project</em>}'.
	 * @return the value of the '<em>Form Groups</em>' reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_FormGroups()
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getProject
	 * @model opposite="project"
	 * @generated
	 */
	EList<FormGroup> getFormGroups();

	/**
	 * Return the value of the '<em><b>Persistence Provider</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration}.
	 * @return the value of the '<em>Persistence Provider</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration
	 * @see #setPersistenceProvider(PersistenceProviderEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_PersistenceProvider()
	 * @model
	 * @generated
	 */
	PersistenceProviderEnumeration getPersistenceProvider();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getPersistenceProvider <em>Persistence
	 * Provider</em>}' attribute
	 * @param value the new value of the '<em>Persistence Provider</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration
	 * @see #getPersistenceProvider()
	 * @generated
	 */
	void setPersistenceProvider(PersistenceProviderEnumeration value);

	/**
	 * Return the value of the '<em><b>Jpa Version</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.JPAVersionEnumeration}.
	 * @return the value of the '<em>Jpa Version</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.JPAVersionEnumeration
	 * @see #setJpaVersion(JPAVersionEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_JpaVersion()
	 * @model
	 * @generated
	 */
	JPAVersionEnumeration getJpaVersion();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getJpaVersion <em>Jpa Version</em>}' attribute
	 * @param value the new value of the '<em>Jpa Version</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.JPAVersionEnumeration
	 * @see #getJpaVersion()
	 * @generated
	 */
	void setJpaVersion(JPAVersionEnumeration value);

	/**
	 * Return the value of the '<em><b>Technology</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration}.
	 * @return the value of the '<em>Technology</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration
	 * @see #setTechnology(TechnologyPlatformEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Technology()
	 * @model
	 * @generated
	 */
	TechnologyPlatformEnumeration getTechnology();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getTechnology <em>Technology</em>}' attribute
	 * @param value the new value of the '<em>Technology</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration
	 * @see #getTechnology()
	 * @generated
	 */
	void setTechnology(TechnologyPlatformEnumeration value);

	/**
	 * Return the value of the '<em><b>Validation Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.ValidationTypeEnumeration}.
	 * @return the value of the '<em>Validation Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.ValidationTypeEnumeration
	 * @see #setValidationType(ValidationTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ValidationType()
	 * @model
	 * @generated
	 */
	ValidationTypeEnumeration getValidationType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getValidationType <em>Validation Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Validation Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.ValidationTypeEnumeration
	 * @see #getValidationType()
	 * @generated
	 */
	void setValidationType(ValidationTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Boundary Mode</b></em>' attribute
	 * @return the value of the '<em>Boundary Mode</em>' attribute
	 * @see #setBoundaryMode(boolean)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BoundaryMode()
	 * @model
	 * @generated
	 */
	boolean isBoundaryMode();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#isBoundaryMode <em>Boundary Mode</em>}' attribute
	 * @param value the new value of the '<em>Boundary Mode</em>' attribute
	 * @see #isBoundaryMode()
	 * @generated
	 */
	void setBoundaryMode(boolean value);

	/**
	 * Return the value of the '<em><b>Exchange Namespace</b></em>' reference
	 * @return the value of the '<em>Exchange Namespace</em>' reference
	 * @see #setExchangeNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ExchangeNamespace()
	 * @model
	 * @generated
	 */
	Namespace getExchangeNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getExchangeNamespace <em>Exchange Namespace</em>}'
	 * reference
	 * @param value the new value of the '<em>Exchange Namespace</em>' reference
	 * @see #getExchangeNamespace()
	 * @generated
	 */
	void setExchangeNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Build Tool</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.BuildToolEnumeration}.
	 * @return the value of the '<em>Build Tool</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.BuildToolEnumeration
	 * @see #setBuildTool(BuildToolEnumeration)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BuildTool()
	 * @model
	 * @generated
	 */
	BuildToolEnumeration getBuildTool();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getBuildTool <em>Build Tool</em>}' attribute
	 * @param value the new value of the '<em>Build Tool</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.BuildToolEnumeration
	 * @see #getBuildTool()
	 * @generated
	 */
	void setBuildTool(BuildToolEnumeration value);

	/**
	 * Return the value of the '<em><b>Build Configuration</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.BuildArtifact}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getProject <em>Project</em>}'.
	 * @return the value of the '<em>Build Configuration</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BuildConfiguration()
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getProject
	 * @model opposite="project" containment="true"
	 * @generated
	 */
	EList<BuildArtifact> getBuildConfiguration();

	/**
	 * Return the value of the '<em><b>Xml Namespace</b></em>' attribute
	 * @return the value of the '<em>Xml Namespace</em>' attribute
	 * @see #setXmlNamespace(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_XmlNamespace()
	 * @model
	 * @generated
	 */
	String getXmlNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getXmlNamespace <em>Xml Namespace</em>}' attribute
	 * @param value the new value of the '<em>Xml Namespace</em>' attribute
	 * @see #getXmlNamespace()
	 * @generated
	 */
	void setXmlNamespace(String value);

	/**
	 * Return the value of the '<em><b>Integration Modules</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.IntegrationModule}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getProject <em>Project</em>}'.
	 * @return the value of the '<em>Integration Modules</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_IntegrationModules()
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getProject
	 * @model opposite="project" containment="true"
	 * @generated
	 */
	EList<IntegrationModule> getIntegrationModules();

	/**
	 * Return the value of the '<em><b>Mapping Strategy</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.MappingAnnotationStrategy}.
	 * @return the value of the '<em>Mapping Strategy</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.MappingAnnotationStrategy
	 * @see #setMappingStrategy(MappingAnnotationStrategy)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_MappingStrategy()
	 * @model
	 * @generated
	 */
	MappingAnnotationStrategy getMappingStrategy();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getMappingStrategy <em>Mapping Strategy</em>}'
	 * attribute
	 * @param value the new value of the '<em>Mapping Strategy</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.MappingAnnotationStrategy
	 * @see #getMappingStrategy()
	 * @generated
	 */
	void setMappingStrategy(MappingAnnotationStrategy value);

	/**
	 * Return the value of the '<em><b>Default XML Mapping Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.XMLMappingType}.
	 * @return the value of the '<em>Default XML Mapping Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.XMLMappingType
	 * @see #setDefaultXMLMappingType(XMLMappingType)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DefaultXMLMappingType()
	 * @model
	 * @generated
	 */
	XMLMappingType getDefaultXMLMappingType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getDefaultXMLMappingType <em>Default XML Mapping
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Default XML Mapping Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.XMLMappingType
	 * @see #getDefaultXMLMappingType()
	 * @generated
	 */
	void setDefaultXMLMappingType(XMLMappingType value);

	/**
	 * Return the value of the '<em><b>Xml Namespace Prefix</b></em>' attribute
	 * @return the value of the '<em>Xml Namespace Prefix</em>' attribute
	 * @see #setXmlNamespacePrefix(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_XmlNamespacePrefix()
	 * @model
	 * @generated
	 */
	String getXmlNamespacePrefix();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#getXmlNamespacePrefix <em>Xml Namespace
	 * Prefix</em>}' attribute
	 * @param value the new value of the '<em>Xml Namespace Prefix</em>' attribute
	 * @see #getXmlNamespacePrefix()
	 * @generated
	 */
	void setXmlNamespacePrefix(String value);

	/**
	 * Return the value of the '<em><b>Test Modules</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.AbstractTestModule}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject <em>Project</em>}'.
	 * @return the value of the '<em>Test Modules</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_TestModules()
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject
	 * @model opposite="project" containment="true"
	 * @generated
	 */
	EList<AbstractTestModule> getTestModules();

	/**
	 * Return the value of the '<em><b>Protect Manual Changes</b></em>' attribute
	 * @return the value of the '<em>Protect Manual Changes</em>' attribute
	 * @see #setProtectManualChanges(boolean)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ProtectManualChanges()
	 * @model
	 * @generated
	 */
	boolean isProtectManualChanges();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Project#isProtectManualChanges <em>Protect Manual
	 * Changes</em>}' attribute
	 * @param value the new value of the '<em>Protect Manual Changes</em>' attribute
	 * @see #isProtectManualChanges()
	 * @generated
	 */
	void setProtectManualChanges(boolean value);

	/**
	 * Get a Java type by its name
	 * @param name
	 * @return the Java type, or null if the Java type could not be found
	 * @generated not
	 */
	JavaType getJavaTypeByName(String name);

	/**
	 * @return all repositories of this project
	 * @generated not
	 */
	EList<Repository> getAllRepositoriesOfProject();

	/**
	 * @return all data transfer objects of this project
	 * @generated not
	 */
	EList<DTOBean> getAllDTOsOfProject();

	/**
	 * @return all boundaries of this project
	 * @generated not
	 */
	EList<BoundaryBean> getAllBoundariesOfProject();

	/**
	 * @param domainObject
	 * @return the boundary bean for a given domain object or null if no boundary bean exists
	 * @generated not
	 */
	BoundaryBean getBoundaryByDomainObject(DomainObject domainObject);

	/**
	 * @return the log-on DTO
	 * @generated not
	 */
	DTOBean getApplicationLogOnDTO();

	/**
	 * Recursive helper method to get all list-of-values forms of all form groups
	 * @param group
	 * @param tempFormList
	 * @generated not
	 */
	void getLOVFormsOfGroup(FormGroup group, EList<Form> tempFormList);

	/**
	 * @return all list-of-values forms of this project
	 * @generated not
	 */
	EList<Form> getLOVFormsOfProject();

	/**
	 * @return all grid panels of this project
	 * @generated not
	 */
	EList<FormPanel> getAllGridPanelsOfProject();

	/**
	 * Get all domain objects of this project
	 * @param includingSuperclasses
	 * @param includingAbstractClasses
	 * @return a list with all domain objects
	 * @generated not
	 */
	EList<DomainObject> getAllDomainObjectsOfProject(boolean includingSuperclasses, boolean includingAbstractClasses);

	/**
	 * @return a list of valid enumeration tags
	 * @generated not
	 */
	ArrayList<String> getValidEnumTags();

	/**
	 * @param isAbstract
	 * @param isMappedSuperClass
	 * @return a list containing all valid domain object tags
	 * @generated not
	 */
	ArrayList<String> getValidDomainObjectTags(boolean isAbstract, boolean isMappedSuperClass);

	/**
	 * Get a domain object by the given tag
	 * @param tag
	 * @return the domain object with the given tag, or null if a domain object with this tag doesn't exist
	 * @generated not
	 */
	DomainObject getDomainObjectByTag(DomainTagEnumeration tag);

	/**
	 * @return all forms of this project
	 * @generated not
	 */
	EList<Form> getAllFormsOfProject();

	/**
	 * Get all data transfer objects of this domain object
	 * @param paramType
	 * @return a list of all data transfer objects
	 * @generated not
	 */
	EList<DTOBean> getDTOsOfDomainObject(DomainObject paramType);

	/**
	 * Get the boundary bean that contains the log-on method
	 * @return the boundary bean that contains the log-on method, or null if this method could not be found
	 * @generated not
	 */
	BoundaryBean getLogOnBoundary();

	/**
	 * Search mappable Java types by the given name
	 * @param name
	 * @param doSort
	 * @return a list containing all types whose names begin with the provided name
	 * @generated not
	 */
	EList<JavaType> searchMappableTypeByName(String name, boolean doSort);

	/**
	 * Search DTOs by the given name
	 * @param name
	 * @param doSort
	 * @return a list containing all DTOs whose names begin with the provided name
	 * @generated not
	 */
	EList<DTOBean> searchDTOByName(String name, boolean doSort);

	/**
	 * Search domain objects by the given name
	 * @param name
	 * @param doSort
	 * @param includeAbstractClasses
	 * @param includeMappedSuperClasses
	 * @return a list containing all domain objects whose names begin with the provided name
	 * @generated not
	 */
	EList<DomainObject> searchDomainObjectByName(String name, boolean doSort, boolean includeAbstractClasses,
			boolean includeMappedSuperClasses);

	/**
	 * Search Java types with the given name
	 * @param name
	 * @return a list containing all types whose names begin with the provided name
	 * @generated not
	 */
	EList<JavaType> searchTypeByName(String name);

	/**
	 * Check if this project supports mandating
	 * @return true if this project supports mandating
	 * @generated not
	 */
	boolean isMandatingSupported();

	/**
	 * @param domainObject
	 * @return a list containing all mapping objects that belong to the given domain object
	 * @generated not
	 */
	EList<ExchangeMappingObject> getAllMappingObjectsOfDomainObject(DomainObject domainObject);

	/**
	 * @param domainObject
	 * @return a list containing all data exchange services that belong to the given domain object
	 * @generated not
	 */
	EList<DataExchangeServiceBean> getAllExchangeServicesOfDomainObject(DomainObject domainObject);

	/**
	 * @return a list containing all data exchange services
	 * @generated not
	 */
	EList<DataExchangeServiceBean> getAllExchangeServices();

	/**
	 * @return true if this project adds interfaces to boundary beans
	 * @generated not
	 */
	boolean isAddBoundaryInterface();

	/**
	 * Get the name of the workspace project due to the given artifact type
	 * @param type
	 * @return the name of the project
	 * @generated not
	 */
	String getTargetProjectName(BuildArtifactType type);

	/**
	 * @return the relative path of the source folder
	 * @generated not
	 */
	String getSourceFolder();

	/**
	 * @return the relative path of the test source folder
	 * @generated not
	 */
	String getTestSourceFolder();

	/**
	 * @return the relative path of the META-INF folder
	 * @generated not
	 */
	String getMetaInfFolder();

	/**
	 * @return the relative path of the WEB-INF folder
	 * @generated not
	 */
	String getWebInfFolder();

	/**
	 * @return the relative path of the web application folder
	 * @generated not
	 */
	String getWebAppFolder();

	/**
	 * @param type
	 * @return the relative path of the configuration folder
	 * @generated not
	 */
	String getConfigFolder(BuildArtifactType type);

	/**
	 * @return the relative path of the test resource folder
	 * @generated not
	 */
	String getTestResourceFolder();

	/**
	 * @return the relative path of the test data folder
	 * @generated not
	 */
	String getTestDataFolder();

	/**
	 * @return the relative path of the XML schema folder
	 * @generated not
	 */
	String getSchemaFolder();

	/**
	 * @return the relative path of the resource folder
	 * @generated not
	 */
	String getResourceFolder();

	/**
	 * @param artifactType
	 * @return the integration module for the given artifact type
	 * @generated not
	 */
	IntegrationModule getIntegrationModuleByArtifact(BuildArtifactType artifactType);

	/**
	 * Search for integration test cases that use the given mapping attribute
	 * @param mappingAttribute
	 * @return a list of integration test cases that use the provided mapping attribute
	 * @generated not
	 */
	Set<IntegrationTestCase> searchIntegrationTestCasesByMappingAttribute(MappingAttribute mappingAttribute);

	/**
	 * Search for integration test cases that use the given integration bean
	 * @param integrationBean
	 * @return a list of integration test cases that use the provided integration bean
	 * @generated not
	 */
	Set<IntegrationTestCase> searchIntegrationTestCasesByIntegrationBean(AbstractIntegrationBean integrationBean);

	/**
	 * Search for integration test cases that use the given integration method
	 * @param integrationMethod
	 * @return a list of integration test cases that use the provided integration method
	 * @generated not
	 */
	Set<IntegrationTestCase> searchIntegrationTestCasesByIntegrationMethod(AbstractIntegrationMethod integrationMethod);

	/**
	 * Search an integration bean by using the provided parameters
	 * @param technology
	 * @param domainObject
	 * @return an existing integration bean or null if it could not be found
	 * @generated not
	 */
	AbstractIntegrationBean searchIntegrationBean(IntegrationTechnology technology, DomainObject domainObject);

	/**
	 * @param artifactType
	 * @return true if the project build configuration contains an artifact of the given type
	 * @generated not
	 */
	boolean artifactExists(BuildArtifactType artifactType);

	/**
	 * @param artifactType
	 * @return the test module for the given artifact type
	 * @generated not
	 */
	AbstractTestModule getTestModuleByArtifact(BuildArtifactType artifactType);

	/**
	 * @return a list containing all GUI test cases of this project
	 * @generated not
	 */
	List<GUITestCase> getAllGUITestCases();

	/**
	 * @return the JNDI name of the data source
	 * @generated not
	 */
	String getDataSourceJNDIName();

	/**
	 * @return the name of the data source file. It will return null if such a file is not necessary for this project!
	 * @generated not
	 */
	String getDataSourceFileName();

	/**
	 * @return true if the project has a graphical user interface
	 * @generated not
	 */
	boolean hasClient();

	/**
	 * @return true if the project has an Angular client
	 * @generated not
	 */
	boolean hasAngularClient();

	/**
	 * @return true if the project has either an Eclipse RCP or an Eclipse RAP client
	 * @generated not
	 */
	boolean hasEclipseClient();

	/**
	 * @return true if the project has a JavaFX client
	 * @generated not
	 */
	boolean hasJavaFXClient();

	/**
	 * @return true if the project has a JSF client
	 * @generated not
	 */
	boolean hasJSFClient();

	/**
	 * @return true if the project has either a JSF or a Vaadin client
	 * @generated not
	 */
	boolean hasJSFOrVaadinClient();

	/**
	 * @return true if the project has an Eclipse RAP client
	 * @generated not
	 */
	boolean hasRAPClient();

	/**
	 * @return true if the project has an Eclipse RCP client
	 * @generated not
	 */
	boolean hasRCPClient();

	/**
	 * @return true if the project has a Swing client
	 * @generated not
	 */
	boolean hasSwingClient();

	/**
	 * @return true if the project has a Vaadin client
	 * @generated not
	 */
	boolean hasVaadinClient();

	/**
	 * @return true if the project is a Jakarta EE application
	 * @generated not
	 */
	boolean isJakartaEEApplication();

	/**
	 * @return true if the project is a Java SE application
	 * @generated not
	 */
	boolean isJavaSEApplication();

	/**
	 * @return true if the project is a Spring Boot application
	 * @generated not
	 */
	boolean isSpringBootApplication();

	/**
	 * @return true if the project is deployed on either Glassfish or Payara
	 * @generated not
	 */
	boolean isDeployedOnGlassfish();

	/**
	 * @return true if the project is deployed on either JBoss or Wildfly
	 * @generated not
	 */
	boolean isDeployedOnJBoss();

	/**
	 * @return true if the project is deployed on Apache Tomcat
	 * @generated not
	 */
	boolean isDeployedOnTomcat();

}
