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
 * @see net.codecadenza.eclipse.model.project.ProjectFactory
 * @model kind="package"
 * @generated
 */
public interface ProjectPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "project";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/project.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.project";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	ProjectPackage eINSTANCE = net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl <em>Datasource</em>}' class
	 * @see net.codecadenza.eclipse.model.project.impl.DatasourceImpl
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getDatasource()
	 * @generated
	 */
	int DATASOURCE = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATASOURCE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Connection URL</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATASOURCE__CONNECTION_URL = 1;

	/**
	 * The feature ID for the '<em><b>User Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATASOURCE__USER_NAME = 2;

	/**
	 * The feature ID for the '<em><b>Password</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATASOURCE__PASSWORD = 3;

	/**
	 * The feature ID for the '<em><b>Driver Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATASOURCE__DRIVER_NAME = 4;

	/**
	 * The feature ID for the '<em><b>Driver List</b></em>' attribute list
	 * @generated
	 * @ordered
	 */
	int DATASOURCE__DRIVER_LIST = 5;

	/**
	 * The number of structural features of the '<em>Datasource</em>' class
	 * @generated
	 * @ordered
	 */
	int DATASOURCE_FEATURE_COUNT = 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.impl.PersistenceUnitPropertyImpl <em>Persistence Unit
	 * Property</em>}' class
	 * @see net.codecadenza.eclipse.model.project.impl.PersistenceUnitPropertyImpl
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getPersistenceUnitProperty()
	 * @generated
	 */
	int PERSISTENCE_UNIT_PROPERTY = 1;

	/**
	 * The feature ID for the '<em><b>Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PERSISTENCE_UNIT_PROPERTY__VALUE = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PERSISTENCE_UNIT_PROPERTY__NAME = 1;

	/**
	 * The number of structural features of the '<em>Persistence Unit Property</em>' class
	 * @generated
	 * @ordered
	 */
	int PERSISTENCE_UNIT_PROPERTY_FEATURE_COUNT = 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl <em>Project</em>}' class
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectImpl
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getProject()
	 * @generated
	 */
	int PROJECT = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Code</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__CODE = 1;

	/**
	 * The feature ID for the '<em><b>Server Platform</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__SERVER_PLATFORM = 2;

	/**
	 * The feature ID for the '<em><b>Client Platform</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__CLIENT_PLATFORM = 3;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__ROLES = 4;

	/**
	 * The feature ID for the '<em><b>Persistence Unit Properties</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__PERSISTENCE_UNIT_PROPERTIES = 5;

	/**
	 * The feature ID for the '<em><b>Data Source</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__DATA_SOURCE = 6;

	/**
	 * The feature ID for the '<em><b>Root Namespace</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__ROOT_NAMESPACE = 7;

	/**
	 * The feature ID for the '<em><b>Repository Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__REPOSITORY_NAMESPACE = 8;

	/**
	 * The feature ID for the '<em><b>DTO Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__DTO_NAMESPACE = 9;

	/**
	 * The feature ID for the '<em><b>Boundary Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__BOUNDARY_NAMESPACE = 10;

	/**
	 * The feature ID for the '<em><b>Domain Namespace</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__DOMAIN_NAMESPACE = 11;

	/**
	 * The feature ID for the '<em><b>Client Namespace</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__CLIENT_NAMESPACE = 12;

	/**
	 * The feature ID for the '<em><b>Supported Standard Namespaces</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__SUPPORTED_STANDARD_NAMESPACES = 13;

	/**
	 * The feature ID for the '<em><b>Database</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__DATABASE = 14;

	/**
	 * The feature ID for the '<em><b>All Supported Types</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__ALL_SUPPORTED_TYPES = 15;

	/**
	 * The feature ID for the '<em><b>Form Groups</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__FORM_GROUPS = 16;

	/**
	 * The feature ID for the '<em><b>Persistence Provider</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__PERSISTENCE_PROVIDER = 17;

	/**
	 * The feature ID for the '<em><b>Jpa Version</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__JPA_VERSION = 18;

	/**
	 * The feature ID for the '<em><b>Technology</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__TECHNOLOGY = 19;

	/**
	 * The feature ID for the '<em><b>Validation Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__VALIDATION_TYPE = 20;

	/**
	 * The feature ID for the '<em><b>Boundary Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__BOUNDARY_MODE = 21;

	/**
	 * The feature ID for the '<em><b>Exchange Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int PROJECT__EXCHANGE_NAMESPACE = 22;

	/**
	 * The feature ID for the '<em><b>Build Tool</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__BUILD_TOOL = 23;

	/**
	 * The feature ID for the '<em><b>Build Configuration</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__BUILD_CONFIGURATION = 24;

	/**
	 * The feature ID for the '<em><b>Xml Namespace</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__XML_NAMESPACE = 25;

	/**
	 * The feature ID for the '<em><b>Integration Modules</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__INTEGRATION_MODULES = 26;

	/**
	 * The feature ID for the '<em><b>Mapping Strategy</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__MAPPING_STRATEGY = 27;

	/**
	 * The feature ID for the '<em><b>Default XML Mapping Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__DEFAULT_XML_MAPPING_TYPE = 28;

	/**
	 * The feature ID for the '<em><b>Xml Namespace Prefix</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__XML_NAMESPACE_PREFIX = 29;

	/**
	 * The feature ID for the '<em><b>Test Modules</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int PROJECT__TEST_MODULES = 30;

	/**
	 * The feature ID for the '<em><b>Protect Manual Changes</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int PROJECT__PROTECT_MANUAL_CHANGES = 31;

	/**
	 * The number of structural features of the '<em>Project</em>' class
	 * @generated
	 * @ordered
	 */
	int PROJECT_FEATURE_COUNT = 32;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.impl.RoleImpl <em>Role</em>}' class
	 * @see net.codecadenza.eclipse.model.project.impl.RoleImpl
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getRole()
	 * @generated
	 */
	int ROLE = 3;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ROLE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Admin Role</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ROLE__ADMIN_ROLE = 1;

	/**
	 * The feature ID for the '<em><b>Readonly Role</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ROLE__READONLY_ROLE = 2;

	/**
	 * The number of structural features of the '<em>Role</em>' class
	 * @generated
	 * @ordered
	 */
	int ROLE_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl <em>Build Artifact</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getBuildArtifact()
	 * @generated
	 */
	int BUILD_ARTIFACT = 4;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BUILD_ARTIFACT__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BUILD_ARTIFACT__TYPE = 1;

	/**
	 * The feature ID for the '<em><b>Contained Artifacts</b></em>' attribute list
	 * @generated
	 * @ordered
	 */
	int BUILD_ARTIFACT__CONTAINED_ARTIFACTS = 2;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int BUILD_ARTIFACT__PROJECT = 3;

	/**
	 * The number of structural features of the '<em>Build Artifact</em>' class
	 * @generated
	 * @ordered
	 */
	int BUILD_ARTIFACT_FEATURE_COUNT = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl <em>Integration
	 * Module</em>}' class
	 * @see net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getIntegrationModule()
	 * @generated
	 */
	int INTEGRATION_MODULE = 5;

	/**
	 * The feature ID for the '<em><b>Technology</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_MODULE__TECHNOLOGY = 0;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_MODULE__PROJECT = 1;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_MODULE__NAMESPACE = 2;

	/**
	 * The feature ID for the '<em><b>Add Security Handler</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_MODULE__ADD_SECURITY_HANDLER = 3;

	/**
	 * The feature ID for the '<em><b>Add Producers</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_MODULE__ADD_PRODUCERS = 4;

	/**
	 * The number of structural features of the '<em>Integration Module</em>' class
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_MODULE_FEATURE_COUNT = 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.ClientPlatformEnumeration <em>Client Platform
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.ClientPlatformEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getClientPlatformEnumeration()
	 * @generated
	 */
	int CLIENT_PLATFORM_ENUMERATION = 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.ServerPlatformEnumeration <em>Server Platform
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.ServerPlatformEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getServerPlatformEnumeration()
	 * @generated
	 */
	int SERVER_PLATFORM_ENUMERATION = 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration <em>Persistence
	 * Provider Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getPersistenceProviderEnumeration()
	 * @generated
	 */
	int PERSISTENCE_PROVIDER_ENUMERATION = 8;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.JPAVersionEnumeration <em>JPA Version
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.JPAVersionEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getJPAVersionEnumeration()
	 * @generated
	 */
	int JPA_VERSION_ENUMERATION = 9;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration <em>Technology Platform
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getTechnologyPlatformEnumeration()
	 * @generated
	 */
	int TECHNOLOGY_PLATFORM_ENUMERATION = 10;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.ValidationTypeEnumeration <em>Validation Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.ValidationTypeEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getValidationTypeEnumeration()
	 * @generated
	 */
	int VALIDATION_TYPE_ENUMERATION = 11;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.BuildToolEnumeration <em>Build Tool
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.BuildToolEnumeration
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getBuildToolEnumeration()
	 * @generated
	 */
	int BUILD_TOOL_ENUMERATION = 12;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.BuildArtifactType <em>Build Artifact Type</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.BuildArtifactType
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getBuildArtifactType()
	 * @generated
	 */
	int BUILD_ARTIFACT_TYPE = 13;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.MappingAnnotationStrategy <em>Mapping Annotation
	 * Strategy</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.MappingAnnotationStrategy
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getMappingAnnotationStrategy()
	 * @generated
	 */
	int MAPPING_ANNOTATION_STRATEGY = 14;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.XMLMappingType <em>XML Mapping Type</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.XMLMappingType
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getXMLMappingType()
	 * @generated
	 */
	int XML_MAPPING_TYPE = 15;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.project.IntegrationTechnology <em>Integration
	 * Technology</em>}' enum
	 * @see net.codecadenza.eclipse.model.project.IntegrationTechnology
	 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getIntegrationTechnology()
	 * @generated
	 */
	int INTEGRATION_TECHNOLOGY = 16;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.project.Datasource <em>Datasource</em>}'
	 * @return the meta object for class '<em>Datasource</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource
	 * @generated
	 */
	EClass getDatasource();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Datasource#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource#getName()
	 * @see #getDatasource()
	 * @generated
	 */
	EAttribute getDatasource_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Datasource#getConnectionURL
	 * <em>Connection URL</em>}'
	 * @return the meta object for the attribute '<em>Connection URL</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource#getConnectionURL()
	 * @see #getDatasource()
	 * @generated
	 */
	EAttribute getDatasource_ConnectionURL();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Datasource#getUserName <em>User
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>User Name</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource#getUserName()
	 * @see #getDatasource()
	 * @generated
	 */
	EAttribute getDatasource_UserName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Datasource#getPassword
	 * <em>Password</em>}'
	 * @return the meta object for the attribute '<em>Password</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource#getPassword()
	 * @see #getDatasource()
	 * @generated
	 */
	EAttribute getDatasource_Password();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Datasource#getDriverName <em>Driver
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>Driver Name</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource#getDriverName()
	 * @see #getDatasource()
	 * @generated
	 */
	EAttribute getDatasource_DriverName();

	/**
	 * Return the meta object for the attribute list '{@link net.codecadenza.eclipse.model.project.Datasource#getDriverList
	 * <em>Driver List</em>}'
	 * @return the meta object for the attribute list '<em>Driver List</em>'
	 * @see net.codecadenza.eclipse.model.project.Datasource#getDriverList()
	 * @see #getDatasource()
	 * @generated
	 */
	EAttribute getDatasource_DriverList();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.project.PersistenceUnitProperty <em>Persistence Unit
	 * Property</em>}'
	 * @return the meta object for class '<em>Persistence Unit Property</em>'
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty
	 * @generated
	 */
	EClass getPersistenceUnitProperty();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.PersistenceUnitProperty#getValue
	 * <em>Value</em>}'
	 * @return the meta object for the attribute '<em>Value</em>'
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty#getValue()
	 * @see #getPersistenceUnitProperty()
	 * @generated
	 */
	EAttribute getPersistenceUnitProperty_Value();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.PersistenceUnitProperty#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty#getName()
	 * @see #getPersistenceUnitProperty()
	 * @generated
	 */
	EAttribute getPersistenceUnitProperty_Name();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.project.Project <em>Project</em>}'
	 * @return the meta object for class '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.project.Project
	 * @generated
	 */
	EClass getProject();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getName()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getCode <em>Code</em>}'
	 * @return the meta object for the attribute '<em>Code</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getCode()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_Code();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getServerPlatform <em>Server
	 * Platform</em>}'
	 * @return the meta object for the attribute '<em>Server Platform</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getServerPlatform()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_ServerPlatform();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getClientPlatform <em>Client
	 * Platform</em>}'
	 * @return the meta object for the attribute '<em>Client Platform</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getClientPlatform()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_ClientPlatform();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.project.Project#getRoles
	 * <em>Roles</em>}'
	 * @return the meta object for the containment reference list '<em>Roles</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getRoles()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_Roles();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.project.Project#getPersistenceUnitProperties <em>Persistence Unit Properties</em>}'
	 * @return the meta object for the containment reference list '<em>Persistence Unit Properties</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getPersistenceUnitProperties()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_PersistenceUnitProperties();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.project.Project#getDataSource
	 * <em>Data Source</em>}'
	 * @return the meta object for the containment reference '<em>Data Source</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getDataSource()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_DataSource();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.project.Project#getRootNamespace
	 * <em>Root Namespace</em>}'
	 * @return the meta object for the containment reference '<em>Root Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getRootNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_RootNamespace();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.project.Project#getRepositoryNamespace
	 * <em>Repository Namespace</em>}'
	 * @return the meta object for the reference '<em>Repository Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getRepositoryNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_RepositoryNamespace();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.project.Project#getDTONamespace <em>DTO
	 * Namespace</em>}'
	 * @return the meta object for the reference '<em>DTO Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getDTONamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_DTONamespace();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.project.Project#getBoundaryNamespace
	 * <em>Boundary Namespace</em>}'
	 * @return the meta object for the reference '<em>Boundary Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getBoundaryNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_BoundaryNamespace();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.project.Project#getDomainNamespace
	 * <em>Domain Namespace</em>}'
	 * @return the meta object for the containment reference '<em>Domain Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getDomainNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_DomainNamespace();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.project.Project#getClientNamespace
	 * <em>Client Namespace</em>}'
	 * @return the meta object for the containment reference '<em>Client Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getClientNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_ClientNamespace();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.project.Project#getSupportedStandardNamespaces <em>Supported Standard Namespaces</em>}'
	 * @return the meta object for the containment reference list '<em>Supported Standard Namespaces</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getSupportedStandardNamespaces()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_SupportedStandardNamespaces();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.project.Project#getDatabase
	 * <em>Database</em>}'
	 * @return the meta object for the containment reference '<em>Database</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getDatabase()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_Database();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.project.Project#getAllSupportedTypes <em>All Supported Types</em>}'
	 * @return the meta object for the containment reference list '<em>All Supported Types</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getAllSupportedTypes()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_AllSupportedTypes();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.project.Project#getFormGroups <em>Form
	 * Groups</em>}'
	 * @return the meta object for the reference list '<em>Form Groups</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getFormGroups()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_FormGroups();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getPersistenceProvider
	 * <em>Persistence Provider</em>}'
	 * @return the meta object for the attribute '<em>Persistence Provider</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getPersistenceProvider()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_PersistenceProvider();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getJpaVersion <em>Jpa
	 * Version</em>}'
	 * @return the meta object for the attribute '<em>Jpa Version</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getJpaVersion()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_JpaVersion();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getTechnology
	 * <em>Technology</em>}'
	 * @return the meta object for the attribute '<em>Technology</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getTechnology()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_Technology();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getValidationType
	 * <em>Validation Type</em>}'
	 * @return the meta object for the attribute '<em>Validation Type</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getValidationType()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_ValidationType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#isBoundaryMode <em>Boundary
	 * Mode</em>}'
	 * @return the meta object for the attribute '<em>Boundary Mode</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#isBoundaryMode()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_BoundaryMode();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.project.Project#getExchangeNamespace
	 * <em>Exchange Namespace</em>}'
	 * @return the meta object for the reference '<em>Exchange Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getExchangeNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_ExchangeNamespace();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getBuildTool <em>Build
	 * Tool</em>}'
	 * @return the meta object for the attribute '<em>Build Tool</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getBuildTool()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_BuildTool();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.project.Project#getBuildConfiguration <em>Build Configuration</em>}'
	 * @return the meta object for the containment reference list '<em>Build Configuration</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getBuildConfiguration()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_BuildConfiguration();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getXmlNamespace <em>Xml
	 * Namespace</em>}'
	 * @return the meta object for the attribute '<em>Xml Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getXmlNamespace()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_XmlNamespace();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.project.Project#getTestModules
	 * <em>Test Modules</em>}'
	 * @return the meta object for the containment reference list '<em>Test Modules</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getTestModules()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_TestModules();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#isProtectManualChanges
	 * <em>Protect Manual Changes</em>}'
	 * @return the meta object for the attribute '<em>Protect Manual Changes</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#isProtectManualChanges()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_ProtectManualChanges();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.project.Project#getIntegrationModules <em>Integration Modules</em>}'
	 * @return the meta object for the containment reference list '<em>Integration Modules</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getIntegrationModules()
	 * @see #getProject()
	 * @generated
	 */
	EReference getProject_IntegrationModules();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getMappingStrategy <em>Mapping
	 * Strategy</em>}'
	 * @return the meta object for the attribute '<em>Mapping Strategy</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getMappingStrategy()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_MappingStrategy();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getDefaultXMLMappingType
	 * <em>Default XML Mapping Type</em>}'
	 * @return the meta object for the attribute '<em>Default XML Mapping Type</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getDefaultXMLMappingType()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_DefaultXMLMappingType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Project#getXmlNamespacePrefix <em>Xml
	 * Namespace Prefix</em>}'
	 * @return the meta object for the attribute '<em>Xml Namespace Prefix</em>'
	 * @see net.codecadenza.eclipse.model.project.Project#getXmlNamespacePrefix()
	 * @see #getProject()
	 * @generated
	 */
	EAttribute getProject_XmlNamespacePrefix();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.project.Role <em>Role</em>}'
	 * @return the meta object for class '<em>Role</em>'
	 * @see net.codecadenza.eclipse.model.project.Role
	 * @generated
	 */
	EClass getRole();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Role#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.project.Role#getName()
	 * @see #getRole()
	 * @generated
	 */
	EAttribute getRole_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Role#isAdminRole <em>Admin Role</em>}'
	 * @return the meta object for the attribute '<em>Admin Role</em>'
	 * @see net.codecadenza.eclipse.model.project.Role#isAdminRole()
	 * @see #getRole()
	 * @generated
	 */
	EAttribute getRole_AdminRole();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.Role#isReadonlyRole <em>Readonly
	 * Role</em>}'
	 * @return the meta object for the attribute '<em>Readonly Role</em>'
	 * @see net.codecadenza.eclipse.model.project.Role#isReadonlyRole()
	 * @see #getRole()
	 * @generated
	 */
	EAttribute getRole_ReadonlyRole();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.project.BuildArtifact <em>Build Artifact</em>}'
	 * @return the meta object for class '<em>Build Artifact</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact
	 * @generated
	 */
	EClass getBuildArtifact();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getName()
	 * @see #getBuildArtifact()
	 * @generated
	 */
	EAttribute getBuildArtifact_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getType <em>Type</em>}'
	 * @return the meta object for the attribute '<em>Type</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getType()
	 * @see #getBuildArtifact()
	 * @generated
	 */
	EAttribute getBuildArtifact_Type();

	/**
	 * Return the meta object for the attribute list
	 * '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getContainedArtifacts <em>Contained Artifacts</em>}'
	 * @return the meta object for the attribute list '<em>Contained Artifacts</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getContainedArtifacts()
	 * @see #getBuildArtifact()
	 * @generated
	 */
	EAttribute getBuildArtifact_ContainedArtifacts();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getProject
	 * <em>Project</em>}'
	 * @return the meta object for the container reference '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getProject()
	 * @see #getBuildArtifact()
	 * @generated
	 */
	EReference getBuildArtifact_Project();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.project.IntegrationModule <em>Integration Module</em>}'
	 * @return the meta object for class '<em>Integration Module</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule
	 * @generated
	 */
	EClass getIntegrationModule();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getTechnology
	 * <em>Technology</em>}'
	 * @return the meta object for the attribute '<em>Technology</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getTechnology()
	 * @see #getIntegrationModule()
	 * @generated
	 */
	EAttribute getIntegrationModule_Technology();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getProject
	 * <em>Project</em>}'
	 * @return the meta object for the container reference '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getProject()
	 * @see #getIntegrationModule()
	 * @generated
	 */
	EReference getIntegrationModule_Project();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getNamespace
	 * <em>Namespace</em>}'
	 * @return the meta object for the reference '<em>Namespace</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getNamespace()
	 * @see #getIntegrationModule()
	 * @generated
	 */
	EReference getIntegrationModule_Namespace();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.IntegrationModule#isAddSecurityHandler
	 * <em>Add Security Handler</em>}'
	 * @return the meta object for the attribute '<em>Add Security Handler</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#isAddSecurityHandler()
	 * @see #getIntegrationModule()
	 * @generated
	 */
	EAttribute getIntegrationModule_AddSecurityHandler();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.project.IntegrationModule#isAddProducers
	 * <em>Add Producers</em>}'
	 * @return the meta object for the attribute '<em>Add Producers</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#isAddProducers()
	 * @see #getIntegrationModule()
	 * @generated
	 */
	EAttribute getIntegrationModule_AddProducers();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.ClientPlatformEnumeration <em>Client Platform
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Client Platform Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.ClientPlatformEnumeration
	 * @generated
	 */
	EEnum getClientPlatformEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.ServerPlatformEnumeration <em>Server Platform
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Server Platform Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.ServerPlatformEnumeration
	 * @generated
	 */
	EEnum getServerPlatformEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration <em>Persistence
	 * Provider Enumeration</em>}'
	 * @return the meta object for enum '<em>Persistence Provider Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration
	 * @generated
	 */
	EEnum getPersistenceProviderEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.JPAVersionEnumeration <em>JPA Version
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>JPA Version Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.JPAVersionEnumeration
	 * @generated
	 */
	EEnum getJPAVersionEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration <em>Technology
	 * Platform Enumeration</em>}'
	 * @return the meta object for enum '<em>Technology Platform Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration
	 * @generated
	 */
	EEnum getTechnologyPlatformEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.ValidationTypeEnumeration <em>Validation Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Validation Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.ValidationTypeEnumeration
	 * @generated
	 */
	EEnum getValidationTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.BuildToolEnumeration <em>Build Tool
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Build Tool Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildToolEnumeration
	 * @generated
	 */
	EEnum getBuildToolEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.BuildArtifactType <em>Build Artifact Type</em>}'
	 * @return the meta object for enum '<em>Build Artifact Type</em>'
	 * @see net.codecadenza.eclipse.model.project.BuildArtifactType
	 * @generated
	 */
	EEnum getBuildArtifactType();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.MappingAnnotationStrategy <em>Mapping Annotation
	 * Strategy</em>}'
	 * @return the meta object for enum '<em>Mapping Annotation Strategy</em>'
	 * @see net.codecadenza.eclipse.model.project.MappingAnnotationStrategy
	 * @generated
	 */
	EEnum getMappingAnnotationStrategy();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.XMLMappingType <em>XML Mapping Type</em>}'
	 * @return the meta object for enum '<em>XML Mapping Type</em>'
	 * @see net.codecadenza.eclipse.model.project.XMLMappingType
	 * @generated
	 */
	EEnum getXMLMappingType();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.project.IntegrationTechnology <em>Integration
	 * Technology</em>}'
	 * @return the meta object for enum '<em>Integration Technology</em>'
	 * @see net.codecadenza.eclipse.model.project.IntegrationTechnology
	 * @generated
	 */
	EEnum getIntegrationTechnology();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	ProjectFactory getProjectFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl <em>Datasource</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.project.impl.DatasourceImpl
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getDatasource()
		 * @generated
		 */
		EClass DATASOURCE = eINSTANCE.getDatasource();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATASOURCE__NAME = eINSTANCE.getDatasource_Name();

		/**
		 * The meta object literal for the '<em><b>Connection URL</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATASOURCE__CONNECTION_URL = eINSTANCE.getDatasource_ConnectionURL();

		/**
		 * The meta object literal for the '<em><b>User Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATASOURCE__USER_NAME = eINSTANCE.getDatasource_UserName();

		/**
		 * The meta object literal for the '<em><b>Password</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATASOURCE__PASSWORD = eINSTANCE.getDatasource_Password();

		/**
		 * The meta object literal for the '<em><b>Driver Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATASOURCE__DRIVER_NAME = eINSTANCE.getDatasource_DriverName();

		/**
		 * The meta object literal for the '<em><b>Driver List</b></em>' attribute list feature
		 * @generated
		 */
		EAttribute DATASOURCE__DRIVER_LIST = eINSTANCE.getDatasource_DriverList();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.impl.PersistenceUnitPropertyImpl
		 * <em>Persistence Unit Property</em>}' class
		 * @see net.codecadenza.eclipse.model.project.impl.PersistenceUnitPropertyImpl
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getPersistenceUnitProperty()
		 * @generated
		 */
		EClass PERSISTENCE_UNIT_PROPERTY = eINSTANCE.getPersistenceUnitProperty();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PERSISTENCE_UNIT_PROPERTY__VALUE = eINSTANCE.getPersistenceUnitProperty_Value();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PERSISTENCE_UNIT_PROPERTY__NAME = eINSTANCE.getPersistenceUnitProperty_Name();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl <em>Project</em>}' class
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectImpl
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getProject()
		 * @generated
		 */
		EClass PROJECT = eINSTANCE.getProject();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__NAME = eINSTANCE.getProject_Name();

		/**
		 * The meta object literal for the '<em><b>Code</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__CODE = eINSTANCE.getProject_Code();

		/**
		 * The meta object literal for the '<em><b>Server Platform</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__SERVER_PLATFORM = eINSTANCE.getProject_ServerPlatform();

		/**
		 * The meta object literal for the '<em><b>Client Platform</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__CLIENT_PLATFORM = eINSTANCE.getProject_ClientPlatform();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__ROLES = eINSTANCE.getProject_Roles();

		/**
		 * The meta object literal for the '<em><b>Persistence Unit Properties</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__PERSISTENCE_UNIT_PROPERTIES = eINSTANCE.getProject_PersistenceUnitProperties();

		/**
		 * The meta object literal for the '<em><b>Data Source</b></em>' containment reference feature
		 * @generated
		 */
		EReference PROJECT__DATA_SOURCE = eINSTANCE.getProject_DataSource();

		/**
		 * The meta object literal for the '<em><b>Root Namespace</b></em>' containment reference feature
		 * @generated
		 */
		EReference PROJECT__ROOT_NAMESPACE = eINSTANCE.getProject_RootNamespace();

		/**
		 * The meta object literal for the '<em><b>Repository Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference PROJECT__REPOSITORY_NAMESPACE = eINSTANCE.getProject_RepositoryNamespace();

		/**
		 * The meta object literal for the '<em><b>DTO Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference PROJECT__DTO_NAMESPACE = eINSTANCE.getProject_DTONamespace();

		/**
		 * The meta object literal for the '<em><b>Boundary Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference PROJECT__BOUNDARY_NAMESPACE = eINSTANCE.getProject_BoundaryNamespace();

		/**
		 * The meta object literal for the '<em><b>Domain Namespace</b></em>' containment reference feature
		 * @generated
		 */
		EReference PROJECT__DOMAIN_NAMESPACE = eINSTANCE.getProject_DomainNamespace();

		/**
		 * The meta object literal for the '<em><b>Client Namespace</b></em>' containment reference feature
		 * @generated
		 */
		EReference PROJECT__CLIENT_NAMESPACE = eINSTANCE.getProject_ClientNamespace();

		/**
		 * The meta object literal for the '<em><b>Supported Standard Namespaces</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__SUPPORTED_STANDARD_NAMESPACES = eINSTANCE.getProject_SupportedStandardNamespaces();

		/**
		 * The meta object literal for the '<em><b>Database</b></em>' containment reference feature
		 * @generated
		 */
		EReference PROJECT__DATABASE = eINSTANCE.getProject_Database();

		/**
		 * The meta object literal for the '<em><b>All Supported Types</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__ALL_SUPPORTED_TYPES = eINSTANCE.getProject_AllSupportedTypes();

		/**
		 * The meta object literal for the '<em><b>Form Groups</b></em>' reference list feature
		 * @generated
		 */
		EReference PROJECT__FORM_GROUPS = eINSTANCE.getProject_FormGroups();

		/**
		 * The meta object literal for the '<em><b>Persistence Provider</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__PERSISTENCE_PROVIDER = eINSTANCE.getProject_PersistenceProvider();

		/**
		 * The meta object literal for the '<em><b>Jpa Version</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__JPA_VERSION = eINSTANCE.getProject_JpaVersion();

		/**
		 * The meta object literal for the '<em><b>Technology</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__TECHNOLOGY = eINSTANCE.getProject_Technology();

		/**
		 * The meta object literal for the '<em><b>Validation Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__VALIDATION_TYPE = eINSTANCE.getProject_ValidationType();

		/**
		 * The meta object literal for the '<em><b>Boundary Mode</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__BOUNDARY_MODE = eINSTANCE.getProject_BoundaryMode();

		/**
		 * The meta object literal for the '<em><b>Exchange Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference PROJECT__EXCHANGE_NAMESPACE = eINSTANCE.getProject_ExchangeNamespace();

		/**
		 * The meta object literal for the '<em><b>Build Tool</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__BUILD_TOOL = eINSTANCE.getProject_BuildTool();

		/**
		 * The meta object literal for the '<em><b>Build Configuration</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__BUILD_CONFIGURATION = eINSTANCE.getProject_BuildConfiguration();

		/**
		 * The meta object literal for the '<em><b>Xml Namespace</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__XML_NAMESPACE = eINSTANCE.getProject_XmlNamespace();

		/**
		 * The meta object literal for the '<em><b>Integration Modules</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__INTEGRATION_MODULES = eINSTANCE.getProject_IntegrationModules();

		/**
		 * The meta object literal for the '<em><b>Mapping Strategy</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__MAPPING_STRATEGY = eINSTANCE.getProject_MappingStrategy();

		/**
		 * The meta object literal for the '<em><b>Default XML Mapping Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__DEFAULT_XML_MAPPING_TYPE = eINSTANCE.getProject_DefaultXMLMappingType();

		/**
		 * The meta object literal for the '<em><b>Xml Namespace Prefix</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__XML_NAMESPACE_PREFIX = eINSTANCE.getProject_XmlNamespacePrefix();

		/**
		 * The meta object literal for the '<em><b>Test Modules</b></em>' containment reference list feature
		 * @generated
		 */
		EReference PROJECT__TEST_MODULES = eINSTANCE.getProject_TestModules();

		/**
		 * The meta object literal for the '<em><b>Protect Manual Changes</b></em>' attribute feature
		 * @generated
		 */
		EAttribute PROJECT__PROTECT_MANUAL_CHANGES = eINSTANCE.getProject_ProtectManualChanges();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.impl.RoleImpl <em>Role</em>}' class
		 * @see net.codecadenza.eclipse.model.project.impl.RoleImpl
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getRole()
		 * @generated
		 */
		EClass ROLE = eINSTANCE.getRole();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ROLE__NAME = eINSTANCE.getRole_Name();

		/**
		 * The meta object literal for the '<em><b>Admin Role</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ROLE__ADMIN_ROLE = eINSTANCE.getRole_AdminRole();

		/**
		 * The meta object literal for the '<em><b>Readonly Role</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ROLE__READONLY_ROLE = eINSTANCE.getRole_ReadonlyRole();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl <em>Build
		 * Artifact</em>}' class
		 * @see net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getBuildArtifact()
		 * @generated
		 */
		EClass BUILD_ARTIFACT = eINSTANCE.getBuildArtifact();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute BUILD_ARTIFACT__NAME = eINSTANCE.getBuildArtifact_Name();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute BUILD_ARTIFACT__TYPE = eINSTANCE.getBuildArtifact_Type();

		/**
		 * The meta object literal for the '<em><b>Contained Artifacts</b></em>' attribute list feature
		 * @generated
		 */
		EAttribute BUILD_ARTIFACT__CONTAINED_ARTIFACTS = eINSTANCE.getBuildArtifact_ContainedArtifacts();

		/**
		 * The meta object literal for the '<em><b>Project</b></em>' container reference feature
		 * @generated
		 */
		EReference BUILD_ARTIFACT__PROJECT = eINSTANCE.getBuildArtifact_Project();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl <em>Integration
		 * Module</em>}' class
		 * @see net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getIntegrationModule()
		 * @generated
		 */
		EClass INTEGRATION_MODULE = eINSTANCE.getIntegrationModule();

		/**
		 * The meta object literal for the '<em><b>Technology</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_MODULE__TECHNOLOGY = eINSTANCE.getIntegrationModule_Technology();

		/**
		 * The meta object literal for the '<em><b>Project</b></em>' container reference feature
		 * @generated
		 */
		EReference INTEGRATION_MODULE__PROJECT = eINSTANCE.getIntegrationModule_Project();

		/**
		 * The meta object literal for the '<em><b>Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference INTEGRATION_MODULE__NAMESPACE = eINSTANCE.getIntegrationModule_Namespace();

		/**
		 * The meta object literal for the '<em><b>Add Security Handler</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_MODULE__ADD_SECURITY_HANDLER = eINSTANCE.getIntegrationModule_AddSecurityHandler();

		/**
		 * The meta object literal for the '<em><b>Add Producers</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_MODULE__ADD_PRODUCERS = eINSTANCE.getIntegrationModule_AddProducers();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.ClientPlatformEnumeration <em>Client Platform
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.ClientPlatformEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getClientPlatformEnumeration()
		 * @generated
		 */
		EEnum CLIENT_PLATFORM_ENUMERATION = eINSTANCE.getClientPlatformEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.ServerPlatformEnumeration <em>Server Platform
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.ServerPlatformEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getServerPlatformEnumeration()
		 * @generated
		 */
		EEnum SERVER_PLATFORM_ENUMERATION = eINSTANCE.getServerPlatformEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration <em>Persistence
		 * Provider Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getPersistenceProviderEnumeration()
		 * @generated
		 */
		EEnum PERSISTENCE_PROVIDER_ENUMERATION = eINSTANCE.getPersistenceProviderEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.JPAVersionEnumeration <em>JPA Version
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.JPAVersionEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getJPAVersionEnumeration()
		 * @generated
		 */
		EEnum JPA_VERSION_ENUMERATION = eINSTANCE.getJPAVersionEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration <em>Technology
		 * Platform Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getTechnologyPlatformEnumeration()
		 * @generated
		 */
		EEnum TECHNOLOGY_PLATFORM_ENUMERATION = eINSTANCE.getTechnologyPlatformEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.ValidationTypeEnumeration <em>Validation Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.ValidationTypeEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getValidationTypeEnumeration()
		 * @generated
		 */
		EEnum VALIDATION_TYPE_ENUMERATION = eINSTANCE.getValidationTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.BuildToolEnumeration <em>Build Tool
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.BuildToolEnumeration
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getBuildToolEnumeration()
		 * @generated
		 */
		EEnum BUILD_TOOL_ENUMERATION = eINSTANCE.getBuildToolEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.BuildArtifactType <em>Build Artifact
		 * Type</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.BuildArtifactType
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getBuildArtifactType()
		 * @generated
		 */
		EEnum BUILD_ARTIFACT_TYPE = eINSTANCE.getBuildArtifactType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.MappingAnnotationStrategy <em>Mapping
		 * Annotation Strategy</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.MappingAnnotationStrategy
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getMappingAnnotationStrategy()
		 * @generated
		 */
		EEnum MAPPING_ANNOTATION_STRATEGY = eINSTANCE.getMappingAnnotationStrategy();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.XMLMappingType <em>XML Mapping Type</em>}'
		 * enum
		 * @see net.codecadenza.eclipse.model.project.XMLMappingType
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getXMLMappingType()
		 * @generated
		 */
		EEnum XML_MAPPING_TYPE = eINSTANCE.getXMLMappingType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.project.IntegrationTechnology <em>Integration
		 * Technology</em>}' enum
		 * @see net.codecadenza.eclipse.model.project.IntegrationTechnology
		 * @see net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl#getIntegrationTechnology()
		 * @generated
		 */
		EEnum INTEGRATION_TECHNOLOGY = eINSTANCE.getIntegrationTechnology();
	}

}
