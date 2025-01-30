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
package net.codecadenza.eclipse.model.project.impl;

import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_DTO_NAME;
import static net.codecadenza.eclipse.shared.Constants.CLASSES_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.CONFIG_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.META_INF_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SCHEMA_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SRC_MAIN_JAVA_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SRC_MAIN_RESOURCES_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SRC_TEST_JAVA_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SRC_TEST_RESOURCES_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SRC_WEBAPP_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.TEST_DATA_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.WEB_INF_FOLDER;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.java.EnumTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.JPAVersionEnumeration;
import net.codecadenza.eclipse.model.project.MappingAnnotationStrategy;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.project.XMLMappingType;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EObjectWithInverseResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Project</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getCode <em>Code</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getServerPlatform <em>Server Platform</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getClientPlatform <em>Client Platform</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getPersistenceUnitProperties <em>Persistence Unit
 * Properties</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getDataSource <em>Data Source</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getRootNamespace <em>Root Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getRepositoryNamespace <em>Repository Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getDTONamespace <em>DTO Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getBoundaryNamespace <em>Boundary Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getDomainNamespace <em>Domain Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getClientNamespace <em>Client Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getSupportedStandardNamespaces <em>Supported Standard
 * Namespaces</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getDatabase <em>Database</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getAllSupportedTypes <em>All Supported Types</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getFormGroups <em>Form Groups</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getPersistenceProvider <em>Persistence Provider</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getJpaVersion <em>Jpa Version</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getTechnology <em>Technology</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getValidationType <em>Validation Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#isBoundaryMode <em>Boundary Mode</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getExchangeNamespace <em>Exchange Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getBuildTool <em>Build Tool</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getBuildConfiguration <em>Build Configuration</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getXmlNamespace() <em>Xml Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getIntegrationModules() <em>Integration Modules</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getMappingStrategy() <em>Mapping Strategy</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getDefaultXMLMappingType() <em>Default XML Mapping
 * Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getXmlNamespacePrefix() <em>Xml Namespace Prefix</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#isProtectManualChanges() <em>Protect Manual
 * Changes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.ProjectImpl#getTestModules() <em>Test Modules</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class ProjectImpl extends EObjectImpl implements Project {
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
	 * The default value of the '{@link #getCode() <em>Code</em>}' attribute
	 * @see #getCode()
	 * @generated
	 * @ordered
	 */
	protected static final String CODE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getCode() <em>Code</em>}' attribute
	 * @see #getCode()
	 * @generated
	 * @ordered
	 */
	protected String code = CODE_EDEFAULT;

	/**
	 * The default value of the '{@link #getServerPlatform() <em>Server Platform</em>}' attribute
	 * @see #getServerPlatform()
	 * @generated
	 * @ordered
	 */
	protected static final ServerPlatformEnumeration SERVER_PLATFORM_EDEFAULT = ServerPlatformEnumeration.JBOSS;

	/**
	 * The cached value of the '{@link #getServerPlatform() <em>Server Platform</em>}' attribute
	 * @see #getServerPlatform()
	 * @generated
	 * @ordered
	 */
	protected ServerPlatformEnumeration serverPlatform = SERVER_PLATFORM_EDEFAULT;

	/**
	 * The default value of the '{@link #getClientPlatform() <em>Client Platform</em>}' attribute
	 * @see #getClientPlatform()
	 * @generated
	 * @ordered
	 */
	protected static final ClientPlatformEnumeration CLIENT_PLATFORM_EDEFAULT = ClientPlatformEnumeration.RCP;

	/**
	 * The cached value of the '{@link #getClientPlatform() <em>Client Platform</em>}' attribute
	 * @see #getClientPlatform()
	 * @generated
	 * @ordered
	 */
	protected ClientPlatformEnumeration clientPlatform = CLIENT_PLATFORM_EDEFAULT;

	/**
	 * The cached value of the '{@link #getRoles() <em>Roles</em>}' containment reference list
	 * @see #getRoles()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> roles;

	/**
	 * The cached value of the '{@link #getPersistenceUnitProperties() <em>Persistence Unit Properties</em>}' containment reference
	 * list
	 * @see #getPersistenceUnitProperties()
	 * @generated
	 * @ordered
	 */
	protected EList<PersistenceUnitProperty> persistenceUnitProperties;

	/**
	 * The cached value of the '{@link #getDataSource() <em>Data Source</em>}' containment reference
	 * @see #getDataSource()
	 * @generated
	 * @ordered
	 */
	protected Datasource dataSource;

	/**
	 * The cached value of the '{@link #getRootNamespace() <em>Root Namespace</em>}' containment reference
	 * @see #getRootNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace rootNamespace;

	/**
	 * The cached value of the '{@link #getRepositoryNamespace() <em>Repository Namespace</em>}' reference
	 * @see #getRepositoryNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace repositoryNamespace;

	/**
	 * The cached value of the '{@link #getDTONamespace() <em>DTO Namespace</em>}' reference
	 * @see #getDTONamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace dTONamespace;

	/**
	 * The cached value of the '{@link #getBoundaryNamespace() <em>Boundary Namespace</em>}' reference
	 * @see #getBoundaryNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace boundaryNamespace;

	/**
	 * The cached value of the '{@link #getDomainNamespace() <em>Domain Namespace</em>}' containment reference
	 * @see #getDomainNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace domainNamespace;

	/**
	 * The cached value of the '{@link #getClientNamespace() <em>Client Namespace</em>}' containment reference
	 * @see #getClientNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace clientNamespace;

	/**
	 * The cached value of the '{@link #getSupportedStandardNamespaces() <em>Supported Standard Namespaces</em>}' containment
	 * reference list
	 * @see #getSupportedStandardNamespaces()
	 * @generated
	 * @ordered
	 */
	protected EList<Namespace> supportedStandardNamespaces;

	/**
	 * The cached value of the '{@link #getDatabase() <em>Database</em>}' containment reference
	 * @see #getDatabase()
	 * @generated
	 * @ordered
	 */
	protected Database database;

	/**
	 * The cached value of the '{@link #getAllSupportedTypes() <em>All Supported Types</em>}' containment reference list
	 * @see #getAllSupportedTypes()
	 * @generated
	 * @ordered
	 */
	protected EList<JavaType> allSupportedTypes;

	/**
	 * The cached value of the '{@link #getFormGroups() <em>Form Groups</em>}' reference list
	 * @see #getFormGroups()
	 * @generated
	 * @ordered
	 */
	protected EList<FormGroup> formGroups;

	/**
	 * The default value of the '{@link #getPersistenceProvider() <em>Persistence Provider</em>}' attribute
	 * @see #getPersistenceProvider()
	 * @generated
	 * @ordered
	 */
	protected static final PersistenceProviderEnumeration PERSISTENCE_PROVIDER_EDEFAULT = PersistenceProviderEnumeration.HIBERNATE;

	/**
	 * The cached value of the '{@link #getPersistenceProvider() <em>Persistence Provider</em>}' attribute
	 * @see #getPersistenceProvider()
	 * @generated
	 * @ordered
	 */
	protected PersistenceProviderEnumeration persistenceProvider = PERSISTENCE_PROVIDER_EDEFAULT;

	/**
	 * The default value of the '{@link #getJpaVersion() <em>Jpa Version</em>}' attribute
	 * @see #getJpaVersion()
	 * @generated
	 * @ordered
	 */
	protected static final JPAVersionEnumeration JPA_VERSION_EDEFAULT = JPAVersionEnumeration.JPA3;

	/**
	 * The cached value of the '{@link #getJpaVersion() <em>Jpa Version</em>}' attribute
	 * @see #getJpaVersion()
	 * @generated
	 * @ordered
	 */
	protected JPAVersionEnumeration jpaVersion = JPA_VERSION_EDEFAULT;

	/**
	 * The default value of the '{@link #getTechnology() <em>Technology</em>}' attribute
	 * @see #getTechnology()
	 * @generated
	 * @ordered
	 */
	protected static final TechnologyPlatformEnumeration TECHNOLOGY_EDEFAULT = TechnologyPlatformEnumeration.JAKARTA_EE;

	/**
	 * The cached value of the '{@link #getTechnology() <em>Technology</em>}' attribute
	 * @see #getTechnology()
	 * @generated
	 * @ordered
	 */
	protected TechnologyPlatformEnumeration technology = TECHNOLOGY_EDEFAULT;

	/**
	 * The default value of the '{@link #getValidationType() <em>Validation Type</em>}' attribute
	 * @see #getValidationType()
	 * @generated
	 * @ordered
	 */
	protected static final ValidationTypeEnumeration VALIDATION_TYPE_EDEFAULT = ValidationTypeEnumeration.INTERNAL;

	/**
	 * The cached value of the '{@link #getValidationType() <em>Validation Type</em>}' attribute
	 * @see #getValidationType()
	 * @generated
	 * @ordered
	 */
	protected ValidationTypeEnumeration validationType = VALIDATION_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #isBoundaryMode() <em>Boundary Mode</em>}' attribute
	 * @see #isBoundaryMode()
	 * @generated
	 * @ordered
	 */
	protected static final boolean BOUNDARY_MODE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isBoundaryMode() <em>Boundary Mode</em>}' attribute
	 * @see #isBoundaryMode()
	 * @generated
	 * @ordered
	 */
	protected boolean boundaryMode = BOUNDARY_MODE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getExchangeNamespace() <em>Exchange Namespace</em>}' reference
	 * @see #getExchangeNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace exchangeNamespace;

	/**
	 * The default value of the '{@link #getBuildTool() <em>Build Tool</em>}' attribute
	 * @see #getBuildTool()
	 * @generated
	 * @ordered
	 */
	protected static final BuildToolEnumeration BUILD_TOOL_EDEFAULT = BuildToolEnumeration.MAVEN;

	/**
	 * The cached value of the '{@link #getBuildTool() <em>Build Tool</em>}' attribute
	 * @see #getBuildTool()
	 * @generated
	 * @ordered
	 */
	protected BuildToolEnumeration buildTool = BUILD_TOOL_EDEFAULT;

	/**
	 * The cached value of the '{@link #getBuildConfiguration() <em>Build Configuration</em>}' containment reference list
	 * @see #getBuildConfiguration()
	 * @generated
	 * @ordered
	 */
	protected EList<BuildArtifact> buildConfiguration;

	/**
	 * The default value of the '{@link #getXmlNamespace() <em>Xml Namespace</em>}' attribute
	 * @see #getXmlNamespace()
	 * @generated
	 * @ordered
	 */
	protected static final String XML_NAMESPACE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getXmlNamespace() <em>Xml Namespace</em>}' attribute
	 * @see #getXmlNamespace()
	 * @generated
	 * @ordered
	 */
	protected String xmlNamespace = XML_NAMESPACE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getIntegrationModules() <em>Integration Modules</em>}' containment reference list
	 * @see #getIntegrationModules()
	 * @generated
	 * @ordered
	 */
	protected EList<IntegrationModule> integrationModules;

	/**
	 * The default value of the '{@link #getMappingStrategy() <em>Mapping Strategy</em>}' attribute
	 * @see #getMappingStrategy()
	 * @generated
	 * @ordered
	 */
	protected static final MappingAnnotationStrategy MAPPING_STRATEGY_EDEFAULT = MappingAnnotationStrategy.NEVER;

	/**
	 * The cached value of the '{@link #getMappingStrategy() <em>Mapping Strategy</em>}' attribute
	 * @see #getMappingStrategy()
	 * @generated
	 * @ordered
	 */
	protected MappingAnnotationStrategy mappingStrategy = MAPPING_STRATEGY_EDEFAULT;

	/**
	 * The default value of the '{@link #getDefaultXMLMappingType() <em>Default XML Mapping Type</em>}' attribute
	 * @see #getDefaultXMLMappingType()
	 * @generated
	 * @ordered
	 */
	protected static final XMLMappingType DEFAULT_XML_MAPPING_TYPE_EDEFAULT = XMLMappingType.ATTRIBUTE;

	/**
	 * The cached value of the '{@link #getDefaultXMLMappingType() <em>Default XML Mapping Type</em>}' attribute
	 * @see #getDefaultXMLMappingType()
	 * @generated
	 * @ordered
	 */
	protected XMLMappingType defaultXMLMappingType = DEFAULT_XML_MAPPING_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getXmlNamespacePrefix() <em>Xml Namespace Prefix</em>}' attribute
	 * @see #getXmlNamespacePrefix()
	 * @generated
	 * @ordered
	 */
	protected static final String XML_NAMESPACE_PREFIX_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getXmlNamespacePrefix() <em>Xml Namespace Prefix</em>}' attribute
	 * @see #getXmlNamespacePrefix()
	 * @generated
	 * @ordered
	 */
	protected String xmlNamespacePrefix = XML_NAMESPACE_PREFIX_EDEFAULT;

	/**
	 * The cached value of the '{@link #getTestModules() <em>Test Modules</em>}' containment reference list
	 * @see #getTestModules()
	 * @generated
	 * @ordered
	 */
	protected EList<AbstractTestModule> testModules;

	/**
	 * The default value of the '{@link #isProtectManualChanges() <em>Protect Manual Changes</em>}' attribute
	 * @see #isProtectManualChanges()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PROTECT_MANUAL_CHANGES_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isProtectManualChanges() <em>Protect Manual Changes</em>}' attribute
	 * @see #isProtectManualChanges()
	 * @generated
	 * @ordered
	 */
	protected boolean protectManualChanges = PROTECT_MANUAL_CHANGES_EDEFAULT;

	/**
	 * @generated
	 */
	protected ProjectImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.PROJECT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getCode()
	 * @generated
	 */
	@Override
	public String getCode() {
		return code;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setCode(java.lang.String)
	 * @generated
	 */
	@Override
	public void setCode(String newCode) {
		final String oldCode = code;
		code = newCode;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__CODE, oldCode, code));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getServerPlatform()
	 * @generated
	 */
	@Override
	public ServerPlatformEnumeration getServerPlatform() {
		return serverPlatform;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setServerPlatform(net.codecadenza.eclipse.model.project.
	 * ServerPlatformEnumeration)
	 * @generated
	 */
	@Override
	public void setServerPlatform(ServerPlatformEnumeration newServerPlatform) {
		final ServerPlatformEnumeration oldServerPlatform = serverPlatform;
		serverPlatform = newServerPlatform == null ? SERVER_PLATFORM_EDEFAULT : newServerPlatform;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__SERVER_PLATFORM, oldServerPlatform,
					serverPlatform));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getClientPlatform()
	 * @generated
	 */
	@Override
	public ClientPlatformEnumeration getClientPlatform() {
		return clientPlatform;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setClientPlatform(net.codecadenza.eclipse.model.project.
	 * ClientPlatformEnumeration)
	 * @generated
	 */
	@Override
	public void setClientPlatform(ClientPlatformEnumeration newClientPlatform) {
		final ClientPlatformEnumeration oldClientPlatform = clientPlatform;
		clientPlatform = newClientPlatform == null ? CLIENT_PLATFORM_EDEFAULT : newClientPlatform;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__CLIENT_PLATFORM, oldClientPlatform,
					clientPlatform));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getRoles()
	 * @generated
	 */
	@Override
	public EList<Role> getRoles() {
		if (roles == null)
			roles = new EObjectContainmentEList<>(Role.class, this, ProjectPackage.PROJECT__ROLES);

		return roles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getPersistenceUnitProperties()
	 * @generated
	 */
	@Override
	public EList<PersistenceUnitProperty> getPersistenceUnitProperties() {
		if (persistenceUnitProperties == null)
			persistenceUnitProperties = new EObjectContainmentEList<>(PersistenceUnitProperty.class, this,
					ProjectPackage.PROJECT__PERSISTENCE_UNIT_PROPERTIES);

		return persistenceUnitProperties;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDataSource()
	 * @generated
	 */
	@Override
	public Datasource getDataSource() {
		return dataSource;
	}

	/**
	 * @param newDataSource
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDataSource(Datasource newDataSource, NotificationChain msgs) {
		final Datasource oldDataSource = dataSource;
		dataSource = newDataSource;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DATA_SOURCE, oldDataSource,
					newDataSource);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setDataSource(net.codecadenza.eclipse.model.project.Datasource)
	 * @generated
	 */
	@Override
	public void setDataSource(Datasource newDataSource) {
		if (newDataSource != dataSource) {
			NotificationChain msgs = null;

			if (dataSource != null)
				msgs = ((InternalEObject) dataSource).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__DATA_SOURCE,
						null, msgs);

			if (newDataSource != null)
				msgs = ((InternalEObject) newDataSource).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__DATA_SOURCE,
						null, msgs);

			msgs = basicSetDataSource(newDataSource, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DATA_SOURCE, newDataSource, newDataSource));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getPersistenceProvider()
	 * @generated
	 */
	@Override
	public PersistenceProviderEnumeration getPersistenceProvider() {
		return persistenceProvider;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setPersistenceProvider(net.codecadenza.eclipse.model.project.
	 * PersistenceProviderEnumeration)
	 * @generated
	 */
	@Override
	public void setPersistenceProvider(PersistenceProviderEnumeration newPersistenceProvider) {
		final PersistenceProviderEnumeration oldPersistenceProvider = persistenceProvider;
		persistenceProvider = newPersistenceProvider == null ? PERSISTENCE_PROVIDER_EDEFAULT : newPersistenceProvider;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__PERSISTENCE_PROVIDER, oldPersistenceProvider,
					persistenceProvider));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getJpaVersion()
	 * @generated
	 */
	@Override
	public JPAVersionEnumeration getJpaVersion() {
		return jpaVersion;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setJpaVersion(net.codecadenza.eclipse.model.project.JPAVersionEnumeration)
	 * @generated
	 */
	@Override
	public void setJpaVersion(JPAVersionEnumeration newJpaVersion) {
		final JPAVersionEnumeration oldJpaVersion = jpaVersion;
		jpaVersion = newJpaVersion == null ? JPA_VERSION_EDEFAULT : newJpaVersion;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__JPA_VERSION, oldJpaVersion, jpaVersion));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTechnology()
	 * @generated
	 */
	@Override
	public TechnologyPlatformEnumeration getTechnology() {
		return technology;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setTechnology(net.codecadenza.eclipse.model.project.
	 * TechnologyPlatformEnumeration)
	 * @generated
	 */
	@Override
	public void setTechnology(TechnologyPlatformEnumeration newTechnology) {
		final TechnologyPlatformEnumeration oldTechnology = technology;
		technology = newTechnology == null ? TECHNOLOGY_EDEFAULT : newTechnology;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__TECHNOLOGY, oldTechnology, technology));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getValidationType()
	 * @generated
	 */
	@Override
	public ValidationTypeEnumeration getValidationType() {
		return validationType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setValidationType(net.codecadenza.eclipse.model.project.
	 * ValidationTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setValidationType(ValidationTypeEnumeration newValidationType) {
		final ValidationTypeEnumeration oldValidationType = validationType;
		validationType = newValidationType == null ? VALIDATION_TYPE_EDEFAULT : newValidationType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__VALIDATION_TYPE, oldValidationType,
					validationType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isBoundaryMode()
	 * @generated
	 */
	@Override
	public boolean isBoundaryMode() {
		return boundaryMode;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setBoundaryMode(boolean)
	 * @generated
	 */
	@Override
	public void setBoundaryMode(boolean newBoundaryMode) {
		final boolean oldBoundaryMode = boundaryMode;
		boundaryMode = newBoundaryMode;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__BOUNDARY_MODE, oldBoundaryMode, boundaryMode));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getExchangeNamespace()
	 * @generated
	 */
	@Override
	public Namespace getExchangeNamespace() {
		if (exchangeNamespace != null && exchangeNamespace.eIsProxy()) {
			final var oldExchangeNamespace = (InternalEObject) exchangeNamespace;
			exchangeNamespace = (Namespace) eResolveProxy(oldExchangeNamespace);

			if (exchangeNamespace != oldExchangeNamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ProjectPackage.PROJECT__EXCHANGE_NAMESPACE,
						oldExchangeNamespace, exchangeNamespace));
		}

		return exchangeNamespace;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Namespace basicGetExchangeNamespace() {
		return exchangeNamespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setExchangeNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setExchangeNamespace(Namespace newExchangeNamespace) {
		final Namespace oldExchangeNamespace = exchangeNamespace;
		exchangeNamespace = newExchangeNamespace;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__EXCHANGE_NAMESPACE, oldExchangeNamespace,
					exchangeNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getBuildTool()
	 * @generated
	 */
	@Override
	public BuildToolEnumeration getBuildTool() {
		return buildTool;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setBuildTool(net.codecadenza.eclipse.model.project.BuildToolEnumeration)
	 * @generated
	 */
	@Override
	public void setBuildTool(BuildToolEnumeration newBuildTool) {
		final BuildToolEnumeration oldBuildTool = buildTool;
		buildTool = newBuildTool == null ? BUILD_TOOL_EDEFAULT : newBuildTool;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__BUILD_TOOL, oldBuildTool, buildTool));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getBuildConfiguration()
	 * @generated
	 */
	@Override
	public EList<BuildArtifact> getBuildConfiguration() {
		if (buildConfiguration == null)
			buildConfiguration = new EObjectContainmentWithInverseEList<>(BuildArtifact.class, this,
					ProjectPackage.PROJECT__BUILD_CONFIGURATION, ProjectPackage.BUILD_ARTIFACT__PROJECT);

		return buildConfiguration;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getRootNamespace()
	 * @generated
	 */
	@Override
	public Namespace getRootNamespace() {
		return rootNamespace;
	}

	/**
	 * @param newRootNamespace
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetRootNamespace(Namespace newRootNamespace, NotificationChain msgs) {
		final Namespace oldRootNamespace = rootNamespace;
		rootNamespace = newRootNamespace;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__ROOT_NAMESPACE,
					oldRootNamespace, newRootNamespace);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setRootNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setRootNamespace(Namespace newRootNamespace) {
		if (newRootNamespace != rootNamespace) {
			NotificationChain msgs = null;

			if (rootNamespace != null)
				msgs = ((InternalEObject) rootNamespace).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__ROOT_NAMESPACE, null, msgs);

			if (newRootNamespace != null)
				msgs = ((InternalEObject) newRootNamespace).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__ROOT_NAMESPACE, null, msgs);

			msgs = basicSetRootNamespace(newRootNamespace, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__ROOT_NAMESPACE, newRootNamespace,
					newRootNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getRepositoryNamespace()
	 * @generated
	 */
	@Override
	public Namespace getRepositoryNamespace() {
		if (repositoryNamespace != null && repositoryNamespace.eIsProxy()) {
			final var oldRepositoryNamespace = (InternalEObject) repositoryNamespace;
			repositoryNamespace = (Namespace) eResolveProxy(oldRepositoryNamespace);

			if (repositoryNamespace != oldRepositoryNamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ProjectPackage.PROJECT__REPOSITORY_NAMESPACE,
						oldRepositoryNamespace, repositoryNamespace));
		}

		return repositoryNamespace;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Namespace basicGetRepositoryNamespace() {
		return repositoryNamespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setRepositoryNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setRepositoryNamespace(Namespace newRepositoryNamespace) {
		final Namespace oldRepositoryNamespace = repositoryNamespace;
		repositoryNamespace = newRepositoryNamespace;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__REPOSITORY_NAMESPACE, oldRepositoryNamespace,
					repositoryNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDTONamespace()
	 * @generated
	 */
	@Override
	public Namespace getDTONamespace() {
		if (dTONamespace != null && dTONamespace.eIsProxy()) {
			final var oldDTONamespace = (InternalEObject) dTONamespace;
			dTONamespace = (Namespace) eResolveProxy(oldDTONamespace);

			if (dTONamespace != oldDTONamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ProjectPackage.PROJECT__DTO_NAMESPACE, oldDTONamespace,
						dTONamespace));
		}

		return dTONamespace;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Namespace basicGetDTONamespace() {
		return dTONamespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setDTONamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setDTONamespace(Namespace newDTONamespace) {
		final Namespace oldDTONamespace = dTONamespace;
		dTONamespace = newDTONamespace;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DTO_NAMESPACE, oldDTONamespace, dTONamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getBoundaryNamespace()
	 * @generated
	 */
	@Override
	public Namespace getBoundaryNamespace() {
		if (boundaryNamespace != null && boundaryNamespace.eIsProxy()) {
			final var oldBoundaryNamespace = (InternalEObject) boundaryNamespace;
			boundaryNamespace = (Namespace) eResolveProxy(oldBoundaryNamespace);

			if (boundaryNamespace != oldBoundaryNamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ProjectPackage.PROJECT__BOUNDARY_NAMESPACE,
						oldBoundaryNamespace, boundaryNamespace));
		}

		return boundaryNamespace;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Namespace basicGetBoundaryNamespace() {
		return boundaryNamespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setBoundaryNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setBoundaryNamespace(Namespace newBoundaryNamespace) {
		final Namespace oldBoundaryNamespace = boundaryNamespace;
		boundaryNamespace = newBoundaryNamespace;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__BOUNDARY_NAMESPACE, oldBoundaryNamespace,
					boundaryNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDomainNamespace()
	 * @generated
	 */
	@Override
	public Namespace getDomainNamespace() {
		return domainNamespace;
	}

	/**
	 * @param newDomainNamespace
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDomainNamespace(Namespace newDomainNamespace, NotificationChain msgs) {
		final Namespace oldDomainNamespace = domainNamespace;
		domainNamespace = newDomainNamespace;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DOMAIN_NAMESPACE,
					oldDomainNamespace, newDomainNamespace);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setDomainNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setDomainNamespace(Namespace newDomainNamespace) {
		if (newDomainNamespace != domainNamespace) {
			NotificationChain msgs = null;

			if (domainNamespace != null)
				msgs = ((InternalEObject) domainNamespace).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__DOMAIN_NAMESPACE, null, msgs);

			if (newDomainNamespace != null)
				msgs = ((InternalEObject) newDomainNamespace).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__DOMAIN_NAMESPACE, null, msgs);

			msgs = basicSetDomainNamespace(newDomainNamespace, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DOMAIN_NAMESPACE, newDomainNamespace,
					newDomainNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getClientNamespace()
	 * @generated
	 */
	@Override
	public Namespace getClientNamespace() {
		return clientNamespace;
	}

	/**
	 * @param newClientNamespace
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetClientNamespace(Namespace newClientNamespace, NotificationChain msgs) {
		final Namespace oldClientNamespace = clientNamespace;
		clientNamespace = newClientNamespace;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__CLIENT_NAMESPACE,
					oldClientNamespace, newClientNamespace);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setClientNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setClientNamespace(Namespace newClientNamespace) {
		if (newClientNamespace != clientNamespace) {
			NotificationChain msgs = null;

			if (clientNamespace != null)
				msgs = ((InternalEObject) clientNamespace).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__CLIENT_NAMESPACE, null, msgs);

			if (newClientNamespace != null)
				msgs = ((InternalEObject) newClientNamespace).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__CLIENT_NAMESPACE, null, msgs);

			msgs = basicSetClientNamespace(newClientNamespace, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__CLIENT_NAMESPACE, newClientNamespace,
					newClientNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getSupportedStandardNamespaces()
	 * @generated
	 */
	@Override
	public EList<Namespace> getSupportedStandardNamespaces() {
		if (supportedStandardNamespaces == null)
			supportedStandardNamespaces = new EObjectContainmentEList<>(Namespace.class, this,
					ProjectPackage.PROJECT__SUPPORTED_STANDARD_NAMESPACES);

		return supportedStandardNamespaces;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDatabase()
	 * @generated
	 */
	@Override
	public Database getDatabase() {
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
			final var notification = new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DATABASE, oldDatabase,
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
	 * @see net.codecadenza.eclipse.model.project.Project#setDatabase(net.codecadenza.eclipse.model.db.Database)
	 * @generated
	 */
	@Override
	public void setDatabase(Database newDatabase) {
		if (newDatabase != database) {
			NotificationChain msgs = null;

			if (database != null)
				msgs = ((InternalEObject) database).eInverseRemove(this, DbPackage.DATABASE__PROJECT, Database.class, msgs);

			if (newDatabase != null)
				msgs = ((InternalEObject) newDatabase).eInverseAdd(this, DbPackage.DATABASE__PROJECT, Database.class, msgs);

			msgs = basicSetDatabase(newDatabase, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DATABASE, newDatabase, newDatabase));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllSupportedTypes()
	 * @generated
	 */
	@Override
	public EList<JavaType> getAllSupportedTypes() {
		if (allSupportedTypes == null)
			allSupportedTypes = new EObjectContainmentEList<>(JavaType.class, this, ProjectPackage.PROJECT__ALL_SUPPORTED_TYPES);

		return allSupportedTypes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getFormGroups()
	 * @generated
	 */
	@Override
	public EList<FormGroup> getFormGroups() {
		if (formGroups == null)
			formGroups = new EObjectWithInverseResolvingEList<>(FormGroup.class, this, ProjectPackage.PROJECT__FORM_GROUPS,
					ClientPackage.FORM_GROUP__PROJECT);

		return formGroups;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getXmlNamespace()
	 * @generated
	 */
	@Override
	public String getXmlNamespace() {
		return xmlNamespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setXmlNamespace(java.lang.String)
	 * @generated
	 */
	@Override
	public void setXmlNamespace(String newXmlNamespace) {
		final String oldXmlNamespace = xmlNamespace;
		xmlNamespace = newXmlNamespace;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__XML_NAMESPACE, oldXmlNamespace, xmlNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getIntegrationModules()
	 * @generated
	 */
	@Override
	public EList<IntegrationModule> getIntegrationModules() {
		if (integrationModules == null)
			integrationModules = new EObjectContainmentWithInverseEList<>(IntegrationModule.class, this,
					ProjectPackage.PROJECT__INTEGRATION_MODULES, ProjectPackage.INTEGRATION_MODULE__PROJECT);

		return integrationModules;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getMappingStrategy()
	 * @generated
	 */
	@Override
	public MappingAnnotationStrategy getMappingStrategy() {
		return mappingStrategy;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setMappingStrategy(net.codecadenza.eclipse.model.project.
	 * MappingAnnotationStrategy)
	 * @generated
	 */
	@Override
	public void setMappingStrategy(MappingAnnotationStrategy newMappingStrategy) {
		final MappingAnnotationStrategy oldMappingStrategy = mappingStrategy;
		mappingStrategy = newMappingStrategy == null ? MAPPING_STRATEGY_EDEFAULT : newMappingStrategy;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__MAPPING_STRATEGY, oldMappingStrategy,
					mappingStrategy));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDefaultXMLMappingType()
	 * @generated
	 */
	@Override
	public XMLMappingType getDefaultXMLMappingType() {
		return defaultXMLMappingType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setDefaultXMLMappingType(net.codecadenza.eclipse.model.project.
	 * XMLMappingType)
	 * @generated
	 */
	@Override
	public void setDefaultXMLMappingType(XMLMappingType newDefaultXMLMappingType) {
		final XMLMappingType oldDefaultXMLMappingType = defaultXMLMappingType;
		defaultXMLMappingType = newDefaultXMLMappingType == null ? DEFAULT_XML_MAPPING_TYPE_EDEFAULT : newDefaultXMLMappingType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__DEFAULT_XML_MAPPING_TYPE,
					oldDefaultXMLMappingType, defaultXMLMappingType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getXmlNamespacePrefix()
	 * @generated
	 */
	@Override
	public String getXmlNamespacePrefix() {
		return xmlNamespacePrefix;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setXmlNamespacePrefix(java.lang.String)
	 * @generated
	 */
	@Override
	public void setXmlNamespacePrefix(String newXmlNamespacePrefix) {
		final String oldXmlNamespacePrefix = xmlNamespacePrefix;
		xmlNamespacePrefix = newXmlNamespacePrefix;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__XML_NAMESPACE_PREFIX, oldXmlNamespacePrefix,
					xmlNamespacePrefix));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTestModules()
	 * @generated
	 */
	@Override
	public EList<AbstractTestModule> getTestModules() {
		if (testModules == null)
			testModules = new EObjectContainmentWithInverseEList<>(AbstractTestModule.class, this, ProjectPackage.PROJECT__TEST_MODULES,
					TestingPackage.ABSTRACT_TEST_MODULE__PROJECT);

		return testModules;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isProtectManualChanges()
	 * @generated
	 */
	@Override
	public boolean isProtectManualChanges() {
		return protectManualChanges;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#setProtectManualChanges(boolean)
	 * @generated
	 */
	@Override
	public void setProtectManualChanges(boolean newProtectManualChanges) {
		final boolean oldProtectManualChanges = protectManualChanges;
		protectManualChanges = newProtectManualChanges;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PROJECT__PROTECT_MANUAL_CHANGES,
					oldProtectManualChanges, protectManualChanges));
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
			case ProjectPackage.PROJECT__DATABASE:
				if (database != null)
					msgs = ((InternalEObject) database).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ProjectPackage.PROJECT__DATABASE,
							null, msgs);

				return basicSetDatabase((Database) otherEnd, msgs);
			case ProjectPackage.PROJECT__FORM_GROUPS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getFormGroups()).basicAdd(otherEnd, msgs);
			case ProjectPackage.PROJECT__BUILD_CONFIGURATION:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getBuildConfiguration()).basicAdd(otherEnd, msgs);
			case ProjectPackage.PROJECT__INTEGRATION_MODULES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getIntegrationModules()).basicAdd(otherEnd, msgs);
			case ProjectPackage.PROJECT__TEST_MODULES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getTestModules()).basicAdd(otherEnd, msgs);
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
			case ProjectPackage.PROJECT__ROLES:
				return ((InternalEList<?>) getRoles()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__PERSISTENCE_UNIT_PROPERTIES:
				return ((InternalEList<?>) getPersistenceUnitProperties()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__DATA_SOURCE:
				return basicSetDataSource(null, msgs);
			case ProjectPackage.PROJECT__ROOT_NAMESPACE:
				return basicSetRootNamespace(null, msgs);
			case ProjectPackage.PROJECT__DOMAIN_NAMESPACE:
				return basicSetDomainNamespace(null, msgs);
			case ProjectPackage.PROJECT__CLIENT_NAMESPACE:
				return basicSetClientNamespace(null, msgs);
			case ProjectPackage.PROJECT__SUPPORTED_STANDARD_NAMESPACES:
				return ((InternalEList<?>) getSupportedStandardNamespaces()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__DATABASE:
				return basicSetDatabase(null, msgs);
			case ProjectPackage.PROJECT__ALL_SUPPORTED_TYPES:
				return ((InternalEList<?>) getAllSupportedTypes()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__FORM_GROUPS:
				return ((InternalEList<?>) getFormGroups()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__BUILD_CONFIGURATION:
				return ((InternalEList<?>) getBuildConfiguration()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__INTEGRATION_MODULES:
				return ((InternalEList<?>) getIntegrationModules()).basicRemove(otherEnd, msgs);
			case ProjectPackage.PROJECT__TEST_MODULES:
				return ((InternalEList<?>) getTestModules()).basicRemove(otherEnd, msgs);
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
			case ProjectPackage.PROJECT__NAME:
				return getName();
			case ProjectPackage.PROJECT__CODE:
				return getCode();
			case ProjectPackage.PROJECT__SERVER_PLATFORM:
				return getServerPlatform();
			case ProjectPackage.PROJECT__CLIENT_PLATFORM:
				return getClientPlatform();
			case ProjectPackage.PROJECT__ROLES:
				return getRoles();
			case ProjectPackage.PROJECT__PERSISTENCE_UNIT_PROPERTIES:
				return getPersistenceUnitProperties();
			case ProjectPackage.PROJECT__DATA_SOURCE:
				return getDataSource();
			case ProjectPackage.PROJECT__ROOT_NAMESPACE:
				return getRootNamespace();
			case ProjectPackage.PROJECT__REPOSITORY_NAMESPACE:
				if (resolve)
					return getRepositoryNamespace();

				return basicGetRepositoryNamespace();
			case ProjectPackage.PROJECT__DTO_NAMESPACE:
				if (resolve)
					return getDTONamespace();

				return basicGetDTONamespace();
			case ProjectPackage.PROJECT__BOUNDARY_NAMESPACE:
				if (resolve)
					return getBoundaryNamespace();

				return basicGetBoundaryNamespace();
			case ProjectPackage.PROJECT__DOMAIN_NAMESPACE:
				return getDomainNamespace();
			case ProjectPackage.PROJECT__CLIENT_NAMESPACE:
				return getClientNamespace();
			case ProjectPackage.PROJECT__SUPPORTED_STANDARD_NAMESPACES:
				return getSupportedStandardNamespaces();
			case ProjectPackage.PROJECT__DATABASE:
				return getDatabase();
			case ProjectPackage.PROJECT__ALL_SUPPORTED_TYPES:
				return getAllSupportedTypes();
			case ProjectPackage.PROJECT__FORM_GROUPS:
				return getFormGroups();
			case ProjectPackage.PROJECT__PERSISTENCE_PROVIDER:
				return getPersistenceProvider();
			case ProjectPackage.PROJECT__JPA_VERSION:
				return getJpaVersion();
			case ProjectPackage.PROJECT__TECHNOLOGY:
				return getTechnology();
			case ProjectPackage.PROJECT__VALIDATION_TYPE:
				return getValidationType();
			case ProjectPackage.PROJECT__BOUNDARY_MODE:
				return isBoundaryMode();
			case ProjectPackage.PROJECT__EXCHANGE_NAMESPACE:
				if (resolve)
					return getExchangeNamespace();

				return basicGetExchangeNamespace();
			case ProjectPackage.PROJECT__BUILD_TOOL:
				return getBuildTool();
			case ProjectPackage.PROJECT__BUILD_CONFIGURATION:
				return getBuildConfiguration();
			case ProjectPackage.PROJECT__XML_NAMESPACE:
				return getXmlNamespace();
			case ProjectPackage.PROJECT__INTEGRATION_MODULES:
				return getIntegrationModules();
			case ProjectPackage.PROJECT__MAPPING_STRATEGY:
				return getMappingStrategy();
			case ProjectPackage.PROJECT__DEFAULT_XML_MAPPING_TYPE:
				return getDefaultXMLMappingType();
			case ProjectPackage.PROJECT__XML_NAMESPACE_PREFIX:
				return getXmlNamespacePrefix();
			case ProjectPackage.PROJECT__TEST_MODULES:
				return getTestModules();
			case ProjectPackage.PROJECT__PROTECT_MANUAL_CHANGES:
				return isProtectManualChanges();
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
			case ProjectPackage.PROJECT__NAME:
				setName((String) newValue);
				return;
			case ProjectPackage.PROJECT__CODE:
				setCode((String) newValue);
				return;
			case ProjectPackage.PROJECT__SERVER_PLATFORM:
				setServerPlatform((ServerPlatformEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__CLIENT_PLATFORM:
				setClientPlatform((ClientPlatformEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__ROLES:
				getRoles().clear();
				getRoles().addAll((Collection<? extends Role>) newValue);
				return;
			case ProjectPackage.PROJECT__PERSISTENCE_UNIT_PROPERTIES:
				getPersistenceUnitProperties().clear();
				getPersistenceUnitProperties().addAll((Collection<? extends PersistenceUnitProperty>) newValue);
				return;
			case ProjectPackage.PROJECT__DATA_SOURCE:
				setDataSource((Datasource) newValue);
				return;
			case ProjectPackage.PROJECT__ROOT_NAMESPACE:
				setRootNamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__REPOSITORY_NAMESPACE:
				setRepositoryNamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__DTO_NAMESPACE:
				setDTONamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__BOUNDARY_NAMESPACE:
				setBoundaryNamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__DOMAIN_NAMESPACE:
				setDomainNamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__CLIENT_NAMESPACE:
				setClientNamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__SUPPORTED_STANDARD_NAMESPACES:
				getSupportedStandardNamespaces().clear();
				getSupportedStandardNamespaces().addAll((Collection<? extends Namespace>) newValue);
				return;
			case ProjectPackage.PROJECT__DATABASE:
				setDatabase((Database) newValue);
				return;
			case ProjectPackage.PROJECT__ALL_SUPPORTED_TYPES:
				getAllSupportedTypes().clear();
				getAllSupportedTypes().addAll((Collection<? extends JavaType>) newValue);
				return;
			case ProjectPackage.PROJECT__FORM_GROUPS:
				getFormGroups().clear();
				getFormGroups().addAll((Collection<? extends FormGroup>) newValue);
				return;
			case ProjectPackage.PROJECT__PERSISTENCE_PROVIDER:
				setPersistenceProvider((PersistenceProviderEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__JPA_VERSION:
				setJpaVersion((JPAVersionEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__TECHNOLOGY:
				setTechnology((TechnologyPlatformEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__VALIDATION_TYPE:
				setValidationType((ValidationTypeEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__BOUNDARY_MODE:
				setBoundaryMode((Boolean) newValue);
				return;
			case ProjectPackage.PROJECT__EXCHANGE_NAMESPACE:
				setExchangeNamespace((Namespace) newValue);
				return;
			case ProjectPackage.PROJECT__BUILD_TOOL:
				setBuildTool((BuildToolEnumeration) newValue);
				return;
			case ProjectPackage.PROJECT__BUILD_CONFIGURATION:
				getBuildConfiguration().clear();
				getBuildConfiguration().addAll((Collection<? extends BuildArtifact>) newValue);
				return;
			case ProjectPackage.PROJECT__XML_NAMESPACE:
				setXmlNamespace((String) newValue);
				return;
			case ProjectPackage.PROJECT__INTEGRATION_MODULES:
				getIntegrationModules().clear();
				getIntegrationModules().addAll((Collection<? extends IntegrationModule>) newValue);
				return;
			case ProjectPackage.PROJECT__MAPPING_STRATEGY:
				setMappingStrategy((MappingAnnotationStrategy) newValue);
				return;
			case ProjectPackage.PROJECT__DEFAULT_XML_MAPPING_TYPE:
				setDefaultXMLMappingType((XMLMappingType) newValue);
				return;
			case ProjectPackage.PROJECT__XML_NAMESPACE_PREFIX:
				setXmlNamespacePrefix((String) newValue);
				return;
			case ProjectPackage.PROJECT__TEST_MODULES:
				getTestModules().clear();
				getTestModules().addAll((Collection<? extends AbstractTestModule>) newValue);
				return;
			case ProjectPackage.PROJECT__PROTECT_MANUAL_CHANGES:
				setProtectManualChanges((Boolean) newValue);
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
			case ProjectPackage.PROJECT__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__CODE:
				setCode(CODE_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__SERVER_PLATFORM:
				setServerPlatform(SERVER_PLATFORM_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__CLIENT_PLATFORM:
				setClientPlatform(CLIENT_PLATFORM_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__ROLES:
				getRoles().clear();
				return;
			case ProjectPackage.PROJECT__PERSISTENCE_UNIT_PROPERTIES:
				getPersistenceUnitProperties().clear();
				return;
			case ProjectPackage.PROJECT__DATA_SOURCE:
				setDataSource((Datasource) null);
				return;
			case ProjectPackage.PROJECT__ROOT_NAMESPACE:
				setRootNamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__REPOSITORY_NAMESPACE:
				setRepositoryNamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__DTO_NAMESPACE:
				setDTONamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__BOUNDARY_NAMESPACE:
				setBoundaryNamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__DOMAIN_NAMESPACE:
				setDomainNamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__CLIENT_NAMESPACE:
				setClientNamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__SUPPORTED_STANDARD_NAMESPACES:
				getSupportedStandardNamespaces().clear();
				return;
			case ProjectPackage.PROJECT__DATABASE:
				setDatabase((Database) null);
				return;
			case ProjectPackage.PROJECT__ALL_SUPPORTED_TYPES:
				getAllSupportedTypes().clear();
				return;
			case ProjectPackage.PROJECT__FORM_GROUPS:
				getFormGroups().clear();
				return;
			case ProjectPackage.PROJECT__PERSISTENCE_PROVIDER:
				setPersistenceProvider(PERSISTENCE_PROVIDER_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__JPA_VERSION:
				setJpaVersion(JPA_VERSION_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__TECHNOLOGY:
				setTechnology(TECHNOLOGY_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__VALIDATION_TYPE:
				setValidationType(VALIDATION_TYPE_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__BOUNDARY_MODE:
				setBoundaryMode(BOUNDARY_MODE_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__EXCHANGE_NAMESPACE:
				setExchangeNamespace((Namespace) null);
				return;
			case ProjectPackage.PROJECT__BUILD_TOOL:
				setBuildTool(BUILD_TOOL_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__BUILD_CONFIGURATION:
				getBuildConfiguration().clear();
				return;
			case ProjectPackage.PROJECT__XML_NAMESPACE:
				setXmlNamespace(XML_NAMESPACE_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__INTEGRATION_MODULES:
				getIntegrationModules().clear();
				return;
			case ProjectPackage.PROJECT__MAPPING_STRATEGY:
				setMappingStrategy(MAPPING_STRATEGY_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__DEFAULT_XML_MAPPING_TYPE:
				setDefaultXMLMappingType(DEFAULT_XML_MAPPING_TYPE_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__XML_NAMESPACE_PREFIX:
				setXmlNamespacePrefix(XML_NAMESPACE_PREFIX_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__PROTECT_MANUAL_CHANGES:
				setProtectManualChanges(PROTECT_MANUAL_CHANGES_EDEFAULT);
				return;
			case ProjectPackage.PROJECT__TEST_MODULES:
				getTestModules().clear();
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
			case ProjectPackage.PROJECT__NAME:
				return name != null;
			case ProjectPackage.PROJECT__CODE:
				return code != null;
			case ProjectPackage.PROJECT__SERVER_PLATFORM:
				return serverPlatform != SERVER_PLATFORM_EDEFAULT;
			case ProjectPackage.PROJECT__CLIENT_PLATFORM:
				return clientPlatform != CLIENT_PLATFORM_EDEFAULT;
			case ProjectPackage.PROJECT__ROLES:
				return roles != null && !roles.isEmpty();
			case ProjectPackage.PROJECT__PERSISTENCE_UNIT_PROPERTIES:
				return persistenceUnitProperties != null && !persistenceUnitProperties.isEmpty();
			case ProjectPackage.PROJECT__DATA_SOURCE:
				return dataSource != null;
			case ProjectPackage.PROJECT__ROOT_NAMESPACE:
				return rootNamespace != null;
			case ProjectPackage.PROJECT__REPOSITORY_NAMESPACE:
				return repositoryNamespace != null;
			case ProjectPackage.PROJECT__DTO_NAMESPACE:
				return dTONamespace != null;
			case ProjectPackage.PROJECT__BOUNDARY_NAMESPACE:
				return boundaryNamespace != null;
			case ProjectPackage.PROJECT__DOMAIN_NAMESPACE:
				return domainNamespace != null;
			case ProjectPackage.PROJECT__CLIENT_NAMESPACE:
				return clientNamespace != null;
			case ProjectPackage.PROJECT__SUPPORTED_STANDARD_NAMESPACES:
				return supportedStandardNamespaces != null && !supportedStandardNamespaces.isEmpty();
			case ProjectPackage.PROJECT__DATABASE:
				return database != null;
			case ProjectPackage.PROJECT__ALL_SUPPORTED_TYPES:
				return allSupportedTypes != null && !allSupportedTypes.isEmpty();
			case ProjectPackage.PROJECT__FORM_GROUPS:
				return formGroups != null && !formGroups.isEmpty();
			case ProjectPackage.PROJECT__PERSISTENCE_PROVIDER:
				return persistenceProvider != PERSISTENCE_PROVIDER_EDEFAULT;
			case ProjectPackage.PROJECT__JPA_VERSION:
				return jpaVersion != JPA_VERSION_EDEFAULT;
			case ProjectPackage.PROJECT__TECHNOLOGY:
				return technology != TECHNOLOGY_EDEFAULT;
			case ProjectPackage.PROJECT__VALIDATION_TYPE:
				return validationType != VALIDATION_TYPE_EDEFAULT;
			case ProjectPackage.PROJECT__BOUNDARY_MODE:
				return boundaryMode != BOUNDARY_MODE_EDEFAULT;
			case ProjectPackage.PROJECT__EXCHANGE_NAMESPACE:
				return exchangeNamespace != null;
			case ProjectPackage.PROJECT__BUILD_TOOL:
				return buildTool != BUILD_TOOL_EDEFAULT;
			case ProjectPackage.PROJECT__BUILD_CONFIGURATION:
				return buildConfiguration != null && !buildConfiguration.isEmpty();
			case ProjectPackage.PROJECT__XML_NAMESPACE:
				return xmlNamespace != null;
			case ProjectPackage.PROJECT__INTEGRATION_MODULES:
				return integrationModules != null && !integrationModules.isEmpty();
			case ProjectPackage.PROJECT__MAPPING_STRATEGY:
				return mappingStrategy != MAPPING_STRATEGY_EDEFAULT;
			case ProjectPackage.PROJECT__DEFAULT_XML_MAPPING_TYPE:
				return defaultXMLMappingType != DEFAULT_XML_MAPPING_TYPE_EDEFAULT;
			case ProjectPackage.PROJECT__XML_NAMESPACE_PREFIX:
				return xmlNamespacePrefix != null;
			case ProjectPackage.PROJECT__PROTECT_MANUAL_CHANGES:
				return protectManualChanges != PROTECT_MANUAL_CHANGES_EDEFAULT;
			case ProjectPackage.PROJECT__TEST_MODULES:
				return testModules != null && !testModules.isEmpty();
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
		result.append(", code: ");
		result.append(code);
		result.append(", serverPlatform: ");
		result.append(serverPlatform);
		result.append(", clientPlatform: ");
		result.append(clientPlatform);
		result.append(", persistenceProvider: ");
		result.append(persistenceProvider);
		result.append(", jpaVersion: ");
		result.append(jpaVersion);
		result.append(", technology: ");
		result.append(technology);
		result.append(", validationType: ");
		result.append(validationType);
		result.append(", boundaryMode: ");
		result.append(boundaryMode);
		result.append(", buildTool: ");
		result.append(buildTool);
		result.append(", xmlNamespace: ");
		result.append(xmlNamespace);
		result.append(", mappingStrategy: ");
		result.append(mappingStrategy);
		result.append(", defaultXMLMappingType: ");
		result.append(defaultXMLMappingType);
		result.append(", xmlNamespacePrefix: ");
		result.append(xmlNamespacePrefix);
		result.append(", protectManualChanges: ");
		result.append(protectManualChanges);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getValidEnumTags()
	 * @generated not
	 */
	@Override
	public ArrayList<String> getValidEnumTags() {
		final var enumNames = new ArrayList<String>();
		enumNames.add(EnumTagEnumeration.NONE.getName());

		final EnumSet<EnumTagEnumeration> usedTags = EnumSet.noneOf(EnumTagEnumeration.class);

		getDomainNamespace().getChildNamespaces()
				.forEach(n -> n.getJavaTypes().stream().filter(JavaEnum.class::isInstance).forEach(t -> {
					final var e = (JavaEnum) t;

					if (e.getTag() != EnumTagEnumeration.NONE)
						usedTags.add(e.getTag());
				}));

		for (final EnumTagEnumeration e : EnumTagEnumeration.values())
			if (e != EnumTagEnumeration.NONE && !usedTags.contains(e))
				enumNames.add(e.getName());

		return enumNames;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getValidDomainObjectTags(boolean, boolean)
	 * @generated not
	 */
	@Override
	public ArrayList<String> getValidDomainObjectTags(boolean isAbstract, boolean isMappedSuperClass) {
		final var list = new ArrayList<String>();
		list.add(DomainTagEnumeration.NONE.getName());

		final var existingTags = new HashSet<String>();

		// Tagging is not supported for abstract and mapped superclasses
		if (isAbstract || isMappedSuperClass)
			return list;

		// Get all domain objects in order to return just tags that are not used yet
		final EList<DomainObject> allDomainObjects = getAllDomainObjectsOfProject(false, false);

		for (final DomainObject o : allDomainObjects)
			// An application may have more than one domain object that represents a document!
			if (o.getTag() != DomainTagEnumeration.DOCUMENT)
				existingTags.add(o.getTag().getName());

		for (final DomainTagEnumeration value : DomainTagEnumeration.values()) {
			// Currently, tagging of domain objects that should represent saved queries is only supported for JSF and Vaadin.
			if (value == DomainTagEnumeration.SAVEDQUERY && !hasJSFOrVaadinClient())
				continue;

			// Logging is not supported for unmanaged environments!
			if (value == DomainTagEnumeration.LOGGING && isJavaSEApplication())
				continue;

			if (!existingTags.contains(value.getName()) && value != DomainTagEnumeration.NONE)
				list.add(value.getName());
		}

		return list;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getJavaTypeByName(java.lang.String)
	 * @generated not
	 */
	@Override
	public JavaType getJavaTypeByName(String name) {
		final EList<JavaType> types = getAllSupportedTypes();

		return types.stream().filter(t -> t.getName().equals(name)).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllRepositoriesOfProject()
	 * @generated not
	 */
	@Override
	public EList<Repository> getAllRepositoriesOfProject() {
		final var beans = new BasicEList<Repository>();

		for (final Namespace n : this.getRepositoryNamespace().getChildNamespaces())
			for (final JavaType t : n.getJavaTypes())
				if (t instanceof final Repository repository)
					beans.add(repository);

		return beans;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllDTOsOfProject()
	 * @generated not
	 */
	@Override
	public EList<DTOBean> getAllDTOsOfProject() {
		final var beans = new BasicEList<DTOBean>();

		for (final Namespace n : this.getDTONamespace().getChildNamespaces())
			for (final JavaType t : n.getJavaTypes())
				if (t instanceof final DTOBean dto)
					beans.add(dto);

		return beans;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllBoundariesOfProject()
	 * @generated not
	 */
	@Override
	public EList<BoundaryBean> getAllBoundariesOfProject() {
		final var beans = new BasicEList<BoundaryBean>();

		for (final Namespace n : this.getBoundaryNamespace().getChildNamespaces())
			for (final JavaType t : n.getJavaTypes())
				if (t instanceof final BoundaryBean boundaryBean)
					beans.add(boundaryBean);

		return beans;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * net.codecadenza.eclipse.model.project.Project#getBoundaryByDomainObject(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated not
	 */
	@Override
	public BoundaryBean getBoundaryByDomainObject(DomainObject domainObject) {
		return getAllBoundariesOfProject().stream().filter(boundary -> boundary.getDomainObject().equals(domainObject)).findFirst()
				.orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getApplicationLogOnDTO()
	 * @generated not
	 */
	@Override
	public DTOBean getApplicationLogOnDTO() {
		for (final Namespace n : this.getDTONamespace().getChildNamespaces())
			for (final JavaType t : n.getJavaTypes())
				if (t instanceof final DTOBean dto && dto.getName().equals(APP_LOGON_DTO_NAME))
					return dto;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getLOVFormsOfGroup(net.codecadenza.eclipse.model.client.FormGroup,
	 * org.eclipse.emf.common.util.EList)
	 * @generated not
	 */
	@Override
	public void getLOVFormsOfGroup(FormGroup group, EList<Form> tempFormList) {
		group.getChildGroups().forEach(g -> {
			for (final Form f : g.getForms())
				if (f.getFormType() == FormTypeEnumeration.LOV)
					tempFormList.add(f);

			g.getChildGroups().forEach(s -> getLOVFormsOfGroup(s, tempFormList));
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getLOVFormsOfProject()
	 * @generated not
	 */
	@Override
	public EList<Form> getLOVFormsOfProject() {
		final var tempFormList = new BasicEList<Form>();

		this.getFormGroups().forEach(g -> {
			for (final Form f : g.getForms())
				if (f.getFormType() == FormTypeEnumeration.LOV)
					tempFormList.add(f);

			getLOVFormsOfGroup(g, tempFormList);
		});

		return tempFormList;
	}

	/**
	 * Recursive helper method to get all grid panels of a given form group
	 * @param group
	 * @param tempPanelList
	 * @generated not
	 */
	private void getGridPanelsOfGroup(FormGroup group, EList<FormPanel> tempPanelList) {
		group.getChildGroups().forEach(g -> {
			g.getPanels().forEach(tempPanelList::add);

			g.getChildGroups().forEach(s -> getGridPanelsOfGroup(s, tempPanelList));
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllGridPanelsOfProject()
	 * @generated not
	 */
	@Override
	public EList<FormPanel> getAllGridPanelsOfProject() {
		final var tempPanelList = new BasicEList<FormPanel>();

		this.getFormGroups().forEach(g -> {
			g.getPanels().forEach(tempPanelList::add);

			getGridPanelsOfGroup(g, tempPanelList);
		});

		return tempPanelList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllEntityBeansOfProject(boolean, boolean)
	 * @generated not
	 */
	@Override
	public EList<DomainObject> getAllDomainObjectsOfProject(boolean includingSuperclasses, boolean includingAbstractClasses) {
		final var beans = new BasicEList<DomainObject>();

		for (final Namespace n : getDomainNamespace().getChildNamespaces())
			for (final JavaType t : n.getJavaTypes()) {
				if (t instanceof final DomainObject domainObject) {
					if (domainObject.getPKAttribute() == null)
						continue;

					if (domainObject.isMappedSuperClass() && !includingSuperclasses)
						continue;

					if (domainObject.isAbstract() && !includingAbstractClasses)
						continue;

					beans.add(domainObject);
				}
			}

		return beans;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllFormsOfProject()
	 * @generated not
	 */
	@Override
	public EList<Form> getAllFormsOfProject() {
		final var forms = new BasicEList<Form>();

		this.getFormGroups().forEach(g -> forms.addAll(getFormsOfFormGroup(g)));

		return forms;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDTOsOfEntityBean(net.codecadenza.eclipse.model.entity.EntityBean)
	 * @generated not
	 */
	@Override
	public EList<DTOBean> getDTOsOfDomainObject(DomainObject entityBean) {
		final var beans = new BasicEList<DTOBean>();

		this.getDTONamespace().getChildNamespaces()
				.forEach(n -> n.getJavaTypes().stream().filter(DTOBean.class::isInstance).forEach(t -> {
					final var b = (DTOBean) t;

					if (b.getDomainObject().equals(entityBean))
						beans.add(b);
				}));

		return beans;
	}

	/**
	 * Get all forms of this form group
	 * @param formGroup
	 * @return a list with all form of this group
	 * @generated not
	 */
	public EList<Form> getFormsOfFormGroup(FormGroup formGroup) {
		final var forms = new BasicEList<Form>();

		formGroup.getForms().forEach(forms::add);
		formGroup.getChildGroups().forEach(g -> forms.addAll(getFormsOfFormGroup(g)));

		return forms;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDomainObjectByTag(net.codecadenza.eclipse.model.domain.
	 * DomainTagEnumeration)
	 * @generated not
	 */
	@Override
	public DomainObject getDomainObjectByTag(DomainTagEnumeration tag) {
		final EList<DomainObject> objList = getAllDomainObjectsOfProject(false, false);

		return objList.stream().filter(obj -> obj.getTag() == tag).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getLogOnBoundary()
	 * @generated not
	 */
	@Override
	public BoundaryBean getLogOnBoundary() {
		for (final BoundaryBean b : getAllBoundariesOfProject())
			for (final BoundaryMethod m : b.getBoundaryMethods())
				if (m.getMethodType() == BoundaryMethodTypeEnumeration.LOG_ON)
					return b;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#searchMappableTypeByName(java.lang.String, boolean)
	 * @generated not
	 */
	@Override
	public EList<JavaType> searchMappableTypeByName(String name, boolean doSort) {
		final var resultList = new BasicEList<JavaType>();

		for (final JavaType t : getAllSupportedTypes())
			if (t.getName().toLowerCase().startsWith(name.toLowerCase()))
				resultList.add(t);

		// Sort result list
		if (doSort)
			ECollections.sort(resultList, (o1, o2) -> o1.getName().compareTo(o2.getName()));

		return resultList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#searchDomainObjectByName(java.lang.String, boolean, boolean, boolean)
	 * @generated not
	 */
	@Override
	public EList<DomainObject> searchDomainObjectByName(String name, boolean doSort, boolean includeAbstractClasses,
			boolean includeMappedSuperClasses) {
		final var resultList = new BasicEList<DomainObject>();

		for (final DomainObject o : getAllDomainObjectsOfProject(includeMappedSuperClasses, includeAbstractClasses))
			if (o.getPKAttribute() != null && o.getName().toLowerCase().startsWith(name.toLowerCase()))
				resultList.add(o);

		// Sort result list
		if (doSort)
			ECollections.sort(resultList, (o1, o2) -> o1.getName().compareTo(o2.getName()));

		return resultList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#searchDTOByName(java.lang.String, boolean)
	 * @generated not
	 */
	@Override
	public EList<DTOBean> searchDTOByName(String name, boolean doSort) {
		final var resultList = new BasicEList<DTOBean>();

		for (final DTOBean o : getAllDTOsOfProject())
			if (o.getName().toLowerCase().startsWith(name.toLowerCase()))
				resultList.add(o);

		// Sort result list
		if (doSort)
			ECollections.sort(resultList, (o1, o2) -> o1.getName().compareTo(o2.getName()));

		return resultList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#searchTypeByName(java.lang.String)
	 * @generated not
	 */
	@Override
	public EList<JavaType> searchTypeByName(String name) {
		final var resultList = new BasicEList<JavaType>();
		resultList.addAll(searchMappableTypeByName(name, false));
		resultList.addAll(searchDomainObjectByName(name, false, true, true));
		resultList.addAll(searchDTOByName(name, false));

		// Sort result list
		ECollections.sort(resultList, (o1, o2) -> o1.getName().compareTo(o2.getName()));

		return resultList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isMandatingSupported()
	 * @generated not
	 */
	@Override
	public boolean isMandatingSupported() {
		final DTOBean logOnDTO = getApplicationLogOnDTO();
		final DomainObject clientDomainObject = getDomainObjectByTag(DomainTagEnumeration.CLIENT);

		if (logOnDTO == null || clientDomainObject == null)
			return false;

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getAssociation() == null || attr.getDomainAttribute() == null)
				continue;

			if (!attr.getAssociation().getTarget().equals(clientDomainObject))
				continue;

			if (attr.getAssociation().getTag() == AssociationTagEnumeration.CLIENT_REFERENCE)
				return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllMappingObjectsOfDomainObject(net.codecadenza.eclipse.model.domain.
	 * DomainObject)
	 * @generated not
	 */
	@Override
	public EList<ExchangeMappingObject> getAllMappingObjectsOfDomainObject(DomainObject domainObject) {
		final var resultList = new BasicEList<ExchangeMappingObject>();

		getExchangeNamespace().getChildNamespaces()
				.forEach(n -> n.getJavaTypes().stream().filter(ExchangeMappingObject.class::isInstance).forEach(t -> {
					final var m = (ExchangeMappingObject) t;

					if (m.getDomainObject().equals(domainObject))
						resultList.add(m);
				}));

		return resultList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllExchangeServicesOfDomainObject(net.codecadenza.eclipse.model.domain.
	 * DomainObject)
	 * @generated not
	 */
	@Override
	public EList<DataExchangeServiceBean> getAllExchangeServicesOfDomainObject(DomainObject domainObject) {
		final var resultList = new BasicEList<DataExchangeServiceBean>();

		getExchangeNamespace().getChildNamespaces()
				.forEach(n -> n.getJavaTypes().stream().filter(DataExchangeServiceBean.class::isInstance).forEach(t -> {
					final var s = (DataExchangeServiceBean) t;

					if (domainObject == null || s.getDomainObject().equals(domainObject))
						resultList.add(s);
				}));

		return resultList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllExchangeServices()
	 * @generated not
	 */
	@Override
	public EList<DataExchangeServiceBean> getAllExchangeServices() {
		return getAllExchangeServicesOfDomainObject(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isAddBoundaryInterface()
	 * @generated not
	 */
	@Override
	public boolean isAddBoundaryInterface() {
		// For Angular, JSF, non-GUI and Vaadin applications we always omit the interface!
		if (!hasClient() || hasAngularClient() || hasJSFOrVaadinClient())
			return false;

		return boundaryMode;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTargetProjectName(net.codecadenza.eclipse.model.project.
	 * BuildArtifactType)
	 * @generated not
	 */
	@Override
	public String getTargetProjectName(BuildArtifactType type) {
		for (final BuildArtifact artifact : getBuildConfiguration()) {
			if (artifact.getType() == type)
				return artifact.getName();

			for (final BuildArtifactType containedType : artifact.getContainedArtifacts())
				if (containedType == type)
					return artifact.getName();
		}

		throw new IllegalStateException("The project name could not be derived from the build configuration!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getSourceFolder()
	 * @generated not
	 */
	@Override
	public String getSourceFolder() {
		return SRC_MAIN_JAVA_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTestSourceFolder()
	 * @generated not
	 */
	@Override
	public String getTestSourceFolder() {
		return SRC_TEST_JAVA_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getMetaInfFolder()
	 * @generated not
	 */
	@Override
	public String getMetaInfFolder() {
		return getResourceFolder() + "/" + META_INF_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getWebInfFolder()
	 * @generated not
	 */
	@Override
	public String getWebInfFolder() {
		return getWebAppFolder() + "/" + WEB_INF_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getWebAppFolder()
	 * @generated not
	 */
	@Override
	public String getWebAppFolder() {
		return SRC_WEBAPP_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getConfigFolder(net.codecadenza.eclipse.model.project.BuildArtifactType)
	 * @generated not
	 */
	@Override
	public String getConfigFolder(BuildArtifactType type) {
		if (type == BuildArtifactType.GUI) {
			if (hasJSFOrVaadinClient()) {
				if (isSpringBootApplication())
					return getResourceFolder();

				return getWebInfFolder() + "/" + CLASSES_FOLDER + "/" + CONFIG_FOLDER;
			}

			return getResourceFolder() + "/" + CONFIG_FOLDER;
		}
		else if (type == BuildArtifactType.INTEGRATION_CLIENT_REST || type == BuildArtifactType.INTEGRATION_CLIENT_SOAP
				|| type == BuildArtifactType.INTEGRATION_CLIENT_KAFKA)
			return getResourceFolder() + "/" + CONFIG_FOLDER;
		else if (type == BuildArtifactType.INTEGRATION_CLIENT_JMS || type == BuildArtifactType.INTEGRATION_CLIENT_RMI)
			return getResourceFolder();
		else {
			if (isSpringBootApplication())
				return getResourceFolder();

			return getWebInfFolder() + "/" + CLASSES_FOLDER + "/" + CONFIG_FOLDER;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTestResourceFolder()
	 * @generated not
	 */
	@Override
	public String getTestResourceFolder() {
		return SRC_TEST_RESOURCES_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTestDataFolder()
	 * @generated not
	 */
	@Override
	public String getTestDataFolder() {
		return getTestResourceFolder() + "/" + TEST_DATA_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getSchemaFolder()
	 * @generated not
	 */
	@Override
	public String getSchemaFolder() {
		if (isSpringBootApplication() || isJavaSEApplication())
			return getResourceFolder() + "/" + SCHEMA_FOLDER;

		return getWebInfFolder() + "/" + CLASSES_FOLDER + "/" + SCHEMA_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getResourceFolder()
	 * @generated not
	 */
	@Override
	public String getResourceFolder() {
		return SRC_MAIN_RESOURCES_FOLDER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getIntegrationModuleByArtifact(net.codecadenza.eclipse.model.project.
	 * BuildArtifactType)
	 * @generated not
	 */
	@Override
	public IntegrationModule getIntegrationModuleByArtifact(BuildArtifactType artifactType) {
		return this.getIntegrationModules().stream()
				.filter(module -> artifactType.getName().endsWith(module.getTechnology().getName())).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#searchIntegrationBean(net.codecadenza.eclipse.model.project.
	 * IntegrationTechnology, net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated not
	 */
	@Override
	public AbstractIntegrationBean searchIntegrationBean(IntegrationTechnology technology, DomainObject domainObject) {
		final IntegrationModule module = getIntegrationModules().stream().filter(m -> m.getTechnology() == technology).findFirst()
				.orElse(null);

		if (module == null)
			return null;

		final Stream<JavaType> moduleTypes = module.getNamespace().getJavaTypes().stream();

		return moduleTypes.map(AbstractIntegrationBean.class::cast).filter(a -> a.getDomainObject().equals(domainObject)).findFirst()
				.orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#artifactExists(net.codecadenza.eclipse.model.project.BuildArtifactType)
	 * @generated not
	 */
	@Override
	public boolean artifactExists(BuildArtifactType artifactType) {
		final boolean found = getBuildConfiguration().stream().anyMatch(e -> e.getType() == artifactType);

		if (found)
			return true;

		return getBuildConfiguration().stream().map(BuildArtifact::getContainedArtifacts).anyMatch(e -> e.contains(artifactType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getTestModuleByArtifact(net.codecadenza.eclipse.model.project.
	 * BuildArtifactType)
	 * @generated not
	 */
	@Override
	public AbstractTestModule getTestModuleByArtifact(BuildArtifactType artifactType) {
		return this.getTestModules().stream()
				.filter(testModule -> artifactType == BuildArtifactType.SELENIUM_TEST && testModule instanceof SeleniumTestModule)
				.findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getAllGUITestCases()
	 * @generated not
	 */
	@Override
	public List<GUITestCase> getAllGUITestCases() {
		final AbstractTestModule testModule = getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);

		if (testModule == null)
			return Collections.emptyList();

		return testModule.getTestCases().stream().map(GUITestCase.class::cast).toList();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDataSourceJNDIName()
	 * @generated not
	 */
	@Override
	public String getDataSourceJNDIName() {
		if (isDeployedOnGlassfish())
			return "java:app/jdbc/" + getDataSource().getName();
		else if (isDeployedOnJBoss())
			return "java:jboss/datasources/" + getDataSource().getName();

		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#getDataSourceFileName()
	 * @generated not
	 */
	@Override
	public String getDataSourceFileName() {
		if (isDeployedOnGlassfish())
			return "glassfish-resources.xml";
		else if (isDeployedOnJBoss())
			return "app-ds.xml";

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasClient()
	 * @generated not
	 */
	@Override
	public boolean hasClient() {
		return clientPlatform != ClientPlatformEnumeration.NONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasAngularClient()
	 * @generated not
	 */
	@Override
	public boolean hasAngularClient() {
		return clientPlatform == ClientPlatformEnumeration.ANGULAR;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasEclipseClient()
	 * @generated not
	 */
	@Override
	public boolean hasEclipseClient() {
		return hasRAPClient() || hasRCPClient();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasJavaFXClient()
	 * @generated not
	 */
	@Override
	public boolean hasJavaFXClient() {
		return clientPlatform == ClientPlatformEnumeration.JAVAFX;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasJSFClient()
	 * @generated not
	 */
	@Override
	public boolean hasJSFClient() {
		return clientPlatform == ClientPlatformEnumeration.JSF_PRIMEFACES;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasJSFOrVaadinClient()
	 * @generated not
	 */
	@Override
	public boolean hasJSFOrVaadinClient() {
		return hasJSFClient() || hasVaadinClient();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasRAPClient()
	 * @generated not
	 */
	@Override
	public boolean hasRAPClient() {
		return clientPlatform == ClientPlatformEnumeration.RAP;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasRCPClient()
	 * @generated not
	 */
	@Override
	public boolean hasRCPClient() {
		return clientPlatform == ClientPlatformEnumeration.RCP;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasSwingClient()
	 * @generated not
	 */
	@Override
	public boolean hasSwingClient() {
		return clientPlatform == ClientPlatformEnumeration.SWING;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#hasVaadinClient()
	 * @generated not
	 */
	@Override
	public boolean hasVaadinClient() {
		return clientPlatform == ClientPlatformEnumeration.VAADIN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isJakartaEEApplication()
	 * @generated not
	 */
	@Override
	public boolean isJakartaEEApplication() {
		return technology == TechnologyPlatformEnumeration.JAKARTA_EE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isJavaSEApplication()
	 * @generated not
	 */
	@Override
	public boolean isJavaSEApplication() {
		return technology == TechnologyPlatformEnumeration.JAVA_SE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isSpringBootApplication()
	 * @generated not
	 */
	@Override
	public boolean isSpringBootApplication() {
		return technology == TechnologyPlatformEnumeration.SPRING_BOOT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isDeployedOnGlassfish()
	 * @generated not
	 */
	@Override
	public boolean isDeployedOnGlassfish() {
		return serverPlatform == ServerPlatformEnumeration.GLASSFISH;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isDeployedOnJBoss()
	 * @generated not
	 */
	@Override
	public boolean isDeployedOnJBoss() {
		return serverPlatform == ServerPlatformEnumeration.JBOSS;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Project#isDeployedOnTomcat()
	 * @generated not
	 */
	@Override
	public boolean isDeployedOnTomcat() {
		return serverPlatform == ServerPlatformEnumeration.TOMCAT;
	}

}
