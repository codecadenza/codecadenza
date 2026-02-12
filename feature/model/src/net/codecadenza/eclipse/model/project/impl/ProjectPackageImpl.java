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

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.impl.ClientPackageImpl;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.impl.DbPackageImpl;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.impl.JavaPackageImpl;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
import net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl;
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
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.project.XMLMappingType;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl;
import net.codecadenza.eclipse.model.service.ServicePackage;
import net.codecadenza.eclipse.model.service.impl.ServicePackageImpl;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;

/**
 * The <b>Package</b> for the model. It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * @generated
 */
public class ProjectPackageImpl extends EPackageImpl implements ProjectPackage {
	/**
	 * @generated
	 */
	private EClass datasourceEClass;

	/**
	 * @generated
	 */
	private EClass persistenceUnitPropertyEClass;

	/**
	 * @generated
	 */
	private EClass projectEClass;

	/**
	 * @generated
	 */
	private EClass roleEClass;

	/**
	 * @generated
	 */
	private EClass buildArtifactEClass;

	/**
	 * @generated
	 */
	private EClass integrationModuleEClass;

	/**
	 * @generated
	 */
	private EEnum clientPlatformEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum serverPlatformEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum persistenceProviderEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum jpaVersionEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum technologyPlatformEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum validationTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum buildToolEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum buildArtifactTypeEEnum;

	/**
	 * @generated
	 */
	private EEnum mappingAnnotationStrategyEEnum;

	/**
	 * @generated
	 */
	private EEnum xmlMappingTypeEEnum;

	/**
	 * @generated
	 */
	private EEnum integrationTechnologyEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ProjectPackageImpl() {
		super(eNS_URI, ProjectFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link ProjectPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized project package
	 * @generated
	 */
	public static ProjectPackage init() {
		if (isInited)
			return (ProjectPackage) EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI);

		// Obtain or create and register package
		final var theProjectPackage = (ProjectPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof ProjectPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new ProjectPackageImpl());

		isInited = true;

		// Obtain or create and register interdependencies
		final var theBoundaryPackage = (BoundaryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(BoundaryPackage.eNS_URI) instanceof BoundaryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI) : BoundaryPackage.eINSTANCE);
		final var theClientPackage = (ClientPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ClientPackage.eNS_URI) instanceof ClientPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI) : ClientPackage.eINSTANCE);
		final var theRepositoryPackage = (RepositoryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(RepositoryPackage.eNS_URI) instanceof RepositoryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(RepositoryPackage.eNS_URI) : RepositoryPackage.eINSTANCE);
		final var theDbPackage = (DbPackageImpl) (EPackage.Registry.INSTANCE.getEPackage(DbPackage.eNS_URI) instanceof DbPackageImpl
				? EPackage.Registry.INSTANCE.getEPackage(DbPackage.eNS_URI) : DbPackage.eINSTANCE);
		final var theDomainPackage = (DomainPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(DomainPackage.eNS_URI) instanceof DomainPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI) : DomainPackage.eINSTANCE);
		final var theDtoPackage = (DtoPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(DtoPackage.eNS_URI) instanceof DtoPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(DtoPackage.eNS_URI)
						: DtoPackage.eINSTANCE);
		final var theJavaPackage = (JavaPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(JavaPackage.eNS_URI) instanceof JavaPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI)
						: JavaPackage.eINSTANCE);
		final var theExchangePackage = (ExchangePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ExchangePackage.eNS_URI) instanceof ExchangePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ExchangePackage.eNS_URI) : ExchangePackage.eINSTANCE);
		final var theServicePackage = (ServicePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ServicePackage.eNS_URI) instanceof ServicePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI) : ServicePackage.eINSTANCE);
		final var theMappingPackage = (MappingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(MappingPackage.eNS_URI) instanceof MappingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI) : MappingPackage.eINSTANCE);
		final var theIntegrationPackage = (IntegrationPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(IntegrationPackage.eNS_URI) instanceof IntegrationPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(IntegrationPackage.eNS_URI) : IntegrationPackage.eINSTANCE);
		final var theTestingPackage = (TestingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(TestingPackage.eNS_URI) instanceof TestingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(TestingPackage.eNS_URI) : TestingPackage.eINSTANCE);

		// Create package meta-data objects
		theProjectPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();
		theIntegrationPackage.createPackageContents();
		theTestingPackage.createPackageContents();

		// Initialize created meta-data
		theProjectPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();
		theIntegrationPackage.initializePackageContents();
		theTestingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theProjectPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ProjectPackage.eNS_URI, theProjectPackage);
		return theProjectPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource()
	 * @generated
	 */
	@Override
	public EClass getDatasource() {
		return datasourceEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDatasource_Name() {
		return (EAttribute) datasourceEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_ConnectionURL()
	 * @generated
	 */
	@Override
	public EAttribute getDatasource_ConnectionURL() {
		return (EAttribute) datasourceEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_UserName()
	 * @generated
	 */
	@Override
	public EAttribute getDatasource_UserName() {
		return (EAttribute) datasourceEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_Password()
	 * @generated
	 */
	@Override
	public EAttribute getDatasource_Password() {
		return (EAttribute) datasourceEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_DriverName()
	 * @generated
	 */
	@Override
	public EAttribute getDatasource_DriverName() {
		return (EAttribute) datasourceEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_DriverList()
	 * @generated
	 */
	@Override
	public EAttribute getDatasource_DriverList() {
		return (EAttribute) datasourceEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getPersistenceUnitProperty()
	 * @generated
	 */
	@Override
	public EClass getPersistenceUnitProperty() {
		return persistenceUnitPropertyEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getPersistenceUnitProperty_Value()
	 * @generated
	 */
	@Override
	public EAttribute getPersistenceUnitProperty_Value() {
		return (EAttribute) persistenceUnitPropertyEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getPersistenceUnitProperty_Name()
	 * @generated
	 */
	@Override
	public EAttribute getPersistenceUnitProperty_Name() {
		return (EAttribute) persistenceUnitPropertyEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject()
	 * @generated
	 */
	@Override
	public EClass getProject() {
		return projectEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Name()
	 * @generated
	 */
	@Override
	public EAttribute getProject_Name() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Code()
	 * @generated
	 */
	@Override
	public EAttribute getProject_Code() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ServerPlatform()
	 * @generated
	 */
	@Override
	public EAttribute getProject_ServerPlatform() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ClientPlatform()
	 * @generated
	 */
	@Override
	public EAttribute getProject_ClientPlatform() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Roles()
	 * @generated
	 */
	@Override
	public EReference getProject_Roles() {
		return (EReference) projectEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_PersistenceUnitProperties()
	 * @generated
	 */
	@Override
	public EReference getProject_PersistenceUnitProperties() {
		return (EReference) projectEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DataSource()
	 * @generated
	 */
	@Override
	public EReference getProject_DataSource() {
		return (EReference) projectEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_RootNamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_RootNamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_RepositoryNamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_RepositoryNamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DTONamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_DTONamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BoundaryNamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_BoundaryNamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DomainNamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_DomainNamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ClientNamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_ClientNamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_SupportedStandardNamespaces()
	 * @generated
	 */
	@Override
	public EReference getProject_SupportedStandardNamespaces() {
		return (EReference) projectEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Database()
	 * @generated
	 */
	@Override
	public EReference getProject_Database() {
		return (EReference) projectEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_AllSupportedTypes()
	 * @generated
	 */
	@Override
	public EReference getProject_AllSupportedTypes() {
		return (EReference) projectEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_FormGroups()
	 * @generated
	 */
	@Override
	public EReference getProject_FormGroups() {
		return (EReference) projectEClass.getEStructuralFeatures().get(16);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_PersistenceProvider()
	 * @generated
	 */
	@Override
	public EAttribute getProject_PersistenceProvider() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(17);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_JpaVersion()
	 * @generated
	 */
	@Override
	public EAttribute getProject_JpaVersion() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(18);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_Technology()
	 * @generated
	 */
	@Override
	public EAttribute getProject_Technology() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(19);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ValidationType()
	 * @generated
	 */
	@Override
	public EAttribute getProject_ValidationType() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(20);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BoundaryMode()
	 * @generated
	 */
	@Override
	public EAttribute getProject_BoundaryMode() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(21);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ExchangeNamespace()
	 * @generated
	 */
	@Override
	public EReference getProject_ExchangeNamespace() {
		return (EReference) projectEClass.getEStructuralFeatures().get(22);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BuildTool()
	 * @generated
	 */
	@Override
	public EAttribute getProject_BuildTool() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(23);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_BuildConfiguration()
	 * @generated
	 */
	@Override
	public EReference getProject_BuildConfiguration() {
		return (EReference) projectEClass.getEStructuralFeatures().get(24);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole()
	 * @generated
	 */
	@Override
	public EClass getRole() {
		return roleEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole_Name()
	 * @generated
	 */
	@Override
	public EAttribute getRole_Name() {
		return (EAttribute) roleEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole_AdminRole()
	 * @generated
	 */
	@Override
	public EAttribute getRole_AdminRole() {
		return (EAttribute) roleEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole_ReadonlyRole()
	 * @generated
	 */
	@Override
	public EAttribute getRole_ReadonlyRole() {
		return (EAttribute) roleEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact()
	 * @generated
	 */
	@Override
	public EClass getBuildArtifact() {
		return buildArtifactEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_Name()
	 * @generated
	 */
	@Override
	public EAttribute getBuildArtifact_Name() {
		return (EAttribute) buildArtifactEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_Type()
	 * @generated
	 */
	@Override
	public EAttribute getBuildArtifact_Type() {
		return (EAttribute) buildArtifactEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_ContainedArtifacts()
	 * @generated
	 */
	@Override
	public EAttribute getBuildArtifact_ContainedArtifacts() {
		return (EAttribute) buildArtifactEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_Project()
	 * @generated
	 */
	@Override
	public EReference getBuildArtifact_Project() {
		return (EReference) buildArtifactEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getClientPlatformEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getClientPlatformEnumeration() {
		return clientPlatformEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getServerPlatformEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getServerPlatformEnumeration() {
		return serverPlatformEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getPersistenceProviderEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getPersistenceProviderEnumeration() {
		return persistenceProviderEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getJPAVersionEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getJPAVersionEnumeration() {
		return jpaVersionEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getTechnologyPlatformEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getTechnologyPlatformEnumeration() {
		return technologyPlatformEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getValidationTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getValidationTypeEnumeration() {
		return validationTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildToolEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getBuildToolEnumeration() {
		return buildToolEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifactType()
	 * @generated
	 */
	@Override
	public EEnum getBuildArtifactType() {
		return buildArtifactTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_XmlNamespace()
	 * @generated
	 */
	@Override
	public EAttribute getProject_XmlNamespace() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(25);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_IntegrationModules()
	 * @generated
	 */
	@Override
	public EReference getProject_IntegrationModules() {
		return (EReference) projectEClass.getEStructuralFeatures().get(26);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_MappingStrategy()
	 * @generated
	 */
	@Override
	public EAttribute getProject_MappingStrategy() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(27);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_DefaultXMLMappingType()
	 * @generated
	 */
	@Override
	public EAttribute getProject_DefaultXMLMappingType() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(28);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_XmlNamespacePrefix()
	 * @generated
	 */
	@Override
	public EAttribute getProject_XmlNamespacePrefix() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(29);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_TestModules()
	 * @generated
	 */
	@Override
	public EReference getProject_TestModules() {
		return (EReference) projectEClass.getEStructuralFeatures().get(30);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProject_ProtectManualChanges()
	 * @generated
	 */
	@Override
	public EAttribute getProject_ProtectManualChanges() {
		return (EAttribute) projectEClass.getEStructuralFeatures().get(31);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule()
	 * @generated
	 */
	@Override
	public EClass getIntegrationModule() {
		return integrationModuleEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_Technology()
	 * @generated
	 */
	@Override
	public EAttribute getIntegrationModule_Technology() {
		return (EAttribute) integrationModuleEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_Project()
	 * @generated
	 */
	@Override
	public EReference getIntegrationModule_Project() {
		return (EReference) integrationModuleEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_Namespace()
	 * @generated
	 */
	@Override
	public EReference getIntegrationModule_Namespace() {
		return (EReference) integrationModuleEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_AddSecurityHandler()
	 * @generated
	 */
	@Override
	public EAttribute getIntegrationModule_AddSecurityHandler() {
		return (EAttribute) integrationModuleEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_AddProducers()
	 * @generated
	 */
	@Override
	public EAttribute getIntegrationModule_AddProducers() {
		return (EAttribute) integrationModuleEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getMappingAnnotationStrategy()
	 * @generated
	 */
	@Override
	public EEnum getMappingAnnotationStrategy() {
		return mappingAnnotationStrategyEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getXMLMappingType()
	 * @generated
	 */
	@Override
	public EEnum getXMLMappingType() {
		return xmlMappingTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationTechnology()
	 * @generated
	 */
	@Override
	public EEnum getIntegrationTechnology() {
		return integrationTechnologyEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getProjectFactory()
	 * @generated
	 */
	@Override
	public ProjectFactory getProjectFactory() {
		return (ProjectFactory) getEFactoryInstance();
	}

	/**
	 * @generated
	 */
	private boolean isCreated;

	/**
	 * Create the meta-model objects for the package. This method is guarded to have no affect on any invocation but its first.
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated)
			return;

		isCreated = true;

		// Create classes and their features
		datasourceEClass = createEClass(DATASOURCE);
		createEAttribute(datasourceEClass, DATASOURCE__NAME);
		createEAttribute(datasourceEClass, DATASOURCE__CONNECTION_URL);
		createEAttribute(datasourceEClass, DATASOURCE__USER_NAME);
		createEAttribute(datasourceEClass, DATASOURCE__PASSWORD);
		createEAttribute(datasourceEClass, DATASOURCE__DRIVER_NAME);
		createEAttribute(datasourceEClass, DATASOURCE__DRIVER_LIST);

		persistenceUnitPropertyEClass = createEClass(PERSISTENCE_UNIT_PROPERTY);
		createEAttribute(persistenceUnitPropertyEClass, PERSISTENCE_UNIT_PROPERTY__VALUE);
		createEAttribute(persistenceUnitPropertyEClass, PERSISTENCE_UNIT_PROPERTY__NAME);

		projectEClass = createEClass(PROJECT);
		createEAttribute(projectEClass, PROJECT__NAME);
		createEAttribute(projectEClass, PROJECT__CODE);
		createEAttribute(projectEClass, PROJECT__SERVER_PLATFORM);
		createEAttribute(projectEClass, PROJECT__CLIENT_PLATFORM);
		createEReference(projectEClass, PROJECT__ROLES);
		createEReference(projectEClass, PROJECT__PERSISTENCE_UNIT_PROPERTIES);
		createEReference(projectEClass, PROJECT__DATA_SOURCE);
		createEReference(projectEClass, PROJECT__ROOT_NAMESPACE);
		createEReference(projectEClass, PROJECT__REPOSITORY_NAMESPACE);
		createEReference(projectEClass, PROJECT__DTO_NAMESPACE);
		createEReference(projectEClass, PROJECT__BOUNDARY_NAMESPACE);
		createEReference(projectEClass, PROJECT__DOMAIN_NAMESPACE);
		createEReference(projectEClass, PROJECT__CLIENT_NAMESPACE);
		createEReference(projectEClass, PROJECT__SUPPORTED_STANDARD_NAMESPACES);
		createEReference(projectEClass, PROJECT__DATABASE);
		createEReference(projectEClass, PROJECT__ALL_SUPPORTED_TYPES);
		createEReference(projectEClass, PROJECT__FORM_GROUPS);
		createEAttribute(projectEClass, PROJECT__PERSISTENCE_PROVIDER);
		createEAttribute(projectEClass, PROJECT__JPA_VERSION);
		createEAttribute(projectEClass, PROJECT__TECHNOLOGY);
		createEAttribute(projectEClass, PROJECT__VALIDATION_TYPE);
		createEAttribute(projectEClass, PROJECT__BOUNDARY_MODE);
		createEReference(projectEClass, PROJECT__EXCHANGE_NAMESPACE);
		createEAttribute(projectEClass, PROJECT__BUILD_TOOL);
		createEReference(projectEClass, PROJECT__BUILD_CONFIGURATION);
		createEAttribute(projectEClass, PROJECT__XML_NAMESPACE);
		createEReference(projectEClass, PROJECT__INTEGRATION_MODULES);
		createEAttribute(projectEClass, PROJECT__MAPPING_STRATEGY);
		createEAttribute(projectEClass, PROJECT__DEFAULT_XML_MAPPING_TYPE);
		createEAttribute(projectEClass, PROJECT__XML_NAMESPACE_PREFIX);
		createEReference(projectEClass, PROJECT__TEST_MODULES);
		createEAttribute(projectEClass, PROJECT__PROTECT_MANUAL_CHANGES);

		roleEClass = createEClass(ROLE);
		createEAttribute(roleEClass, ROLE__NAME);
		createEAttribute(roleEClass, ROLE__ADMIN_ROLE);
		createEAttribute(roleEClass, ROLE__READONLY_ROLE);

		buildArtifactEClass = createEClass(BUILD_ARTIFACT);
		createEAttribute(buildArtifactEClass, BUILD_ARTIFACT__NAME);
		createEAttribute(buildArtifactEClass, BUILD_ARTIFACT__TYPE);
		createEAttribute(buildArtifactEClass, BUILD_ARTIFACT__CONTAINED_ARTIFACTS);
		createEReference(buildArtifactEClass, BUILD_ARTIFACT__PROJECT);

		integrationModuleEClass = createEClass(INTEGRATION_MODULE);
		createEAttribute(integrationModuleEClass, INTEGRATION_MODULE__TECHNOLOGY);
		createEReference(integrationModuleEClass, INTEGRATION_MODULE__PROJECT);
		createEReference(integrationModuleEClass, INTEGRATION_MODULE__NAMESPACE);
		createEAttribute(integrationModuleEClass, INTEGRATION_MODULE__ADD_SECURITY_HANDLER);
		createEAttribute(integrationModuleEClass, INTEGRATION_MODULE__ADD_PRODUCERS);

		// Create enums
		clientPlatformEnumerationEEnum = createEEnum(CLIENT_PLATFORM_ENUMERATION);
		serverPlatformEnumerationEEnum = createEEnum(SERVER_PLATFORM_ENUMERATION);
		persistenceProviderEnumerationEEnum = createEEnum(PERSISTENCE_PROVIDER_ENUMERATION);
		jpaVersionEnumerationEEnum = createEEnum(JPA_VERSION_ENUMERATION);
		technologyPlatformEnumerationEEnum = createEEnum(TECHNOLOGY_PLATFORM_ENUMERATION);
		validationTypeEnumerationEEnum = createEEnum(VALIDATION_TYPE_ENUMERATION);
		buildToolEnumerationEEnum = createEEnum(BUILD_TOOL_ENUMERATION);
		buildArtifactTypeEEnum = createEEnum(BUILD_ARTIFACT_TYPE);
		mappingAnnotationStrategyEEnum = createEEnum(MAPPING_ANNOTATION_STRATEGY);
		xmlMappingTypeEEnum = createEEnum(XML_MAPPING_TYPE);
		integrationTechnologyEEnum = createEEnum(INTEGRATION_TECHNOLOGY);
	}

	/**
	 * @generated
	 */
	private boolean isInitialized;

	/**
	 * Complete the initialization of the package and its meta-model. This method is guarded to have no affect on any invocation but
	 * its first.
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized)
			return;

		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		final var theJavaPackage = (JavaPackage) EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI);
		final var theDbPackage = (DbPackage) EPackage.Registry.INSTANCE.getEPackage(DbPackage.eNS_URI);
		final var theClientPackage = (ClientPackage) EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI);
		final var theTestingPackage = (TestingPackage) EPackage.Registry.INSTANCE.getEPackage(TestingPackage.eNS_URI);

		// Initialize classes and features; add operations and parameters
		initEClass(datasourceEClass, Datasource.class, "Datasource", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDatasource_Name(), ecorePackage.getEString(), "name", null, 0, 1, Datasource.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatasource_ConnectionURL(), ecorePackage.getEString(), "connectionURL", null, 0, 1, Datasource.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatasource_UserName(), ecorePackage.getEString(), "userName", null, 0, 1, Datasource.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatasource_Password(), ecorePackage.getEString(), "password", null, 0, 1, Datasource.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatasource_DriverName(), ecorePackage.getEString(), "driverName", null, 0, 1, Datasource.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatasource_DriverList(), ecorePackage.getEString(), "driverList", null, 0, -1, Datasource.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(persistenceUnitPropertyEClass, PersistenceUnitProperty.class, "PersistenceUnitProperty", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getPersistenceUnitProperty_Value(), ecorePackage.getEString(), "value", null, 0, 1,
				PersistenceUnitProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getPersistenceUnitProperty_Name(), ecorePackage.getEString(), "name", null, 0, 1,
				PersistenceUnitProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(projectEClass, Project.class, "Project", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getProject_Name(), ecorePackage.getEString(), "name", null, 0, 1, Project.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_Code(), ecorePackage.getEString(), "code", null, 0, 1, Project.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_ServerPlatform(), this.getServerPlatformEnumeration(), "serverPlatform", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_ClientPlatform(), this.getClientPlatformEnumeration(), "clientPlatform", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_Roles(), this.getRole(), null, "roles", null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_PersistenceUnitProperties(), this.getPersistenceUnitProperty(), null, "persistenceUnitProperties",
				null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_DataSource(), this.getDatasource(), null, "dataSource", null, 0, 1, Project.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_RootNamespace(), theJavaPackage.getNamespace(), null, "rootNamespace", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getProject_RepositoryNamespace(), theJavaPackage.getNamespace(), null, "repositoryNamespace", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getProject_DTONamespace(), theJavaPackage.getNamespace(), null, "dTONamespace", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getProject_BoundaryNamespace(), theJavaPackage.getNamespace(), null, "boundaryNamespace", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getProject_DomainNamespace(), theJavaPackage.getNamespace(), null, "domainNamespace", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getProject_ClientNamespace(), theJavaPackage.getNamespace(), null, "clientNamespace", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getProject_SupportedStandardNamespaces(), theJavaPackage.getNamespace(), null, "supportedStandardNamespaces",
				null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_Database(), theDbPackage.getDatabase(), theDbPackage.getDatabase_Project(), "database", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getProject_AllSupportedTypes(), theJavaPackage.getJavaType(), null, "allSupportedTypes", null, 0, -1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getProject_FormGroups(), theClientPackage.getFormGroup(), theClientPackage.getFormGroup_Project(),
				"formGroups", null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_PersistenceProvider(), this.getPersistenceProviderEnumeration(), "persistenceProvider", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_JpaVersion(), this.getJPAVersionEnumeration(), "jpaVersion", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_Technology(), this.getTechnologyPlatformEnumeration(), "technology", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_ValidationType(), this.getValidationTypeEnumeration(), "validationType", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_BoundaryMode(), ecorePackage.getEBoolean(), "boundaryMode", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_ExchangeNamespace(), theJavaPackage.getNamespace(), null, "exchangeNamespace", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_BuildTool(), this.getBuildToolEnumeration(), "buildTool", null, 0, 1, Project.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_BuildConfiguration(), this.getBuildArtifact(), this.getBuildArtifact_Project(),
				"buildConfiguration", null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_XmlNamespace(), ecorePackage.getEString(), "xmlNamespace", null, 0, 1, Project.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_IntegrationModules(), this.getIntegrationModule(), this.getIntegrationModule_Project(),
				"integrationModules", null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_MappingStrategy(), this.getMappingAnnotationStrategy(), "mappingStrategy", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_DefaultXMLMappingType(), this.getXMLMappingType(), "defaultXMLMappingType", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_XmlNamespacePrefix(), ecorePackage.getEString(), "xmlNamespacePrefix", null, 0, 1, Project.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getProject_TestModules(), theTestingPackage.getAbstractTestModule(),
				theTestingPackage.getAbstractTestModule_Project(), "testModules", null, 0, -1, Project.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getProject_ProtectManualChanges(), ecorePackage.getEBoolean(), "protectManualChanges", null, 0, 1,
				Project.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(roleEClass, Role.class, "Role", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getRole_Name(), ecorePackage.getEString(), "name", null, 0, 1, Role.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getRole_AdminRole(), ecorePackage.getEBoolean(), "adminRole", null, 0, 1, Role.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getRole_ReadonlyRole(), ecorePackage.getEBoolean(), "readonlyRole", null, 0, 1, Role.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(buildArtifactEClass, BuildArtifact.class, "BuildArtifact", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getBuildArtifact_Name(), ecorePackage.getEString(), "name", null, 0, 1, BuildArtifact.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getBuildArtifact_Type(), this.getBuildArtifactType(), "type", null, 0, 1, BuildArtifact.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getBuildArtifact_ContainedArtifacts(), this.getBuildArtifactType(), "containedArtifacts", null, 0, -1,
				BuildArtifact.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getBuildArtifact_Project(), this.getProject(), this.getProject_BuildConfiguration(), "project", null, 0, 1,
				BuildArtifact.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(integrationModuleEClass, IntegrationModule.class, "IntegrationModule", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getIntegrationModule_Technology(), this.getIntegrationTechnology(), "technology", null, 0, 1,
				IntegrationModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getIntegrationModule_Project(), this.getProject(), this.getProject_IntegrationModules(), "project", null, 0, 1,
				IntegrationModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getIntegrationModule_Namespace(), theJavaPackage.getNamespace(), null, "namespace", null, 0, 1,
				IntegrationModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getIntegrationModule_AddSecurityHandler(), ecorePackage.getEBoolean(), "addSecurityHandler", null, 0, 1,
				IntegrationModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getIntegrationModule_AddProducers(), ecorePackage.getEBoolean(), "addProducers", null, 0, 1,
				IntegrationModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.class, "ClientPlatformEnumeration");
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.NONE);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.RCP);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.SWING);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.JSF_PRIMEFACES);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.RAP);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.VAADIN);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.JAVAFX);
		addEEnumLiteral(clientPlatformEnumerationEEnum, ClientPlatformEnumeration.ANGULAR);

		initEEnum(serverPlatformEnumerationEEnum, ServerPlatformEnumeration.class, "ServerPlatformEnumeration");
		addEEnumLiteral(serverPlatformEnumerationEEnum, ServerPlatformEnumeration.WILDFLY);
		addEEnumLiteral(serverPlatformEnumerationEEnum, ServerPlatformEnumeration.PAYARA);
		addEEnumLiteral(serverPlatformEnumerationEEnum, ServerPlatformEnumeration.NONE);
		addEEnumLiteral(serverPlatformEnumerationEEnum, ServerPlatformEnumeration.TOMCAT);

		initEEnum(persistenceProviderEnumerationEEnum, PersistenceProviderEnumeration.class, "PersistenceProviderEnumeration");
		addEEnumLiteral(persistenceProviderEnumerationEEnum, PersistenceProviderEnumeration.HIBERNATE);
		addEEnumLiteral(persistenceProviderEnumerationEEnum, PersistenceProviderEnumeration.ECLIPSELINK);

		initEEnum(jpaVersionEnumerationEEnum, JPAVersionEnumeration.class, "JPAVersionEnumeration");
		addEEnumLiteral(jpaVersionEnumerationEEnum, JPAVersionEnumeration.JPA3);

		initEEnum(technologyPlatformEnumerationEEnum, TechnologyPlatformEnumeration.class, "TechnologyPlatformEnumeration");
		addEEnumLiteral(technologyPlatformEnumerationEEnum, TechnologyPlatformEnumeration.JAKARTA_EE);
		addEEnumLiteral(technologyPlatformEnumerationEEnum, TechnologyPlatformEnumeration.JAVA_SE);
		addEEnumLiteral(technologyPlatformEnumerationEEnum, TechnologyPlatformEnumeration.SPRING_BOOT);

		initEEnum(validationTypeEnumerationEEnum, ValidationTypeEnumeration.class, "ValidationTypeEnumeration");
		addEEnumLiteral(validationTypeEnumerationEEnum, ValidationTypeEnumeration.INTERNAL);
		addEEnumLiteral(validationTypeEnumerationEEnum, ValidationTypeEnumeration.STANDARD);

		initEEnum(buildToolEnumerationEEnum, BuildToolEnumeration.class, "BuildToolEnumeration");
		addEEnumLiteral(buildToolEnumerationEEnum, BuildToolEnumeration.MAVEN);

		initEEnum(buildArtifactTypeEEnum, BuildArtifactType.class, "BuildArtifactType");
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.DOMAIN);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.REPOSITORY);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.BOUNDARY);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.FACADE);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.SERVICE);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.CLIENT_INTERFACE);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.SERVER);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.GUI);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.MASTER);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.DTO);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.DATA_EXCHANGE);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_IMP_SOAP);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_SEI_SOAP);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_CLIENT_SOAP);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_IMP_REST);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_SEI_REST);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_CLIENT_REST);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_IMP_RMI);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_SEI_RMI);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_CLIENT_RMI);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_IMP_KAFKA);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_SEI_KAFKA);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_CLIENT_KAFKA);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_IMP_JMS);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_SEI_JMS);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_CLIENT_JMS);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.SELENIUM_TEST);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.SHARED);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_TEST_SOAP);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_TEST_REST);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_TEST_RMI);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_TEST_KAFKA);
		addEEnumLiteral(buildArtifactTypeEEnum, BuildArtifactType.INTEGRATION_TEST_JMS);

		initEEnum(mappingAnnotationStrategyEEnum, MappingAnnotationStrategy.class, "MappingAnnotationStrategy");
		addEEnumLiteral(mappingAnnotationStrategyEEnum, MappingAnnotationStrategy.NEVER);
		addEEnumLiteral(mappingAnnotationStrategyEEnum, MappingAnnotationStrategy.ALWAYS);
		addEEnumLiteral(mappingAnnotationStrategyEEnum, MappingAnnotationStrategy.ON_DEMAND);

		initEEnum(xmlMappingTypeEEnum, XMLMappingType.class, "XMLMappingType");
		addEEnumLiteral(xmlMappingTypeEEnum, XMLMappingType.ATTRIBUTE);
		addEEnumLiteral(xmlMappingTypeEEnum, XMLMappingType.ELEMENT);

		initEEnum(integrationTechnologyEEnum, IntegrationTechnology.class, "IntegrationTechnology");
		addEEnumLiteral(integrationTechnologyEEnum, IntegrationTechnology.REST);
		addEEnumLiteral(integrationTechnologyEEnum, IntegrationTechnology.SOAP);
		addEEnumLiteral(integrationTechnologyEEnum, IntegrationTechnology.RMI);
		addEEnumLiteral(integrationTechnologyEEnum, IntegrationTechnology.KAFKA);
		addEEnumLiteral(integrationTechnologyEEnum, IntegrationTechnology.JMS);

		// Create resource
		createResource(eNS_URI);
	}

}
