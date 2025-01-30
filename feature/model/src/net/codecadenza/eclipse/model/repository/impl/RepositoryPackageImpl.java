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
package net.codecadenza.eclipse.model.repository.impl;

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
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.impl.JavaPackageImpl;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
import net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryFactory;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;
import net.codecadenza.eclipse.model.service.ServicePackage;
import net.codecadenza.eclipse.model.service.impl.ServicePackageImpl;
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
public class RepositoryPackageImpl extends EPackageImpl implements RepositoryPackage {
	/**
	 * @generated
	 */
	private EClass repositoryEClass;

	/**
	 * @generated
	 */
	private EClass repositoryMethodEClass;

	/**
	 * @generated
	 */
	private EClass repositoryMethodParameterEClass;

	/**
	 * @generated
	 */
	private EEnum repositoryMethodTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum permissionModeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum transactionTypeEnumerationEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private RepositoryPackageImpl() {
		super(eNS_URI, RepositoryFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link RepositoryPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized repository package
	 * @generated
	 */
	public static RepositoryPackage init() {
		if (isInited)
			return (RepositoryPackage) EPackage.Registry.INSTANCE.getEPackage(RepositoryPackage.eNS_URI);

		// Obtain or create and register package
		final var theRepositoryPackage = (RepositoryPackageImpl) (EPackage.Registry.INSTANCE
				.get(eNS_URI) instanceof RepositoryPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new RepositoryPackageImpl());

		isInited = true;

		// Obtain or create and register interdependencies
		final var theBoundaryPackage = (BoundaryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(BoundaryPackage.eNS_URI) instanceof BoundaryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI) : BoundaryPackage.eINSTANCE);
		final var theClientPackage = (ClientPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ClientPackage.eNS_URI) instanceof ClientPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI) : ClientPackage.eINSTANCE);
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
		final var theProjectPackage = (ProjectPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ProjectPackage.eNS_URI) instanceof ProjectPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI) : ProjectPackage.eINSTANCE);
		final var theExchangePackage = (ExchangePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ExchangePackage.eNS_URI) instanceof ExchangePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ExchangePackage.eNS_URI) : ExchangePackage.eINSTANCE);
		final var theServicePackage = (ServicePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ServicePackage.eNS_URI) instanceof ServicePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI) : ServicePackage.eINSTANCE);
		final var theMappingPackage = (MappingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(MappingPackage.eNS_URI) instanceof MappingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI) : MappingPackage.eINSTANCE);

		// Create package meta-data objects
		theRepositoryPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theRepositoryPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theRepositoryPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(RepositoryPackage.eNS_URI, theRepositoryPackage);
		return theRepositoryPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepository()
	 * @generated
	 */
	@Override
	public EClass getRepository() {
		return repositoryEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepository_RepositoryMethods()
	 * @generated
	 */
	@Override
	public EReference getRepository_RepositoryMethods() {
		return (EReference) repositoryEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod()
	 * @generated
	 */
	@Override
	public EClass getRepositoryMethod() {
		return repositoryMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod_Repository()
	 * @generated
	 */
	@Override
	public EReference getRepositoryMethod_Repository() {
		return (EReference) repositoryMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod_MethodType()
	 * @generated
	 */
	@Override
	public EAttribute getRepositoryMethod_MethodType() {
		return (EAttribute) repositoryMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod_Hint()
	 * @generated
	 */
	@Override
	public EAttribute getRepositoryMethod_Hint() {
		return (EAttribute) repositoryMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethodParameter()
	 * @generated
	 */
	@Override
	public EClass getRepositoryMethodParameter() {
		return repositoryMethodParameterEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethodParameter_Association()
	 * @generated
	 */
	@Override
	public EReference getRepositoryMethodParameter_Association() {
		return (EReference) repositoryMethodParameterEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethodParameter_Attribute()
	 * @generated
	 */
	@Override
	public EReference getRepositoryMethodParameter_Attribute() {
		return (EReference) repositoryMethodParameterEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethodTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getRepositoryMethodTypeEnumeration() {
		return repositoryMethodTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getPermissionModeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getPermissionModeEnumeration() {
		return permissionModeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getTransactionTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getTransactionTypeEnumeration() {
		return transactionTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryFactory()
	 * @generated
	 */
	@Override
	public RepositoryFactory getRepositoryFactory() {
		return (RepositoryFactory) getEFactoryInstance();
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
		repositoryEClass = createEClass(REPOSITORY);
		createEReference(repositoryEClass, REPOSITORY__REPOSITORY_METHODS);

		repositoryMethodEClass = createEClass(REPOSITORY_METHOD);
		createEReference(repositoryMethodEClass, REPOSITORY_METHOD__REPOSITORY);
		createEAttribute(repositoryMethodEClass, REPOSITORY_METHOD__METHOD_TYPE);
		createEAttribute(repositoryMethodEClass, REPOSITORY_METHOD__HINT);

		repositoryMethodParameterEClass = createEClass(REPOSITORY_METHOD_PARAMETER);
		createEReference(repositoryMethodParameterEClass, REPOSITORY_METHOD_PARAMETER__ASSOCIATION);
		createEReference(repositoryMethodParameterEClass, REPOSITORY_METHOD_PARAMETER__ATTRIBUTE);

		// Create enums
		repositoryMethodTypeEnumerationEEnum = createEEnum(REPOSITORY_METHOD_TYPE_ENUMERATION);
		permissionModeEnumerationEEnum = createEEnum(PERMISSION_MODE_ENUMERATION);
		transactionTypeEnumerationEEnum = createEEnum(TRANSACTION_TYPE_ENUMERATION);
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
		final var theServicePackage = (ServicePackage) EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI);
		final var theJavaPackage = (JavaPackage) EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI);
		final var theDomainPackage = (DomainPackage) EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI);

		// Add supertypes to classes
		repositoryEClass.getESuperTypes().add(theServicePackage.getServiceBean());
		repositoryMethodEClass.getESuperTypes().add(theServicePackage.getServiceMethod());
		repositoryMethodParameterEClass.getESuperTypes().add(theJavaPackage.getMethodParameter());

		// Initialize classes and features; add operations and parameters
		initEClass(repositoryEClass, Repository.class, "Repository", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getRepository_RepositoryMethods(), this.getRepositoryMethod(), this.getRepositoryMethod_Repository(),
				"repositoryMethods", null, 0, -1, Repository.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(repositoryMethodEClass, RepositoryMethod.class, "RepositoryMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getRepositoryMethod_Repository(), this.getRepository(), this.getRepository_RepositoryMethods(), "repository",
				null, 0, 1, RepositoryMethod.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getRepositoryMethod_MethodType(), this.getRepositoryMethodTypeEnumeration(), "methodType", null, 0, 1,
				RepositoryMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getRepositoryMethod_Hint(), ecorePackage.getEString(), "hint", null, 0, 1, RepositoryMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(repositoryMethodParameterEClass, RepositoryMethodParameter.class, "RepositoryMethodParameter", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getRepositoryMethodParameter_Association(), theDomainPackage.getAbstractDomainAssociation(), null,
				"association", null, 0, 1, RepositoryMethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getRepositoryMethodParameter_Attribute(), theDomainPackage.getDomainAttribute(), null, "attribute", null, 0, 1,
				RepositoryMethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.class, "RepositoryMethodTypeEnumeration");
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.FIND_ALL);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.FIND_BY_ID);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.EXISTS_BY_ID);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.DELETE_ALL);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.GET_ASSOCIATION);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.FIND_BY_OBJECT);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.COUNT_ALL);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.DELETE);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.MERGE);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.PERSIST);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.FIND_EXISTING);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.SEARCH);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.COUNT);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.CHANGE_PARENT);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.ADD_TO_ASSOCIATION);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.COPY);
		addEEnumLiteral(repositoryMethodTypeEnumerationEEnum, RepositoryMethodTypeEnumeration.SAVE);

		initEEnum(permissionModeEnumerationEEnum, PermissionModeEnumeration.class, "PermissionModeEnumeration");
		addEEnumLiteral(permissionModeEnumerationEEnum, PermissionModeEnumeration.DENY_ALL);
		addEEnumLiteral(permissionModeEnumerationEEnum, PermissionModeEnumeration.PERMIT_ALL);
		addEEnumLiteral(permissionModeEnumerationEEnum, PermissionModeEnumeration.DEDICATED_ROLES);

		initEEnum(transactionTypeEnumerationEEnum, TransactionTypeEnumeration.class, "TransactionTypeEnumeration");
		addEEnumLiteral(transactionTypeEnumerationEEnum, TransactionTypeEnumeration.REQUIRES_NEW);
		addEEnumLiteral(transactionTypeEnumerationEEnum, TransactionTypeEnumeration.REQUIRED);
		addEEnumLiteral(transactionTypeEnumerationEEnum, TransactionTypeEnumeration.SUPPORTS);
		addEEnumLiteral(transactionTypeEnumerationEEnum, TransactionTypeEnumeration.NOT_SUPPORTED);
		addEEnumLiteral(transactionTypeEnumerationEEnum, TransactionTypeEnumeration.MANDATORY);

		// Create resource
		createResource(eNS_URI);
	}

}
