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
package net.codecadenza.eclipse.model.boundary.impl;

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
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
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl;
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
public class BoundaryPackageImpl extends EPackageImpl implements BoundaryPackage {
	/**
	 * @generated
	 */
	private EClass boundaryBeanEClass;

	/**
	 * @generated
	 */
	private EClass boundaryMethodEClass;

	/**
	 * @generated
	 */
	private EEnum boundaryMethodTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum boundaryMethodDataFetchTypeEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private BoundaryPackageImpl() {
		super(eNS_URI, BoundaryFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link BoundaryPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized boundary package
	 * @generated
	 */
	public static BoundaryPackage init() {
		if (isInited)
			return (BoundaryPackage) EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI);

		// Obtain or create and register package
		final var theBoundaryPackage = (BoundaryPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof BoundaryPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new BoundaryPackageImpl());

		isInited = true;

		// Obtain or create and register interdependencies
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
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theBoundaryPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(BoundaryPackage.eNS_URI, theBoundaryPackage);
		return theBoundaryPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryBean()
	 * @generated
	 */
	@Override
	public EClass getBoundaryBean() {
		return boundaryBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryBean_BoundaryMethods()
	 * @generated
	 */
	@Override
	public EReference getBoundaryBean_BoundaryMethods() {
		return (EReference) boundaryBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryBean_Repository()
	 * @generated
	 */
	@Override
	public EReference getBoundaryBean_Repository() {
		return (EReference) boundaryBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod()
	 * @generated
	 */
	@Override
	public EClass getBoundaryMethod() {
		return boundaryMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_BoundaryBean()
	 * @generated
	 */
	@Override
	public EReference getBoundaryMethod_BoundaryBean() {
		return (EReference) boundaryMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_MethodType()
	 * @generated
	 */
	@Override
	public EAttribute getBoundaryMethod_MethodType() {
		return (EAttribute) boundaryMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_DomainAttribute()
	 * @generated
	 */
	@Override
	public EReference getBoundaryMethod_DomainAttribute() {
		return (EReference) boundaryMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_Association()
	 * @generated
	 */
	@Override
	public EReference getBoundaryMethod_Association() {
		return (EReference) boundaryMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_DataFetchType()
	 * @generated
	 */
	@Override
	public EAttribute getBoundaryMethod_DataFetchType() {
		return (EAttribute) boundaryMethodEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_ServiceMethod()
	 * @generated
	 */
	@Override
	public EReference getBoundaryMethod_ServiceMethod() {
		return (EReference) boundaryMethodEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethodTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getBoundaryMethodTypeEnumeration() {
		return boundaryMethodTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethodDataFetchType()
	 * @generated
	 */
	@Override
	public EEnum getBoundaryMethodDataFetchType() {
		return boundaryMethodDataFetchTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryFactory()
	 * @generated
	 */
	@Override
	public BoundaryFactory getBoundaryFactory() {
		return (BoundaryFactory) getEFactoryInstance();
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
		boundaryBeanEClass = createEClass(BOUNDARY_BEAN);
		createEReference(boundaryBeanEClass, BOUNDARY_BEAN__BOUNDARY_METHODS);
		createEReference(boundaryBeanEClass, BOUNDARY_BEAN__REPOSITORY);

		boundaryMethodEClass = createEClass(BOUNDARY_METHOD);
		createEReference(boundaryMethodEClass, BOUNDARY_METHOD__BOUNDARY_BEAN);
		createEAttribute(boundaryMethodEClass, BOUNDARY_METHOD__METHOD_TYPE);
		createEReference(boundaryMethodEClass, BOUNDARY_METHOD__DOMAIN_ATTRIBUTE);
		createEReference(boundaryMethodEClass, BOUNDARY_METHOD__ASSOCIATION);
		createEAttribute(boundaryMethodEClass, BOUNDARY_METHOD__DATA_FETCH_TYPE);
		createEReference(boundaryMethodEClass, BOUNDARY_METHOD__SERVICE_METHOD);

		// Create enums
		boundaryMethodTypeEnumerationEEnum = createEEnum(BOUNDARY_METHOD_TYPE_ENUMERATION);
		boundaryMethodDataFetchTypeEEnum = createEEnum(BOUNDARY_METHOD_DATA_FETCH_TYPE);
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
		final var theRepositoryPackage = (RepositoryPackage) EPackage.Registry.INSTANCE.getEPackage(RepositoryPackage.eNS_URI);
		final var theDomainPackage = (DomainPackage) EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI);

		// Add supertypes to classes
		boundaryBeanEClass.getESuperTypes().add(theServicePackage.getServiceBean());
		boundaryMethodEClass.getESuperTypes().add(theServicePackage.getServiceMethod());

		// Initialize classes and features; add operations and parameters
		initEClass(boundaryBeanEClass, BoundaryBean.class, "BoundaryBean", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getBoundaryBean_BoundaryMethods(), this.getBoundaryMethod(), this.getBoundaryMethod_BoundaryBean(),
				"boundaryMethods", null, 0, -1, BoundaryBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getBoundaryBean_Repository(), theRepositoryPackage.getRepository(), null, "repository", null, 0, 1,
				BoundaryBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(boundaryMethodEClass, BoundaryMethod.class, "BoundaryMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getBoundaryMethod_BoundaryBean(), this.getBoundaryBean(), this.getBoundaryBean_BoundaryMethods(),
				"boundaryBean", null, 0, 1, BoundaryMethod.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getBoundaryMethod_MethodType(), this.getBoundaryMethodTypeEnumeration(), "methodType", null, 0, 1,
				BoundaryMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getBoundaryMethod_DomainAttribute(), theDomainPackage.getDomainAttribute(), null, "domainAttribute", null, 0,
				1, BoundaryMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getBoundaryMethod_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association", null, 0,
				1, BoundaryMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getBoundaryMethod_DataFetchType(), this.getBoundaryMethodDataFetchType(), "dataFetchType", null, 0, 1,
				BoundaryMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getBoundaryMethod_ServiceMethod(), theServicePackage.getServiceMethod(), null, "serviceMethod", null, 0, 1,
				BoundaryMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.class, "BoundaryMethodTypeEnumeration");
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.DELETE);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.UPDATE);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.FIND_ALL);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.CREATE);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.SEARCH);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.FIND_BY_PARENT);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.FIND_BY_ID);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.EXISTS_BY_ID);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.DELETE_ALL);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.GET_ASSOCIATION);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.FIND_BY_OBJECT);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.COUNT_ALL);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.FIND_EXISTING);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.CHANGE_PARENT);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.COUNT);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.LOG_ON);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.DOWNLOAD);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.UPLOAD);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.CHANGE_PASSWORD);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.SERVICE_CALL);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.UPLOAD_IMPORT);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.COPY);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.CHANGE_ASSOCIATION);
		addEEnumLiteral(boundaryMethodTypeEnumerationEEnum, BoundaryMethodTypeEnumeration.SAVE);

		initEEnum(boundaryMethodDataFetchTypeEEnum, BoundaryMethodDataFetchType.class, "BoundaryMethodDataFetchType");
		addEEnumLiteral(boundaryMethodDataFetchTypeEEnum, BoundaryMethodDataFetchType.DEFAULT);
		addEEnumLiteral(boundaryMethodDataFetchTypeEEnum, BoundaryMethodDataFetchType.CLIENT);
		addEEnumLiteral(boundaryMethodDataFetchTypeEEnum, BoundaryMethodDataFetchType.USER);

		// Create resource
		createResource(eNS_URI);
	}

}
