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
package net.codecadenza.eclipse.model.service.impl;

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
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.MethodInvocation;
import net.codecadenza.eclipse.model.service.ScheduledInvocation;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.model.service.ServiceFactory;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;

/**
 * An implementation of the model <b>Package</b>.
 * @generated
 */
public class ServicePackageImpl extends EPackageImpl implements ServicePackage {
	/**
	 * @generated
	 */
	private EClass serviceBeanEClass;

	/**
	 * @generated
	 */
	private EClass serviceMethodEClass;

	/**
	 * @generated
	 */
	private EClass methodInvocationEClass;

	/**
	 * @generated
	 */
	private EClass scheduledInvocationEClass;

	/**
	 * @generated
	 */
	private EClass asynchronousInvocationEClass;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ServicePackageImpl() {
		super(eNS_URI, ServiceFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link ServicePackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized service package
	 * @generated
	 */
	public static ServicePackage init() {
		if (isInited)
			return (ServicePackage) EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI);

		// Obtain or create and register package
		final var theServicePackage = (ServicePackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof ServicePackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new ServicePackageImpl());

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
		final var theProjectPackage = (ProjectPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ProjectPackage.eNS_URI) instanceof ProjectPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI) : ProjectPackage.eINSTANCE);
		final var theExchangePackage = (ExchangePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ExchangePackage.eNS_URI) instanceof ExchangePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ExchangePackage.eNS_URI) : ExchangePackage.eINSTANCE);
		final var theMappingPackage = (MappingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(MappingPackage.eNS_URI) instanceof MappingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI) : MappingPackage.eINSTANCE);

		// Create package meta-data objects
		theServicePackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theServicePackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theServicePackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ServicePackage.eNS_URI, theServicePackage);
		return theServicePackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceBean()
	 * @generated
	 */
	@Override
	public EClass getServiceBean() {
		return serviceBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceBean_DomainObject()
	 * @generated
	 */
	@Override
	public EReference getServiceBean_DomainObject() {
		return (EReference) serviceBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceBean_InterfaceName()
	 * @generated
	 */
	@Override
	public EAttribute getServiceBean_InterfaceName() {
		return (EAttribute) serviceBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod()
	 * @generated
	 */
	@Override
	public EClass getServiceMethod() {
		return serviceMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_QueryStatement()
	 * @generated
	 */
	@Override
	public EAttribute getServiceMethod_QueryStatement() {
		return (EAttribute) serviceMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_PermissionMode()
	 * @generated
	 */
	@Override
	public EAttribute getServiceMethod_PermissionMode() {
		return (EAttribute) serviceMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_TransactionType()
	 * @generated
	 */
	@Override
	public EAttribute getServiceMethod_TransactionType() {
		return (EAttribute) serviceMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_Roles()
	 * @generated
	 */
	@Override
	public EReference getServiceMethod_Roles() {
		return (EReference) serviceMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_MethodInvocation()
	 * @generated
	 */
	@Override
	public EReference getServiceMethod_MethodInvocation() {
		return (EReference) serviceMethodEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_CustomStatement()
	 * @generated
	 */
	@Override
	public EAttribute getServiceMethod_CustomStatement() {
		return (EAttribute) serviceMethodEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getMethodInvocation()
	 * @generated
	 */
	@Override
	public EClass getMethodInvocation() {
		return methodInvocationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getMethodInvocation_Method()
	 * @generated
	 */
	@Override
	public EReference getMethodInvocation_Method() {
		return (EReference) methodInvocationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation()
	 * @generated
	 */
	@Override
	public EClass getScheduledInvocation() {
		return scheduledInvocationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Second()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_Second() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Minute()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_Minute() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Hour()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_Hour() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_DayOfWeek()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_DayOfWeek() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_DayOfMonth()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_DayOfMonth() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Month()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_Month() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getScheduledInvocation_Year()
	 * @generated
	 */
	@Override
	public EAttribute getScheduledInvocation_Year() {
		return (EAttribute) scheduledInvocationEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getAsynchronousInvocation()
	 * @generated
	 */
	@Override
	public EClass getAsynchronousInvocation() {
		return asynchronousInvocationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getAsynchronousInvocation_DelayInMilliseconds()
	 * @generated
	 */
	@Override
	public EAttribute getAsynchronousInvocation_DelayInMilliseconds() {
		return (EAttribute) asynchronousInvocationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceFactory()
	 * @generated
	 */
	@Override
	public ServiceFactory getServiceFactory() {
		return (ServiceFactory) getEFactoryInstance();
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
		serviceBeanEClass = createEClass(SERVICE_BEAN);
		createEReference(serviceBeanEClass, SERVICE_BEAN__DOMAIN_OBJECT);
		createEAttribute(serviceBeanEClass, SERVICE_BEAN__INTERFACE_NAME);

		serviceMethodEClass = createEClass(SERVICE_METHOD);
		createEAttribute(serviceMethodEClass, SERVICE_METHOD__QUERY_STATEMENT);
		createEAttribute(serviceMethodEClass, SERVICE_METHOD__PERMISSION_MODE);
		createEAttribute(serviceMethodEClass, SERVICE_METHOD__TRANSACTION_TYPE);
		createEReference(serviceMethodEClass, SERVICE_METHOD__ROLES);
		createEReference(serviceMethodEClass, SERVICE_METHOD__METHOD_INVOCATION);
		createEAttribute(serviceMethodEClass, SERVICE_METHOD__CUSTOM_STATEMENT);

		methodInvocationEClass = createEClass(METHOD_INVOCATION);
		createEReference(methodInvocationEClass, METHOD_INVOCATION__METHOD);

		scheduledInvocationEClass = createEClass(SCHEDULED_INVOCATION);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__SECOND);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__MINUTE);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__HOUR);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__DAY_OF_WEEK);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__DAY_OF_MONTH);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__MONTH);
		createEAttribute(scheduledInvocationEClass, SCHEDULED_INVOCATION__YEAR);

		asynchronousInvocationEClass = createEClass(ASYNCHRONOUS_INVOCATION);
		createEAttribute(asynchronousInvocationEClass, ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS);
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
		final var theDomainPackage = (DomainPackage) EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI);
		final var theRepositoryPackage = (RepositoryPackage) EPackage.Registry.INSTANCE.getEPackage(RepositoryPackage.eNS_URI);
		final var theProjectPackage = (ProjectPackage) EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI);

		// Add supertypes to classes
		serviceBeanEClass.getESuperTypes().add(theJavaPackage.getJavaType());
		serviceMethodEClass.getESuperTypes().add(theJavaPackage.getJavaMethod());
		scheduledInvocationEClass.getESuperTypes().add(this.getMethodInvocation());
		asynchronousInvocationEClass.getESuperTypes().add(this.getMethodInvocation());

		// Initialize classes and features; add operations and parameters
		initEClass(serviceBeanEClass, ServiceBean.class, "ServiceBean", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getServiceBean_DomainObject(), theDomainPackage.getDomainObject(), null, "domainObject", null, 0, 1,
				ServiceBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getServiceBean_InterfaceName(), ecorePackage.getEString(), "interfaceName", null, 0, 1, ServiceBean.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(serviceMethodEClass, ServiceMethod.class, "ServiceMethod", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getServiceMethod_QueryStatement(), ecorePackage.getEString(), "queryStatement", null, 0, 1,
				ServiceMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getServiceMethod_PermissionMode(), theRepositoryPackage.getPermissionModeEnumeration(), "permissionMode", null,
				0, 1, ServiceMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getServiceMethod_TransactionType(), theRepositoryPackage.getTransactionTypeEnumeration(), "transactionType",
				null, 0, 1, ServiceMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getServiceMethod_Roles(), theProjectPackage.getRole(), null, "roles", null, 0, -1, ServiceMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getServiceMethod_MethodInvocation(), this.getMethodInvocation(), this.getMethodInvocation_Method(),
				"methodInvocation", null, 0, 1, ServiceMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getServiceMethod_CustomStatement(), ecorePackage.getEString(), "customStatement", null, 0, 1,
				ServiceMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(methodInvocationEClass, MethodInvocation.class, "MethodInvocation", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getMethodInvocation_Method(), this.getServiceMethod(), this.getServiceMethod_MethodInvocation(), "method",
				null, 0, 1, MethodInvocation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(scheduledInvocationEClass, ScheduledInvocation.class, "ScheduledInvocation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getScheduledInvocation_Second(), ecorePackage.getEString(), "second", null, 0, 1, ScheduledInvocation.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getScheduledInvocation_Minute(), ecorePackage.getEString(), "minute", null, 0, 1, ScheduledInvocation.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getScheduledInvocation_Hour(), ecorePackage.getEString(), "hour", null, 0, 1, ScheduledInvocation.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getScheduledInvocation_DayOfWeek(), ecorePackage.getEString(), "dayOfWeek", null, 0, 1,
				ScheduledInvocation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getScheduledInvocation_DayOfMonth(), ecorePackage.getEString(), "dayOfMonth", null, 0, 1,
				ScheduledInvocation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getScheduledInvocation_Month(), ecorePackage.getEString(), "month", null, 0, 1, ScheduledInvocation.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getScheduledInvocation_Year(), ecorePackage.getEString(), "year", null, 0, 1, ScheduledInvocation.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(asynchronousInvocationEClass, AsynchronousInvocation.class, "AsynchronousInvocation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getAsynchronousInvocation_DelayInMilliseconds(), ecorePackage.getEIntegerObject(), "delayInMilliseconds", null,
				0, 1, AsynchronousInvocation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		// Create resource
		createResource(eNS_URI);
	}

}
