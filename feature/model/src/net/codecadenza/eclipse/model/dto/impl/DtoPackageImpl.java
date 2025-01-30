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
package net.codecadenza.eclipse.model.dto.impl;

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.impl.ClientPackageImpl;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.impl.DbPackageImpl;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.dto.DtoPackage;
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
public class DtoPackageImpl extends EPackageImpl implements DtoPackage {
	/**
	 * @generated
	 */
	private EClass dtoBeanEClass;

	/**
	 * @generated
	 */
	private EClass dtoBeanAttributeEClass;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private DtoPackageImpl() {
		super(eNS_URI, DtoFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link DtoPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly. Instead,
	 * they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized DTO package
	 * @generated
	 */
	public static DtoPackage init() {
		if (isInited)
			return (DtoPackage) EPackage.Registry.INSTANCE.getEPackage(DtoPackage.eNS_URI);

		// Obtain or create and register package
		final var theDtoPackage = (DtoPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof DtoPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new DtoPackageImpl());

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
		theDtoPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theDtoPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theDtoPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(DtoPackage.eNS_URI, theDtoPackage);
		return theDtoPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean()
	 * @generated
	 */
	@Override
	public EClass getDTOBean() {
		return dtoBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_Attributes()
	 * @generated
	 */
	@Override
	public EReference getDTOBean_Attributes() {
		return (EReference) dtoBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_StandardConversion()
	 * @generated
	 */
	@Override
	public EAttribute getDTOBean_StandardConversion() {
		return (EAttribute) dtoBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_Shared()
	 */
	@Override
	public EAttribute getDTOBean_Shared() {
		return (EAttribute) dtoBeanEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_CreatedManually()
	 */
	@Override
	public EAttribute getDTOBean_CreatedManually() {
		return (EAttribute) dtoBeanEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute()
	 * @generated
	 */
	@Override
	public EClass getDTOBeanAttribute() {
		return dtoBeanAttributeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_DTOBean()
	 * @generated
	 */
	@Override
	public EReference getDTOBeanAttribute_DTOBean() {
		return (EReference) dtoBeanAttributeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_ReferencedDTOBean()
	 * @generated
	 */
	@Override
	public EReference getDTOBeanAttribute_ReferencedDTOBean() {
		return (EReference) dtoBeanAttributeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_SelectToken()
	 * @generated
	 */
	@Override
	public EAttribute getDTOBeanAttribute_SelectToken() {
		return (EAttribute) dtoBeanAttributeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_LovReturn()
	 * @generated
	 */
	@Override
	public EAttribute getDTOBeanAttribute_LovReturn() {
		return (EAttribute) dtoBeanAttributeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDtoFactory()
	 * @generated
	 */
	@Override
	public DtoFactory getDtoFactory() {
		return (DtoFactory) getEFactoryInstance();
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
		dtoBeanEClass = createEClass(DTO_BEAN);
		createEReference(dtoBeanEClass, DTO_BEAN__ATTRIBUTES);
		createEAttribute(dtoBeanEClass, DTO_BEAN__STANDARD_CONVERSION);
		createEAttribute(dtoBeanEClass, DTO_BEAN__SHARED);
		createEAttribute(dtoBeanEClass, DTO_BEAN__CREATED_MANUALLY);

		dtoBeanAttributeEClass = createEClass(DTO_BEAN_ATTRIBUTE);
		createEReference(dtoBeanAttributeEClass, DTO_BEAN_ATTRIBUTE__DTO_BEAN);
		createEReference(dtoBeanAttributeEClass, DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN);
		createEAttribute(dtoBeanAttributeEClass, DTO_BEAN_ATTRIBUTE__SELECT_TOKEN);
		createEAttribute(dtoBeanAttributeEClass, DTO_BEAN_ATTRIBUTE__LOV_RETURN);
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
		final var theMappingPackage = (MappingPackage) EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI);

		// Add supertypes to classes
		dtoBeanEClass.getESuperTypes().add(theMappingPackage.getMappingObject());
		dtoBeanAttributeEClass.getESuperTypes().add(theMappingPackage.getMappingAttribute());

		// Initialize classes and features; add operations and parameters
		initEClass(dtoBeanEClass, DTOBean.class, "DTOBean", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDTOBean_Attributes(), this.getDTOBeanAttribute(), this.getDTOBeanAttribute_DTOBean(), "attributes", null, 0,
				-1, DTOBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDTOBean_StandardConversion(), ecorePackage.getEBoolean(), "standardConversion", null, 0, 1, DTOBean.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDTOBean_Shared(), ecorePackage.getEBoolean(), "shared", null, 0, 1, DTOBean.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDTOBean_CreatedManually(), ecorePackage.getEBoolean(), "createdManually", null, 0, 1, DTOBean.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(dtoBeanAttributeEClass, DTOBeanAttribute.class, "DTOBeanAttribute", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDTOBeanAttribute_DTOBean(), this.getDTOBean(), this.getDTOBean_Attributes(), "dTOBean", null, 0, 1,
				DTOBeanAttribute.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDTOBeanAttribute_ReferencedDTOBean(), this.getDTOBean(), null, "referencedDTOBean", null, 0, 1,
				DTOBeanAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDTOBeanAttribute_SelectToken(), ecorePackage.getEString(), "selectToken", null, 0, 1,
				DTOBeanAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDTOBeanAttribute_LovReturn(), ecorePackage.getEBoolean(), "lovReturn", null, 0, 1, DTOBeanAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Create resource
		createResource(eNS_URI);
	}

}
