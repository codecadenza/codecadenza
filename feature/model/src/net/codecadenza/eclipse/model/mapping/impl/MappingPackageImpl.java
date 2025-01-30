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
package net.codecadenza.eclipse.model.mapping.impl;

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
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingFactory;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
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
 * An implementation of the model <b>Package</b>.
 * @generated
 */
public class MappingPackageImpl extends EPackageImpl implements MappingPackage {
	/**
	 * @generated
	 */
	private EClass mappingObjectEClass;

	/**
	 * @generated
	 */
	private EClass mappingAttributeEClass;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private MappingPackageImpl() {
		super(eNS_URI, MappingFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link MappingPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized mapping package
	 * @generated
	 */
	public static MappingPackage init() {
		if (isInited)
			return (MappingPackage) EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI);

		// Obtain or create and register package
		final var theMappingPackage = (MappingPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof MappingPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new MappingPackageImpl());

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
		final var theServicePackage = (ServicePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ServicePackage.eNS_URI) instanceof ServicePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI) : ServicePackage.eINSTANCE);

		// Create package meta-data objects
		theMappingPackage.createPackageContents();
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

		// Initialize created meta-data
		theMappingPackage.initializePackageContents();
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

		// Mark meta-data to indicate it can't be changed
		theMappingPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(MappingPackage.eNS_URI, theMappingPackage);
		return theMappingPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingObject()
	 * @generated
	 */
	@Override
	public EClass getMappingObject() {
		return mappingObjectEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingObject_DomainObject()
	 * @generated
	 */
	@Override
	public EReference getMappingObject_DomainObject() {
		return (EReference) mappingObjectEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute()
	 * @generated
	 */
	@Override
	public EClass getMappingAttribute() {
		return mappingAttributeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_Name()
	 * @generated
	 */
	@Override
	public EAttribute getMappingAttribute_Name() {
		return (EAttribute) mappingAttributeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_Association()
	 * @generated
	 */
	@Override
	public EReference getMappingAttribute_Association() {
		return (EReference) mappingAttributeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_DomainAttribute()
	 * @generated
	 */
	@Override
	public EReference getMappingAttribute_DomainAttribute() {
		return (EReference) mappingAttributeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_AssociationList()
	 * @generated
	 */
	@Override
	public EReference getMappingAttribute_AssociationList() {
		return (EReference) mappingAttributeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_MappingType()
	 * @generated
	 */
	@Override
	public EReference getMappingAttribute_MappingType() {
		return (EReference) mappingAttributeEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_Modifier()
	 * @generated
	 */
	@Override
	public EAttribute getMappingAttribute_Modifier() {
		return (EAttribute) mappingAttributeEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_DefaultValue()
	 * @generated
	 */
	@Override
	public EAttribute getMappingAttribute_DefaultValue() {
		return (EAttribute) mappingAttributeEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingFactory()
	 * @generated
	 */
	@Override
	public MappingFactory getMappingFactory() {
		return (MappingFactory) getEFactoryInstance();
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
		mappingObjectEClass = createEClass(MAPPING_OBJECT);
		createEReference(mappingObjectEClass, MAPPING_OBJECT__DOMAIN_OBJECT);

		mappingAttributeEClass = createEClass(MAPPING_ATTRIBUTE);
		createEAttribute(mappingAttributeEClass, MAPPING_ATTRIBUTE__NAME);
		createEReference(mappingAttributeEClass, MAPPING_ATTRIBUTE__ASSOCIATION);
		createEReference(mappingAttributeEClass, MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE);
		createEReference(mappingAttributeEClass, MAPPING_ATTRIBUTE__ASSOCIATION_LIST);
		createEReference(mappingAttributeEClass, MAPPING_ATTRIBUTE__MAPPING_TYPE);
		createEAttribute(mappingAttributeEClass, MAPPING_ATTRIBUTE__MODIFIER);
		createEAttribute(mappingAttributeEClass, MAPPING_ATTRIBUTE__DEFAULT_VALUE);
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

		// Add supertypes to classes
		mappingObjectEClass.getESuperTypes().add(theJavaPackage.getJavaType());

		// Initialize classes and features; add operations and parameters
		initEClass(mappingObjectEClass, MappingObject.class, "MappingObject", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getMappingObject_DomainObject(), theDomainPackage.getDomainObject(), null, "domainObject", null, 0, 1,
				MappingObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(mappingAttributeEClass, MappingAttribute.class, "MappingAttribute", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getMappingAttribute_Name(), ecorePackage.getEString(), "name", null, 0, 1, MappingAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getMappingAttribute_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association", null,
				0, 1, MappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getMappingAttribute_DomainAttribute(), theDomainPackage.getDomainAttribute(), null, "domainAttribute", null, 0,
				1, MappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getMappingAttribute_AssociationList(), theDomainPackage.getAbstractDomainAssociation(), null,
				"associationList", null, 0, -1, MappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getMappingAttribute_MappingType(), theJavaPackage.getJavaType(), null, "mappingType", null, 0, 1,
				MappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getMappingAttribute_Modifier(), theJavaPackage.getJavaTypeModifierEnumeration(), "modifier", null, 0, 1,
				MappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getMappingAttribute_DefaultValue(), ecorePackage.getEString(), "defaultValue", null, 0, 1,
				MappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		// Create resource
		createResource(eNS_URI);
	}

}
