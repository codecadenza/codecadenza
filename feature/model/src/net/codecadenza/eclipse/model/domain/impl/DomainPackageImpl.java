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
package net.codecadenza.eclipse.model.domain.impl;

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.impl.ClientPackageImpl;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.impl.DbPackageImpl;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
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
public class DomainPackageImpl extends EPackageImpl implements DomainPackage {
	/**
	 * @generated
	 */
	private EClass abstractDomainAssociationEClass;

	/**
	 * @generated
	 */
	private EClass domainObjectEClass;

	/**
	 * @generated
	 */
	private EClass domainAttributeEClass;

	/**
	 * @generated
	 */
	private EClass domainAttributeValidatorEClass;

	/**
	 * @generated
	 */
	private EClass domainInheritanceEClass;

	/**
	 * @generated
	 */
	private EClass domainNamespaceEClass;

	/**
	 * @generated
	 */
	private EClass enumAssociationEClass;

	/**
	 * @generated
	 */
	private EClass idGeneratorEClass;

	/**
	 * @generated
	 */
	private EClass manyToManyAssociationEClass;

	/**
	 * @generated
	 */
	private EClass manyToOneAssociationEClass;

	/**
	 * @generated
	 */
	private EClass oneToManyAssociationEClass;

	/**
	 * @generated
	 */
	private EClass oneToOneAssociationEClass;

	/**
	 * @generated
	 */
	private EEnum discriminatorColumnTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum idGeneratorTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum inheritanceTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum temporalTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum domainTagEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum attributeTagEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum associationTagEnumerationEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private DomainPackageImpl() {
		super(eNS_URI, DomainFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link DomainPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized domain package
	 * @generated
	 */
	public static DomainPackage init() {
		if (isInited)
			return (DomainPackage) EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI);

		// Obtain or create and register package
		final var theDomainPackage = (DomainPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof DomainPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new DomainPackageImpl());

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
		theDomainPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theDomainPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theDomainPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(DomainPackage.eNS_URI, theDomainPackage);
		return theDomainPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation()
	 * @generated
	 */
	@Override
	public EClass getAbstractDomainAssociation() {
		return abstractDomainAssociationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Name()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_Name() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Owner()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_Owner() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadePersist()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_CascadePersist() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadeMerge()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_CascadeMerge() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadeRemove()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_CascadeRemove() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadeRefresh()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_CascadeRefresh() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_FetchTypeEager()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_FetchTypeEager() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_DomainObject()
	 * @generated
	 */
	@Override
	public EReference getAbstractDomainAssociation_DomainObject() {
		return (EReference) abstractDomainAssociationEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Target()
	 * @generated
	 */
	@Override
	public EReference getAbstractDomainAssociation_Target() {
		return (EReference) abstractDomainAssociationEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Tag()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_Tag() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_InternalComment()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_InternalComment() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_UserComment()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractDomainAssociation_UserComment() {
		return (EAttribute) abstractDomainAssociationEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_ReverseAssociation()
	 * @generated
	 */
	@Override
	public EReference getAbstractDomainAssociation_ReverseAssociation() {
		return (EReference) abstractDomainAssociationEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject()
	 * @generated
	 */
	@Override
	public EClass getDomainObject() {
		return domainObjectEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Label()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_Label() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_NamePlural()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_NamePlural() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_LabelPlural()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_LabelPlural() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DiscriminatorValue()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_DiscriminatorValue() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DiscriminatorColumnType()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_DiscriminatorColumnType() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_InheritanceType()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_InheritanceType() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_PropertyAccess()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_PropertyAccess() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Abstract()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_Abstract() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_MappedSuperClass()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_MappedSuperClass() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Parent()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_Parent() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_IDGenerator()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_IDGenerator() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Inheritance()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_Inheritance() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Attributes()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_Attributes() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Associations()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_Associations() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_EnumAssociations()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_EnumAssociations() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_TargetInheritances()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_TargetInheritances() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DiscriminatorColumn()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_DiscriminatorColumn() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(16);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DatabaseTable()
	 * @generated
	 */
	@Override
	public EReference getDomainObject_DatabaseTable() {
		return (EReference) domainObjectEClass.getEStructuralFeatures().get(17);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Tag()
	 * @generated
	 */
	@Override
	public EAttribute getDomainObject_Tag() {
		return (EAttribute) domainObjectEClass.getEStructuralFeatures().get(18);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute()
	 * @generated
	 */
	@Override
	public EClass getDomainAttribute() {
		return domainAttributeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Name() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Pk()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Pk() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Label()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Label() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_LabelPlural()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_LabelPlural() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Persistent()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Persistent() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_FetchTypeEager()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_FetchTypeEager() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Insertable()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Insertable() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Updatable()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Updatable() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_TrackVersion()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_TrackVersion() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_SetDateOnPersist()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_SetDateOnPersist() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_SetDateOnUpdate()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_SetDateOnUpdate() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_DisplayAttribute()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_DisplayAttribute() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_DomainObject()
	 * @generated
	 */
	@Override
	public EReference getDomainAttribute_DomainObject() {
		return (EReference) domainAttributeEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_DomainAttributeValidator()
	 * @generated
	 */
	@Override
	public EReference getDomainAttribute_DomainAttributeValidator() {
		return (EReference) domainAttributeEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_TemporalType()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_TemporalType() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_JavaType()
	 * @generated
	 */
	@Override
	public EReference getDomainAttribute_JavaType() {
		return (EReference) domainAttributeEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Column()
	 * @generated
	 */
	@Override
	public EReference getDomainAttribute_Column() {
		return (EReference) domainAttributeEClass.getEStructuralFeatures().get(16);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Tag()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Tag() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(17);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Lob()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_Lob() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(18);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_InternalComment()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_InternalComment() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(19);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_UserComment()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_UserComment() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(20);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_RemoveWhitespaceCharacters()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_RemoveWhitespaceCharacters() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(21);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_ConvertToUpperCase()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_ConvertToUpperCase() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(22);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_ConvertToLowerCase()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttribute_ConvertToLowerCase() {
		return (EAttribute) domainAttributeEClass.getEStructuralFeatures().get(23);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator()
	 * @generated
	 */
	@Override
	public EClass getDomainAttributeValidator() {
		return domainAttributeValidatorEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_FutureDate()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_FutureDate() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_PastDate()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_PastDate() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MaxLength()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_MaxLength() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MinLength()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_MinLength() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_Nullable()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_Nullable() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MaxValue()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_MaxValue() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MinValue()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_MinValue() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_RegularExpression()
	 * @generated
	 */
	@Override
	public EAttribute getDomainAttributeValidator_RegularExpression() {
		return (EAttribute) domainAttributeValidatorEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainInheritance()
	 * @generated
	 */
	@Override
	public EClass getDomainInheritance() {
		return domainInheritanceEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainInheritance_Source()
	 * @generated
	 */
	@Override
	public EReference getDomainInheritance_Source() {
		return (EReference) domainInheritanceEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainInheritance_Target()
	 * @generated
	 */
	@Override
	public EReference getDomainInheritance_Target() {
		return (EReference) domainInheritanceEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainNamespace()
	 * @generated
	 */
	@Override
	public EClass getDomainNamespace() {
		return domainNamespaceEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainNamespace_DomainObjects()
	 * @generated
	 */
	@Override
	public EReference getDomainNamespace_DomainObjects() {
		return (EReference) domainNamespaceEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainNamespace_Enumerations()
	 * @generated
	 */
	@Override
	public EReference getDomainNamespace_Enumerations() {
		return (EReference) domainNamespaceEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation()
	 * @generated
	 */
	@Override
	public EClass getEnumAssociation() {
		return enumAssociationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation_Source()
	 * @generated
	 */
	@Override
	public EReference getEnumAssociation_Source() {
		return (EReference) enumAssociationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation_Target()
	 * @generated
	 */
	@Override
	public EReference getEnumAssociation_Target() {
		return (EReference) enumAssociationEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation_DomainAttribute()
	 * @generated
	 */
	@Override
	public EReference getEnumAssociation_DomainAttribute() {
		return (EReference) enumAssociationEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator()
	 * @generated
	 */
	@Override
	public EClass getIDGenerator() {
		return idGeneratorEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_Name()
	 * @generated
	 */
	@Override
	public EAttribute getIDGenerator_Name() {
		return (EAttribute) idGeneratorEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_BlockSize()
	 * @generated
	 */
	@Override
	public EAttribute getIDGenerator_BlockSize() {
		return (EAttribute) idGeneratorEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_InitialValue()
	 * @generated
	 */
	@Override
	public EAttribute getIDGenerator_InitialValue() {
		return (EAttribute) idGeneratorEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_GeneratorType()
	 * @generated
	 */
	@Override
	public EAttribute getIDGenerator_GeneratorType() {
		return (EAttribute) idGeneratorEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToManyAssociation()
	 * @generated
	 */
	@Override
	public EClass getManyToManyAssociation() {
		return manyToManyAssociationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToManyAssociation_Table()
	 * @generated
	 */
	@Override
	public EReference getManyToManyAssociation_Table() {
		return (EReference) manyToManyAssociationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation()
	 * @generated
	 */
	@Override
	public EClass getManyToOneAssociation() {
		return manyToOneAssociationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Insertable()
	 * @generated
	 */
	@Override
	public EAttribute getManyToOneAssociation_Insertable() {
		return (EAttribute) manyToOneAssociationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Updatable()
	 * @generated
	 */
	@Override
	public EAttribute getManyToOneAssociation_Updatable() {
		return (EAttribute) manyToOneAssociationEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Optional()
	 * @generated
	 */
	@Override
	public EAttribute getManyToOneAssociation_Optional() {
		return (EAttribute) manyToOneAssociationEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Column()
	 * @generated
	 */
	@Override
	public EReference getManyToOneAssociation_Column() {
		return (EReference) manyToOneAssociationEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToManyAssociation()
	 * @generated
	 */
	@Override
	public EClass getOneToManyAssociation() {
		return oneToManyAssociationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToManyAssociation_Table()
	 * @generated
	 */
	@Override
	public EReference getOneToManyAssociation_Table() {
		return (EReference) oneToManyAssociationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToOneAssociation()
	 * @generated
	 */
	@Override
	public EClass getOneToOneAssociation() {
		return oneToOneAssociationEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToOneAssociation_Optional()
	 * @generated
	 */
	@Override
	public EAttribute getOneToOneAssociation_Optional() {
		return (EAttribute) oneToOneAssociationEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToOneAssociation_Column()
	 * @generated
	 */
	@Override
	public EReference getOneToOneAssociation_Column() {
		return (EReference) oneToOneAssociationEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDiscriminatorColumnTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getDiscriminatorColumnTypeEnumeration() {
		return discriminatorColumnTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGeneratorTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getIDGeneratorTypeEnumeration() {
		return idGeneratorTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getInheritanceTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getInheritanceTypeEnumeration() {
		return inheritanceTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getTemporalTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getTemporalTypeEnumeration() {
		return temporalTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainTagEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getDomainTagEnumeration() {
		return domainTagEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAttributeTagEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getAttributeTagEnumeration() {
		return attributeTagEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAssociationTagEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getAssociationTagEnumeration() {
		return associationTagEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainFactory()
	 * @generated
	 */
	@Override
	public DomainFactory getDomainFactory() {
		return (DomainFactory) getEFactoryInstance();
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
		abstractDomainAssociationEClass = createEClass(ABSTRACT_DOMAIN_ASSOCIATION);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__NAME);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__OWNER);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER);
		createEReference(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT);
		createEReference(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__TARGET);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__TAG);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT);
		createEAttribute(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT);
		createEReference(abstractDomainAssociationEClass, ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION);

		domainObjectEClass = createEClass(DOMAIN_OBJECT);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__LABEL);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__NAME_PLURAL);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__LABEL_PLURAL);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__DISCRIMINATOR_VALUE);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__INHERITANCE_TYPE);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__PROPERTY_ACCESS);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__ABSTRACT);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__MAPPED_SUPER_CLASS);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__PARENT);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__ID_GENERATOR);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__INHERITANCE);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__ATTRIBUTES);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__ASSOCIATIONS);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__ENUM_ASSOCIATIONS);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__TARGET_INHERITANCES);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__DISCRIMINATOR_COLUMN);
		createEReference(domainObjectEClass, DOMAIN_OBJECT__DATABASE_TABLE);
		createEAttribute(domainObjectEClass, DOMAIN_OBJECT__TAG);

		domainAttributeEClass = createEClass(DOMAIN_ATTRIBUTE);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__NAME);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__PK);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__LABEL);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__LABEL_PLURAL);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__PERSISTENT);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__INSERTABLE);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__UPDATABLE);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__TRACK_VERSION);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE);
		createEReference(domainAttributeEClass, DOMAIN_ATTRIBUTE__DOMAIN_OBJECT);
		createEReference(domainAttributeEClass, DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__TEMPORAL_TYPE);
		createEReference(domainAttributeEClass, DOMAIN_ATTRIBUTE__JAVA_TYPE);
		createEReference(domainAttributeEClass, DOMAIN_ATTRIBUTE__COLUMN);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__TAG);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__LOB);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__INTERNAL_COMMENT);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__USER_COMMENT);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE);
		createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE);

		domainAttributeValidatorEClass = createEClass(DOMAIN_ATTRIBUTE_VALIDATOR);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE);
		createEAttribute(domainAttributeValidatorEClass, DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION);

		domainInheritanceEClass = createEClass(DOMAIN_INHERITANCE);
		createEReference(domainInheritanceEClass, DOMAIN_INHERITANCE__SOURCE);
		createEReference(domainInheritanceEClass, DOMAIN_INHERITANCE__TARGET);

		domainNamespaceEClass = createEClass(DOMAIN_NAMESPACE);
		createEReference(domainNamespaceEClass, DOMAIN_NAMESPACE__DOMAIN_OBJECTS);
		createEReference(domainNamespaceEClass, DOMAIN_NAMESPACE__ENUMERATIONS);

		enumAssociationEClass = createEClass(ENUM_ASSOCIATION);
		createEReference(enumAssociationEClass, ENUM_ASSOCIATION__SOURCE);
		createEReference(enumAssociationEClass, ENUM_ASSOCIATION__TARGET);
		createEReference(enumAssociationEClass, ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE);

		idGeneratorEClass = createEClass(ID_GENERATOR);
		createEAttribute(idGeneratorEClass, ID_GENERATOR__NAME);
		createEAttribute(idGeneratorEClass, ID_GENERATOR__BLOCK_SIZE);
		createEAttribute(idGeneratorEClass, ID_GENERATOR__INITIAL_VALUE);
		createEAttribute(idGeneratorEClass, ID_GENERATOR__GENERATOR_TYPE);

		manyToManyAssociationEClass = createEClass(MANY_TO_MANY_ASSOCIATION);
		createEReference(manyToManyAssociationEClass, MANY_TO_MANY_ASSOCIATION__TABLE);

		manyToOneAssociationEClass = createEClass(MANY_TO_ONE_ASSOCIATION);
		createEAttribute(manyToOneAssociationEClass, MANY_TO_ONE_ASSOCIATION__INSERTABLE);
		createEAttribute(manyToOneAssociationEClass, MANY_TO_ONE_ASSOCIATION__UPDATABLE);
		createEAttribute(manyToOneAssociationEClass, MANY_TO_ONE_ASSOCIATION__OPTIONAL);
		createEReference(manyToOneAssociationEClass, MANY_TO_ONE_ASSOCIATION__COLUMN);

		oneToManyAssociationEClass = createEClass(ONE_TO_MANY_ASSOCIATION);
		createEReference(oneToManyAssociationEClass, ONE_TO_MANY_ASSOCIATION__TABLE);

		oneToOneAssociationEClass = createEClass(ONE_TO_ONE_ASSOCIATION);
		createEAttribute(oneToOneAssociationEClass, ONE_TO_ONE_ASSOCIATION__OPTIONAL);
		createEReference(oneToOneAssociationEClass, ONE_TO_ONE_ASSOCIATION__COLUMN);

		// Create enums
		discriminatorColumnTypeEnumerationEEnum = createEEnum(DISCRIMINATOR_COLUMN_TYPE_ENUMERATION);
		idGeneratorTypeEnumerationEEnum = createEEnum(ID_GENERATOR_TYPE_ENUMERATION);
		inheritanceTypeEnumerationEEnum = createEEnum(INHERITANCE_TYPE_ENUMERATION);
		temporalTypeEnumerationEEnum = createEEnum(TEMPORAL_TYPE_ENUMERATION);
		domainTagEnumerationEEnum = createEEnum(DOMAIN_TAG_ENUMERATION);
		attributeTagEnumerationEEnum = createEEnum(ATTRIBUTE_TAG_ENUMERATION);
		associationTagEnumerationEEnum = createEEnum(ASSOCIATION_TAG_ENUMERATION);
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

		// Add supertypes to classes
		domainObjectEClass.getESuperTypes().add(theJavaPackage.getJavaType());
		domainNamespaceEClass.getESuperTypes().add(theJavaPackage.getNamespace());
		manyToManyAssociationEClass.getESuperTypes().add(this.getAbstractDomainAssociation());
		manyToOneAssociationEClass.getESuperTypes().add(this.getAbstractDomainAssociation());
		oneToManyAssociationEClass.getESuperTypes().add(this.getAbstractDomainAssociation());
		oneToOneAssociationEClass.getESuperTypes().add(this.getAbstractDomainAssociation());

		// Initialize classes and features; add operations and parameters
		initEClass(abstractDomainAssociationEClass, AbstractDomainAssociation.class, "AbstractDomainAssociation", IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getAbstractDomainAssociation_Name(), ecorePackage.getEString(), "name", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_Owner(), ecorePackage.getEBoolean(), "owner", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_CascadePersist(), ecorePackage.getEBoolean(), "cascadePersist", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_CascadeMerge(), ecorePackage.getEBoolean(), "cascadeMerge", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_CascadeRemove(), ecorePackage.getEBoolean(), "cascadeRemove", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_CascadeRefresh(), ecorePackage.getEBoolean(), "cascadeRefresh", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_FetchTypeEager(), ecorePackage.getEBoolean(), "fetchTypeEager", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getAbstractDomainAssociation_DomainObject(), this.getDomainObject(), this.getDomainObject_Associations(),
				"domainObject", null, 0, 1, AbstractDomainAssociation.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getAbstractDomainAssociation_Target(), this.getDomainObject(), null, "target", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_Tag(), this.getAssociationTagEnumeration(), "tag", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_InternalComment(), ecorePackage.getEString(), "internalComment", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractDomainAssociation_UserComment(), ecorePackage.getEString(), "userComment", null, 0, 1,
				AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getAbstractDomainAssociation_ReverseAssociation(), this.getAbstractDomainAssociation(), null,
				"reverseAssociation", null, 0, 1, AbstractDomainAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(domainObjectEClass, DomainObject.class, "DomainObject", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDomainObject_Label(), ecorePackage.getEString(), "label", null, 0, 1, DomainObject.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_NamePlural(), ecorePackage.getEString(), "namePlural", null, 0, 1, DomainObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_LabelPlural(), ecorePackage.getEString(), "labelPlural", null, 0, 1, DomainObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_DiscriminatorValue(), ecorePackage.getEString(), "discriminatorValue", null, 0, 1,
				DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainObject_DiscriminatorColumnType(), this.getDiscriminatorColumnTypeEnumeration(),
				"discriminatorColumnType", null, 0, 1, DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE,
				!IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_InheritanceType(), this.getInheritanceTypeEnumeration(), "inheritanceType", null, 0, 1,
				DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainObject_PropertyAccess(), ecorePackage.getEBoolean(), "propertyAccess", null, 0, 1, DomainObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_Abstract(), ecorePackage.getEBoolean(), "abstract", null, 0, 1, DomainObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_MappedSuperClass(), ecorePackage.getEBoolean(), "mappedSuperClass", null, 0, 1,
				DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDomainObject_Parent(), this.getDomainObject(), null, "parent", null, 0, 1, DomainObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDomainObject_IDGenerator(), this.getIDGenerator(), null, "iDGenerator", null, 0, 1, DomainObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDomainObject_Inheritance(), this.getDomainInheritance(), this.getDomainInheritance_Source(), "inheritance",
				null, 0, 1, DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainObject_Attributes(), this.getDomainAttribute(), this.getDomainAttribute_DomainObject(), "attributes",
				null, 0, -1, DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainObject_Associations(), this.getAbstractDomainAssociation(),
				this.getAbstractDomainAssociation_DomainObject(), "associations", null, 0, -1, DomainObject.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainObject_EnumAssociations(), this.getEnumAssociation(), this.getEnumAssociation_Source(),
				"enumAssociations", null, 0, -1, DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainObject_TargetInheritances(), this.getDomainInheritance(), null, "targetInheritances", null, 0, -1,
				DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainObject_DiscriminatorColumn(), theDbPackage.getDBColumn(), null, "discriminatorColumn", null, 0, 1,
				DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainObject_DatabaseTable(), theDbPackage.getDBTable(), null, "databaseTable", null, 0, 1,
				DomainObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainObject_Tag(), this.getDomainTagEnumeration(), "tag", null, 0, 1, DomainObject.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(domainAttributeEClass, DomainAttribute.class, "DomainAttribute", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDomainAttribute_Name(), ecorePackage.getEString(), "name", null, 0, 1, DomainAttribute.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_Pk(), ecorePackage.getEBoolean(), "pk", "false", 0, 1, DomainAttribute.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_Label(), ecorePackage.getEString(), "label", null, 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_LabelPlural(), ecorePackage.getEString(), "labelPlural", null, 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_Persistent(), ecorePackage.getEBoolean(), "persistent", "true", 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_FetchTypeEager(), ecorePackage.getEBoolean(), "fetchTypeEager", null, 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_Insertable(), ecorePackage.getEBoolean(), "insertable", "true", 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_Updatable(), ecorePackage.getEBoolean(), "updatable", "true", 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_TrackVersion(), ecorePackage.getEBoolean(), "trackVersion", "false", 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_SetDateOnPersist(), ecorePackage.getEBoolean(), "setDateOnPersist", "false", 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_SetDateOnUpdate(), ecorePackage.getEBoolean(), "setDateOnUpdate", "false", 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_DisplayAttribute(), ecorePackage.getEBoolean(), "displayAttribute", "false", 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDomainAttribute_DomainObject(), this.getDomainObject(), this.getDomainObject_Attributes(), "domainObject",
				null, 0, 1, DomainAttribute.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainAttribute_DomainAttributeValidator(), this.getDomainAttributeValidator(), null,
				"domainAttributeValidator", null, 0, 1, DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_TemporalType(), this.getTemporalTypeEnumeration(), "temporalType", null, 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDomainAttribute_JavaType(), theJavaPackage.getJavaType(), null, "javaType", null, 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainAttribute_Column(), theDbPackage.getDBColumn(), null, "column", null, 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_Tag(), this.getAttributeTagEnumeration(), "tag", null, 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_Lob(), ecorePackage.getEBoolean(), "lob", null, 0, 1, DomainAttribute.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_InternalComment(), ecorePackage.getEString(), "internalComment", null, 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_UserComment(), ecorePackage.getEString(), "userComment", null, 0, 1, DomainAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_RemoveWhitespaceCharacters(), ecorePackage.getEBoolean(), "removeWhitespaceCharacters",
				null, 0, 1, DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttribute_ConvertToUpperCase(), ecorePackage.getEBoolean(), "convertToUpperCase", null, 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDomainAttribute_ConvertToLowerCase(), ecorePackage.getEBoolean(), "convertToLowerCase", null, 0, 1,
				DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(domainAttributeValidatorEClass, DomainAttributeValidator.class, "DomainAttributeValidator", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDomainAttributeValidator_FutureDate(), ecorePackage.getEBoolean(), "futureDate", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_PastDate(), ecorePackage.getEBoolean(), "pastDate", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_MaxLength(), ecorePackage.getEIntegerObject(), "maxLength", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_MinLength(), ecorePackage.getEIntegerObject(), "minLength", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_Nullable(), ecorePackage.getEBoolean(), "nullable", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_MaxValue(), ecorePackage.getEString(), "maxValue", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_MinValue(), ecorePackage.getEString(), "minValue", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDomainAttributeValidator_RegularExpression(), ecorePackage.getEString(), "regularExpression", null, 0, 1,
				DomainAttributeValidator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(domainInheritanceEClass, DomainInheritance.class, "DomainInheritance", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDomainInheritance_Source(), this.getDomainObject(), this.getDomainObject_Inheritance(), "source", null, 0,
				1, DomainInheritance.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainInheritance_Target(), this.getDomainObject(), null, "target", null, 0, 1, DomainInheritance.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(domainNamespaceEClass, DomainNamespace.class, "DomainNamespace", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDomainNamespace_DomainObjects(), this.getDomainObject(), null, "domainObjects", null, 0, -1,
				DomainNamespace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDomainNamespace_Enumerations(), theJavaPackage.getJavaEnum(), null, "enumerations", null, 0, -1,
				DomainNamespace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(enumAssociationEClass, EnumAssociation.class, "EnumAssociation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getEnumAssociation_Source(), this.getDomainObject(), this.getDomainObject_EnumAssociations(), "source", null,
				0, 1, EnumAssociation.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getEnumAssociation_Target(), theJavaPackage.getJavaEnum(), null, "target", null, 0, 1, EnumAssociation.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getEnumAssociation_DomainAttribute(), this.getDomainAttribute(), null, "domainAttribute", null, 0, 1,
				EnumAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(idGeneratorEClass, IDGenerator.class, "IDGenerator", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getIDGenerator_Name(), ecorePackage.getEString(), "name", null, 0, 1, IDGenerator.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getIDGenerator_BlockSize(), ecorePackage.getEInt(), "blockSize", null, 0, 1, IDGenerator.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getIDGenerator_InitialValue(), ecorePackage.getEInt(), "initialValue", null, 0, 1, IDGenerator.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getIDGenerator_GeneratorType(), this.getIDGeneratorTypeEnumeration(), "generatorType", null, 0, 1,
				IDGenerator.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(manyToManyAssociationEClass, ManyToManyAssociation.class, "ManyToManyAssociation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getManyToManyAssociation_Table(), theDbPackage.getDBTable(), null, "table", null, 0, 1,
				ManyToManyAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(manyToOneAssociationEClass, ManyToOneAssociation.class, "ManyToOneAssociation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getManyToOneAssociation_Insertable(), ecorePackage.getEBoolean(), "insertable", null, 0, 1,
				ManyToOneAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getManyToOneAssociation_Updatable(), ecorePackage.getEBoolean(), "updatable", null, 0, 1,
				ManyToOneAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getManyToOneAssociation_Optional(), ecorePackage.getEBoolean(), "optional", null, 0, 1,
				ManyToOneAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getManyToOneAssociation_Column(), theDbPackage.getDBColumn(), null, "column", null, 0, 1,
				ManyToOneAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(oneToManyAssociationEClass, OneToManyAssociation.class, "OneToManyAssociation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getOneToManyAssociation_Table(), theDbPackage.getDBTable(), null, "table", null, 0, 1,
				OneToManyAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(oneToOneAssociationEClass, OneToOneAssociation.class, "OneToOneAssociation", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getOneToOneAssociation_Optional(), ecorePackage.getEBoolean(), "optional", null, 0, 1,
				OneToOneAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getOneToOneAssociation_Column(), theDbPackage.getDBColumn(), null, "column", null, 0, 1,
				OneToOneAssociation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(discriminatorColumnTypeEnumerationEEnum, DiscriminatorColumnTypeEnumeration.class,
				"DiscriminatorColumnTypeEnumeration");
		addEEnumLiteral(discriminatorColumnTypeEnumerationEEnum, DiscriminatorColumnTypeEnumeration.STRING);
		addEEnumLiteral(discriminatorColumnTypeEnumerationEEnum, DiscriminatorColumnTypeEnumeration.INTEGER);
		addEEnumLiteral(discriminatorColumnTypeEnumerationEEnum, DiscriminatorColumnTypeEnumeration.CHAR);

		initEEnum(idGeneratorTypeEnumerationEEnum, IDGeneratorTypeEnumeration.class, "IDGeneratorTypeEnumeration");
		addEEnumLiteral(idGeneratorTypeEnumerationEEnum, IDGeneratorTypeEnumeration.NONE);
		addEEnumLiteral(idGeneratorTypeEnumerationEEnum, IDGeneratorTypeEnumeration.SEQUENCE);
		addEEnumLiteral(idGeneratorTypeEnumerationEEnum, IDGeneratorTypeEnumeration.IDENTITY);
		addEEnumLiteral(idGeneratorTypeEnumerationEEnum, IDGeneratorTypeEnumeration.TABLE);
		addEEnumLiteral(idGeneratorTypeEnumerationEEnum, IDGeneratorTypeEnumeration.UUID);

		initEEnum(inheritanceTypeEnumerationEEnum, InheritanceTypeEnumeration.class, "InheritanceTypeEnumeration");
		addEEnumLiteral(inheritanceTypeEnumerationEEnum, InheritanceTypeEnumeration.NONE);
		addEEnumLiteral(inheritanceTypeEnumerationEEnum, InheritanceTypeEnumeration.SINGLE_TABLE);
		addEEnumLiteral(inheritanceTypeEnumerationEEnum, InheritanceTypeEnumeration.JOINED);

		initEEnum(temporalTypeEnumerationEEnum, TemporalTypeEnumeration.class, "TemporalTypeEnumeration");
		addEEnumLiteral(temporalTypeEnumerationEEnum, TemporalTypeEnumeration.NONE);
		addEEnumLiteral(temporalTypeEnumerationEEnum, TemporalTypeEnumeration.TIMESTAMP);
		addEEnumLiteral(temporalTypeEnumerationEEnum, TemporalTypeEnumeration.DATE);

		initEEnum(domainTagEnumerationEEnum, DomainTagEnumeration.class, "DomainTagEnumeration");
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.NONE);
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.USER);
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.LOGGING);
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.ROLE);
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.DOCUMENT);
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.SAVEDQUERY);
		addEEnumLiteral(domainTagEnumerationEEnum, DomainTagEnumeration.CLIENT);

		initEEnum(attributeTagEnumerationEEnum, AttributeTagEnumeration.class, "AttributeTagEnumeration");
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.NONE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.USER_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.USER_PASSWORD);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.USER_EMAIL);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.USER_ACTIVE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_DURATION);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_STACK_TRACE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_CLASS_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_METHOD_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_DATE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_MESSAGE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_LEVEL);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.LOGGING_HOST);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.ROLE_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.DOCUMENT_REF);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.DOCUMENT_DATA);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.DOCUMENT_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.DOCUMENT_SIZE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.SAVEDQUERY_TITLE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.SAVEDQUERY_VIEW_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.SAVEDQUERY_DATA_OBJ);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.CLIENT_NAME);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.CLIENT_ACTIVE);
		addEEnumLiteral(attributeTagEnumerationEEnum, AttributeTagEnumeration.CLIENT_DISPLAY);

		initEEnum(associationTagEnumerationEEnum, AssociationTagEnumeration.class, "AssociationTagEnumeration");
		addEEnumLiteral(associationTagEnumerationEEnum, AssociationTagEnumeration.NONE);
		addEEnumLiteral(associationTagEnumerationEEnum, AssociationTagEnumeration.USER_ROLE);
		addEEnumLiteral(associationTagEnumerationEEnum, AssociationTagEnumeration.LOGGING_USER);
		addEEnumLiteral(associationTagEnumerationEEnum, AssociationTagEnumeration.SAVEDQUERY_OWNER);
		addEEnumLiteral(associationTagEnumerationEEnum, AssociationTagEnumeration.CLIENT_REFERENCE);

		// Create resource
		createResource(eNS_URI);
	}

}
