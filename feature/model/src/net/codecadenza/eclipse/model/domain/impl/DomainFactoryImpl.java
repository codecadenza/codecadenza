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

import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
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
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class DomainFactoryImpl extends EFactoryImpl implements DomainFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static DomainFactory init() {
		try {
			final var theDomainFactory = (DomainFactory) EPackage.Registry.INSTANCE.getEFactory(DomainPackage.eNS_URI);

			if (theDomainFactory != null)
				return theDomainFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new DomainFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case DomainPackage.DOMAIN_OBJECT -> createDomainObject();
			case DomainPackage.DOMAIN_ATTRIBUTE -> createDomainAttribute();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR -> createDomainAttributeValidator();
			case DomainPackage.DOMAIN_INHERITANCE -> createDomainInheritance();
			case DomainPackage.DOMAIN_NAMESPACE -> createDomainNamespace();
			case DomainPackage.ENUM_ASSOCIATION -> createEnumAssociation();
			case DomainPackage.ID_GENERATOR -> createIDGenerator();
			case DomainPackage.MANY_TO_MANY_ASSOCIATION -> createManyToManyAssociation();
			case DomainPackage.MANY_TO_ONE_ASSOCIATION -> createManyToOneAssociation();
			case DomainPackage.ONE_TO_MANY_ASSOCIATION -> createOneToManyAssociation();
			case DomainPackage.ONE_TO_ONE_ASSOCIATION -> createOneToOneAssociation();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#createFromString(org.eclipse.emf.ecore.EDataType, java.lang.String)
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		return switch (eDataType.getClassifierID()) {
			case DomainPackage.DISCRIMINATOR_COLUMN_TYPE_ENUMERATION -> createDiscriminatorColumnTypeEnumerationFromString(eDataType,
					initialValue);
			case DomainPackage.ID_GENERATOR_TYPE_ENUMERATION -> createIDGeneratorTypeEnumerationFromString(eDataType, initialValue);
			case DomainPackage.INHERITANCE_TYPE_ENUMERATION -> createInheritanceTypeEnumerationFromString(eDataType, initialValue);
			case DomainPackage.TEMPORAL_TYPE_ENUMERATION -> createTemporalTypeEnumerationFromString(eDataType, initialValue);
			case DomainPackage.DOMAIN_TAG_ENUMERATION -> createDomainTagEnumerationFromString(eDataType, initialValue);
			case DomainPackage.ATTRIBUTE_TAG_ENUMERATION -> createAttributeTagEnumerationFromString(eDataType, initialValue);
			case DomainPackage.ASSOCIATION_TAG_ENUMERATION -> createAssociationTagEnumerationFromString(eDataType, initialValue);
			case DomainPackage.COLLECTION_TYPE_ENUMERATION -> createCollectionTypeEnumerationFromString(eDataType, initialValue);
			case DomainPackage.COLLECTION_MAPPING_STRATEGY_ENUMERATION -> createCollectionMappingStrategyEnumerationFromString(
					eDataType, initialValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#convertToString(org.eclipse.emf.ecore.EDataType, java.lang.Object)
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		return switch (eDataType.getClassifierID()) {
			case DomainPackage.DISCRIMINATOR_COLUMN_TYPE_ENUMERATION -> convertDiscriminatorColumnTypeEnumerationToString(eDataType,
					instanceValue);
			case DomainPackage.ID_GENERATOR_TYPE_ENUMERATION -> convertIDGeneratorTypeEnumerationToString(eDataType, instanceValue);
			case DomainPackage.INHERITANCE_TYPE_ENUMERATION -> convertInheritanceTypeEnumerationToString(eDataType, instanceValue);
			case DomainPackage.TEMPORAL_TYPE_ENUMERATION -> convertTemporalTypeEnumerationToString(eDataType, instanceValue);
			case DomainPackage.DOMAIN_TAG_ENUMERATION -> convertDomainTagEnumerationToString(eDataType, instanceValue);
			case DomainPackage.ATTRIBUTE_TAG_ENUMERATION -> convertAttributeTagEnumerationToString(eDataType, instanceValue);
			case DomainPackage.ASSOCIATION_TAG_ENUMERATION -> convertAssociationTagEnumerationToString(eDataType, instanceValue);
			case DomainPackage.COLLECTION_TYPE_ENUMERATION -> convertCollectionTypeEnumerationToString(eDataType, instanceValue);
			case DomainPackage.COLLECTION_MAPPING_STRATEGY_ENUMERATION -> convertCollectionMappingStrategyEnumerationToString(eDataType,
					instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createDomainObject()
	 * @generated
	 */
	@Override
	public DomainObject createDomainObject() {
		return new DomainObjectImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createDomainAttribute()
	 * @generated
	 */
	@Override
	public DomainAttribute createDomainAttribute() {
		return new DomainAttributeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createDomainAttributeValidator()
	 * @generated
	 */
	@Override
	public DomainAttributeValidator createDomainAttributeValidator() {
		return new DomainAttributeValidatorImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createDomainInheritance()
	 * @generated
	 */
	@Override
	public DomainInheritance createDomainInheritance() {
		return new DomainInheritanceImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createDomainNamespace()
	 * @generated
	 */
	@Override
	public DomainNamespace createDomainNamespace() {
		return new DomainNamespaceImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createEnumAssociation()
	 * @generated
	 */
	@Override
	public EnumAssociation createEnumAssociation() {
		return new EnumAssociationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createIDGenerator()
	 * @generated
	 */
	@Override
	public IDGenerator createIDGenerator() {
		return new IDGeneratorImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createManyToManyAssociation()
	 * @generated
	 */
	@Override
	public ManyToManyAssociation createManyToManyAssociation() {
		return new ManyToManyAssociationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createManyToOneAssociation()
	 * @generated
	 */
	@Override
	public ManyToOneAssociation createManyToOneAssociation() {
		return new ManyToOneAssociationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createOneToManyAssociation()
	 * @generated
	 */
	@Override
	public OneToManyAssociation createOneToManyAssociation() {
		return new OneToManyAssociationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#createOneToOneAssociation()
	 * @generated
	 */
	@Override
	public OneToOneAssociation createOneToOneAssociation() {
		return new OneToOneAssociationImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public DiscriminatorColumnTypeEnumeration createDiscriminatorColumnTypeEnumerationFromString(EDataType eDataType,
			String initialValue) {
		final DiscriminatorColumnTypeEnumeration result = DiscriminatorColumnTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertDiscriminatorColumnTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public IDGeneratorTypeEnumeration createIDGeneratorTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final IDGeneratorTypeEnumeration result = IDGeneratorTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertIDGeneratorTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public InheritanceTypeEnumeration createInheritanceTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final InheritanceTypeEnumeration result = InheritanceTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertInheritanceTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public TemporalTypeEnumeration createTemporalTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final TemporalTypeEnumeration result = TemporalTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertTemporalTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public DomainTagEnumeration createDomainTagEnumerationFromString(EDataType eDataType, String initialValue) {
		final DomainTagEnumeration result = DomainTagEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertDomainTagEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public AttributeTagEnumeration createAttributeTagEnumerationFromString(EDataType eDataType, String initialValue) {
		final AttributeTagEnumeration result = AttributeTagEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertAttributeTagEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public AssociationTagEnumeration createAssociationTagEnumerationFromString(EDataType eDataType, String initialValue) {
		final AssociationTagEnumeration result = AssociationTagEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertAssociationTagEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public CollectionTypeEnumeration createCollectionTypeEnumerationFromString(EDataType eDataType, String instanceValue) {
		final CollectionTypeEnumeration result = CollectionTypeEnumeration.get(instanceValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + instanceValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertCollectionTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	public CollectionMappingStrategyEnumeration createCollectionMappingStrategyEnumerationFromString(EDataType eDataType,
			String instanceValue) {
		final CollectionMappingStrategyEnumeration result = CollectionMappingStrategyEnumeration.get(instanceValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + instanceValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertCollectionMappingStrategyEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainFactory#getDomainPackage()
	 * @generated
	 */
	@Override
	public DomainPackage getDomainPackage() {
		return (DomainPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the domain object package
	 * @generated
	 */
	@Deprecated
	public static DomainPackage getPackage() {
		return DomainPackage.eINSTANCE;
	}

}
