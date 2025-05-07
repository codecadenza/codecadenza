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
package net.codecadenza.eclipse.model.mapping;

import net.codecadenza.eclipse.model.java.JavaPackage;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
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
 * @see net.codecadenza.eclipse.model.mapping.MappingFactory
 * @model kind="package"
 * @generated
 */
public interface MappingPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "mapping";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/mapping.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.mapping";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	MappingPackage eINSTANCE = net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl <em>Object</em>}' class
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl#getMappingObject()
	 * @generated
	 */
	int MAPPING_OBJECT = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT__NAME = JavaPackage.JAVA_TYPE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT__COMMENT = JavaPackage.JAVA_TYPE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT__MAPPABLE = JavaPackage.JAVA_TYPE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT__PRIMITIVE = JavaPackage.JAVA_TYPE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT__NAMESPACE = JavaPackage.JAVA_TYPE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT__DOMAIN_OBJECT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Object</em>' class
	 * @generated
	 * @ordered
	 */
	int MAPPING_OBJECT_FEATURE_COUNT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl <em>Mapping
	 * Attribute</em>}' class
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl#getMappingAttribute()
	 * @generated
	 */
	int MAPPING_ATTRIBUTE = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__ASSOCIATION = 1;

	/**
	 * The feature ID for the '<em><b>Domain Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE = 2;

	/**
	 * The feature ID for the '<em><b>Association List</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__ASSOCIATION_LIST = 3;

	/**
	 * The feature ID for the '<em><b>Mapping Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__MAPPING_TYPE = 4;

	/**
	 * The feature ID for the '<em><b>Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__MODIFIER = 5;

	/**
	 * The feature ID for the '<em><b>Default Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE__DEFAULT_VALUE = 6;

	/**
	 * The number of structural features of the '<em>Mapping Attribute</em>' class
	 * @generated
	 * @ordered
	 */
	int MAPPING_ATTRIBUTE_FEATURE_COUNT = 7;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.mapping.MappingObject <em>Mapping Object</em>}'
	 * @return the meta object for class '<em>Object</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingObject
	 * @generated
	 */
	EClass getMappingObject();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.mapping.MappingObject#getDomainObject
	 * <em>Domain Object</em>}'
	 * @return the meta object for the reference '<em>Domain Object</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingObject#getDomainObject()
	 * @see #getMappingObject()
	 * @generated
	 */
	EReference getMappingObject_DomainObject();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute <em>Mapping Attribute</em>}'
	 * @return the meta object for class '<em>Mapping Attribute</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute
	 * @generated
	 */
	EClass getMappingAttribute();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getName()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EAttribute getMappingAttribute_Name();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociation()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EReference getMappingAttribute_Association();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getDomainAttribute
	 * <em>Domain Attribute</em>}'
	 * @return the meta object for the reference '<em>Domain Attribute</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getDomainAttribute()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EReference getMappingAttribute_DomainAttribute();

	/**
	 * Return the meta object for the reference list
	 * '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociationList <em>Association List</em>}'
	 * @return the meta object for the reference list '<em>Association List</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociationList()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EReference getMappingAttribute_AssociationList();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getMappingType
	 * <em>Mapping Type</em>}'
	 * @return the meta object for the reference '<em>Mapping Type</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getMappingType()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EReference getMappingAttribute_MappingType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getModifier
	 * <em>Modifier</em>}'
	 * @return the meta object for the attribute '<em>Modifier</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getModifier()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EAttribute getMappingAttribute_Modifier();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getDefaultValue
	 * <em>Default Value</em>}'
	 * @return the meta object for the attribute '<em>Default Value</em>'
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getDefaultValue()
	 * @see #getMappingAttribute()
	 * @generated
	 */
	EAttribute getMappingAttribute_DefaultValue();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	MappingFactory getMappingFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl <em>Object</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl
		 * @see net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl#getMappingObject()
		 * @generated
		 */
		EClass MAPPING_OBJECT = eINSTANCE.getMappingObject();

		/**
		 * The meta object literal for the '<em><b>Domain Object</b></em>' reference feature
		 * @generated
		 */
		EReference MAPPING_OBJECT__DOMAIN_OBJECT = eINSTANCE.getMappingObject_DomainObject();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl <em>Mapping
		 * Attribute</em>}' class
		 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl
		 * @see net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl#getMappingAttribute()
		 * @generated
		 */
		EClass MAPPING_ATTRIBUTE = eINSTANCE.getMappingAttribute();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute MAPPING_ATTRIBUTE__NAME = eINSTANCE.getMappingAttribute_Name();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference MAPPING_ATTRIBUTE__ASSOCIATION = eINSTANCE.getMappingAttribute_Association();

		/**
		 * The meta object literal for the '<em><b>Domain Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE = eINSTANCE.getMappingAttribute_DomainAttribute();

		/**
		 * The meta object literal for the '<em><b>Association List</b></em>' reference list feature
		 * @generated
		 */
		EReference MAPPING_ATTRIBUTE__ASSOCIATION_LIST = eINSTANCE.getMappingAttribute_AssociationList();

		/**
		 * The meta object literal for the '<em><b>Mapping Type</b></em>' reference feature
		 * @generated
		 */
		EReference MAPPING_ATTRIBUTE__MAPPING_TYPE = eINSTANCE.getMappingAttribute_MappingType();

		/**
		 * The meta object literal for the '<em><b>Modifier</b></em>' attribute feature
		 * @generated
		 */
		EAttribute MAPPING_ATTRIBUTE__MODIFIER = eINSTANCE.getMappingAttribute_Modifier();

		/**
		 * The meta object literal for the '<em><b>Default Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute MAPPING_ATTRIBUTE__DEFAULT_VALUE = eINSTANCE.getMappingAttribute_DefaultValue();

	}

}
