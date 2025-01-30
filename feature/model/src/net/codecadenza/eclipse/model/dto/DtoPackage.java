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
package net.codecadenza.eclipse.model.dto;

import net.codecadenza.eclipse.model.mapping.MappingPackage;
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
 * @see net.codecadenza.eclipse.model.dto.DtoFactory
 * @model kind="package"
 * @generated
 */
public interface DtoPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "dto";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/dto.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.dto";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	DtoPackage eINSTANCE = net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl <em>DTO Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl
	 * @see net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl#getDTOBean()
	 * @generated
	 */
	int DTO_BEAN = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__NAME = MappingPackage.MAPPING_OBJECT__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__COMMENT = MappingPackage.MAPPING_OBJECT__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__MAPPABLE = MappingPackage.MAPPING_OBJECT__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__PRIMITIVE = MappingPackage.MAPPING_OBJECT__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__NAMESPACE = MappingPackage.MAPPING_OBJECT__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__DOMAIN_OBJECT = MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Attributes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__ATTRIBUTES = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Standard Conversion</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__STANDARD_CONVERSION = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Shared</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__SHARED = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Created Manually</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN__CREATED_MANUALLY = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>DTO Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_FEATURE_COUNT = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl <em>DTO Bean Attribute</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl
	 * @see net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl#getDTOBeanAttribute()
	 * @generated
	 */
	int DTO_BEAN_ATTRIBUTE = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__NAME = MappingPackage.MAPPING_ATTRIBUTE__NAME;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__ASSOCIATION = MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION;

	/**
	 * The feature ID for the '<em><b>Domain Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__DOMAIN_ATTRIBUTE = MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE;

	/**
	 * The feature ID for the '<em><b>Association List</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__ASSOCIATION_LIST = MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST;

	/**
	 * The feature ID for the '<em><b>Mapping Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__MAPPING_TYPE = MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE;

	/**
	 * The feature ID for the '<em><b>Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__MODIFIER = MappingPackage.MAPPING_ATTRIBUTE__MODIFIER;

	/**
	 * The feature ID for the '<em><b>Default Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__DEFAULT_VALUE = MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE;

	/**
	 * The feature ID for the '<em><b>DTO Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__DTO_BEAN = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Referenced DTO Bean</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Select Token</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__SELECT_TOKEN = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Lov Return</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE__LOV_RETURN = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>DTO Bean Attribute</em>' class
	 * @generated
	 * @ordered
	 */
	int DTO_BEAN_ATTRIBUTE_FEATURE_COUNT = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 4;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.dto.DTOBean <em>DTO Bean</em>}'
	 * @return the meta object for class '<em>DTO Bean</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBean
	 * @generated
	 */
	EClass getDTOBean();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.dto.DTOBean#getAttributes
	 * <em>Attributes</em>}'
	 * @return the meta object for the containment reference list '<em>Attributes</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getAttributes()
	 * @see #getDTOBean()
	 * @generated
	 */
	EReference getDTOBean_Attributes();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.dto.DTOBean#isStandardConversion <em>Standard
	 * Conversion</em>}'
	 * @return the meta object for the attribute '<em>Standard Conversion</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isStandardConversion()
	 * @see #getDTOBean()
	 * @generated
	 */
	EAttribute getDTOBean_StandardConversion();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute <em>DTO Bean Attribute</em>}'
	 * @return the meta object for class '<em>DTO Bean Attribute</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute
	 * @generated
	 */
	EClass getDTOBeanAttribute();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.dto.DTOBean#isShared <em>Shared</em>}'
	 * @return the meta object for the attribute '<em>Shared</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isShared()
	 * @see #getDTOBean()
	 * @generated
	 */
	EAttribute getDTOBean_Shared();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.dto.DTOBean#isCreatedManually <em>Created
	 * Manually</em>}'
	 * @return the meta object for the attribute '<em>Created Manually</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isCreatedManually()
	 * @see #getDTOBean()
	 * @generated
	 */
	EAttribute getDTOBean_CreatedManually();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean
	 * <em>DTO Bean</em>}'
	 * @return the meta object for the container reference '<em>DTO Bean</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean()
	 * @see #getDTOBeanAttribute()
	 * @generated
	 */
	EReference getDTOBeanAttribute_DTOBean();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getReferencedDTOBean
	 * <em>Referenced DTO Bean</em>}'
	 * @return the meta object for the reference '<em>Referenced DTO Bean</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getReferencedDTOBean()
	 * @see #getDTOBeanAttribute()
	 * @generated
	 */
	EReference getDTOBeanAttribute_ReferencedDTOBean();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectToken <em>Select
	 * Token</em>}'
	 * @return the meta object for the attribute '<em>Select Token</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectToken()
	 * @see #getDTOBeanAttribute()
	 * @generated
	 */
	EAttribute getDTOBeanAttribute_SelectToken();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#isLovReturn <em>Lov
	 * Return</em>}'
	 * @return the meta object for the attribute '<em>Lov Return</em>'
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#isLovReturn()
	 * @see #getDTOBeanAttribute()
	 * @generated
	 */
	EAttribute getDTOBeanAttribute_LovReturn();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	DtoFactory getDtoFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl <em>DTO Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl
		 * @see net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl#getDTOBean()
		 * @generated
		 */
		EClass DTO_BEAN = eINSTANCE.getDTOBean();

		/**
		 * The meta object literal for the '<em><b>Attributes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DTO_BEAN__ATTRIBUTES = eINSTANCE.getDTOBean_Attributes();

		/**
		 * The meta object literal for the '<em><b>Standard Conversion</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DTO_BEAN__STANDARD_CONVERSION = eINSTANCE.getDTOBean_StandardConversion();

		/**
		 * The meta object literal for the '<em><b>Shared</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DTO_BEAN__SHARED = eINSTANCE.getDTOBean_Shared();

		/**
		 * The meta object literal for the '<em><b>Created Manually</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DTO_BEAN__CREATED_MANUALLY = eINSTANCE.getDTOBean_CreatedManually();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl <em>DTO Bean
		 * Attribute</em>}' class
		 * @see net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl
		 * @see net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl#getDTOBeanAttribute()
		 * @generated
		 */
		EClass DTO_BEAN_ATTRIBUTE = eINSTANCE.getDTOBeanAttribute();

		/**
		 * The meta object literal for the '<em><b>DTO Bean</b></em>' container reference feature
		 * @generated
		 */
		EReference DTO_BEAN_ATTRIBUTE__DTO_BEAN = eINSTANCE.getDTOBeanAttribute_DTOBean();

		/**
		 * The meta object literal for the '<em><b>Referenced DTO Bean</b></em>' reference feature
		 * @generated
		 */
		EReference DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN = eINSTANCE.getDTOBeanAttribute_ReferencedDTOBean();

		/**
		 * The meta object literal for the '<em><b>Select Token</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DTO_BEAN_ATTRIBUTE__SELECT_TOKEN = eINSTANCE.getDTOBeanAttribute_SelectToken();

		/**
		 * The meta object literal for the '<em><b>Lov Return</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DTO_BEAN_ATTRIBUTE__LOV_RETURN = eINSTANCE.getDTOBeanAttribute_LovReturn();

	}

}
