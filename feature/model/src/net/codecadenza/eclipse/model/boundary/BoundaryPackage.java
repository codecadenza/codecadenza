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
package net.codecadenza.eclipse.model.boundary;

import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
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
 * @see net.codecadenza.eclipse.model.boundary.BoundaryFactory
 * @model kind="package"
 * @generated
 */
public interface BoundaryPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "boundary";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/boundary.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.boundary";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	BoundaryPackage eINSTANCE = net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryBeanImpl <em>Boundary Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryBeanImpl
	 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryBean()
	 * @generated
	 */
	int BOUNDARY_BEAN = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__NAME = ServicePackage.SERVICE_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__COMMENT = ServicePackage.SERVICE_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__MAPPABLE = ServicePackage.SERVICE_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__PRIMITIVE = ServicePackage.SERVICE_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__NAMESPACE = ServicePackage.SERVICE_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__DOMAIN_OBJECT = ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__INTERFACE_NAME = ServicePackage.SERVICE_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Boundary Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__BOUNDARY_METHODS = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Repository</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN__REPOSITORY = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Boundary Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_BEAN_FEATURE_COUNT = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl <em>Boundary Method</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl
	 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryMethod()
	 * @generated
	 */
	int BOUNDARY_METHOD = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__NAME = ServicePackage.SERVICE_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__COMMENT = ServicePackage.SERVICE_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__JAVA_TYPE = ServicePackage.SERVICE_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__RETURN_TYPE = ServicePackage.SERVICE_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__RETURN_TYPE_MODIFIER = ServicePackage.SERVICE_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__METHOD_PARAMETERS = ServicePackage.SERVICE_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__QUERY_STATEMENT = ServicePackage.SERVICE_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__PERMISSION_MODE = ServicePackage.SERVICE_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__TRANSACTION_TYPE = ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__ROLES = ServicePackage.SERVICE_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__METHOD_INVOCATION = ServicePackage.SERVICE_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__CUSTOM_STATEMENT = ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Boundary Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__BOUNDARY_BEAN = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Method Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__METHOD_TYPE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Domain Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__DOMAIN_ATTRIBUTE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__ASSOCIATION = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Data Fetch Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__DATA_FETCH_TYPE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Service Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD__SERVICE_METHOD = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 5;

	/**
	 * The number of structural features of the '<em>Boundary Method</em>' class
	 * @generated
	 * @ordered
	 */
	int BOUNDARY_METHOD_FEATURE_COUNT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration <em>Method Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration
	 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryMethodTypeEnumeration()
	 * @generated
	 */
	int BOUNDARY_METHOD_TYPE_ENUMERATION = 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType <em>Method Data Fetch
	 * Type</em>}' enum
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType
	 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryMethodDataFetchType()
	 * @generated
	 */
	int BOUNDARY_METHOD_DATA_FETCH_TYPE = 3;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.boundary.BoundaryBean <em>Boundary Bean</em>}'
	 * @return the meta object for class '<em>Boundary Bean</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean
	 * @generated
	 */
	EClass getBoundaryBean();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethods <em>Boundary Methods</em>}'
	 * @return the meta object for the containment reference list '<em>Boundary Methods</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethods()
	 * @see #getBoundaryBean()
	 * @generated
	 */
	EReference getBoundaryBean_BoundaryMethods();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.boundary.BoundaryBean#getRepository
	 * <em>Repository</em>}'
	 * @return the meta object for the reference '<em>Repository</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getRepository()
	 * @see #getBoundaryBean()
	 * @generated
	 */
	EReference getBoundaryBean_Repository();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod <em>Boundary Method</em>}'
	 * @return the meta object for class '<em>Boundary Method</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod
	 * @generated
	 */
	EClass getBoundaryMethod();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean <em>Boundary Bean</em>}'
	 * @return the meta object for the container reference '<em>Boundary Bean</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean()
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	EReference getBoundaryMethod_BoundaryBean();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getMethodType
	 * <em>Method Type</em>}'
	 * @return the meta object for the attribute '<em>Method Type</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getMethodType()
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	EAttribute getBoundaryMethod_MethodType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDomainAttribute
	 * <em>Domain Attribute</em>}'
	 * @return the meta object for the reference '<em>Domain Attribute</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDomainAttribute()
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	EReference getBoundaryMethod_DomainAttribute();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getAssociation()
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	EReference getBoundaryMethod_Association();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDataFetchType
	 * <em>Data Fetch Type</em>}'
	 * @return the meta object for the attribute '<em>Data Fetch Type</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDataFetchType()
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	EAttribute getBoundaryMethod_DataFetchType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getServiceMethod
	 * <em>Service Method</em>}'
	 * @return the meta object for the reference '<em>Service Method</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getServiceMethod()
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	EReference getBoundaryMethod_ServiceMethod();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration <em>Method Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Method Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration
	 * @generated
	 */
	EEnum getBoundaryMethodTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType <em>Method Data
	 * Fetch Type</em>}'
	 * @return the meta object for enum '<em>Method Data Fetch Type</em>'
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType
	 * @generated
	 */
	EEnum getBoundaryMethodDataFetchType();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	BoundaryFactory getBoundaryFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryBeanImpl <em>Boundary
		 * Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryBeanImpl
		 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryBean()
		 * @generated
		 */
		EClass BOUNDARY_BEAN = eINSTANCE.getBoundaryBean();

		/**
		 * The meta object literal for the '<em><b>Boundary Methods</b></em>' containment reference list feature
		 * @generated
		 */
		EReference BOUNDARY_BEAN__BOUNDARY_METHODS = eINSTANCE.getBoundaryBean_BoundaryMethods();

		/**
		 * The meta object literal for the '<em><b>Repository</b></em>' reference feature
		 * @generated
		 */
		EReference BOUNDARY_BEAN__REPOSITORY = eINSTANCE.getBoundaryBean_Repository();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl <em>Boundary
		 * Method</em>}' class
		 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl
		 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryMethod()
		 * @generated
		 */
		EClass BOUNDARY_METHOD = eINSTANCE.getBoundaryMethod();

		/**
		 * The meta object literal for the '<em><b>Boundary Bean</b></em>' container reference feature
		 * @generated
		 */
		EReference BOUNDARY_METHOD__BOUNDARY_BEAN = eINSTANCE.getBoundaryMethod_BoundaryBean();

		/**
		 * The meta object literal for the '<em><b>Method Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute BOUNDARY_METHOD__METHOD_TYPE = eINSTANCE.getBoundaryMethod_MethodType();

		/**
		 * The meta object literal for the '<em><b>Domain Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference BOUNDARY_METHOD__DOMAIN_ATTRIBUTE = eINSTANCE.getBoundaryMethod_DomainAttribute();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference BOUNDARY_METHOD__ASSOCIATION = eINSTANCE.getBoundaryMethod_Association();

		/**
		 * The meta object literal for the '<em><b>Data Fetch Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute BOUNDARY_METHOD__DATA_FETCH_TYPE = eINSTANCE.getBoundaryMethod_DataFetchType();

		/**
		 * The meta object literal for the '<em><b>Service Method</b></em>' reference feature
		 * @generated
		 */
		EReference BOUNDARY_METHOD__SERVICE_METHOD = eINSTANCE.getBoundaryMethod_ServiceMethod();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration <em>Method Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration
		 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryMethodTypeEnumeration()
		 * @generated
		 */
		EEnum BOUNDARY_METHOD_TYPE_ENUMERATION = eINSTANCE.getBoundaryMethodTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType <em>Method Data
		 * Fetch Type</em>}' enum
		 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType
		 * @see net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl#getBoundaryMethodDataFetchType()
		 * @generated
		 */
		EEnum BOUNDARY_METHOD_DATA_FETCH_TYPE = eINSTANCE.getBoundaryMethodDataFetchType();

	}

}
