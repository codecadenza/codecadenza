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
package net.codecadenza.eclipse.model.repository;

import net.codecadenza.eclipse.model.java.JavaPackage;
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
 * @see net.codecadenza.eclipse.model.repository.RepositoryFactory
 * @model kind="package"
 * @generated
 */
public interface RepositoryPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "repository";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/repository.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.repository";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	RepositoryPackage eINSTANCE = net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.repository.impl.RepositoryImpl <em>Repository</em>}' class
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryImpl
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepository()
	 * @generated
	 */
	int REPOSITORY = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__NAME = ServicePackage.SERVICE_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__COMMENT = ServicePackage.SERVICE_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__MAPPABLE = ServicePackage.SERVICE_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__PRIMITIVE = ServicePackage.SERVICE_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__NAMESPACE = ServicePackage.SERVICE_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__DOMAIN_OBJECT = ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__INTERFACE_NAME = ServicePackage.SERVICE_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Repository Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int REPOSITORY__REPOSITORY_METHODS = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Repository</em>' class
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_FEATURE_COUNT = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl <em>Repository
	 * Method</em>}' class
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepositoryMethod()
	 * @generated
	 */
	int REPOSITORY_METHOD = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__NAME = ServicePackage.SERVICE_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__COMMENT = ServicePackage.SERVICE_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__JAVA_TYPE = ServicePackage.SERVICE_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__RETURN_TYPE = ServicePackage.SERVICE_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__RETURN_TYPE_MODIFIER = ServicePackage.SERVICE_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__METHOD_PARAMETERS = ServicePackage.SERVICE_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__QUERY_STATEMENT = ServicePackage.SERVICE_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__PERMISSION_MODE = ServicePackage.SERVICE_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__TRANSACTION_TYPE = ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__ROLES = ServicePackage.SERVICE_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__METHOD_INVOCATION = ServicePackage.SERVICE_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__CUSTOM_STATEMENT = ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Repository</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__REPOSITORY = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Method Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__METHOD_TYPE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Hint</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD__HINT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Repository Method</em>' class
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_FEATURE_COUNT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodParameterImpl <em>Repository
	 * Method Parameter</em>}' class
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryMethodParameterImpl
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepositoryMethodParameter()
	 * @generated
	 */
	int REPOSITORY_METHOD_PARAMETER = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__NAME = JavaPackage.METHOD_PARAMETER__NAME;

	/**
	 * The feature ID for the '<em><b>Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__METHOD = JavaPackage.METHOD_PARAMETER__METHOD;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__TYPE = JavaPackage.METHOD_PARAMETER__TYPE;

	/**
	 * The feature ID for the '<em><b>Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__MODIFIER = JavaPackage.METHOD_PARAMETER__MODIFIER;

	/**
	 * The feature ID for the '<em><b>Hint</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__HINT = JavaPackage.METHOD_PARAMETER__HINT;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__ASSOCIATION = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER__ATTRIBUTE = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Repository Method Parameter</em>' class
	 * @generated
	 * @ordered
	 */
	int REPOSITORY_METHOD_PARAMETER_FEATURE_COUNT = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration <em>Repository
	 * Method Type Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepositoryMethodTypeEnumeration()
	 * @generated
	 */
	int REPOSITORY_METHOD_TYPE_ENUMERATION = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.repository.PermissionModeEnumeration <em>Permission Mode
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.repository.PermissionModeEnumeration
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getPermissionModeEnumeration()
	 * @generated
	 */
	int PERMISSION_MODE_ENUMERATION = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration <em>Transaction Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration
	 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getTransactionTypeEnumeration()
	 * @generated
	 */
	int TRANSACTION_TYPE_ENUMERATION = 5;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.repository.Repository <em>Repository</em>}'
	 * @return the meta object for class '<em>Repository</em>'
	 * @see net.codecadenza.eclipse.model.repository.Repository
	 * @generated
	 */
	EClass getRepository();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.repository.Repository#getRepositoryMethods <em>Repository Methods</em>}'
	 * @return the meta object for the containment reference list '<em>Repository Methods</em>'
	 * @see net.codecadenza.eclipse.model.repository.Repository#getRepositoryMethods()
	 * @see #getRepository()
	 * @generated
	 */
	EReference getRepository_RepositoryMethods();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod <em>Repository
	 * Method</em>}'
	 * @return the meta object for class '<em>Repository Method</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod
	 * @generated
	 */
	EClass getRepositoryMethod();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository <em>Repository</em>}'
	 * @return the meta object for the container reference '<em>Repository</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository()
	 * @see #getRepositoryMethod()
	 * @generated
	 */
	EReference getRepositoryMethod_Repository();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getMethodType
	 * <em>Method Type</em>}'
	 * @return the meta object for the attribute '<em>Method Type</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getMethodType()
	 * @see #getRepositoryMethod()
	 * @generated
	 */
	EAttribute getRepositoryMethod_MethodType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getHint
	 * <em>Hint</em>}'
	 * @return the meta object for the attribute '<em>Hint</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getHint()
	 * @see #getRepositoryMethod()
	 * @generated
	 */
	EAttribute getRepositoryMethod_Hint();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.repository.RepositoryMethodParameter <em>Repository
	 * Method Parameter</em>}'
	 * @return the meta object for class '<em>Repository Method Parameter</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter
	 * @generated
	 */
	EClass getRepositoryMethodParameter();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#getAssociation <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#getAssociation()
	 * @see #getRepositoryMethodParameter()
	 * @generated
	 */
	EReference getRepositoryMethodParameter_Association();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#getAttribute <em>Domain Attribute</em>}'
	 * @return the meta object for the reference '<em>Domain Attribute</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#getAttribute()
	 * @see #getRepositoryMethodParameter()
	 * @generated
	 */
	EReference getRepositoryMethodParameter_Attribute();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
	 * <em>Repository Method Type Enumeration</em>}'
	 * @return the meta object for enum '<em>Repository Method Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
	 * @generated
	 */
	EEnum getRepositoryMethodTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.repository.PermissionModeEnumeration <em>Permission
	 * Mode Enumeration</em>}'
	 * @return the meta object for enum '<em>Permission Mode Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.repository.PermissionModeEnumeration
	 * @generated
	 */
	EEnum getPermissionModeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration <em>Transaction
	 * Type Enumeration</em>}'
	 * @return the meta object for enum '<em>Transaction Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration
	 * @generated
	 */
	EEnum getTransactionTypeEnumeration();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	RepositoryFactory getRepositoryFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.repository.impl.RepositoryImpl <em>Repository</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryImpl
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepository()
		 * @generated
		 */
		EClass REPOSITORY = eINSTANCE.getRepository();

		/**
		 * The meta object literal for the '<em><b>Repository Methods</b></em>' containment reference list feature
		 * @generated
		 */
		EReference REPOSITORY__REPOSITORY_METHODS = eINSTANCE.getRepository_RepositoryMethods();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl <em>Repository
		 * Method</em>}' class
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepositoryMethod()
		 * @generated
		 */
		EClass REPOSITORY_METHOD = eINSTANCE.getRepositoryMethod();

		/**
		 * The meta object literal for the '<em><b>Repository</b></em>' container reference feature
		 * @generated
		 */
		EReference REPOSITORY_METHOD__REPOSITORY = eINSTANCE.getRepositoryMethod_Repository();

		/**
		 * The meta object literal for the '<em><b>Method Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REPOSITORY_METHOD__METHOD_TYPE = eINSTANCE.getRepositoryMethod_MethodType();

		/**
		 * The meta object literal for the '<em><b>Hint</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REPOSITORY_METHOD__HINT = eINSTANCE.getRepositoryMethod_Hint();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodParameterImpl
		 * <em>Repository Method Parameter</em>}' class
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryMethodParameterImpl
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepositoryMethodParameter()
		 * @generated
		 */
		EClass REPOSITORY_METHOD_PARAMETER = eINSTANCE.getRepositoryMethodParameter();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference REPOSITORY_METHOD_PARAMETER__ASSOCIATION = eINSTANCE.getRepositoryMethodParameter_Association();

		/**
		 * The meta object literal for the '<em><b>Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference REPOSITORY_METHOD_PARAMETER__ATTRIBUTE = eINSTANCE.getRepositoryMethodParameter_Attribute();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
		 * <em>Repository Method Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getRepositoryMethodTypeEnumeration()
		 * @generated
		 */
		EEnum REPOSITORY_METHOD_TYPE_ENUMERATION = eINSTANCE.getRepositoryMethodTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.repository.PermissionModeEnumeration <em>Permission
		 * Mode Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.repository.PermissionModeEnumeration
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getPermissionModeEnumeration()
		 * @generated
		 */
		EEnum PERMISSION_MODE_ENUMERATION = eINSTANCE.getPermissionModeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration <em>Transaction
		 * Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration
		 * @see net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl#getTransactionTypeEnumeration()
		 * @generated
		 */
		EEnum TRANSACTION_TYPE_ENUMERATION = eINSTANCE.getTransactionTypeEnumeration();

	}

}
