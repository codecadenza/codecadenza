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
package net.codecadenza.eclipse.model.integration;

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
 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory
 * @model kind="package"
 * @generated
 */
public interface IntegrationPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "integration";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/integration.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.integration";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	IntegrationPackage eINSTANCE = net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl <em>Abstract
	 * Integration Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getAbstractIntegrationBean()
	 * @generated
	 */
	int ABSTRACT_INTEGRATION_BEAN = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__NAME = ServicePackage.SERVICE_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__COMMENT = ServicePackage.SERVICE_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__MAPPABLE = ServicePackage.SERVICE_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__PRIMITIVE = ServicePackage.SERVICE_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__NAMESPACE = ServicePackage.SERVICE_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__DOMAIN_OBJECT = ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__INTERFACE_NAME = ServicePackage.SERVICE_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__METHODS = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Client Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Producer Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Abstract Integration Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl <em>SOAP
	 * Integration Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getSOAPIntegrationBean()
	 * @generated
	 */
	int SOAP_INTEGRATION_BEAN = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__NAME = ABSTRACT_INTEGRATION_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__COMMENT = ABSTRACT_INTEGRATION_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__MAPPABLE = ABSTRACT_INTEGRATION_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__PRIMITIVE = ABSTRACT_INTEGRATION_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__NAMESPACE = ABSTRACT_INTEGRATION_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__DOMAIN_OBJECT = ABSTRACT_INTEGRATION_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__INTERFACE_NAME = ABSTRACT_INTEGRATION_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__METHODS = ABSTRACT_INTEGRATION_BEAN__METHODS;

	/**
	 * The feature ID for the '<em><b>Client Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__CLIENT_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME;

	/**
	 * The feature ID for the '<em><b>Producer Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME;

	/**
	 * The feature ID for the '<em><b>Service Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__SERVICE_NAME = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Port Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__PORT_NAME = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Rpc Stype</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__RPC_STYPE = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Bare Parameter Style</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Port Type Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>SOAP Integration Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_BEAN_FEATURE_COUNT = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationBeanImpl <em>REST
	 * Integration Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.RESTIntegrationBeanImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRESTIntegrationBean()
	 * @generated
	 */
	int REST_INTEGRATION_BEAN = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__NAME = ABSTRACT_INTEGRATION_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__COMMENT = ABSTRACT_INTEGRATION_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__MAPPABLE = ABSTRACT_INTEGRATION_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__PRIMITIVE = ABSTRACT_INTEGRATION_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__NAMESPACE = ABSTRACT_INTEGRATION_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__DOMAIN_OBJECT = ABSTRACT_INTEGRATION_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__INTERFACE_NAME = ABSTRACT_INTEGRATION_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__METHODS = ABSTRACT_INTEGRATION_BEAN__METHODS;

	/**
	 * The feature ID for the '<em><b>Client Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__CLIENT_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME;

	/**
	 * The feature ID for the '<em><b>Producer Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME;

	/**
	 * The feature ID for the '<em><b>Path</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN__PATH = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>REST Integration Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_BEAN_FEATURE_COUNT = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl <em>Abstract
	 * Integration Method</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getAbstractIntegrationMethod()
	 * @generated
	 */
	int ABSTRACT_INTEGRATION_METHOD = 5;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__NAME = ServicePackage.SERVICE_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__COMMENT = ServicePackage.SERVICE_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__JAVA_TYPE = ServicePackage.SERVICE_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE = ServicePackage.SERVICE_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER = ServicePackage.SERVICE_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__METHOD_PARAMETERS = ServicePackage.SERVICE_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__QUERY_STATEMENT = ServicePackage.SERVICE_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__PERMISSION_MODE = ServicePackage.SERVICE_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__TRANSACTION_TYPE = ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__ROLES = ServicePackage.SERVICE_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__METHOD_INVOCATION = ServicePackage.SERVICE_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__CUSTOM_STATEMENT = ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Integration Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Start New Thread</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Abstract Integration Method</em>' class
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl <em>SOAP
	 * Integration Method</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getSOAPIntegrationMethod()
	 * @generated
	 */
	int SOAP_INTEGRATION_METHOD = 3;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__NAME = ABSTRACT_INTEGRATION_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__COMMENT = ABSTRACT_INTEGRATION_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__JAVA_TYPE = ABSTRACT_INTEGRATION_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__RETURN_TYPE = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__METHOD_PARAMETERS = ABSTRACT_INTEGRATION_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__QUERY_STATEMENT = ABSTRACT_INTEGRATION_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__PERMISSION_MODE = ABSTRACT_INTEGRATION_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__TRANSACTION_TYPE = ABSTRACT_INTEGRATION_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__ROLES = ABSTRACT_INTEGRATION_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__METHOD_INVOCATION = ABSTRACT_INTEGRATION_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__CUSTOM_STATEMENT = ABSTRACT_INTEGRATION_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Integration Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__INTEGRATION_BEAN = ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__BOUNDARY_METHOD = ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD;

	/**
	 * The feature ID for the '<em><b>Start New Thread</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__START_NEW_THREAD = ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD;

	/**
	 * The feature ID for the '<em><b>Operation Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__OPERATION_NAME = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Add Parameter Annotations</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Return Value Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Return Value Part Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>SOAP Integration Method</em>' class
	 * @generated
	 * @ordered
	 */
	int SOAP_INTEGRATION_METHOD_FEATURE_COUNT = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl <em>REST
	 * Integration Method</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRESTIntegrationMethod()
	 * @generated
	 */
	int REST_INTEGRATION_METHOD = 4;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__NAME = ABSTRACT_INTEGRATION_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__COMMENT = ABSTRACT_INTEGRATION_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__JAVA_TYPE = ABSTRACT_INTEGRATION_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__RETURN_TYPE = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__METHOD_PARAMETERS = ABSTRACT_INTEGRATION_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__QUERY_STATEMENT = ABSTRACT_INTEGRATION_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__PERMISSION_MODE = ABSTRACT_INTEGRATION_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__TRANSACTION_TYPE = ABSTRACT_INTEGRATION_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__ROLES = ABSTRACT_INTEGRATION_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__METHOD_INVOCATION = ABSTRACT_INTEGRATION_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__CUSTOM_STATEMENT = ABSTRACT_INTEGRATION_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Integration Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__INTEGRATION_BEAN = ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__BOUNDARY_METHOD = ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD;

	/**
	 * The feature ID for the '<em><b>Start New Thread</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__START_NEW_THREAD = ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD;

	/**
	 * The feature ID for the '<em><b>Path</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__PATH = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Http Method</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__HTTP_METHOD = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Input Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__INPUT_TYPE = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Output Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD__OUTPUT_TYPE = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>REST Integration Method</em>' class
	 * @generated
	 * @ordered
	 */
	int REST_INTEGRATION_METHOD_FEATURE_COUNT = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.RMIIntegrationMethodImpl <em>RMI
	 * Integration Method</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.RMIIntegrationMethodImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRMIIntegrationMethod()
	 * @generated
	 */
	int RMI_INTEGRATION_METHOD = 6;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__NAME = ABSTRACT_INTEGRATION_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__COMMENT = ABSTRACT_INTEGRATION_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__JAVA_TYPE = ABSTRACT_INTEGRATION_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__RETURN_TYPE = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__METHOD_PARAMETERS = ABSTRACT_INTEGRATION_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__QUERY_STATEMENT = ABSTRACT_INTEGRATION_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__PERMISSION_MODE = ABSTRACT_INTEGRATION_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__TRANSACTION_TYPE = ABSTRACT_INTEGRATION_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__ROLES = ABSTRACT_INTEGRATION_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__METHOD_INVOCATION = ABSTRACT_INTEGRATION_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__CUSTOM_STATEMENT = ABSTRACT_INTEGRATION_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Integration Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__INTEGRATION_BEAN = ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__BOUNDARY_METHOD = ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD;

	/**
	 * The feature ID for the '<em><b>Start New Thread</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD__START_NEW_THREAD = ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD;

	/**
	 * The number of structural features of the '<em>RMI Integration Method</em>' class
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_METHOD_FEATURE_COUNT = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 0;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.impl.RMIIntegrationBeanImpl <em>RMI Integration
	 * Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.RMIIntegrationBeanImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRMIIntegrationBean()
	 * @generated
	 */
	int RMI_INTEGRATION_BEAN = 7;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__NAME = ABSTRACT_INTEGRATION_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__COMMENT = ABSTRACT_INTEGRATION_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__MAPPABLE = ABSTRACT_INTEGRATION_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__PRIMITIVE = ABSTRACT_INTEGRATION_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__NAMESPACE = ABSTRACT_INTEGRATION_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__DOMAIN_OBJECT = ABSTRACT_INTEGRATION_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__INTERFACE_NAME = ABSTRACT_INTEGRATION_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__METHODS = ABSTRACT_INTEGRATION_BEAN__METHODS;

	/**
	 * The feature ID for the '<em><b>Client Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__CLIENT_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME;

	/**
	 * The feature ID for the '<em><b>Producer Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME;

	/**
	 * The number of structural features of the '<em>RMI Integration Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int RMI_INTEGRATION_BEAN_FEATURE_COUNT = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl <em>Kafka
	 * Integration Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getKafkaIntegrationBean()
	 * @generated
	 */
	int KAFKA_INTEGRATION_BEAN = 8;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__NAME = ABSTRACT_INTEGRATION_BEAN__NAME;

	/**
	 * The feature id for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__COMMENT = ABSTRACT_INTEGRATION_BEAN__COMMENT;

	/**
	 * The feature id for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__MAPPABLE = ABSTRACT_INTEGRATION_BEAN__MAPPABLE;

	/**
	 * The feature id for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__PRIMITIVE = ABSTRACT_INTEGRATION_BEAN__PRIMITIVE;

	/**
	 * The feature id for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__NAMESPACE = ABSTRACT_INTEGRATION_BEAN__NAMESPACE;

	/**
	 * The feature id for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__DOMAIN_OBJECT = ABSTRACT_INTEGRATION_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature id for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__INTERFACE_NAME = ABSTRACT_INTEGRATION_BEAN__INTERFACE_NAME;

	/**
	 * The feature id for the '<em><b>Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__METHODS = ABSTRACT_INTEGRATION_BEAN__METHODS;

	/**
	 * The feature id for the '<em><b>Client Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__CLIENT_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME;

	/**
	 * The feature id for the '<em><b>Producer Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME;

	/**
	 * The feature id for the '<em><b>Request Topic</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Response Topic</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Consumer Group</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Kafka Integration Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_BEAN_FEATURE_COUNT = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl <em>Kafka
	 * Integration Method</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getKafkaIntegrationMethod()
	 * @generated
	 */
	int KAFKA_INTEGRATION_METHOD = 9;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__NAME = ABSTRACT_INTEGRATION_METHOD__NAME;

	/**
	 * The feature id for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__COMMENT = ABSTRACT_INTEGRATION_METHOD__COMMENT;

	/**
	 * The feature id for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__JAVA_TYPE = ABSTRACT_INTEGRATION_METHOD__JAVA_TYPE;

	/**
	 * The feature id for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__RETURN_TYPE = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE;

	/**
	 * The feature id for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature id for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__METHOD_PARAMETERS = ABSTRACT_INTEGRATION_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature id for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__QUERY_STATEMENT = ABSTRACT_INTEGRATION_METHOD__QUERY_STATEMENT;

	/**
	 * The feature id for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__PERMISSION_MODE = ABSTRACT_INTEGRATION_METHOD__PERMISSION_MODE;

	/**
	 * The feature id for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__TRANSACTION_TYPE = ABSTRACT_INTEGRATION_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature id for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__ROLES = ABSTRACT_INTEGRATION_METHOD__ROLES;

	/**
	 * The feature id for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__METHOD_INVOCATION = ABSTRACT_INTEGRATION_METHOD__METHOD_INVOCATION;

	/**
	 * The feature id for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__CUSTOM_STATEMENT = ABSTRACT_INTEGRATION_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature id for the '<em><b>Integration Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__INTEGRATION_BEAN = ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN;

	/**
	 * The feature id for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__BOUNDARY_METHOD = ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD;

	/**
	 * The feature id for the '<em><b>Start New Thread</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__START_NEW_THREAD = ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD;

	/**
	 * The feature id for the '<em><b>Request Schema Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Response Schema Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Use Dedicated Partition</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Send Response</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD__SEND_RESPONSE = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>Kafka Integration Method</em>' class
	 * @generated
	 * @ordered
	 */
	int KAFKA_INTEGRATION_METHOD_FEATURE_COUNT = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 4;

	/**
	 * The meta object id for the '{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationMethodImpl <em>JMS
	 * Integration Method</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.JMSIntegrationMethodImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getJMSIntegrationMethod()
	 * @generated
	 */
	int JMS_INTEGRATION_METHOD = 10;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__NAME = ABSTRACT_INTEGRATION_METHOD__NAME;

	/**
	 * The feature id for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__COMMENT = ABSTRACT_INTEGRATION_METHOD__COMMENT;

	/**
	 * The feature id for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER = ABSTRACT_INTEGRATION_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature id for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__QUERY_STATEMENT = ABSTRACT_INTEGRATION_METHOD__QUERY_STATEMENT;

	/**
	 * The feature id for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__PERMISSION_MODE = ABSTRACT_INTEGRATION_METHOD__PERMISSION_MODE;

	/**
	 * The feature id for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__TRANSACTION_TYPE = ABSTRACT_INTEGRATION_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature id for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__ROLES = ABSTRACT_INTEGRATION_METHOD__ROLES;

	/**
	 * The feature id for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__METHOD_INVOCATION = ABSTRACT_INTEGRATION_METHOD__METHOD_INVOCATION;

	/**
	 * The feature id for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__CUSTOM_STATEMENT = ABSTRACT_INTEGRATION_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature id for the '<em><b>Integration Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__INTEGRATION_BEAN = ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN;

	/**
	 * The feature id for the '<em><b>Start New Thread</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__START_NEW_THREAD = ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD;

	/**
	 * The feature id for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__BOUNDARY_METHOD = ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD;

	/**
	 * The feature id for the '<em><b>Operation ID</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__OPERATION_ID = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Send Response</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD__SEND_RESPONSE = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>JMS Integration Method</em>' class
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_METHOD_FEATURE_COUNT = ABSTRACT_INTEGRATION_METHOD_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationBeanImpl <em>JMS Integration
	 * Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.integration.impl.JMSIntegrationBeanImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getJMSIntegrationBean()
	 * @generated
	 */
	int JMS_INTEGRATION_BEAN = 11;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__NAME = ABSTRACT_INTEGRATION_BEAN__NAME;

	/**
	 * The feature id for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__COMMENT = ABSTRACT_INTEGRATION_BEAN__COMMENT;

	/**
	 * The feature id for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__MAPPABLE = ABSTRACT_INTEGRATION_BEAN__MAPPABLE;

	/**
	 * The feature id for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__PRIMITIVE = ABSTRACT_INTEGRATION_BEAN__PRIMITIVE;

	/**
	 * The feature id for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__NAMESPACE = ABSTRACT_INTEGRATION_BEAN__NAMESPACE;

	/**
	 * The feature id for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__DOMAIN_OBJECT = ABSTRACT_INTEGRATION_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature id for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__INTERFACE_NAME = ABSTRACT_INTEGRATION_BEAN__INTERFACE_NAME;

	/**
	 * The feature id for the '<em><b>Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__METHODS = ABSTRACT_INTEGRATION_BEAN__METHODS;

	/**
	 * The feature id for the '<em><b>Client Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__CLIENT_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME;

	/**
	 * The feature id for the '<em><b>Producer Class Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME;

	/**
	 * The feature id for the '<em><b>Request Destination</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__REQUEST_DESTINATION = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Response Destination</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>JMS Integration Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int JMS_INTEGRATION_BEAN_FEATURE_COUNT = ABSTRACT_INTEGRATION_BEAN_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link net.codecadenza.eclipse.model.integration.impl.JMSResourceImpl <em>JMS Resource</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.integration.impl.JMSResourceImpl
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getJMSResource()
	 * @generated
	 */
	int JMS_RESOURCE = 12;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_RESOURCE__NAME = 0;

	/**
	 * The feature id for the '<em><b>Topic</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JMS_RESOURCE__TOPIC = 1;

	/**
	 * The number of structural features of the '<em>JMS Resource</em>' class
	 * @generated
	 * @ordered
	 */
	int JMS_RESOURCE_FEATURE_COUNT = 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.MediaTypeEnumeration <em>Media Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getMediaTypeEnumeration()
	 * @generated
	 */
	int MEDIA_TYPE_ENUMERATION = 10;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.integration.HttpMethodEnumeration <em>Http Method
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.integration.HttpMethodEnumeration
	 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getHttpMethodEnumeration()
	 * @generated
	 */
	int HTTP_METHOD_ENUMERATION = 11;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean <em>SOAP Integration
	 * Bean</em>}'
	 * @return the meta object for class '<em>SOAP Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean
	 * @generated
	 */
	EClass getSOAPIntegrationBean();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getServiceName
	 * <em>Service Name</em>}'
	 * @return the meta object for the attribute '<em>Service Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getServiceName()
	 * @see #getSOAPIntegrationBean()
	 * @generated
	 */
	EAttribute getSOAPIntegrationBean_ServiceName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortName
	 * <em>Port Name</em>}'
	 * @return the meta object for the attribute '<em>Port Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortName()
	 * @see #getSOAPIntegrationBean()
	 * @generated
	 */
	EAttribute getSOAPIntegrationBean_PortName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isRpcStype
	 * <em>Rpc Stype</em>}'
	 * @return the meta object for the attribute '<em>Rpc Stype</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isRpcStype()
	 * @see #getSOAPIntegrationBean()
	 * @generated
	 */
	EAttribute getSOAPIntegrationBean_RpcStype();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isBareParameterStyle <em>Bare Parameter Style</em>}'
	 * @return the meta object for the attribute '<em>Bare Parameter Style</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isBareParameterStyle()
	 * @see #getSOAPIntegrationBean()
	 * @generated
	 */
	EAttribute getSOAPIntegrationBean_BareParameterStyle();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortTypeName <em>Port Type Name</em>}'
	 * @return the meta object for the attribute '<em>Port Type Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortTypeName()
	 * @see #getSOAPIntegrationBean()
	 * @generated
	 */
	EAttribute getSOAPIntegrationBean_PortTypeName();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationBean <em>REST Integration
	 * Bean</em>}'
	 * @return the meta object for class '<em>REST Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean
	 * @generated
	 */
	EClass getRESTIntegrationBean();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPath
	 * <em>Path</em>}'
	 * @return the meta object for the attribute '<em>Path</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPath()
	 * @see #getRESTIntegrationBean()
	 * @generated
	 */
	EAttribute getRESTIntegrationBean_Path();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean <em>Abstract
	 * Integration Bean</em>}'
	 * @return the meta object for class '<em>Abstract Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean
	 * @generated
	 */
	EClass getAbstractIntegrationBean();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getMethods <em>Methods</em>}'
	 * @return the meta object for the containment reference list '<em>Methods</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getMethods()
	 * @see #getAbstractIntegrationBean()
	 * @generated
	 */
	EReference getAbstractIntegrationBean_Methods();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getClientClassName <em>Client Class Name</em>}'
	 * @return the meta object for the attribute '<em>Client Class Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getClientClassName()
	 * @see #getAbstractIntegrationBean()
	 * @generated
	 */
	EAttribute getAbstractIntegrationBean_ClientClassName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getProducerClassName <em>Producer Class Name</em>}'
	 * @return the meta object for the attribute '<em>Producer Class Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getProducerClassName()
	 * @see #getAbstractIntegrationBean()
	 * @generated
	 */
	EAttribute getAbstractIntegrationBean_ProducerClassName();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod <em>SOAP Integration
	 * Method</em>}'
	 * @return the meta object for class '<em>SOAP Integration Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod
	 * @generated
	 */
	EClass getSOAPIntegrationMethod();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getOperationName <em>Operation Name</em>}'
	 * @return the meta object for the attribute '<em>Operation Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getOperationName()
	 * @see #getSOAPIntegrationMethod()
	 * @generated
	 */
	EAttribute getSOAPIntegrationMethod_OperationName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#isAddParameterAnnotations <em>Add Parameter
	 * Annotations</em>}'
	 * @return the meta object for the attribute '<em>Add Parameter Annotations</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#isAddParameterAnnotations()
	 * @see #getSOAPIntegrationMethod()
	 * @generated
	 */
	EAttribute getSOAPIntegrationMethod_AddParameterAnnotations();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValueName <em>Return Value Name</em>}'
	 * @return the meta object for the attribute '<em>Return Value Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValueName()
	 * @see #getSOAPIntegrationMethod()
	 * @generated
	 */
	EAttribute getSOAPIntegrationMethod_ReturnValueName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValuePartName <em>Return Value Part
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>Return Value Part Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValuePartName()
	 * @see #getSOAPIntegrationMethod()
	 * @generated
	 */
	EAttribute getSOAPIntegrationMethod_ReturnValuePartName();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod <em>REST Integration
	 * Method</em>}'
	 * @return the meta object for class '<em>REST Integration Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod
	 * @generated
	 */
	EClass getRESTIntegrationMethod();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getPath
	 * <em>Path</em>}'
	 * @return the meta object for the attribute '<em>Path</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getPath()
	 * @see #getRESTIntegrationMethod()
	 * @generated
	 */
	EAttribute getRESTIntegrationMethod_Path();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getHttpMethod <em>Http Method</em>}'
	 * @return the meta object for the attribute '<em>Http Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getHttpMethod()
	 * @see #getRESTIntegrationMethod()
	 * @generated
	 */
	EAttribute getRESTIntegrationMethod_HttpMethod();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getInputType
	 * <em>Input Type</em>}'
	 * @return the meta object for the attribute '<em>Input Type</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getInputType()
	 * @see #getRESTIntegrationMethod()
	 * @generated
	 */
	EAttribute getRESTIntegrationMethod_InputType();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getOutputType <em>Output Type</em>}'
	 * @return the meta object for the attribute '<em>Output Type</em>'
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getOutputType()
	 * @see #getRESTIntegrationMethod()
	 * @generated
	 */
	EAttribute getRESTIntegrationMethod_OutputType();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod <em>Abstract
	 * Integration Method</em>}'
	 * @return the meta object for class '<em>Abstract Integration Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod
	 * @generated
	 */
	EClass getAbstractIntegrationMethod();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean <em>Integration Bean</em>}'
	 * @return the meta object for the container reference '<em>Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean()
	 * @see #getAbstractIntegrationMethod()
	 * @generated
	 */
	EReference getAbstractIntegrationMethod_IntegrationBean();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getBoundaryMethod <em>Boundary Method</em>}'
	 * @return the meta object for the reference '<em>Boundary Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getBoundaryMethod()
	 * @see #getAbstractIntegrationMethod()
	 * @generated
	 */
	EReference getAbstractIntegrationMethod_BoundaryMethod();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#isStartNewThread <em>Start New Thread</em>}'
	 * @return the meta object for the attribute '<em>Start New Thread</em>'
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#isStartNewThread()
	 * @see #getAbstractIntegrationMethod()
	 * @generated
	 */
	EAttribute getAbstractIntegrationMethod_StartNewThread();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.RMIIntegrationMethod <em>RMI Integration
	 * Method</em>}'
	 * @return the meta object for class '<em>RMI Integration Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.RMIIntegrationMethod
	 * @generated
	 */
	EClass getRMIIntegrationMethod();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.RMIIntegrationBean <em>RMI Integration
	 * Bean</em>}'
	 * @return the meta object for class '<em>RMI Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.RMIIntegrationBean
	 * @generated
	 */
	EClass getRMIIntegrationBean();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean <em>Kafka Integration
	 * Bean</em>}'
	 * @return the meta object for class '<em>Kafka Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean
	 * @generated
	 */
	EClass getKafkaIntegrationBean();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getRequestTopic <em>Request Topic</em>}'
	 * @return the meta object for the attribute '<em>Request Topic</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getRequestTopic()
	 * @see #getKafkaIntegrationBean()
	 * @generated
	 */
	EAttribute getKafkaIntegrationBean_RequestTopic();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getResponseTopic <em>Response Topic</em>}'
	 * @return the meta object for the attribute '<em>Response Topic</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getResponseTopic()
	 * @see #getKafkaIntegrationBean()
	 * @generated
	 */
	EAttribute getKafkaIntegrationBean_ResponseTopic();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getConsumerGroup <em>Consumer Group</em>}'
	 * @return the meta object for the attribute '<em>Consumer Group</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getConsumerGroup()
	 * @see #getKafkaIntegrationBean()
	 * @generated
	 */
	EAttribute getKafkaIntegrationBean_ConsumerGroup();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod <em>Kafka
	 * Integration Method</em>}'
	 * @return the meta object for class '<em>Kafka Integration Method</em>'.
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod
	 * @generated
	 */
	EClass getKafkaIntegrationMethod();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getRequestSchemaName <em>Request Schema Name</em>}'
	 * @return the meta object for the attribute '<em>Request Schema Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getRequestSchemaName()
	 * @see #getKafkaIntegrationMethod()
	 * @generated
	 */
	EAttribute getKafkaIntegrationMethod_RequestSchemaName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getResponseSchemaName <em>Response Schema
	 * Name</em>}'
	 * @return the meta object for the attribute '<em>Response Schema Name</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getResponseSchemaName()
	 * @see #getKafkaIntegrationMethod()
	 * @generated
	 */
	EAttribute getKafkaIntegrationMethod_ResponseSchemaName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isUseDedicatedPartition() <em>Use Dedicated
	 * Partition</em>}'
	 * @return the meta object for the attribute '<em>Use Dedicated Partition</em>'
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isUseDedicatedPartition()
	 * @see #getKafkaIntegrationMethod()
	 * @generated
	 */
	EAttribute getKafkaIntegrationMethod_UseDedicatedPartition();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isSendResponse <em>Send Response</em>}'
	 * @return the meta object for the attribute '<em>Send Response</em>'.
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isSendResponse()
	 * @see #getKafkaIntegrationMethod()
	 * @generated
	 */
	EAttribute getKafkaIntegrationMethod_SendResponse();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod <em>JMS Integration
	 * Method</em>}'
	 * @return the meta object for class '<em>JMS Integration Method</em>'
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod
	 * @generated
	 */
	EClass getJMSIntegrationMethod();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#getOperationID <em>Operation ID</em>}'
	 * @return the meta object for the attribute '<em>Operation ID</em>'
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#getOperationID()
	 * @see #getJMSIntegrationMethod()
	 * @generated
	 */
	EAttribute getJMSIntegrationMethod_OperationID();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#isSendResponse <em>Send Response</em>}'
	 * @return the meta object for the attribute '<em>Send Response</em>'
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#isSendResponse()
	 * @see #getJMSIntegrationMethod()
	 * @generated
	 */
	EAttribute getJMSIntegrationMethod_SendResponse();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean <em>JMS Integration
	 * Bean</em>}'
	 * @return the meta object for class '<em>JMS Integration Bean</em>'
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean
	 * @generated
	 */
	EClass getJMSIntegrationBean();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getRequestDestination <em>Request Destination</em>}'
	 * @return the meta object for the containment reference '<em>Request Destination</em>'.
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getRequestDestination()
	 * @see #getJMSIntegrationBean()
	 * @generated
	 */
	EReference getJMSIntegrationBean_RequestDestination();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getResponseDestination <em>Response Destination</em>}'
	 * @return the meta object for the containment reference '<em>Response Destination</em>'.
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getResponseDestination()
	 * @see #getJMSIntegrationBean()
	 * @generated
	 */
	EReference getJMSIntegrationBean_ResponseDestination();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.integration.JMSResource <em>JMS Resource</em>}'
	 * @return the meta object for class '<em>JMS Resource</em>'.
	 * @see net.codecadenza.eclipse.model.integration.JMSResource
	 * @generated
	 */
	EClass getJMSResource();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.JMSResource#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see net.codecadenza.eclipse.model.integration.JMSResource#getName()
	 * @see #getJMSResource()
	 * @generated
	 */
	EAttribute getJMSResource_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.integration.JMSResource#isTopic
	 * <em>Topic</em>}'
	 * @return the meta object for the attribute '<em>Topic</em>'.
	 * @see net.codecadenza.eclipse.model.integration.JMSResource#isTopic()
	 * @see #getJMSResource()
	 * @generated
	 */
	EAttribute getJMSResource_Topic();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.integration.MediaTypeEnumeration <em>Media Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Media Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
	 * @generated
	 */
	EEnum getMediaTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.integration.HttpMethodEnumeration <em>Http Method
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Http Method Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.integration.HttpMethodEnumeration
	 * @generated
	 */
	EEnum getHttpMethodEnumeration();

	/**
	 * Return the factory that creates the instances of the model
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	IntegrationFactory getIntegrationFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl <em>SOAP
		 * Integration Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getSOAPIntegrationBean()
		 * @generated
		 */
		EClass SOAP_INTEGRATION_BEAN = eINSTANCE.getSOAPIntegrationBean();

		/**
		 * The meta object literal for the '<em><b>Service Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_BEAN__SERVICE_NAME = eINSTANCE.getSOAPIntegrationBean_ServiceName();

		/**
		 * The meta object literal for the '<em><b>Port Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_BEAN__PORT_NAME = eINSTANCE.getSOAPIntegrationBean_PortName();

		/**
		 * The meta object literal for the '<em><b>Rpc Stype</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_BEAN__RPC_STYPE = eINSTANCE.getSOAPIntegrationBean_RpcStype();

		/**
		 * The meta object literal for the '<em><b>Bare Parameter Style</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE = eINSTANCE.getSOAPIntegrationBean_BareParameterStyle();

		/**
		 * The meta object literal for the '<em><b>Port Type Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME = eINSTANCE.getSOAPIntegrationBean_PortTypeName();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationBeanImpl <em>REST
		 * Integration Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.RESTIntegrationBeanImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRESTIntegrationBean()
		 * @generated
		 */
		EClass REST_INTEGRATION_BEAN = eINSTANCE.getRESTIntegrationBean();

		/**
		 * The meta object literal for the '<em><b>Path</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REST_INTEGRATION_BEAN__PATH = eINSTANCE.getRESTIntegrationBean_Path();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl
		 * <em>Abstract Integration Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getAbstractIntegrationBean()
		 * @generated
		 */
		EClass ABSTRACT_INTEGRATION_BEAN = eINSTANCE.getAbstractIntegrationBean();

		/**
		 * The meta object literal for the '<em><b>Methods</b></em>' containment reference list feature
		 * @generated
		 */
		EReference ABSTRACT_INTEGRATION_BEAN__METHODS = eINSTANCE.getAbstractIntegrationBean_Methods();

		/**
		 * The meta object literal for the '<em><b>Client Class Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME = eINSTANCE.getAbstractIntegrationBean_ClientClassName();

		/**
		 * The meta object literal for the '<em><b>Producer Class Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME = eINSTANCE.getAbstractIntegrationBean_ProducerClassName();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl <em>SOAP
		 * Integration Method</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getSOAPIntegrationMethod()
		 * @generated
		 */
		EClass SOAP_INTEGRATION_METHOD = eINSTANCE.getSOAPIntegrationMethod();

		/**
		 * The meta object literal for the '<em><b>Operation Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_METHOD__OPERATION_NAME = eINSTANCE.getSOAPIntegrationMethod_OperationName();

		/**
		 * The meta object literal for the '<em><b>Add Parameter Annotations</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS = eINSTANCE.getSOAPIntegrationMethod_AddParameterAnnotations();

		/**
		 * The meta object literal for the '<em><b>Return Value Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME = eINSTANCE.getSOAPIntegrationMethod_ReturnValueName();

		/**
		 * The meta object literal for the '<em><b>Return Value Part Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME = eINSTANCE.getSOAPIntegrationMethod_ReturnValuePartName();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl <em>REST
		 * Integration Method</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRESTIntegrationMethod()
		 * @generated
		 */
		EClass REST_INTEGRATION_METHOD = eINSTANCE.getRESTIntegrationMethod();

		/**
		 * The meta object literal for the '<em><b>Path</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REST_INTEGRATION_METHOD__PATH = eINSTANCE.getRESTIntegrationMethod_Path();

		/**
		 * The meta object literal for the '<em><b>Http Method</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REST_INTEGRATION_METHOD__HTTP_METHOD = eINSTANCE.getRESTIntegrationMethod_HttpMethod();

		/**
		 * The meta object literal for the '<em><b>Input Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REST_INTEGRATION_METHOD__INPUT_TYPE = eINSTANCE.getRESTIntegrationMethod_InputType();

		/**
		 * The meta object literal for the '<em><b>Output Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute REST_INTEGRATION_METHOD__OUTPUT_TYPE = eINSTANCE.getRESTIntegrationMethod_OutputType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl
		 * <em>Abstract Integration Method</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getAbstractIntegrationMethod()
		 * @generated
		 */
		EClass ABSTRACT_INTEGRATION_METHOD = eINSTANCE.getAbstractIntegrationMethod();

		/**
		 * The meta object literal for the '<em><b>Integration Bean</b></em>' container reference feature
		 * @generated
		 */
		EReference ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN = eINSTANCE.getAbstractIntegrationMethod_IntegrationBean();

		/**
		 * The meta object literal for the '<em><b>Boundary Method</b></em>' reference feature
		 * @generated
		 */
		EReference ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD = eINSTANCE.getAbstractIntegrationMethod_BoundaryMethod();

		/**
		 * The meta object literal for the '<em><b>Start New Thread</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD = eINSTANCE.getAbstractIntegrationMethod_StartNewThread();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.RMIIntegrationMethodImpl <em>RMI
		 * Integration Method</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.RMIIntegrationMethodImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRMIIntegrationMethod()
		 * @generated
		 */
		EClass RMI_INTEGRATION_METHOD = eINSTANCE.getRMIIntegrationMethod();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.RMIIntegrationBeanImpl <em>RMI
		 * Integration Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.RMIIntegrationBeanImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getRMIIntegrationBean()
		 * @generated
		 */
		EClass RMI_INTEGRATION_BEAN = eINSTANCE.getRMIIntegrationBean();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl <em>Kafka
		 * Integration Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getKafkaIntegrationBean()
		 * @generated
		 */
		EClass KAFKA_INTEGRATION_BEAN = eINSTANCE.getKafkaIntegrationBean();

		/**
		 * The meta object literal for the '<em><b>Request Topic</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC = eINSTANCE.getKafkaIntegrationBean_RequestTopic();

		/**
		 * The meta object literal for the '<em><b>Response Topic</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC = eINSTANCE.getKafkaIntegrationBean_ResponseTopic();

		/**
		 * The meta object literal for the '<em><b>Consumer Group</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP = eINSTANCE.getKafkaIntegrationBean_ConsumerGroup();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl <em>Kafka
		 * Integration Method</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getKafkaIntegrationMethod()
		 * @generated
		 */
		EClass KAFKA_INTEGRATION_METHOD = eINSTANCE.getKafkaIntegrationMethod();

		/**
		 * The meta object literal for the '<em><b>Request Schema Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME = eINSTANCE.getKafkaIntegrationMethod_RequestSchemaName();

		/**
		 * The meta object literal for the '<em><b>Response Schema Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME = eINSTANCE.getKafkaIntegrationMethod_ResponseSchemaName();

		/**
		 * The meta object literal for the '<em><b>Use Dedicated Partition</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION = eINSTANCE.getKafkaIntegrationMethod_UseDedicatedPartition();

		/**
		 * The meta object literal for the '<em><b>Send Response</b></em>' attribute feature
		 * @generated
		 */
		EAttribute KAFKA_INTEGRATION_METHOD__SEND_RESPONSE = eINSTANCE.getKafkaIntegrationMethod_SendResponse();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationMethodImpl <em>JMS
		 * Integration Method</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.JMSIntegrationMethodImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getJMSIntegrationMethod()
		 * @generated
		 */
		EClass JMS_INTEGRATION_METHOD = eINSTANCE.getJMSIntegrationMethod();

		/**
		 * The meta object literal for the '<em><b>Operation ID</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JMS_INTEGRATION_METHOD__OPERATION_ID = eINSTANCE.getJMSIntegrationMethod_OperationID();

		/**
		 * The meta object literal for the '<em><b>Send Response</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JMS_INTEGRATION_METHOD__SEND_RESPONSE = eINSTANCE.getJMSIntegrationMethod_SendResponse();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationBeanImpl <em>JMS
		 * Integration Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.JMSIntegrationBeanImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getJMSIntegrationBean()
		 * @generated
		 */
		EClass JMS_INTEGRATION_BEAN = eINSTANCE.getJMSIntegrationBean();

		/**
		 * The meta object literal for the '<em><b>Request Destination</b></em>' containment reference feature
		 * @generated
		 */
		EReference JMS_INTEGRATION_BEAN__REQUEST_DESTINATION = eINSTANCE.getJMSIntegrationBean_RequestDestination();

		/**
		 * The meta object literal for the '<em><b>Response Destination</b></em>' containment reference feature
		 * @generated
		 */
		EReference JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION = eINSTANCE.getJMSIntegrationBean_ResponseDestination();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.impl.JMSResourceImpl <em>JMS
		 * Resource</em>}' class
		 * @see net.codecadenza.eclipse.model.integration.impl.JMSResourceImpl
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getJMSResource()
		 * @generated
		 */
		EClass JMS_RESOURCE = eINSTANCE.getJMSResource();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JMS_RESOURCE__NAME = eINSTANCE.getJMSResource_Name();

		/**
		 * The meta object literal for the '<em><b>Topic</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JMS_RESOURCE__TOPIC = eINSTANCE.getJMSResource_Topic();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.MediaTypeEnumeration <em>Media Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getMediaTypeEnumeration()
		 * @generated
		 */
		EEnum MEDIA_TYPE_ENUMERATION = eINSTANCE.getMediaTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.integration.HttpMethodEnumeration <em>Http Method
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.integration.HttpMethodEnumeration
		 * @see net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl#getHttpMethodEnumeration()
		 * @generated
		 */
		EEnum HTTP_METHOD_ENUMERATION = eINSTANCE.getHttpMethodEnumeration();
	}

}
