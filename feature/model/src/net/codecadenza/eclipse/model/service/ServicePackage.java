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
package net.codecadenza.eclipse.model.service;

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
 * @see net.codecadenza.eclipse.model.service.ServiceFactory
 * @model kind="package"
 * @generated
 */
public interface ServicePackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "service";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/service.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.service";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	ServicePackage eINSTANCE = net.codecadenza.eclipse.model.service.impl.ServicePackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl <em>Service Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl
	 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getServiceBean()
	 * @generated
	 */
	int SERVICE_BEAN = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__NAME = JavaPackage.JAVA_TYPE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__COMMENT = JavaPackage.JAVA_TYPE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__MAPPABLE = JavaPackage.JAVA_TYPE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__PRIMITIVE = JavaPackage.JAVA_TYPE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__NAMESPACE = JavaPackage.JAVA_TYPE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__DOMAIN_OBJECT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN__INTERFACE_NAME = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Service Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int SERVICE_BEAN_FEATURE_COUNT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl <em>Service Method</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl
	 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getServiceMethod()
	 * @generated
	 */
	int SERVICE_METHOD = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__NAME = JavaPackage.JAVA_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__COMMENT = JavaPackage.JAVA_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__JAVA_TYPE = JavaPackage.JAVA_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__RETURN_TYPE = JavaPackage.JAVA_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__RETURN_TYPE_MODIFIER = JavaPackage.JAVA_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__METHOD_PARAMETERS = JavaPackage.JAVA_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__QUERY_STATEMENT = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__PERMISSION_MODE = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__TRANSACTION_TYPE = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__ROLES = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__METHOD_INVOCATION = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD__CUSTOM_STATEMENT = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 5;

	/**
	 * The number of structural features of the '<em>Service Method</em>' class
	 * @generated
	 * @ordered
	 */
	int SERVICE_METHOD_FEATURE_COUNT = JavaPackage.JAVA_METHOD_FEATURE_COUNT + 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl <em>Method
	 * Invocation</em>}' class
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl
	 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getMethodInvocation()
	 * @generated
	 */
	int METHOD_INVOCATION = 2;

	/**
	 * The feature ID for the '<em><b>Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION__METHOD = 0;

	/**
	 * The number of structural features of the '<em>Method Invocation</em>' class
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION_FEATURE_COUNT = 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl <em>Scheduled
	 * Invocation</em>}' class
	 * @see net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl
	 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getScheduledInvocation()
	 * @generated
	 */
	int SCHEDULED_INVOCATION = 3;

	/**
	 * The feature ID for the '<em><b>Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__METHOD = METHOD_INVOCATION__METHOD;

	/**
	 * The feature ID for the '<em><b>Second</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__SECOND = METHOD_INVOCATION_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Minute</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__MINUTE = METHOD_INVOCATION_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Hour</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__HOUR = METHOD_INVOCATION_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Day Of Week</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__DAY_OF_WEEK = METHOD_INVOCATION_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Day Of Month</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__DAY_OF_MONTH = METHOD_INVOCATION_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Month</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__MONTH = METHOD_INVOCATION_FEATURE_COUNT + 5;

	/**
	 * The feature ID for the '<em><b>Year</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION__YEAR = METHOD_INVOCATION_FEATURE_COUNT + 6;

	/**
	 * The number of structural features of the '<em>Scheduled Invocation</em>' class
	 * @generated
	 * @ordered
	 */
	int SCHEDULED_INVOCATION_FEATURE_COUNT = METHOD_INVOCATION_FEATURE_COUNT + 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.service.impl.AsynchronousInvocationImpl <em>Asynchronous
	 * Invocation</em>}' class
	 * @see net.codecadenza.eclipse.model.service.impl.AsynchronousInvocationImpl
	 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getAsynchronousInvocation()
	 * @generated
	 */
	int ASYNCHRONOUS_INVOCATION = 4;

	/**
	 * The feature ID for the '<em><b>Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ASYNCHRONOUS_INVOCATION__METHOD = METHOD_INVOCATION__METHOD;

	/**
	 * The feature ID for the '<em><b>Delay In Milliseconds</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS = METHOD_INVOCATION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Asynchronous Invocation</em>' class
	 * @generated
	 * @ordered
	 */
	int ASYNCHRONOUS_INVOCATION_FEATURE_COUNT = METHOD_INVOCATION_FEATURE_COUNT + 1;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.service.ServiceBean <em>Service Bean</em>}'
	 * @return the meta object for class '<em>Service Bean</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceBean
	 * @generated
	 */
	EClass getServiceBean();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.service.ServiceBean#getDomainObject <em>Domain
	 * Object</em>}'
	 * @return the meta object for the reference '<em>Domain Object</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceBean#getDomainObject()
	 * @see #getServiceBean()
	 * @generated
	 */
	EReference getServiceBean_DomainObject();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ServiceBean#getInterfaceName
	 * <em>Interface Name</em>}'
	 * @return the meta object for the attribute '<em>Interface Name</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceBean#getInterfaceName()
	 * @see #getServiceBean()
	 * @generated
	 */
	EAttribute getServiceBean_InterfaceName();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.service.ServiceMethod <em>Service Method</em>}'
	 * @return the meta object for class '<em>Service Method</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod
	 * @generated
	 */
	EClass getServiceMethod();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getQueryStatement
	 * <em>Query Statement</em>}'
	 * @return the meta object for the attribute '<em>Query Statement</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getQueryStatement()
	 * @see #getServiceMethod()
	 * @generated
	 */
	EAttribute getServiceMethod_QueryStatement();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getPermissionMode
	 * <em>Permission Mode</em>}'
	 * @return the meta object for the attribute '<em>Permission Mode</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getPermissionMode()
	 * @see #getServiceMethod()
	 * @generated
	 */
	EAttribute getServiceMethod_PermissionMode();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getTransactionType
	 * <em>Transaction Type</em>}'
	 * @return the meta object for the attribute '<em>Transaction Type</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getTransactionType()
	 * @see #getServiceMethod()
	 * @generated
	 */
	EAttribute getServiceMethod_TransactionType();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getRoles
	 * <em>Roles</em>}'
	 * @return the meta object for the reference list '<em>Roles</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getRoles()
	 * @see #getServiceMethod()
	 * @generated
	 */
	EReference getServiceMethod_Roles();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation <em>Method Invocation</em>}'
	 * @return the meta object for the containment reference '<em>Method Invocation</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation()
	 * @see #getServiceMethod()
	 * @generated
	 */
	EReference getServiceMethod_MethodInvocation();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getCustomStatement
	 * <em>Custom Statement</em>}'
	 * @return the meta object for the attribute '<em>Custom Statement</em>'
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getCustomStatement()
	 * @see #getServiceMethod()
	 * @generated
	 */
	EAttribute getServiceMethod_CustomStatement();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.service.MethodInvocation <em>Method Invocation</em>}'
	 * @return the meta object for class '<em>Method Invocation</em>'
	 * @see net.codecadenza.eclipse.model.service.MethodInvocation
	 * @generated
	 */
	EClass getMethodInvocation();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.service.MethodInvocation#getMethod
	 * <em>Method</em>}'
	 * @return the meta object for the container reference '<em>Method</em>'
	 * @see net.codecadenza.eclipse.model.service.MethodInvocation#getMethod()
	 * @see #getMethodInvocation()
	 * @generated
	 */
	EReference getMethodInvocation_Method();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation <em>Scheduled
	 * Invocation</em>}'
	 * @return the meta object for class '<em>Scheduled Invocation</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation
	 * @generated
	 */
	EClass getScheduledInvocation();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getSecond
	 * <em>Second</em>}'
	 * @return the meta object for the attribute '<em>Second</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getSecond()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_Second();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getMinute
	 * <em>Minute</em>}'
	 * @return the meta object for the attribute '<em>Minute</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getMinute()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_Minute();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getHour
	 * <em>Hour</em>}'
	 * @return the meta object for the attribute '<em>Hour</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getHour()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_Hour();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfWeek
	 * <em>Day Of Week</em>}'
	 * @return the meta object for the attribute '<em>Day Of Week</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfWeek()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_DayOfWeek();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfMonth
	 * <em>Day Of Month</em>}'
	 * @return the meta object for the attribute '<em>Day Of Month</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfMonth()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_DayOfMonth();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getMonth
	 * <em>Month</em>}'
	 * @return the meta object for the attribute '<em>Month</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getMonth()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_Month();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.service.ScheduledInvocation#getYear
	 * <em>Year</em>}'
	 * @return the meta object for the attribute '<em>Year</em>'
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getYear()
	 * @see #getScheduledInvocation()
	 * @generated
	 */
	EAttribute getScheduledInvocation_Year();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.service.AsynchronousInvocation <em>Asynchronous
	 * Invocation</em>}'
	 * @return the meta object for class '<em>Asynchronous Invocation</em>'
	 * @see net.codecadenza.eclipse.model.service.AsynchronousInvocation
	 * @generated
	 */
	EClass getAsynchronousInvocation();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.service.AsynchronousInvocation#getDelayInMilliseconds <em>Delay In Milliseconds</em>}'
	 * @return the meta object for the attribute '<em>Delay In Milliseconds</em>'
	 * @see net.codecadenza.eclipse.model.service.AsynchronousInvocation#getDelayInMilliseconds()
	 * @see #getAsynchronousInvocation()
	 * @generated
	 */
	EAttribute getAsynchronousInvocation_DelayInMilliseconds();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	ServiceFactory getServiceFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl <em>Service Bean</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl
		 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getServiceBean()
		 * @generated
		 */
		EClass SERVICE_BEAN = eINSTANCE.getServiceBean();

		/**
		 * The meta object literal for the '<em><b>Domain Object</b></em>' reference feature
		 * @generated
		 */
		EReference SERVICE_BEAN__DOMAIN_OBJECT = eINSTANCE.getServiceBean_DomainObject();

		/**
		 * The meta object literal for the '<em><b>Interface Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SERVICE_BEAN__INTERFACE_NAME = eINSTANCE.getServiceBean_InterfaceName();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl <em>Service
		 * Method</em>}' class
		 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl
		 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getServiceMethod()
		 * @generated
		 */
		EClass SERVICE_METHOD = eINSTANCE.getServiceMethod();

		/**
		 * The meta object literal for the '<em><b>Query Statement</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SERVICE_METHOD__QUERY_STATEMENT = eINSTANCE.getServiceMethod_QueryStatement();

		/**
		 * The meta object literal for the '<em><b>Permission Mode</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SERVICE_METHOD__PERMISSION_MODE = eINSTANCE.getServiceMethod_PermissionMode();

		/**
		 * The meta object literal for the '<em><b>Transaction Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SERVICE_METHOD__TRANSACTION_TYPE = eINSTANCE.getServiceMethod_TransactionType();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' reference list feature
		 * @generated
		 */
		EReference SERVICE_METHOD__ROLES = eINSTANCE.getServiceMethod_Roles();

		/**
		 * The meta object literal for the '<em><b>Method Invocation</b></em>' containment reference feature
		 * @generated
		 */
		EReference SERVICE_METHOD__METHOD_INVOCATION = eINSTANCE.getServiceMethod_MethodInvocation();

		/**
		 * The meta object literal for the '<em><b>Custom Statement</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SERVICE_METHOD__CUSTOM_STATEMENT = eINSTANCE.getServiceMethod_CustomStatement();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl <em>Method
		 * Invocation</em>}' class
		 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl
		 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getMethodInvocation()
		 * @generated
		 */
		EClass METHOD_INVOCATION = eINSTANCE.getMethodInvocation();

		/**
		 * The meta object literal for the '<em><b>Method</b></em>' container reference feature
		 * @generated
		 */
		EReference METHOD_INVOCATION__METHOD = eINSTANCE.getMethodInvocation_Method();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl <em>Scheduled
		 * Invocation</em>}' class
		 * @see net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl
		 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getScheduledInvocation()
		 * @generated
		 */
		EClass SCHEDULED_INVOCATION = eINSTANCE.getScheduledInvocation();

		/**
		 * The meta object literal for the '<em><b>Second</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__SECOND = eINSTANCE.getScheduledInvocation_Second();

		/**
		 * The meta object literal for the '<em><b>Minute</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__MINUTE = eINSTANCE.getScheduledInvocation_Minute();

		/**
		 * The meta object literal for the '<em><b>Hour</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__HOUR = eINSTANCE.getScheduledInvocation_Hour();

		/**
		 * The meta object literal for the '<em><b>Day Of Week</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__DAY_OF_WEEK = eINSTANCE.getScheduledInvocation_DayOfWeek();

		/**
		 * The meta object literal for the '<em><b>Day Of Month</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__DAY_OF_MONTH = eINSTANCE.getScheduledInvocation_DayOfMonth();

		/**
		 * The meta object literal for the '<em><b>Month</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__MONTH = eINSTANCE.getScheduledInvocation_Month();

		/**
		 * The meta object literal for the '<em><b>Year</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SCHEDULED_INVOCATION__YEAR = eINSTANCE.getScheduledInvocation_Year();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.service.impl.AsynchronousInvocationImpl
		 * <em>Asynchronous Invocation</em>}' class
		 * @see net.codecadenza.eclipse.model.service.impl.AsynchronousInvocationImpl
		 * @see net.codecadenza.eclipse.model.service.impl.ServicePackageImpl#getAsynchronousInvocation()
		 * @generated
		 */
		EClass ASYNCHRONOUS_INVOCATION = eINSTANCE.getAsynchronousInvocation();

		/**
		 * The meta object literal for the '<em><b>Delay In Milliseconds</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS = eINSTANCE.getAsynchronousInvocation_DelayInMilliseconds();

	}

}
