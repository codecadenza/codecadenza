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
package net.codecadenza.eclipse.model.testing;

import net.codecadenza.eclipse.model.java.JavaPackage;
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
 * @see net.codecadenza.eclipse.model.testing.TestingFactory
 * @model kind="package"
 * @generated
 */
public interface TestingPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "testing";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/testing.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.testing";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	TestingPackage eINSTANCE = net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl <em>Abstract Test
	 * Module</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getAbstractTestModule()
	 * @generated
	 */
	int ABSTRACT_TEST_MODULE = 0;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_MODULE__NAMESPACE = 0;

	/**
	 * The feature ID for the '<em><b>Test Case Suffix</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX = 1;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_MODULE__PROJECT = 2;

	/**
	 * The number of structural features of the '<em>Abstract Test Module</em>' class
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_MODULE_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl <em>Selenium Test
	 * Module</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getSeleniumTestModule()
	 * @generated
	 */
	int SELENIUM_TEST_MODULE = 1;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__NAMESPACE = ABSTRACT_TEST_MODULE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Test Case Suffix</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__TEST_CASE_SUFFIX = ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__PROJECT = ABSTRACT_TEST_MODULE__PROJECT;

	/**
	 * The feature ID for the '<em><b>Driver</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__DRIVER = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Driver Path</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__DRIVER_PATH = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Maximize Window</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Implicit Wait Time</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Page Load Time</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE__PAGE_LOAD_TIME = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>Selenium Test Module</em>' class
	 * @generated
	 * @ordered
	 */
	int SELENIUM_TEST_MODULE_FEATURE_COUNT = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.TestSuiteImpl <em>Test Suite</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.TestSuiteImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getTestSuite()
	 * @generated
	 */
	int TEST_SUITE = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE__NAME = JavaPackage.JAVA_TYPE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE__COMMENT = JavaPackage.JAVA_TYPE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE__MAPPABLE = JavaPackage.JAVA_TYPE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE__PRIMITIVE = JavaPackage.JAVA_TYPE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE__NAMESPACE = JavaPackage.JAVA_TYPE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Test Cases</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE__TEST_CASES = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Test Suite</em>' class
	 * @generated
	 * @ordered
	 */
	int TEST_SUITE_FEATURE_COUNT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestCaseImpl <em>Abstract Test
	 * Case</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestCaseImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getAbstractTestCase()
	 * @generated
	 */
	int ABSTRACT_TEST_CASE = 3;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_CASE__NAME = JavaPackage.JAVA_TYPE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_CASE__COMMENT = JavaPackage.JAVA_TYPE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_CASE__MAPPABLE = JavaPackage.JAVA_TYPE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_CASE__PRIMITIVE = JavaPackage.JAVA_TYPE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_CASE__NAMESPACE = JavaPackage.JAVA_TYPE__NAMESPACE;

	/**
	 * The number of structural features of the '<em>Abstract Test Case</em>' class
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_TEST_CASE_FEATURE_COUNT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 0;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestCaseImpl <em>GUI Test Case</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.GUITestCaseImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestCase()
	 * @generated
	 */
	int GUI_TEST_CASE = 4;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE__NAME = ABSTRACT_TEST_CASE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE__COMMENT = ABSTRACT_TEST_CASE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE__MAPPABLE = ABSTRACT_TEST_CASE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE__PRIMITIVE = ABSTRACT_TEST_CASE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE__NAMESPACE = ABSTRACT_TEST_CASE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Test Actions</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE__TEST_ACTIONS = ABSTRACT_TEST_CASE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>GUI Test Case</em>' class
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_CASE_FEATURE_COUNT = ABSTRACT_TEST_CASE_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl <em>GUI Test Action</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestAction()
	 * @generated
	 */
	int GUI_TEST_ACTION = 5;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__COMMENT = 0;

	/**
	 * The feature ID for the '<em><b>Form</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__FORM = 1;

	/**
	 * The feature ID for the '<em><b>Target Form</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__TARGET_FORM = 2;

	/**
	 * The feature ID for the '<em><b>Form Action</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__FORM_ACTION = 3;

	/**
	 * The feature ID for the '<em><b>Form Panel</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__FORM_PANEL = 4;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__TYPE = 5;

	/**
	 * The feature ID for the '<em><b>Test Data</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__TEST_DATA = 6;

	/**
	 * The feature ID for the '<em><b>Action Result</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__ACTION_RESULT = 7;

	/**
	 * The feature ID for the '<em><b>Test Case</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__TEST_CASE = 8;

	/**
	 * The feature ID for the '<em><b>Delay Before</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__DELAY_BEFORE = 9;

	/**
	 * The feature ID for the '<em><b>Delay After</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION__DELAY_AFTER = 10;

	/**
	 * The number of structural features of the '<em>GUI Test Action</em>' class
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION_FEATURE_COUNT = 11;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl <em>GUI Test Action
	 * Result</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionResult()
	 * @generated
	 */
	int GUI_TEST_ACTION_RESULT = 6;

	/**
	 * The feature ID for the '<em><b>Message Text</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION_RESULT__MESSAGE_TEXT = 0;

	/**
	 * The feature ID for the '<em><b>Status</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION_RESULT__STATUS = 1;

	/**
	 * The feature ID for the '<em><b>Test Action</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION_RESULT__TEST_ACTION = 2;

	/**
	 * The feature ID for the '<em><b>Component Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION_RESULT__COMPONENT_TYPE = 3;

	/**
	 * The number of structural features of the '<em>GUI Test Action Result</em>' class
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_ACTION_RESULT_FEATURE_COUNT = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl <em>GUI Test Data</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestData()
	 * @generated
	 */
	int GUI_TEST_DATA = 7;

	/**
	 * The feature ID for the '<em><b>Form Field</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__FORM_FIELD = 0;

	/**
	 * The feature ID for the '<em><b>Table Column Field</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__TABLE_COLUMN_FIELD = 1;

	/**
	 * The feature ID for the '<em><b>New Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__NEW_VALUE = 2;

	/**
	 * The feature ID for the '<em><b>Expected Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__EXPECTED_VALUE = 3;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__TYPE = 4;

	/**
	 * The feature ID for the '<em><b>Test Action</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__TEST_ACTION = 5;

	/**
	 * The feature ID for the '<em><b>Filter Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA__FILTER_VALUE = 6;

	/**
	 * The number of structural features of the '<em>GUI Test Data</em>' class
	 * @generated
	 * @ordered
	 */
	int GUI_TEST_DATA_FEATURE_COUNT = 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl <em>Integration Test
	 * Module</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getIntegrationTestModule()
	 * @generated
	 */
	int INTEGRATION_TEST_MODULE = 8;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__NAMESPACE = ABSTRACT_TEST_MODULE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Test Case Suffix</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__TEST_CASE_SUFFIX = ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__PROJECT = ABSTRACT_TEST_MODULE__PROJECT;

	/**
	 * The feature ID for the '<em><b>Default Timeout</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Decimal Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__DECIMAL_FORMAT = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Date Time Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Integration Module</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__INTEGRATION_MODULE = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Date Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__DATE_FORMAT = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Decimal Separator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 5;

	/**
	 * The feature ID for the '<em><b>Grouping Separator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 6;

	/**
	 * The number of structural features of the '<em>Integration Test Module</em>' class
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_MODULE_FEATURE_COUNT = ABSTRACT_TEST_MODULE_FEATURE_COUNT + 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl <em>Integration Test
	 * Case</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getIntegrationTestCase()
	 * @generated
	 */
	int INTEGRATION_TEST_CASE = 9;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__NAME = ABSTRACT_TEST_CASE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__COMMENT = ABSTRACT_TEST_CASE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__MAPPABLE = ABSTRACT_TEST_CASE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__PRIMITIVE = ABSTRACT_TEST_CASE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__NAMESPACE = ABSTRACT_TEST_CASE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>User Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__USER_NAME = ABSTRACT_TEST_CASE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Password</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__PASSWORD = ABSTRACT_TEST_CASE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Method Invocations</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE__METHOD_INVOCATIONS = ABSTRACT_TEST_CASE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Integration Test Case</em>' class
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_TEST_CASE_FEATURE_COUNT = ABSTRACT_TEST_CASE_FEATURE_COUNT + 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl
	 * <em>Integration Method Test Invocation</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getIntegrationMethodTestInvocation()
	 * @generated
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION = 10;

	/**
	 * The feature ID for the '<em><b>Expect To Fail</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL = 0;

	/**
	 * The feature ID for the '<em><b>Post Processing Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT = 1;

	/**
	 * The feature ID for the '<em><b>Timeout</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT = 2;

	/**
	 * The feature ID for the '<em><b>Integration Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD = 3;

	/**
	 * The feature ID for the '<em><b>Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS = 4;

	/**
	 * The feature ID for the '<em><b>Return Values</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES = 5;

	/**
	 * The feature ID for the '<em><b>Post Processing Attributes</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES = 6;

	/**
	 * The feature ID for the '<em><b>Nested Invocations</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS = 7;

	/**
	 * The feature ID for the '<em><b>Parent Invocation</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION = 8;

	/**
	 * The feature ID for the '<em><b>Test Method Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME = 9;

	/**
	 * The feature ID for the '<em><b>Expected Size</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE = 10;

	/**
	 * The feature ID for the '<em><b>Expected Size Operator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR = 11;

	/**
	 * The feature ID for the '<em><b>Expected Return Null</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL = 12;

	/**
	 * The number of structural features of the '<em>Integration Method Test Invocation</em>' class
	 * @generated
	 * @ordered
	 */
	int INTEGRATION_METHOD_TEST_INVOCATION_FEATURE_COUNT = 13;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl <em>Method
	 * Invocation Parameter</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getMethodInvocationParameter()
	 * @generated
	 */
	int METHOD_INVOCATION_PARAMETER = 11;

	/**
	 * The feature ID for the '<em><b>Parameter Values</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION_PARAMETER__NAME = 1;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION_PARAMETER__TYPE = 2;

	/**
	 * The feature ID for the '<em><b>Represents List</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST = 3;

	/**
	 * The number of structural features of the '<em>Method Invocation Parameter</em>' class
	 * @generated
	 * @ordered
	 */
	int METHOD_INVOCATION_PARAMETER_FEATURE_COUNT = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl <em>Test Data Object</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getTestDataObject()
	 * @generated
	 */
	int TEST_DATA_OBJECT = 12;

	/**
	 * The feature ID for the '<em><b>Mapping Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_OBJECT__MAPPING_OBJECT = 0;

	/**
	 * The feature ID for the '<em><b>Attributes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_OBJECT__ATTRIBUTES = 1;

	/**
	 * The feature ID for the '<em><b>Referenced Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_OBJECT__REFERENCED_OBJECT = 2;

	/**
	 * The number of structural features of the '<em>Test Data Object</em>' class
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_OBJECT_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl <em>Test Data
	 * Attribute</em>}' class
	 * @see net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getTestDataAttribute()
	 * @generated
	 */
	int TEST_DATA_ATTRIBUTE = 13;

	/**
	 * The feature ID for the '<em><b>Mapping Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE = 0;

	/**
	 * The feature ID for the '<em><b>Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__VALUE = 1;

	/**
	 * The feature ID for the '<em><b>Track Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__TRACK_VALUE = 2;

	/**
	 * The feature ID for the '<em><b>Operator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__OPERATOR = 3;

	/**
	 * The feature ID for the '<em><b>Referenced Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE = 4;

	/**
	 * The feature ID for the '<em><b>Referenced Objects</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS = 5;

	/**
	 * The feature ID for the '<em><b>Mapping Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__MAPPING_TYPE = 6;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__NAME = 7;

	/**
	 * The feature ID for the '<em><b>Expected Size</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__EXPECTED_SIZE = 8;

	/**
	 * The feature ID for the '<em><b>Expected Size Operator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR = 9;

	/**
	 * The feature ID for the '<em><b>Id</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE__ID = 10;

	/**
	 * The number of structural features of the '<em>Test Data Attribute</em>' class
	 * @generated
	 * @ordered
	 */
	int TEST_DATA_ATTRIBUTE_FEATURE_COUNT = 11;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.GUITestActionStatus <em>GUI Test Action
	 * Status</em>}' enum
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionStatus
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionStatus()
	 * @generated
	 */
	int GUI_TEST_ACTION_STATUS = 14;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.GUITestActionType <em>GUI Test Action Type</em>}'
	 * enum
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionType
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionType()
	 * @generated
	 */
	int GUI_TEST_ACTION_TYPE = 15;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.GUITestDataType <em>GUI Test Data Type</em>}' enum
	 * @see net.codecadenza.eclipse.model.testing.GUITestDataType
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestDataType()
	 * @generated
	 */
	int GUI_TEST_DATA_TYPE = 16;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.SeleniumDriver <em>Selenium Driver</em>}' enum
	 * @see net.codecadenza.eclipse.model.testing.SeleniumDriver
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getSeleniumDriver()
	 * @generated
	 */
	int SELENIUM_DRIVER = 17;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType <em>GUI Test Action
	 * Result Component Type</em>}' enum
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionResultComponentType()
	 * @generated
	 */
	int GUI_TEST_ACTION_RESULT_COMPONENT_TYPE = 18;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.testing.AssertionOperator <em>Assertion Operator</em>}' enum
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getAssertionOperator()
	 * @generated
	 */
	int ASSERTION_OPERATOR = 19;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule <em>Abstract Test
	 * Module</em>}'
	 * @return the meta object for class '<em>Abstract Test Module</em>'
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule
	 * @generated
	 */
	EClass getAbstractTestModule();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getNamespace
	 * <em>Namespace</em>}'
	 * @return the meta object for the reference '<em>Namespace</em>'
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getNamespace()
	 * @see #getAbstractTestModule()
	 * @generated
	 */
	EReference getAbstractTestModule_Namespace();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestCaseSuffix
	 * <em>Test Case Suffix</em>}'
	 * @return the meta object for the attribute '<em>Test Case Suffix</em>'
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestCaseSuffix()
	 * @see #getAbstractTestModule()
	 * @generated
	 */
	EAttribute getAbstractTestModule_TestCaseSuffix();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject <em>Project</em>}'
	 * @return the meta object for the container reference '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject()
	 * @see #getAbstractTestModule()
	 * @generated
	 */
	EReference getAbstractTestModule_Project();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule <em>Selenium Test
	 * Module</em>}'
	 * @return the meta object for class '<em>Selenium Test Module</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule
	 * @generated
	 */
	EClass getSeleniumTestModule();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriver
	 * <em>Driver</em>}'
	 * @return the meta object for the attribute '<em>Driver</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriver()
	 * @see #getSeleniumTestModule()
	 * @generated
	 */
	EAttribute getSeleniumTestModule_Driver();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriverPath
	 * <em>Driver Path</em>}'
	 * @return the meta object for the attribute '<em>Driver Path</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriverPath()
	 * @see #getSeleniumTestModule()
	 * @generated
	 */
	EAttribute getSeleniumTestModule_DriverPath();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#isMaximizeWindow
	 * <em>Maximize Window</em>}'
	 * @return the meta object for the attribute '<em>Maximize Window</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#isMaximizeWindow()
	 * @see #getSeleniumTestModule()
	 * @generated
	 */
	EAttribute getSeleniumTestModule_MaximizeWindow();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getImplicitWaitTime
	 * <em>Implicit Wait Time</em>}'
	 * @return the meta object for the attribute '<em>Implicit Wait Time</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getImplicitWaitTime()
	 * @see #getSeleniumTestModule()
	 * @generated
	 */
	EAttribute getSeleniumTestModule_ImplicitWaitTime();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getPageLoadTime
	 * <em>Page Load Time</em>}'
	 * @return the meta object for the attribute '<em>Page Load Time</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getPageLoadTime()
	 * @see #getSeleniumTestModule()
	 * @generated
	 */
	EAttribute getSeleniumTestModule_PageLoadTime();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.TestSuite <em>Test Suite</em>}'
	 * @return the meta object for class '<em>Test Suite</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestSuite
	 * @generated
	 */
	EClass getTestSuite();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.testing.TestSuite#getTestCases <em>Test
	 * Cases</em>}'
	 * @return the meta object for the reference list '<em>Test Cases</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestSuite#getTestCases()
	 * @see #getTestSuite()
	 * @generated
	 */
	EReference getTestSuite_TestCases();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.AbstractTestCase <em>Abstract Test Case</em>}'
	 * @return the meta object for class '<em>Abstract Test Case</em>'
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestCase
	 * @generated
	 */
	EClass getAbstractTestCase();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.GUITestCase <em>GUI Test Case</em>}'
	 * @return the meta object for class '<em>GUI Test Case</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestCase
	 * @generated
	 */
	EClass getGUITestCase();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestCase#getTestActions <em>Test Actions</em>}'
	 * @return the meta object for the containment reference list '<em>Test Actions</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestCase#getTestActions()
	 * @see #getGUITestCase()
	 * @generated
	 */
	EReference getGUITestCase_TestActions();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.GUITestAction <em>GUI Test Action</em>}'
	 * @return the meta object for class '<em>GUI Test Action</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction
	 * @generated
	 */
	EClass getGUITestAction();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getComment
	 * <em>Comment</em>}'
	 * @return the meta object for the attribute '<em>Comment</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getComment()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EAttribute getGUITestAction_Comment();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getForm <em>Form</em>}'
	 * @return the meta object for the reference '<em>Form</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getForm()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_Form();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTargetForm <em>Target
	 * Form</em>}'
	 * @return the meta object for the reference '<em>Target Form</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTargetForm()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_TargetForm();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getFormAction <em>Form
	 * Action</em>}'
	 * @return the meta object for the reference '<em>Form Action</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getFormAction()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_FormAction();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getFormPanel <em>Form
	 * Panel</em>}'
	 * @return the meta object for the reference '<em>Form Panel</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getFormPanel()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_FormPanel();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getType <em>Type</em>}'
	 * @return the meta object for the attribute '<em>Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getType()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EAttribute getGUITestAction_Type();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestData <em>Test Data</em>}'
	 * @return the meta object for the containment reference list '<em>Test Data</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestData()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_TestData();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult <em>Action Result</em>}'
	 * @return the meta object for the containment reference '<em>Action Result</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_ActionResult();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase
	 * <em>Test Case</em>}'
	 * @return the meta object for the container reference '<em>Test Case</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EReference getGUITestAction_TestCase();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getDelayBefore <em>Delay
	 * Before</em>}'
	 * @return the meta object for the attribute '<em>Delay Before</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getDelayBefore()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EAttribute getGUITestAction_DelayBefore();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getDelayAfter <em>Delay
	 * After</em>}'
	 * @return the meta object for the attribute '<em>Delay After</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getDelayAfter()
	 * @see #getGUITestAction()
	 * @generated
	 */
	EAttribute getGUITestAction_DelayAfter();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult <em>GUI Test Action
	 * Result</em>}'
	 * @return the meta object for class '<em>GUI Test Action Result</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult
	 * @generated
	 */
	EClass getGUITestActionResult();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getMessageText
	 * <em>Message Text</em>}'
	 * @return the meta object for the attribute '<em>Message Text</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getMessageText()
	 * @see #getGUITestActionResult()
	 * @generated
	 */
	EAttribute getGUITestActionResult_MessageText();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getStatus
	 * <em>Status</em>}'
	 * @return the meta object for the attribute '<em>Status</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getStatus()
	 * @see #getGUITestActionResult()
	 * @generated
	 */
	EAttribute getGUITestActionResult_Status();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction <em>Test Action</em>}'
	 * @return the meta object for the container reference '<em>Test Action</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction()
	 * @see #getGUITestActionResult()
	 * @generated
	 */
	EReference getGUITestActionResult_TestAction();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getComponentType
	 * <em>Component Type</em>}'
	 * @return the meta object for the attribute '<em>Component Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getComponentType()
	 * @see #getGUITestActionResult()
	 * @generated
	 */
	EAttribute getGUITestActionResult_ComponentType();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.GUITestData <em>GUI Test Data</em>}'
	 * @return the meta object for class '<em>GUI Test Data</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData
	 * @generated
	 */
	EClass getGUITestData();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.GUITestData#getFormField <em>Form
	 * Field</em>}'
	 * @return the meta object for the reference '<em>Form Field</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getFormField()
	 * @see #getGUITestData()
	 * @generated
	 */
	EReference getGUITestData_FormField();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.GUITestData#getTableColumnField
	 * <em>Table Column Field</em>}'
	 * @return the meta object for the reference '<em>Table Column Field</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getTableColumnField()
	 * @see #getGUITestData()
	 * @generated
	 */
	EReference getGUITestData_TableColumnField();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestData#getNewValue <em>New
	 * Value</em>}'
	 * @return the meta object for the attribute '<em>New Value</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getNewValue()
	 * @see #getGUITestData()
	 * @generated
	 */
	EAttribute getGUITestData_NewValue();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestData#getExpectedValue
	 * <em>Expected Value</em>}'
	 * @return the meta object for the attribute '<em>Expected Value</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getExpectedValue()
	 * @see #getGUITestData()
	 * @generated
	 */
	EAttribute getGUITestData_ExpectedValue();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestData#getType <em>Type</em>}'
	 * @return the meta object for the attribute '<em>Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getType()
	 * @see #getGUITestData()
	 * @generated
	 */
	EAttribute getGUITestData_Type();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.testing.GUITestData#getTestAction
	 * <em>Test Action</em>}'
	 * @return the meta object for the container reference '<em>Test Action</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getTestAction()
	 * @see #getGUITestData()
	 * @generated
	 */
	EReference getGUITestData_TestAction();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.GUITestData#getFilterValue <em>Filter
	 * Value</em>}'
	 * @return the meta object for the attribute '<em>Filter Value</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getFilterValue()
	 * @see #getGUITestData()
	 * @generated
	 */
	EAttribute getGUITestData_FilterValue();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule <em>Integration Test
	 * Module</em>}'
	 * @return the meta object for class '<em>Integration Test Module</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule
	 * @generated
	 */
	EClass getIntegrationTestModule();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDefaultTimeout <em>Default Timeout</em>}'
	 * @return the meta object for the attribute '<em>Default Timeout</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDefaultTimeout()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EAttribute getIntegrationTestModule_DefaultTimeout();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalFormat
	 * <em>Decimal Format</em>}'
	 * @return the meta object for the attribute '<em>Decimal Format</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalFormat()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EAttribute getIntegrationTestModule_DecimalFormat();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateTimeFormat <em>Date Time Format</em>}'
	 * @return the meta object for the attribute '<em>Date Time Format</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateTimeFormat()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EAttribute getIntegrationTestModule_DateTimeFormat();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getIntegrationModule <em>Integration Module</em>}'
	 * @return the meta object for the reference '<em>Integration Module</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getIntegrationModule()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EReference getIntegrationTestModule_IntegrationModule();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateFormat
	 * <em>Date Format</em>}'
	 * @return the meta object for the attribute '<em>Date Format</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateFormat()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EAttribute getIntegrationTestModule_DateFormat();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalSeparator <em>Decimal Separator</em>}'
	 * @return the meta object for the attribute '<em>Decimal Separator</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalSeparator()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EAttribute getIntegrationTestModule_DecimalSeparator();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getGroupingSeparator <em>Grouping Separator</em>}'
	 * @return the meta object for the attribute '<em>Grouping Separator</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getGroupingSeparator()
	 * @see #getIntegrationTestModule()
	 * @generated
	 */
	EAttribute getIntegrationTestModule_GroupingSeparator();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase <em>Integration Test
	 * Case</em>}'
	 * @return the meta object for class '<em>Integration Test Case</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase
	 * @generated
	 */
	EClass getIntegrationTestCase();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getUserName
	 * <em>User Name</em>}'
	 * @return the meta object for the attribute '<em>User Name</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#getUserName()
	 * @see #getIntegrationTestCase()
	 * @generated
	 */
	EAttribute getIntegrationTestCase_UserName();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPassword
	 * <em>Password</em>}'
	 * @return the meta object for the attribute '<em>Password</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPassword()
	 * @see #getIntegrationTestCase()
	 * @generated
	 */
	EAttribute getIntegrationTestCase_Password();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getMethodInvocations <em>Method Invocations</em>}'
	 * @return the meta object for the containment reference list '<em>Method Invocations</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#getMethodInvocations()
	 * @see #getIntegrationTestCase()
	 * @generated
	 */
	EReference getIntegrationTestCase_MethodInvocations();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation
	 * <em>Integration Method Test Invocation</em>}'
	 * @return the meta object for class '<em>Integration Method Test Invocation</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation
	 * @generated
	 */
	EClass getIntegrationMethodTestInvocation();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectToFail <em>Expect To Fail</em>}'
	 * @return the meta object for the attribute '<em>Expect To Fail</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectToFail()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_ExpectToFail();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingStatement <em>Post Processing
	 * Statement</em>}'
	 * @return the meta object for the attribute '<em>Post Processing Statement</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingStatement()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_PostProcessingStatement();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTimeout <em>Timeout</em>}'
	 * @return the meta object for the attribute '<em>Timeout</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTimeout()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_Timeout();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getIntegrationMethod <em>Integration
	 * Method</em>}'
	 * @return the meta object for the reference '<em>Integration Method</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getIntegrationMethod()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EReference getIntegrationMethodTestInvocation_IntegrationMethod();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParameters <em>Parameters</em>}'
	 * @return the meta object for the containment reference list '<em>Parameters</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParameters()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EReference getIntegrationMethodTestInvocation_Parameters();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getReturnValues <em>Return Values</em>}'
	 * @return the meta object for the containment reference list '<em>Return Values</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getReturnValues()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EReference getIntegrationMethodTestInvocation_ReturnValues();

	/**
	 * Return the meta object for the reference list
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingAttributes <em>Post Processing
	 * Attributes</em>}'
	 * @return the meta object for the reference list '<em>Post Processing Attributes</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingAttributes()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EReference getIntegrationMethodTestInvocation_PostProcessingAttributes();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getNestedInvocations <em>Nested
	 * Invocations</em>}'
	 * @return the meta object for the containment reference list '<em>Nested Invocations</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getNestedInvocations()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EReference getIntegrationMethodTestInvocation_NestedInvocations();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation <em>Parent
	 * Invocation</em>}'
	 * @return the meta object for the container reference '<em>Parent Invocation</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EReference getIntegrationMethodTestInvocation_ParentInvocation();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTestMethodName <em>Test Method Name</em>}'
	 * @return the meta object for the attribute '<em>Test Method Name</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTestMethodName()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_TestMethodName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSize <em>Expected Size</em>}'
	 * @return the meta object for the attribute '<em>Expected Size</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSize()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_ExpectedSize();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSizeOperator <em>Expected Size
	 * Operator</em>}'
	 * @return the meta object for the attribute '<em>Expected Size Operator</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSizeOperator()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_ExpectedSizeOperator();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectedReturnNull <em>Expected Return
	 * Null</em>}'
	 * @return the meta object for the attribute '<em>Expected Return Null</em>'
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectedReturnNull()
	 * @see #getIntegrationMethodTestInvocation()
	 * @generated
	 */
	EAttribute getIntegrationMethodTestInvocation_ExpectedReturnNull();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter <em>Method
	 * Invocation Parameter</em>}'
	 * @return the meta object for class '<em>Method Invocation Parameter</em>'
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter
	 * @generated
	 */
	EClass getMethodInvocationParameter();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getParameterValues <em>Parameter Values</em>}'
	 * @return the meta object for the containment reference list '<em>Parameter Values</em>'
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getParameterValues()
	 * @see #getMethodInvocationParameter()
	 * @generated
	 */
	EReference getMethodInvocationParameter_ParameterValues();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getName()
	 * @see #getMethodInvocationParameter()
	 * @generated
	 */
	EAttribute getMethodInvocationParameter_Name();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getType
	 * <em>Type</em>}'
	 * @return the meta object for the reference '<em>Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getType()
	 * @see #getMethodInvocationParameter()
	 * @generated
	 */
	EReference getMethodInvocationParameter_Type();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#isRepresentsList <em>Represents List</em>}'
	 * @return the meta object for the attribute '<em>Represents List</em>'
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#isRepresentsList()
	 * @see #getMethodInvocationParameter()
	 * @generated
	 */
	EAttribute getMethodInvocationParameter_RepresentsList();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.TestDataObject <em>Test Data Object</em>}'
	 * @return the meta object for class '<em>Test Data Object</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject
	 * @generated
	 */
	EClass getTestDataObject();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.TestDataObject#getMappingObject
	 * <em>Mapping Object</em>}'
	 * @return the meta object for the reference '<em>Mapping Object</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getMappingObject()
	 * @see #getTestDataObject()
	 * @generated
	 */
	EReference getTestDataObject_MappingObject();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.TestDataObject#getAttributes <em>Attributes</em>}'
	 * @return the meta object for the containment reference list '<em>Attributes</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getAttributes()
	 * @see #getTestDataObject()
	 * @generated
	 */
	EReference getTestDataObject_Attributes();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.TestDataObject#getReferencedObject
	 * <em>Referenced Object</em>}'
	 * @return the meta object for the reference '<em>Referenced Object</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getReferencedObject()
	 * @see #getTestDataObject()
	 * @generated
	 */
	EReference getTestDataObject_ReferencedObject();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute <em>Test Data
	 * Attribute</em>}'
	 * @return the meta object for class '<em>Test Data Attribute</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute
	 * @generated
	 */
	EClass getTestDataAttribute();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingAttribute
	 * <em>Mapping Attribute</em>}'
	 * @return the meta object for the reference '<em>Mapping Attribute</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingAttribute()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EReference getTestDataAttribute_MappingAttribute();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getValue
	 * <em>Value</em>}'
	 * @return the meta object for the attribute '<em>Value</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getValue()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_Value();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#isTrackValue
	 * <em>Track Value</em>}'
	 * @return the meta object for the attribute '<em>Track Value</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#isTrackValue()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_TrackValue();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getOperator
	 * <em>Operator</em>}'
	 * @return the meta object for the attribute '<em>Operator</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getOperator()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_Operator();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedAttribute <em>Referenced Attribute</em>}'
	 * @return the meta object for the reference '<em>Referenced Attribute</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedAttribute()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EReference getTestDataAttribute_ReferencedAttribute();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedObjects <em>Referenced Objects</em>}'
	 * @return the meta object for the containment reference list '<em>Referenced Objects</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedObjects()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EReference getTestDataAttribute_ReferencedObjects();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingType
	 * <em>Mapping Type</em>}'
	 * @return the meta object for the reference '<em>Mapping Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingType()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EReference getTestDataAttribute_MappingType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getName()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSize
	 * <em>Expected Size</em>}'
	 * @return the meta object for the attribute '<em>Expected Size</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSize()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_ExpectedSize();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSizeOperator <em>Expected Size Operator</em>}'
	 * @return the meta object for the attribute '<em>Expected Size Operator</em>'
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSizeOperator()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_ExpectedSizeOperator();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getId <em>Id</em>}'
	 * @return the meta object for the attribute '<em>Id</em>'.
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getId()
	 * @see #getTestDataAttribute()
	 * @generated
	 */
	EAttribute getTestDataAttribute_Id();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.testing.GUITestActionStatus <em>GUI Test Action
	 * Status</em>}'
	 * @return the meta object for enum '<em>GUI Test Action Status</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionStatus
	 * @generated
	 */
	EEnum getGUITestActionStatus();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.testing.GUITestActionType <em>GUI Test Action
	 * Type</em>}'
	 * @return the meta object for enum '<em>GUI Test Action Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionType
	 * @generated
	 */
	EEnum getGUITestActionType();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.testing.GUITestDataType <em>GUI Test Data Type</em>}'
	 * @return the meta object for enum '<em>GUI Test Data Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestDataType
	 * @generated
	 */
	EEnum getGUITestDataType();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.testing.SeleniumDriver <em>Selenium Driver</em>}'
	 * @return the meta object for enum '<em>Selenium Driver</em>'
	 * @see net.codecadenza.eclipse.model.testing.SeleniumDriver
	 * @generated
	 */
	EEnum getSeleniumDriver();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType <em>GUI Test
	 * Action Result Component Type</em>}'
	 * @return the meta object for enum '<em>GUI Test Action Result Component Type</em>'
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType
	 * @generated
	 */
	EEnum getGUITestActionResultComponentType();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.testing.AssertionOperator <em>Assertion Operator</em>}'
	 * @return the meta object for enum '<em>Assertion Operator</em>'
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @generated
	 */
	EEnum getAssertionOperator();

	/**
	 * Return the factory that creates the instances of the model
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	TestingFactory getTestingFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl <em>Abstract Test
		 * Module</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getAbstractTestModule()
		 * @generated
		 */
		EClass ABSTRACT_TEST_MODULE = eINSTANCE.getAbstractTestModule();

		/**
		 * The meta object literal for the '<em><b>Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference ABSTRACT_TEST_MODULE__NAMESPACE = eINSTANCE.getAbstractTestModule_Namespace();

		/**
		 * The meta object literal for the '<em><b>Test Case Suffix</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX = eINSTANCE.getAbstractTestModule_TestCaseSuffix();

		/**
		 * The meta object literal for the '<em><b>Project</b></em>' container reference feature
		 * @generated
		 */
		EReference ABSTRACT_TEST_MODULE__PROJECT = eINSTANCE.getAbstractTestModule_Project();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl <em>Selenium Test
		 * Module</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getSeleniumTestModule()
		 * @generated
		 */
		EClass SELENIUM_TEST_MODULE = eINSTANCE.getSeleniumTestModule();

		/**
		 * The meta object literal for the '<em><b>Driver</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SELENIUM_TEST_MODULE__DRIVER = eINSTANCE.getSeleniumTestModule_Driver();

		/**
		 * The meta object literal for the '<em><b>Driver Path</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SELENIUM_TEST_MODULE__DRIVER_PATH = eINSTANCE.getSeleniumTestModule_DriverPath();

		/**
		 * The meta object literal for the '<em><b>Maximize Window</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW = eINSTANCE.getSeleniumTestModule_MaximizeWindow();

		/**
		 * The meta object literal for the '<em><b>Implicit Wait Time</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME = eINSTANCE.getSeleniumTestModule_ImplicitWaitTime();

		/**
		 * The meta object literal for the '<em><b>Page Load Time</b></em>' attribute feature
		 * @generated
		 */
		EAttribute SELENIUM_TEST_MODULE__PAGE_LOAD_TIME = eINSTANCE.getSeleniumTestModule_PageLoadTime();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.TestSuiteImpl <em>Test Suite</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.testing.impl.TestSuiteImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getTestSuite()
		 * @generated
		 */
		EClass TEST_SUITE = eINSTANCE.getTestSuite();

		/**
		 * The meta object literal for the '<em><b>Test Cases</b></em>' reference list feature
		 * @generated
		 */
		EReference TEST_SUITE__TEST_CASES = eINSTANCE.getTestSuite_TestCases();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestCaseImpl <em>Abstract Test
		 * Case</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestCaseImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getAbstractTestCase()
		 * @generated
		 */
		EClass ABSTRACT_TEST_CASE = eINSTANCE.getAbstractTestCase();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestCaseImpl <em>GUI Test Case</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.testing.impl.GUITestCaseImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestCase()
		 * @generated
		 */
		EClass GUI_TEST_CASE = eINSTANCE.getGUITestCase();

		/**
		 * The meta object literal for the '<em><b>Test Actions</b></em>' containment reference list feature
		 * @generated
		 */
		EReference GUI_TEST_CASE__TEST_ACTIONS = eINSTANCE.getGUITestCase_TestActions();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl <em>GUI Test
		 * Action</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestAction()
		 * @generated
		 */
		EClass GUI_TEST_ACTION = eINSTANCE.getGUITestAction();

		/**
		 * The meta object literal for the '<em><b>Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION__COMMENT = eINSTANCE.getGUITestAction_Comment();

		/**
		 * The meta object literal for the '<em><b>Form</b></em>' reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__FORM = eINSTANCE.getGUITestAction_Form();

		/**
		 * The meta object literal for the '<em><b>Target Form</b></em>' reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__TARGET_FORM = eINSTANCE.getGUITestAction_TargetForm();

		/**
		 * The meta object literal for the '<em><b>Form Action</b></em>' reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__FORM_ACTION = eINSTANCE.getGUITestAction_FormAction();

		/**
		 * The meta object literal for the '<em><b>Form Panel</b></em>' reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__FORM_PANEL = eINSTANCE.getGUITestAction_FormPanel();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION__TYPE = eINSTANCE.getGUITestAction_Type();

		/**
		 * The meta object literal for the '<em><b>Test Data</b></em>' containment reference list feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__TEST_DATA = eINSTANCE.getGUITestAction_TestData();

		/**
		 * The meta object literal for the '<em><b>Action Result</b></em>' containment reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__ACTION_RESULT = eINSTANCE.getGUITestAction_ActionResult();

		/**
		 * The meta object literal for the '<em><b>Test Case</b></em>' container reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION__TEST_CASE = eINSTANCE.getGUITestAction_TestCase();

		/**
		 * The meta object literal for the '<em><b>Delay Before</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION__DELAY_BEFORE = eINSTANCE.getGUITestAction_DelayBefore();

		/**
		 * The meta object literal for the '<em><b>Delay After</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION__DELAY_AFTER = eINSTANCE.getGUITestAction_DelayAfter();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl <em>GUI Test
		 * Action Result</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionResult()
		 * @generated
		 */
		EClass GUI_TEST_ACTION_RESULT = eINSTANCE.getGUITestActionResult();

		/**
		 * The meta object literal for the '<em><b>Message Text</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION_RESULT__MESSAGE_TEXT = eINSTANCE.getGUITestActionResult_MessageText();

		/**
		 * The meta object literal for the '<em><b>Status</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION_RESULT__STATUS = eINSTANCE.getGUITestActionResult_Status();

		/**
		 * The meta object literal for the '<em><b>Test Action</b></em>' container reference feature
		 * @generated
		 */
		EReference GUI_TEST_ACTION_RESULT__TEST_ACTION = eINSTANCE.getGUITestActionResult_TestAction();

		/**
		 * The meta object literal for the '<em><b>Component Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_ACTION_RESULT__COMPONENT_TYPE = eINSTANCE.getGUITestActionResult_ComponentType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl <em>GUI Test Data</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestData()
		 * @generated
		 */
		EClass GUI_TEST_DATA = eINSTANCE.getGUITestData();

		/**
		 * The meta object literal for the '<em><b>Form Field</b></em>' reference feature
		 * @generated
		 */
		EReference GUI_TEST_DATA__FORM_FIELD = eINSTANCE.getGUITestData_FormField();

		/**
		 * The meta object literal for the '<em><b>Table Column Field</b></em>' reference feature
		 * @generated
		 */
		EReference GUI_TEST_DATA__TABLE_COLUMN_FIELD = eINSTANCE.getGUITestData_TableColumnField();

		/**
		 * The meta object literal for the '<em><b>New Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_DATA__NEW_VALUE = eINSTANCE.getGUITestData_NewValue();

		/**
		 * The meta object literal for the '<em><b>Expected Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_DATA__EXPECTED_VALUE = eINSTANCE.getGUITestData_ExpectedValue();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_DATA__TYPE = eINSTANCE.getGUITestData_Type();

		/**
		 * The meta object literal for the '<em><b>Test Action</b></em>' container reference feature
		 * @generated
		 */
		EReference GUI_TEST_DATA__TEST_ACTION = eINSTANCE.getGUITestData_TestAction();

		/**
		 * The meta object literal for the '<em><b>Filter Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute GUI_TEST_DATA__FILTER_VALUE = eINSTANCE.getGUITestData_FilterValue();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl
		 * <em>Integration Test Module</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getIntegrationTestModule()
		 * @generated
		 */
		EClass INTEGRATION_TEST_MODULE = eINSTANCE.getIntegrationTestModule();

		/**
		 * The meta object literal for the '<em><b>Default Timeout</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT = eINSTANCE.getIntegrationTestModule_DefaultTimeout();

		/**
		 * The meta object literal for the '<em><b>Decimal Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_MODULE__DECIMAL_FORMAT = eINSTANCE.getIntegrationTestModule_DecimalFormat();

		/**
		 * The meta object literal for the '<em><b>Date Time Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT = eINSTANCE.getIntegrationTestModule_DateTimeFormat();

		/**
		 * The meta object literal for the '<em><b>Integration Module</b></em>' reference feature
		 * @generated
		 */
		EReference INTEGRATION_TEST_MODULE__INTEGRATION_MODULE = eINSTANCE.getIntegrationTestModule_IntegrationModule();

		/**
		 * The meta object literal for the '<em><b>Date Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_MODULE__DATE_FORMAT = eINSTANCE.getIntegrationTestModule_DateFormat();

		/**
		 * The meta object literal for the '<em><b>Decimal Separator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR = eINSTANCE.getIntegrationTestModule_DecimalSeparator();

		/**
		 * The meta object literal for the '<em><b>Grouping Separator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR = eINSTANCE.getIntegrationTestModule_GroupingSeparator();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl <em>Integration
		 * Test Case</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getIntegrationTestCase()
		 * @generated
		 */
		EClass INTEGRATION_TEST_CASE = eINSTANCE.getIntegrationTestCase();

		/**
		 * The meta object literal for the '<em><b>User Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_CASE__USER_NAME = eINSTANCE.getIntegrationTestCase_UserName();

		/**
		 * The meta object literal for the '<em><b>Password</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_TEST_CASE__PASSWORD = eINSTANCE.getIntegrationTestCase_Password();

		/**
		 * The meta object literal for the '<em><b>Method Invocations</b></em>' containment reference list feature
		 * @generated
		 */
		EReference INTEGRATION_TEST_CASE__METHOD_INVOCATIONS = eINSTANCE.getIntegrationTestCase_MethodInvocations();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl
		 * <em>Integration Method Test Invocation</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.IntegrationMethodTestInvocationImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getIntegrationMethodTestInvocation()
		 * @generated
		 */
		EClass INTEGRATION_METHOD_TEST_INVOCATION = eINSTANCE.getIntegrationMethodTestInvocation();

		/**
		 * The meta object literal for the '<em><b>Expect To Fail</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__EXPECT_TO_FAIL = eINSTANCE.getIntegrationMethodTestInvocation_ExpectToFail();

		/**
		 * The meta object literal for the '<em><b>Post Processing Statement</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_STATEMENT = eINSTANCE
				.getIntegrationMethodTestInvocation_PostProcessingStatement();

		/**
		 * The meta object literal for the '<em><b>Timeout</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__TIMEOUT = eINSTANCE.getIntegrationMethodTestInvocation_Timeout();

		/**
		 * The meta object literal for the '<em><b>Integration Method</b></em>' reference feature
		 * @generated
		 */
		EReference INTEGRATION_METHOD_TEST_INVOCATION__INTEGRATION_METHOD = eINSTANCE
				.getIntegrationMethodTestInvocation_IntegrationMethod();

		/**
		 * The meta object literal for the '<em><b>Parameters</b></em>' containment reference list feature
		 * @generated
		 */
		EReference INTEGRATION_METHOD_TEST_INVOCATION__PARAMETERS = eINSTANCE.getIntegrationMethodTestInvocation_Parameters();

		/**
		 * The meta object literal for the '<em><b>Return Values</b></em>' containment reference list feature
		 * @generated
		 */
		EReference INTEGRATION_METHOD_TEST_INVOCATION__RETURN_VALUES = eINSTANCE.getIntegrationMethodTestInvocation_ReturnValues();

		/**
		 * The meta object literal for the '<em><b>Post Processing Attributes</b></em>' reference list feature
		 * @generated
		 */
		EReference INTEGRATION_METHOD_TEST_INVOCATION__POST_PROCESSING_ATTRIBUTES = eINSTANCE
				.getIntegrationMethodTestInvocation_PostProcessingAttributes();

		/**
		 * The meta object literal for the '<em><b>Nested Invocations</b></em>' containment reference list feature
		 * @generated
		 */
		EReference INTEGRATION_METHOD_TEST_INVOCATION__NESTED_INVOCATIONS = eINSTANCE
				.getIntegrationMethodTestInvocation_NestedInvocations();

		/**
		 * The meta object literal for the '<em><b>Parent Invocation</b></em>' container reference feature
		 * @generated
		 */
		EReference INTEGRATION_METHOD_TEST_INVOCATION__PARENT_INVOCATION = eINSTANCE
				.getIntegrationMethodTestInvocation_ParentInvocation();

		/**
		 * The meta object literal for the '<em><b>Test Method Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__TEST_METHOD_NAME = eINSTANCE
				.getIntegrationMethodTestInvocation_TestMethodName();

		/**
		 * The meta object literal for the '<em><b>Expected Size</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE = eINSTANCE.getIntegrationMethodTestInvocation_ExpectedSize();

		/**
		 * The meta object literal for the '<em><b>Expected Size Operator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_SIZE_OPERATOR = eINSTANCE
				.getIntegrationMethodTestInvocation_ExpectedSizeOperator();

		/**
		 * The meta object literal for the '<em><b>Expected Return Null</b></em>' attribute feature
		 * @generated
		 */
		EAttribute INTEGRATION_METHOD_TEST_INVOCATION__EXPECTED_RETURN_NULL = eINSTANCE
				.getIntegrationMethodTestInvocation_ExpectedReturnNull();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl <em>Method
		 * Invocation Parameter</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getMethodInvocationParameter()
		 * @generated
		 */
		EClass METHOD_INVOCATION_PARAMETER = eINSTANCE.getMethodInvocationParameter();

		/**
		 * The meta object literal for the '<em><b>Parameter Values</b></em>' containment reference list feature
		 * @generated
		 */
		EReference METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES = eINSTANCE.getMethodInvocationParameter_ParameterValues();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute METHOD_INVOCATION_PARAMETER__NAME = eINSTANCE.getMethodInvocationParameter_Name();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' reference feature
		 * @generated
		 */
		EReference METHOD_INVOCATION_PARAMETER__TYPE = eINSTANCE.getMethodInvocationParameter_Type();

		/**
		 * The meta object literal for the '<em><b>Represents List</b></em>' attribute feature
		 * @generated
		 */
		EAttribute METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST = eINSTANCE.getMethodInvocationParameter_RepresentsList();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl <em>Test Data
		 * Object</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getTestDataObject()
		 * @generated
		 */
		EClass TEST_DATA_OBJECT = eINSTANCE.getTestDataObject();

		/**
		 * The meta object literal for the '<em><b>Mapping Object</b></em>' reference feature
		 * @generated
		 */
		EReference TEST_DATA_OBJECT__MAPPING_OBJECT = eINSTANCE.getTestDataObject_MappingObject();

		/**
		 * The meta object literal for the '<em><b>Attributes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference TEST_DATA_OBJECT__ATTRIBUTES = eINSTANCE.getTestDataObject_Attributes();

		/**
		 * The meta object literal for the '<em><b>Referenced Object</b></em>' reference feature
		 * @generated
		 */
		EReference TEST_DATA_OBJECT__REFERENCED_OBJECT = eINSTANCE.getTestDataObject_ReferencedObject();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl <em>Test Data
		 * Attribute</em>}' class
		 * @see net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getTestDataAttribute()
		 * @generated
		 */
		EClass TEST_DATA_ATTRIBUTE = eINSTANCE.getTestDataAttribute();

		/**
		 * The meta object literal for the '<em><b>Mapping Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE = eINSTANCE.getTestDataAttribute_MappingAttribute();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__VALUE = eINSTANCE.getTestDataAttribute_Value();

		/**
		 * The meta object literal for the '<em><b>Track Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__TRACK_VALUE = eINSTANCE.getTestDataAttribute_TrackValue();

		/**
		 * The meta object literal for the '<em><b>Operator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__OPERATOR = eINSTANCE.getTestDataAttribute_Operator();

		/**
		 * The meta object literal for the '<em><b>Referenced Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE = eINSTANCE.getTestDataAttribute_ReferencedAttribute();

		/**
		 * The meta object literal for the '<em><b>Referenced Objects</b></em>' containment reference list feature
		 * @generated
		 */
		EReference TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS = eINSTANCE.getTestDataAttribute_ReferencedObjects();

		/**
		 * The meta object literal for the '<em><b>Mapping Type</b></em>' reference feature
		 * @generated
		 */
		EReference TEST_DATA_ATTRIBUTE__MAPPING_TYPE = eINSTANCE.getTestDataAttribute_MappingType();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__NAME = eINSTANCE.getTestDataAttribute_Name();

		/**
		 * The meta object literal for the '<em><b>Expected Size</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__EXPECTED_SIZE = eINSTANCE.getTestDataAttribute_ExpectedSize();

		/**
		 * The meta object literal for the '<em><b>Expected Size Operator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR = eINSTANCE.getTestDataAttribute_ExpectedSizeOperator();

		/**
		 * The meta object literal for the '<em><b>Id</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TEST_DATA_ATTRIBUTE__ID = eINSTANCE.getTestDataAttribute_Id();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.GUITestActionStatus <em>GUI Test Action
		 * Status</em>}' enum
		 * @see net.codecadenza.eclipse.model.testing.GUITestActionStatus
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionStatus()
		 * @generated
		 */
		EEnum GUI_TEST_ACTION_STATUS = eINSTANCE.getGUITestActionStatus();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.GUITestActionType <em>GUI Test Action
		 * Type</em>}' enum
		 * @see net.codecadenza.eclipse.model.testing.GUITestActionType
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionType()
		 * @generated
		 */
		EEnum GUI_TEST_ACTION_TYPE = eINSTANCE.getGUITestActionType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.GUITestDataType <em>GUI Test Data Type</em>}'
		 * enum
		 * @see net.codecadenza.eclipse.model.testing.GUITestDataType
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestDataType()
		 * @generated
		 */
		EEnum GUI_TEST_DATA_TYPE = eINSTANCE.getGUITestDataType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.SeleniumDriver <em>Selenium Driver</em>}'
		 * enum
		 * @see net.codecadenza.eclipse.model.testing.SeleniumDriver
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getSeleniumDriver()
		 * @generated
		 */
		EEnum SELENIUM_DRIVER = eINSTANCE.getSeleniumDriver();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType <em>GUI Test
		 * Action Result Component Type</em>}' enum
		 * @see net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getGUITestActionResultComponentType()
		 * @generated
		 */
		EEnum GUI_TEST_ACTION_RESULT_COMPONENT_TYPE = eINSTANCE.getGUITestActionResultComponentType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.testing.AssertionOperator <em>Assertion
		 * Operator</em>}' enum
		 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
		 * @see net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl#getAssertionOperator()
		 * @generated
		 */
		EEnum ASSERTION_OPERATOR = eINSTANCE.getAssertionOperator();

	}

}
