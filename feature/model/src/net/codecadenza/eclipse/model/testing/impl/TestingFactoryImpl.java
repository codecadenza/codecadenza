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
package net.codecadenza.eclipse.model.testing.impl;

import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType;
import net.codecadenza.eclipse.model.testing.GUITestActionStatus;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.SeleniumDriver;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model <b>Factory</b>.
 * @generated
 */
public class TestingFactoryImpl extends EFactoryImpl implements TestingFactory {
	/**
	 * Create the default factory implementation
	 * @return the factory
	 * @generated
	 */
	public static TestingFactory init() {
		try {
			final var theTestingFactory = (TestingFactory) EPackage.Registry.INSTANCE.getEFactory(TestingPackage.eNS_URI);

			if (theTestingFactory != null)
				return theTestingFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new TestingFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case TestingPackage.SELENIUM_TEST_MODULE -> createSeleniumTestModule();
			case TestingPackage.TEST_SUITE -> createTestSuite();
			case TestingPackage.GUI_TEST_CASE -> createGUITestCase();
			case TestingPackage.GUI_TEST_ACTION -> createGUITestAction();
			case TestingPackage.GUI_TEST_ACTION_RESULT -> createGUITestActionResult();
			case TestingPackage.GUI_TEST_DATA -> createGUITestData();
			case TestingPackage.INTEGRATION_TEST_MODULE -> createIntegrationTestModule();
			case TestingPackage.INTEGRATION_TEST_CASE -> createIntegrationTestCase();
			case TestingPackage.INTEGRATION_METHOD_TEST_INVOCATION -> createIntegrationMethodTestInvocation();
			case TestingPackage.METHOD_INVOCATION_PARAMETER -> createMethodInvocationParameter();
			case TestingPackage.TEST_DATA_OBJECT -> createTestDataObject();
			case TestingPackage.TEST_DATA_ATTRIBUTE -> createTestDataAttribute();
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
			case TestingPackage.GUI_TEST_ACTION_STATUS -> createGUITestActionStatusFromString(eDataType, initialValue);
			case TestingPackage.GUI_TEST_ACTION_TYPE -> createGUITestActionTypeFromString(eDataType, initialValue);
			case TestingPackage.GUI_TEST_DATA_TYPE -> createGUITestDataTypeFromString(eDataType, initialValue);
			case TestingPackage.SELENIUM_DRIVER -> createSeleniumDriverFromString(eDataType, initialValue);
			case TestingPackage.GUI_TEST_ACTION_RESULT_COMPONENT_TYPE -> createGUITestActionResultComponentTypeFromString(eDataType,
					initialValue);
			case TestingPackage.ASSERTION_OPERATOR -> createAssertionOperatorFromString(eDataType, initialValue);
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
			case TestingPackage.GUI_TEST_ACTION_STATUS -> convertGUITestActionStatusToString(eDataType, instanceValue);
			case TestingPackage.GUI_TEST_ACTION_TYPE -> convertGUITestActionTypeToString(eDataType, instanceValue);
			case TestingPackage.GUI_TEST_DATA_TYPE -> convertGUITestDataTypeToString(eDataType, instanceValue);
			case TestingPackage.SELENIUM_DRIVER -> convertSeleniumDriverToString(eDataType, instanceValue);
			case TestingPackage.GUI_TEST_ACTION_RESULT_COMPONENT_TYPE -> convertGUITestActionResultComponentTypeToString(eDataType,
					instanceValue);
			case TestingPackage.ASSERTION_OPERATOR -> convertAssertionOperatorToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createSeleniumTestModule()
	 * @generated
	 */
	@Override
	public SeleniumTestModule createSeleniumTestModule() {
		return new SeleniumTestModuleImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createTestSuite()
	 * @generated
	 */
	@Override
	public TestSuite createTestSuite() {
		return new TestSuiteImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createGUITestCase()
	 * @generated
	 */
	@Override
	public GUITestCase createGUITestCase() {
		return new GUITestCaseImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createGUITestAction()
	 * @generated
	 */
	@Override
	public GUITestAction createGUITestAction() {
		return new GUITestActionImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createGUITestActionResult()
	 * @generated
	 */
	@Override
	public GUITestActionResult createGUITestActionResult() {
		return new GUITestActionResultImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createGUITestData()
	 * @generated
	 */
	@Override
	public GUITestData createGUITestData() {
		return new GUITestDataImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createIntegrationTestModule()
	 * @generated
	 */
	@Override
	public IntegrationTestModule createIntegrationTestModule() {
		return new IntegrationTestModuleImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createIntegrationTestCase()
	 * @generated
	 */
	@Override
	public IntegrationTestCase createIntegrationTestCase() {
		return new IntegrationTestCaseImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createIntegrationMethodTestInvocation()
	 * @generated
	 */
	@Override
	public IntegrationMethodTestInvocation createIntegrationMethodTestInvocation() {
		return new IntegrationMethodTestInvocationImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createMethodInvocationParameter()
	 * @generated
	 */
	@Override
	public MethodInvocationParameter createMethodInvocationParameter() {
		return new MethodInvocationParameterImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createTestDataObject()
	 * @generated
	 */
	@Override
	public TestDataObject createTestDataObject() {
		return new TestDataObjectImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#createTestDataAttribute()
	 * @generated
	 */
	@Override
	public TestDataAttribute createTestDataAttribute() {
		return new TestDataAttributeImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public GUITestActionStatus createGUITestActionStatusFromString(EDataType eDataType, String initialValue) {
		final GUITestActionStatus result = GUITestActionStatus.get(initialValue);

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
	public String convertGUITestActionStatusToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public GUITestActionType createGUITestActionTypeFromString(EDataType eDataType, String initialValue) {
		final GUITestActionType result = GUITestActionType.get(initialValue);

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
	public String convertGUITestActionTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public GUITestDataType createGUITestDataTypeFromString(EDataType eDataType, String initialValue) {
		final GUITestDataType result = GUITestDataType.get(initialValue);

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
	public String convertGUITestDataTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public SeleniumDriver createSeleniumDriverFromString(EDataType eDataType, String initialValue) {
		final SeleniumDriver result = SeleniumDriver.get(initialValue);

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
	public String convertSeleniumDriverToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public GUITestActionResultComponentType createGUITestActionResultComponentTypeFromString(EDataType eDataType,
			String initialValue) {
		final GUITestActionResultComponentType result = GUITestActionResultComponentType.get(initialValue);

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
	public String convertGUITestActionResultComponentTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public AssertionOperator createAssertionOperatorFromString(EDataType eDataType, String initialValue) {
		final AssertionOperator result = AssertionOperator.get(initialValue);

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
	public String convertAssertionOperatorToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingFactory#getTestingPackage()
	 * @generated
	 */
	@Override
	public TestingPackage getTestingPackage() {
		return (TestingPackage) getEPackage();
	}

	/**
	 * @return the testing package
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static TestingPackage getPackage() {
		return TestingPackage.eINSTANCE;
	}

}
