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
package net.codecadenza.eclipse.model.testing.util;

import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.AbstractTestCase;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage
 * @generated
 */
public class TestingSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static TestingPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public TestingSwitch() {
		if (modelPackage == null)
			modelPackage = TestingPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#isSwitchFor(org.eclipse.emf.ecore.EPackage)
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#doSwitch(int, org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case TestingPackage.ABSTRACT_TEST_MODULE: {
				final var abstractTestModule = (AbstractTestModule) theEObject;
				T result = caseAbstractTestModule(abstractTestModule);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.SELENIUM_TEST_MODULE: {
				final var seleniumTestModule = (SeleniumTestModule) theEObject;

				T result = caseSeleniumTestModule(seleniumTestModule);

				if (result == null)
					result = caseAbstractTestModule(seleniumTestModule);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.TEST_SUITE: {
				final var testSuite = (TestSuite) theEObject;

				T result = caseTestSuite(testSuite);

				if (result == null)
					result = caseJavaType(testSuite);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.ABSTRACT_TEST_CASE: {
				final var abstractTestCase = (AbstractTestCase) theEObject;

				T result = caseAbstractTestCase(abstractTestCase);

				if (result == null)
					result = caseJavaType(abstractTestCase);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.GUI_TEST_CASE: {
				final var guiTestCase = (GUITestCase) theEObject;

				T result = caseGUITestCase(guiTestCase);

				if (result == null)
					result = caseAbstractTestCase(guiTestCase);

				if (result == null)
					result = caseJavaType(guiTestCase);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.GUI_TEST_ACTION: {
				final var guiTestAction = (GUITestAction) theEObject;

				T result = caseGUITestAction(guiTestAction);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.GUI_TEST_ACTION_RESULT: {
				final var guiTestActionResult = (GUITestActionResult) theEObject;

				T result = caseGUITestActionResult(guiTestActionResult);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case TestingPackage.GUI_TEST_DATA: {
				final var guiTestData = (GUITestData) theEObject;

				T result = caseGUITestData(guiTestData);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Abstract Test Module</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Abstract Test Module</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAbstractTestModule(AbstractTestModule object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Selenium Test Module</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Selenium Test Module</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseSeleniumTestModule(SeleniumTestModule object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Test Suite</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Test Suite</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseTestSuite(TestSuite object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Abstract Test Case</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Abstract Test Case</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAbstractTestCase(AbstractTestCase object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>GUI Test Case</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>GUI Test Case</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseGUITestCase(GUITestCase object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>GUI Test Action</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>GUI Test Action</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseGUITestAction(GUITestAction object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>GUI Test Action Result</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>GUI Test Action Result</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseGUITestActionResult(GUITestActionResult object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>GUI Test Data</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>GUI Test Data</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseGUITestData(GUITestData object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Type</em>'. This implementation returns null; returning a
	 * non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Type</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaType(JavaType object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#defaultCase(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

}
