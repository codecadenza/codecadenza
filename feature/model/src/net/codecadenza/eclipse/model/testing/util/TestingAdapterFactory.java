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
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage
 * @generated
 */
public class TestingAdapterFactory extends AdapterFactoryImpl {
	/**
	 * @generated
	 */
	protected static TestingPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public TestingAdapterFactory() {
		if (modelPackage == null)
			modelPackage = TestingPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#isFactoryForType(java.lang.Object)
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage)
			return true;

		if (object instanceof final EObject eObject)
			return eObject.eClass().getEPackage() == modelPackage;

		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * @generated
	 */
	protected TestingSwitch<Adapter> modelSwitch = new TestingSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseAbstractTestModule(net.codecadenza.eclipse.model.testing.AbstractTestModule)
		 */
		@Override
		public Adapter caseAbstractTestModule(AbstractTestModule object) {
			return createAbstractTestModuleAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseSeleniumTestModule(net.codecadenza.eclipse.model.testing.SeleniumTestModule)
		 */
		@Override
		public Adapter caseSeleniumTestModule(SeleniumTestModule object) {
			return createSeleniumTestModuleAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseTestSuite(net.codecadenza.eclipse.model.testing.TestSuite)
		 */
		@Override
		public Adapter caseTestSuite(TestSuite object) {
			return createTestSuiteAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseAbstractTestCase(net.codecadenza.eclipse.model.testing.AbstractTestCase)
		 */
		@Override
		public Adapter caseAbstractTestCase(AbstractTestCase object) {
			return createAbstractTestCaseAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseGUITestCase(net.codecadenza.eclipse.model.testing.GUITestCase)
		 */
		@Override
		public Adapter caseGUITestCase(GUITestCase object) {
			return createGUITestCaseAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseGUITestAction(net.codecadenza.eclipse.model.testing.GUITestAction)
		 */
		@Override
		public Adapter caseGUITestAction(GUITestAction object) {
			return createGUITestActionAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseGUITestActionResult(net.codecadenza.eclipse.model.testing.GUITestActionResult)
		 */
		@Override
		public Adapter caseGUITestActionResult(GUITestActionResult object) {
			return createGUITestActionResultAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseGUITestData(net.codecadenza.eclipse.model.testing.GUITestData)
		 */
		@Override
		public Adapter caseGUITestData(GUITestData object) {
			return createGUITestDataAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseIntegrationTestModule(net.codecadenza.eclipse.model.testing.IntegrationTestModule)
		 */
		@Override
		public Adapter caseIntegrationTestModule(IntegrationTestModule object) {
			return createIntegrationTestModuleAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseIntegrationTestCase(net.codecadenza.eclipse.model.testing.IntegrationTestCase)
		 */
		@Override
		public Adapter caseIntegrationTestCase(IntegrationTestCase object) {
			return createIntegrationTestCaseAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseIntegrationMethodTestInvocation(net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation)
		 */
		@Override
		public Adapter caseIntegrationMethodTestInvocation(IntegrationMethodTestInvocation object) {
			return createIntegrationMethodTestInvocationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseMethodInvocationParameter(net.codecadenza.eclipse.model.testing.MethodInvocationParameter)
		 */
		@Override
		public Adapter caseMethodInvocationParameter(MethodInvocationParameter object) {
			return createMethodInvocationParameterAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseTestDataObject(net.codecadenza.eclipse.model.testing.TestDataObject)
		 */
		@Override
		public Adapter caseTestDataObject(TestDataObject object) {
			return createTestDataObjectAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#
		 * caseTestDataAttribute(net.codecadenza.eclipse.model.testing.TestDataAttribute)
		 */
		@Override
		public Adapter caseTestDataAttribute(TestDataAttribute object) {
			return createTestDataAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#caseJavaType(net.codecadenza.eclipse.model.java.JavaType)
		 */
		@Override
		public Adapter caseJavaType(JavaType object) {
			return createJavaTypeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.testing.util.TestingSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public Adapter defaultCase(EObject object) {
			return createEObjectAdapter();
		}
	};

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#createAdapter(org.eclipse.emf.common.notify.Notifier)
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule <em>Abstract
	 * Test Module</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule
	 * @generated
	 */
	public Adapter createAbstractTestModuleAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule <em>Selenium
	 * Test Module</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule
	 * @generated
	 */
	public Adapter createSeleniumTestModuleAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.TestSuite <em>Test Suite</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.TestSuite
	 * @generated
	 */
	public Adapter createTestSuiteAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.AbstractTestCase <em>Abstract Test
	 * Case</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestCase
	 * @generated
	 */
	public Adapter createAbstractTestCaseAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.GUITestCase <em>GUI Test
	 * Case</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.GUITestCase
	 * @generated
	 */
	public Adapter createGUITestCaseAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.GUITestAction <em>GUI Test
	 * Action</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction
	 * @generated
	 */
	public Adapter createGUITestActionAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult <em>GUI Test
	 * Action Result</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult
	 * @generated
	 */
	public Adapter createGUITestActionResultAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.GUITestData <em>GUI Test
	 * Data</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.GUITestData
	 * @generated
	 */
	public Adapter createGUITestDataAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule
	 * <em>Integration Test Module</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful
	 * to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule
	 * @generated
	 */
	public Adapter createIntegrationTestModuleAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase <em>Integration
	 * Test Case</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase
	 * @generated
	 */
	public Adapter createIntegrationTestCaseAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation
	 * <em>Integration Method Test Invocation</em>}'.This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation
	 * @generated
	 */
	public Adapter createIntegrationMethodTestInvocationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter
	 * <em>Method Invocation Parameter</em>}'. This default implementation returns null so that we can easily ignore cases; it's
	 * useful to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter
	 * @generated
	 */
	public Adapter createMethodInvocationParameterAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.TestDataObject <em>Test Data
	 * Object</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject
	 * @generated
	 */
	public Adapter createTestDataObjectAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute <em>Test Data
	 * Attribute</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute
	 * @generated
	 */
	public Adapter createTestDataAttributeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.JavaType <em>Type</em>}'. This default
	 * implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will catch all
	 * the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.JavaType
	 * @generated
	 */
	public Adapter createJavaTypeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for the default case. This default implementation returns null.
	 * @return the new adapter
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

}
