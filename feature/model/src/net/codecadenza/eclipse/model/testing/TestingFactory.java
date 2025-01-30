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

import org.eclipse.emf.ecore.EFactory;

/**
 * The <b>Factory</b> for the model. It provides a create method for each non-abstract class of the model.
 * @see net.codecadenza.eclipse.model.testing.TestingPackage
 * @generated
 */
public interface TestingFactory extends EFactory {
	/**
	 * The singleton instance of the factory
	 * @generated
	 */
	TestingFactory eINSTANCE = net.codecadenza.eclipse.model.testing.impl.TestingFactoryImpl.init();

	/**
	 * Return a new object of class '<em>Selenium Test Module</em>'
	 * @return a new object of class '<em>Selenium Test Module</em>'
	 * @generated
	 */
	SeleniumTestModule createSeleniumTestModule();

	/**
	 * Return a new object of class '<em>Test Suite</em>'
	 * @return a new object of class '<em>Test Suite</em>'
	 * @generated
	 */
	TestSuite createTestSuite();

	/**
	 * Return a new object of class '<em>GUI Test Case</em>'
	 * @return a new object of class '<em>GUI Test Case</em>'
	 * @generated
	 */
	GUITestCase createGUITestCase();

	/**
	 * Return a new object of class '<em>GUI Test Action</em>'
	 * @return a new object of class '<em>GUI Test Action</em>'
	 * @generated
	 */
	GUITestAction createGUITestAction();

	/**
	 * Return a new object of class '<em>GUI Test Action Result</em>'
	 * @return a new object of class '<em>GUI Test Action Result</em>'
	 * @generated
	 */
	GUITestActionResult createGUITestActionResult();

	/**
	 * Return a new object of class '<em>GUI Test Data</em>'
	 * @return a new object of class '<em>GUI Test Data</em>'
	 * @generated
	 */
	GUITestData createGUITestData();

	/**
	 * Return the package supported by this factory
	 * @return the package supported by this factory
	 * @generated
	 */
	TestingPackage getTestingPackage();

}
