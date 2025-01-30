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

import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Abstract Test Module</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getNamespace <em>Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestCaseSuffix <em>Test Case Suffix</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject <em>Project</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule()
 * @model abstract="true"
 * @generated
 */
public interface AbstractTestModule extends EObject {
	/**
	 * Return the value of the '<em><b>Namespace</b></em>' reference
	 * @return the value of the '<em>Namespace</em>' reference
	 * @see #setNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule_Namespace()
	 * @model
	 * @generated
	 */
	Namespace getNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getNamespace <em>Namespace</em>}'
	 * reference
	 * @param value the new value of the '<em>Namespace</em>' reference
	 * @see #getNamespace()
	 * @generated
	 */
	void setNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Test Case Suffix</b></em>' attribute
	 * @return the value of the '<em>Test Case Suffix</em>' attribute
	 * @see #setTestCaseSuffix(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule_TestCaseSuffix()
	 * @model
	 * @generated
	 */
	String getTestCaseSuffix();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestCaseSuffix <em>Test Case
	 * Suffix</em>}' attribute
	 * @param value the new value of the '<em>Test Case Suffix</em>' attribute
	 * @see #getTestCaseSuffix()
	 * @generated
	 */
	void setTestCaseSuffix(String value);

	/**
	 * Return the value of the '<em><b>Project</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.Project#getTestModules <em>Test Modules</em>}'.
	 * @return the value of the '<em>Project</em>' container reference
	 * @see #setProject(Project)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule_Project()
	 * @see net.codecadenza.eclipse.model.project.Project#getTestModules
	 * @model opposite="testModules" transient="false"
	 * @generated
	 */
	Project getProject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject <em>Project</em>}' container
	 * reference
	 * @param value the new value of the '<em>Project</em>' container reference
	 * @see #getProject()
	 * @generated
	 */
	void setProject(Project value);

	/**
	 * @model kind="operation"
	 * @return a list containing all test suites of this module
	 * @generated
	 */
	EList<TestSuite> getTestSuites();

	/**
	 * @model kind="operation"
	 * @return a list containing all test cases of this module
	 * @generated
	 */
	EList<AbstractTestCase> getTestCases();

	/**
	 * @return the artifact type of this test module
	 * @generated not
	 */
	BuildArtifactType getArtifactType();

}
