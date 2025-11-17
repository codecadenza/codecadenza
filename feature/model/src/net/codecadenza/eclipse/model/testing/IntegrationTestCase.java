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

import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Integration Test Case</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getUserName <em>User Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPassword <em>Password</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getMethodInvocations <em>Method Invocations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPreProcessingStatements <em>Pre-Processing
 * Statements</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPostProcessingStatements <em>Post-Processing
 * Statements</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestCase()
 * @model
 * @generated
 */
public interface IntegrationTestCase extends AbstractTestCase {
	/**
	 * Return the value of the '<em><b>User Name</b></em>' attribute
	 * @return the value of the '<em>User Name</em>' attribute
	 * @see #setUserName(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestCase_UserName()
	 * @model
	 * @generated
	 */
	String getUserName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getUserName <em>User Name</em>}'
	 * attribute
	 * @param value the new value of the '<em>User Name</em>' attribute
	 * @see #getUserName()
	 * @generated
	 */
	void setUserName(String value);

	/**
	 * Return the value of the '<em><b>Password</b></em>' attribute
	 * @return the value of the '<em>Password</em>' attribute
	 * @see #setPassword(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestCase_Password()
	 * @model
	 * @generated
	 */
	String getPassword();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPassword <em>Password</em>}'
	 * attribute
	 * @param value the new value of the '<em>Password</em>' attribute
	 * @see #getPassword()
	 * @generated
	 */
	void setPassword(String value);

	/**
	 * Return the value of the '<em><b>Method Invocations</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation}.
	 * @return the value of the '<em>Method Invocations</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestCase_MethodInvocations()
	 * @model containment="true"
	 * @generated
	 */
	EList<IntegrationMethodTestInvocation> getMethodInvocations();

	/**
	 * Return the value of the '<em><b>Pre-Processing Statements</b></em>' attribute
	 * @return the value of the '<em>Pre-Processing Statements</em>' attribute
	 * @see #setPreProcessingStatements(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestCase_PreProcessingStatements()
	 * @model
	 * @generated
	 */
	String getPreProcessingStatements();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPreProcessingStatements
	 * <em>Pre-Processing Statements</em>}' attribute
	 * @param value the new value of the '<em>Pre-Processing Statements</em>' attribute
	 * @see #getPreProcessingStatements()
	 * @generated
	 */
	void setPreProcessingStatements(String value);

	/**
	 * Return the value of the '<em><b>Post-Processing Statements</b></em>' attribute
	 * @return the value of the '<em>Post-Processing Statements</em>' attribute
	 * @see #setPostProcessingStatements(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestCase_PostProcessingStatements()
	 * @model
	 * @generated
	 */
	String getPostProcessingStatements();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPostProcessingStatements
	 * <em>Post-Processing Statements</em>}' attribute
	 * @param value the new value of the '<em>Post-Processing Statements</em>' attribute
	 * @see #getPostProcessingStatements()
	 * @generated
	 */
	void setPostProcessingStatements(String value);

	/**
	 * @return if at least one invocation needs to either upload or download a file
	 * @generated not
	 */
	boolean isFileHandlingRequired();

	/**
	 * @return true if the credentials should be added when initializing the necessary integration clients
	 * @generated not
	 */
	boolean addCredentials();

}
