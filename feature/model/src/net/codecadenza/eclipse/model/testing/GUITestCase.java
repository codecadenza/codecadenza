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

import java.util.List;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>GUI Test Case</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestCase#getTestActions <em>Test Actions</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestCase()
 * @model
 * @generated
 */
public interface GUITestCase extends AbstractTestCase {
	/**
	 * Return the value of the '<em><b>Test Actions</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.GUITestAction}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase <em>Test Case</em>}'.
	 * @return the value of the '<em>Test Actions</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestCase_TestActions()
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase
	 * @model opposite="testCase" containment="true"
	 * @generated
	 */
	EList<GUITestAction> getTestActions();

	/**
	 * @return a list containing all test suites this test case is included
	 * @generated not
	 */
	List<TestSuite> getTestSuites();

}
