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

import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Test Suite</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestSuite#getTestCases <em>Test Cases</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestSuite()
 * @model
 * @generated
 */
public interface TestSuite extends JavaType {
	/**
	 * Return the value of the '<em><b>Test Cases</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.AbstractTestCase}.
	 * @return the value of the '<em>Test Cases</em>' reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestSuite_TestCases()
	 * @model
	 * @generated
	 */
	EList<AbstractTestCase> getTestCases();

	/**
	 * @return the test module this test suite belongs to
	 * @generated not
	 */
	AbstractTestModule getTestModule();

	/**
	 * @return the internal representation of the test suite source file
	 * @generated not
	 */
	JavaFile getSourceFile();

}
