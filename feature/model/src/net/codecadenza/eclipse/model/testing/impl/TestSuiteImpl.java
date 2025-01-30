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

import java.util.Collection;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaTestFile;
import net.codecadenza.eclipse.model.java.impl.JavaTypeImpl;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.testing.AbstractTestCase;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * An implementation of the model object '<em><b>Test Suite</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestSuiteImpl#getTestCases <em>Test Cases</em>}</li>
 * </ul>
 * @generated
 */
public class TestSuiteImpl extends JavaTypeImpl implements TestSuite {
	/**
	 * The cached value of the '{@link #getTestCases() <em>Test Cases</em>}' reference list
	 * @see #getTestCases()
	 * @generated
	 * @ordered
	 */
	protected EList<AbstractTestCase> testCases;

	/**
	 * @generated
	 */
	protected TestSuiteImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.TEST_SUITE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestSuite#getTestCases()
	 * @generated
	 */
	@Override
	public EList<AbstractTestCase> getTestCases() {
		if (testCases == null)
			testCases = new EObjectResolvingEList<>(AbstractTestCase.class, this, TestingPackage.TEST_SUITE__TEST_CASES);

		return testCases;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.TEST_SUITE__TEST_CASES:
				return getTestCases();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.TEST_SUITE__TEST_CASES:
				getTestCases().clear();
				getTestCases().addAll((Collection<? extends AbstractTestCase>) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case TestingPackage.TEST_SUITE__TEST_CASES:
				getTestCases().clear();
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case TestingPackage.TEST_SUITE__TEST_CASES:
				return testCases != null && !testCases.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestSuite#getTestModule()
	 * @generated not
	 */
	@Override
	public AbstractTestModule getTestModule() {
		return getNamespace().getProject().getTestModules().stream()
				.filter(testModule -> testModule.getNamespace().equals(getNamespace())).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestSuite#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final var javaFile = new JavaTestFile(getNamespace().getProject(), BuildArtifactType.SELENIUM_TEST, name,
				getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

}
