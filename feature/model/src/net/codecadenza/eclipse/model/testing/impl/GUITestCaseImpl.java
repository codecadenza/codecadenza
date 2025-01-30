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
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaTestFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>GUI Test Case</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestCaseImpl#getTestActions <em>Test Actions</em>}</li>
 * </ul>
 * @generated
 */
public class GUITestCaseImpl extends AbstractTestCaseImpl implements GUITestCase {
	/**
	 * The cached value of the '{@link #getTestActions() <em>Test Actions</em>}' containment reference list
	 * @see #getTestActions()
	 * @generated
	 * @ordered
	 */
	protected EList<GUITestAction> testActions;

	/**
	 * @generated
	 */
	protected GUITestCaseImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestCaseImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.GUI_TEST_CASE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestCase#getTestActions()
	 * @generated
	 */
	@Override
	public EList<GUITestAction> getTestActions() {
		if (testActions == null)
			testActions = new EObjectContainmentWithInverseEList<>(GUITestAction.class, this,
					TestingPackage.GUI_TEST_CASE__TEST_ACTIONS, TestingPackage.GUI_TEST_ACTION__TEST_CASE);

		return testActions;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	@SuppressWarnings("unchecked")
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.GUI_TEST_CASE__TEST_ACTIONS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getTestActions()).basicAdd(otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.GUI_TEST_CASE__TEST_ACTIONS:
				return ((InternalEList<?>) getTestActions()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.GUI_TEST_CASE__TEST_ACTIONS:
				return getTestActions();
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
			case TestingPackage.GUI_TEST_CASE__TEST_ACTIONS:
				getTestActions().clear();
				getTestActions().addAll((Collection<? extends GUITestAction>) newValue);
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
			case TestingPackage.GUI_TEST_CASE__TEST_ACTIONS:
				getTestActions().clear();
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
			case TestingPackage.GUI_TEST_CASE__TEST_ACTIONS:
				return testActions != null && !testActions.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestCase#getTestSuites()
	 * @generated not
	 */
	@Override
	public List<TestSuite> getTestSuites() {
		return getTestModule().getTestSuites().stream().filter(e -> e.getTestCases().contains(this)).toList();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestCase#getSourceFile()
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
