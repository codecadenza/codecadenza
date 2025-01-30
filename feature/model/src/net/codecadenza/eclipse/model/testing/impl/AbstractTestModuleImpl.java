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

import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.testing.AbstractTestCase;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Abstract Test Module</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#getNamespace <em>Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#getTestCaseSuffix <em>Test Case Suffix</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#getProject <em>Project</em>}</li>
 * </ul>
 * @generated
 */
public abstract class AbstractTestModuleImpl extends EObjectImpl implements AbstractTestModule {
	/**
	 * The cached value of the '{@link #getNamespace() <em>Namespace</em>}' reference
	 * @see #getNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace namespace;

	/**
	 * The default value of the '{@link #getTestCaseSuffix() <em>Test Case Suffix</em>}' attribute
	 * @see #getTestCaseSuffix()
	 * @generated
	 * @ordered
	 */
	protected static final String TEST_CASE_SUFFIX_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTestCaseSuffix() <em>Test Case Suffix</em>}' attribute
	 * @see #getTestCaseSuffix()
	 * @generated
	 * @ordered
	 */
	protected String testCaseSuffix = TEST_CASE_SUFFIX_EDEFAULT;

	/**
	 * @generated
	 */
	protected AbstractTestModuleImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.ABSTRACT_TEST_MODULE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getNamespace()
	 * @generated
	 */
	@Override
	public Namespace getNamespace() {
		if (namespace != null && namespace.eIsProxy()) {
			final var oldNamespace = (InternalEObject) namespace;
			namespace = (Namespace) eResolveProxy(oldNamespace);

			if (namespace != oldNamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.ABSTRACT_TEST_MODULE__NAMESPACE, oldNamespace,
						namespace));
		}

		return namespace;
	}

	/**
	 * @return the namespace of this test module
	 * @generated
	 */
	public Namespace basicGetNamespace() {
		return namespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#setNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setNamespace(Namespace newNamespace) {
		final Namespace oldNamespace = namespace;
		namespace = newNamespace;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.ABSTRACT_TEST_MODULE__NAMESPACE, oldNamespace, namespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestCaseSuffix()
	 * @generated
	 */
	@Override
	public String getTestCaseSuffix() {
		return testCaseSuffix;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#setTestCaseSuffix(java.lang.String)
	 * @generated
	 */
	@Override
	public void setTestCaseSuffix(String newTestCaseSuffix) {
		final String oldTestCaseSuffix = testCaseSuffix;
		testCaseSuffix = newTestCaseSuffix;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX,
					oldTestCaseSuffix, testCaseSuffix));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getProject()
	 * @generated
	 */
	@Override
	public Project getProject() {
		if (eContainerFeatureID() != TestingPackage.ABSTRACT_TEST_MODULE__PROJECT)
			return null;

		return (Project) eInternalContainer();
	}

	/**
	 * @param newProject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetProject(Project newProject, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newProject, TestingPackage.ABSTRACT_TEST_MODULE__PROJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#setProject(net.codecadenza.eclipse.model.project.Project)
	 * @generated
	 */
	@Override
	public void setProject(Project newProject) {
		if (newProject != eInternalContainer()
				|| (eContainerFeatureID() != TestingPackage.ABSTRACT_TEST_MODULE__PROJECT && newProject != null)) {
			if (EcoreUtil.isAncestor(this, newProject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newProject != null)
				msgs = ((InternalEObject) newProject).eInverseAdd(this, ProjectPackage.PROJECT__TEST_MODULES, Project.class, msgs);

			msgs = basicSetProject(newProject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.ABSTRACT_TEST_MODULE__PROJECT, newProject, newProject));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetProject((Project) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				return basicSetProject(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				return eInternalContainer().eInverseRemove(this, ProjectPackage.PROJECT__TEST_MODULES, Project.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.ABSTRACT_TEST_MODULE__NAMESPACE:
				if (resolve)
					return getNamespace();

				return basicGetNamespace();
			case TestingPackage.ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX:
				return getTestCaseSuffix();
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				return getProject();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.ABSTRACT_TEST_MODULE__NAMESPACE:
				setNamespace((Namespace) newValue);
				return;
			case TestingPackage.ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX:
				setTestCaseSuffix((String) newValue);
				return;
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				setProject((Project) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case TestingPackage.ABSTRACT_TEST_MODULE__NAMESPACE:
				setNamespace((Namespace) null);
				return;
			case TestingPackage.ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX:
				setTestCaseSuffix(TEST_CASE_SUFFIX_EDEFAULT);
				return;
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				setProject((Project) null);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case TestingPackage.ABSTRACT_TEST_MODULE__NAMESPACE:
				return namespace != null;
			case TestingPackage.ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX:
				return testCaseSuffix != null;
			case TestingPackage.ABSTRACT_TEST_MODULE__PROJECT:
				return getProject() != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (testCaseSuffix: ");
		result.append(testCaseSuffix);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestSuites()
	 * @generated
	 */
	@Override
	public EList<TestSuite> getTestSuites() {
		final var testSuites = new BasicEList<TestSuite>();

		for (final JavaType type : getNamespace().getJavaTypes())
			if (type instanceof final TestSuite testSuite)
				testSuites.add(testSuite);

		return testSuites;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getTestCases()
	 * @generated
	 */
	@Override
	public EList<AbstractTestCase> getTestCases() {
		final var testCases = new BasicEList<AbstractTestCase>();

		for (final JavaType type : getNamespace().getJavaTypes())
			if (type instanceof final GUITestCase guiTestCase)
				testCases.add(guiTestCase);

		return testCases;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestModule#getArtifactType()
	 * @generated not
	 */
	@Override
	public BuildArtifactType getArtifactType() {
		if (this instanceof SeleniumTestModule)
			return BuildArtifactType.SELENIUM_TEST;

		return null;
	}

}
