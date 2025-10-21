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
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Integration Test Case</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl#getUserName <em>User Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl#getPassword <em>Password</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestCaseImpl#getMethodInvocations <em>Method
 * Invocations</em>}</li>
 * </ul>
 * @generated
 */
public class IntegrationTestCaseImpl extends AbstractTestCaseImpl implements IntegrationTestCase {
	/**
	 * The default value of the '{@link #getUserName() <em>User Name</em>}' attribute
	 * @see #getUserName()
	 * @generated
	 * @ordered
	 */
	protected static final String USER_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUserName() <em>User Name</em>}' attribute
	 * @see #getUserName()
	 * @generated
	 * @ordered
	 */
	protected String userName = USER_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getPassword() <em>Password</em>}' attribute
	 * @see #getPassword()
	 * @generated
	 * @ordered
	 */
	protected static final String PASSWORD_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPassword() <em>Password</em>}' attribute
	 * @see #getPassword()
	 * @generated
	 * @ordered
	 */
	protected String password = PASSWORD_EDEFAULT;

	/**
	 * The cached value of the '{@link #getMethodInvocations() <em>Method Invocations</em>}' containment reference list
	 * @see #getMethodInvocations()
	 * @generated
	 * @ordered
	 */
	protected EList<IntegrationMethodTestInvocation> methodInvocations;

	/**
	 * @generated
	 */
	protected IntegrationTestCaseImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.INTEGRATION_TEST_CASE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#getUserName()
	 * @generated
	 */
	@Override
	public String getUserName() {
		return userName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#setUserName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setUserName(String newUserName) {
		final String oldUserName = userName;
		userName = newUserName;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_CASE__USER_NAME, oldUserName, userName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#getPassword()
	 * @generated
	 */
	@Override
	public String getPassword() {
		return password;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#setPassword(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPassword(String newPassword) {
		final String oldPassword = password;
		password = newPassword;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_CASE__PASSWORD, oldPassword, password));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#getMethodInvocations()
	 * @generated
	 */
	@Override
	public EList<IntegrationMethodTestInvocation> getMethodInvocations() {
		if (methodInvocations == null)
			methodInvocations = new EObjectContainmentEList<>(IntegrationMethodTestInvocation.class, this,
					TestingPackage.INTEGRATION_TEST_CASE__METHOD_INVOCATIONS);

		return methodInvocations;
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
			case TestingPackage.INTEGRATION_TEST_CASE__METHOD_INVOCATIONS:
				return ((InternalEList<?>) getMethodInvocations()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_TEST_CASE__USER_NAME:
				return getUserName();
			case TestingPackage.INTEGRATION_TEST_CASE__PASSWORD:
				return getPassword();
			case TestingPackage.INTEGRATION_TEST_CASE__METHOD_INVOCATIONS:
				return getMethodInvocations();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_TEST_CASE__USER_NAME:
				setUserName((String) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_CASE__PASSWORD:
				setPassword((String) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_CASE__METHOD_INVOCATIONS:
				getMethodInvocations().clear();
				getMethodInvocations().addAll((Collection<? extends IntegrationMethodTestInvocation>) newValue);
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
			case TestingPackage.INTEGRATION_TEST_CASE__USER_NAME:
				setUserName(USER_NAME_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_CASE__PASSWORD:
				setPassword(PASSWORD_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_CASE__METHOD_INVOCATIONS:
				getMethodInvocations().clear();
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
			case TestingPackage.INTEGRATION_TEST_CASE__USER_NAME:
				return userName != null;
			case TestingPackage.INTEGRATION_TEST_CASE__PASSWORD:
				return password != null;
			case TestingPackage.INTEGRATION_TEST_CASE__METHOD_INVOCATIONS:
				return methodInvocations != null && !methodInvocations.isEmpty();
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

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (userName: ");
		result.append(userName);
		result.append(", password: ");
		result.append(password);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestCase#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final BuildArtifactType artifactType = getTestModule().getArtifactType();

		final var javaFile = new JavaTestFile(getNamespace().getProject(), artifactType, name, getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#isFileHandlingRequired()
	 * @generated not
	 */
	@Override
	public boolean isFileHandlingRequired() {
		return getMethodInvocations().stream().anyMatch(IntegrationMethodTestInvocation::isFileHandlingRequired);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestCase#addCredentials()
	 * @generated not
	 */
	@Override
	public boolean addCredentials() {
		return getUserName() != null && !getUserName().isEmpty() && getPassword() != null && !getPassword().isEmpty();
	}

}
