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
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Method Invocation Parameter</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl#getParameterValues <em>Parameter
 * Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.MethodInvocationParameterImpl#isRepresentsList <em>Represents
 * List</em>}</li>
 * </ul>
 * @generated
 */
public class MethodInvocationParameterImpl extends EObjectImpl implements MethodInvocationParameter {
	/**
	 * The cached value of the '{@link #getParameterValues() <em>Parameter Values</em>}' containment reference list
	 * @see #getParameterValues()
	 * @generated
	 * @ordered
	 */
	protected EList<TestDataObject> parameterValues;

	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' reference
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected JavaType type;

	/**
	 * The default value of the '{@link #isRepresentsList() <em>Represents List</em>}' attribute
	 * @see #isRepresentsList()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REPRESENTS_LIST_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRepresentsList() <em>Represents List</em>}' attribute
	 * @see #isRepresentsList()
	 * @generated
	 * @ordered
	 */
	protected boolean representsList = REPRESENTS_LIST_EDEFAULT;

	/**
	 * @generated
	 */
	protected MethodInvocationParameterImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.METHOD_INVOCATION_PARAMETER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getParameterValues()
	 * @generated
	 */
	@Override
	public EList<TestDataObject> getParameterValues() {
		if (parameterValues == null)
			parameterValues = new EObjectContainmentEList<>(TestDataObject.class, this,
					TestingPackage.METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES);

		return parameterValues;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.METHOD_INVOCATION_PARAMETER__NAME, oldName, name));
	}

	/**
	 * @return the parameter type
	 * @generated
	 */
	public JavaType basicGetType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getType()
	 * @generated
	 */
	@Override
	public JavaType getType() {
		if (type != null && type.eIsProxy()) {
			final InternalEObject oldType = (InternalEObject) type;
			type = (JavaType) eResolveProxy(oldType);

			if ((type != oldType) && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.METHOD_INVOCATION_PARAMETER__TYPE, oldType, type));
		}

		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#setType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setType(JavaType newType) {
		final JavaType oldType = type;
		type = newType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.METHOD_INVOCATION_PARAMETER__TYPE, oldType, type));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#isRepresentsList()
	 * @generated
	 */
	@Override
	public boolean isRepresentsList() {
		return representsList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.MethodInvocationParameter#setRepresentsList(boolean)
	 * @generated
	 */
	@Override
	public void setRepresentsList(boolean newRepresentsList) {
		final boolean oldRepresentsList = representsList;
		representsList = newRepresentsList;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST,
					oldRepresentsList, representsList));
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
			case TestingPackage.METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES:
				return ((InternalEList<?>) getParameterValues()).basicRemove(otherEnd, msgs);
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
			case TestingPackage.METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES:
				return getParameterValues();
			case TestingPackage.METHOD_INVOCATION_PARAMETER__NAME:
				return getName();
			case TestingPackage.METHOD_INVOCATION_PARAMETER__TYPE:
				if (resolve)
					return getType();
				return basicGetType();
			case TestingPackage.METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST:
				return isRepresentsList();
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
			case TestingPackage.METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES:
				getParameterValues().clear();
				getParameterValues().addAll((Collection<? extends TestDataObject>) newValue);
				return;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__NAME:
				setName((String) newValue);
				return;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__TYPE:
				setType((JavaType) newValue);
				return;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST:
				setRepresentsList((Boolean) newValue);
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
			case TestingPackage.METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES:
				getParameterValues().clear();
				return;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__NAME:
				setName(NAME_EDEFAULT);
				return;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__TYPE:
				setType((JavaType) null);
				return;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST:
				setRepresentsList(REPRESENTS_LIST_EDEFAULT);
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
			case TestingPackage.METHOD_INVOCATION_PARAMETER__PARAMETER_VALUES:
				return parameterValues != null && !parameterValues.isEmpty();
			case TestingPackage.METHOD_INVOCATION_PARAMETER__NAME:
				return name != null;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__TYPE:
				return type != null;
			case TestingPackage.METHOD_INVOCATION_PARAMETER__REPRESENTS_LIST:
				return representsList != REPRESENTS_LIST_EDEFAULT;
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
		result.append(" (name: ");
		result.append(name);
		result.append(", representsList: ");
		result.append(representsList);
		result.append(')');

		return result.toString();
	}

}
