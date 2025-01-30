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
package net.codecadenza.eclipse.model.java.impl;

import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Method Parameter</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#getMethod <em>Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#getType <em>Java Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#getModifier <em>Modifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#getHint <em>Hint</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class MethodParameterImpl extends EObjectImpl implements MethodParameter {
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
	 * The cached value of the '{@link #getType() <em>Java Type</em>}' reference
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected JavaType type;

	/**
	 * The default value of the '{@link #getModifier() <em>Modifier</em>}' attribute
	 * @see #getModifier()
	 * @generated
	 * @ordered
	 */
	protected static final JavaTypeModifierEnumeration MODIFIER_EDEFAULT = JavaTypeModifierEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getModifier() <em>Modifier</em>}' attribute
	 * @see #getModifier()
	 * @generated
	 * @ordered
	 */
	protected JavaTypeModifierEnumeration modifier = MODIFIER_EDEFAULT;

	/**
	 * The default value of the '{@link #getHint() <em>Hint</em>}' attribute
	 * @see #getHint()
	 * @generated
	 * @ordered
	 */
	protected static final String HINT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getHint() <em>Hint</em>}' attribute
	 * @see #getHint()
	 * @generated
	 * @ordered
	 */
	protected String hint = HINT_EDEFAULT;

	/**
	 * @generated
	 */
	protected MethodParameterImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JavaPackage.Literals.METHOD_PARAMETER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.METHOD_PARAMETER__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getMethod()
	 * @generated
	 */
	@Override
	public JavaMethod getMethod() {
		if (eContainerFeatureID() != JavaPackage.METHOD_PARAMETER__METHOD)
			return null;

		return (JavaMethod) eInternalContainer();
	}

	/**
	 * @param newMethod
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetMethod(JavaMethod newMethod, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newMethod, JavaPackage.METHOD_PARAMETER__METHOD, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#setMethod(net.codecadenza.eclipse.model.java.JavaMethod)
	 * @generated
	 */
	@Override
	public void setMethod(JavaMethod newMethod) {
		if (newMethod != eInternalContainer()
				|| (eContainerFeatureID() != JavaPackage.METHOD_PARAMETER__METHOD && newMethod != null)) {
			if (EcoreUtil.isAncestor(this, newMethod))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newMethod != null)
				msgs = ((InternalEObject) newMethod).eInverseAdd(this, JavaPackage.JAVA_METHOD__METHOD_PARAMETERS, JavaMethod.class,
						msgs);

			msgs = basicSetMethod(newMethod, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.METHOD_PARAMETER__METHOD, newMethod, newMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getType()
	 * @generated
	 */
	@Override
	public JavaType getType() {
		if (type != null && type.eIsProxy()) {
			final var oldType = (InternalEObject) type;
			type = (JavaType) eResolveProxy(oldType);

			if (type != oldType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, JavaPackage.METHOD_PARAMETER__TYPE, oldType, type));
		}

		return type;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaType basicGetType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#setType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setType(JavaType newType) {
		final JavaType oldType = type;
		type = newType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.METHOD_PARAMETER__TYPE, oldType, type));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getModifier()
	 * @generated
	 */
	@Override
	public JavaTypeModifierEnumeration getModifier() {
		return modifier;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#setModifier(net.codecadenza.eclipse.model.java.
	 * JavaTypeModifierEnumeration)
	 * @generated
	 */
	@Override
	public void setModifier(JavaTypeModifierEnumeration newModifier) {
		final JavaTypeModifierEnumeration oldModifier = modifier;
		modifier = newModifier == null ? MODIFIER_EDEFAULT : newModifier;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.METHOD_PARAMETER__MODIFIER, oldModifier, modifier));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getHint()
	 * @generated
	 */
	@Override
	public String getHint() {
		return hint;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#setHint(java.lang.String)
	 * @generated
	 */
	@Override
	public void setHint(String newHint) {
		final String oldHint = hint;
		hint = newHint;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.METHOD_PARAMETER__HINT, oldHint, hint));
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
			case JavaPackage.METHOD_PARAMETER__METHOD:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetMethod((JavaMethod) otherEnd, msgs);
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
			case JavaPackage.METHOD_PARAMETER__METHOD:
				return basicSetMethod(null, msgs);
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
			case JavaPackage.METHOD_PARAMETER__METHOD:
				return eInternalContainer().eInverseRemove(this, JavaPackage.JAVA_METHOD__METHOD_PARAMETERS, JavaMethod.class, msgs);
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
			case JavaPackage.METHOD_PARAMETER__NAME:
				return getName();
			case JavaPackage.METHOD_PARAMETER__METHOD:
				return getMethod();
			case JavaPackage.METHOD_PARAMETER__TYPE:
				if (resolve)
					return getType();

				return basicGetType();
			case JavaPackage.METHOD_PARAMETER__MODIFIER:
				return getModifier();
			case JavaPackage.METHOD_PARAMETER__HINT:
				return getHint();
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
			case JavaPackage.METHOD_PARAMETER__NAME:
				setName((String) newValue);
				return;
			case JavaPackage.METHOD_PARAMETER__METHOD:
				setMethod((JavaMethod) newValue);
				return;
			case JavaPackage.METHOD_PARAMETER__TYPE:
				setType((JavaType) newValue);
				return;
			case JavaPackage.METHOD_PARAMETER__MODIFIER:
				setModifier((JavaTypeModifierEnumeration) newValue);
				return;
			case JavaPackage.METHOD_PARAMETER__HINT:
				setHint((String) newValue);
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
			case JavaPackage.METHOD_PARAMETER__NAME:
				setName(NAME_EDEFAULT);
				return;
			case JavaPackage.METHOD_PARAMETER__METHOD:
				setMethod((JavaMethod) null);
				return;
			case JavaPackage.METHOD_PARAMETER__TYPE:
				setType((JavaType) null);
				return;
			case JavaPackage.METHOD_PARAMETER__MODIFIER:
				setModifier(MODIFIER_EDEFAULT);
				return;
			case JavaPackage.METHOD_PARAMETER__HINT:
				setHint(HINT_EDEFAULT);
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
			case JavaPackage.METHOD_PARAMETER__NAME:
				return name != null;
			case JavaPackage.METHOD_PARAMETER__METHOD:
				return getMethod() != null;
			case JavaPackage.METHOD_PARAMETER__TYPE:
				return type != null;
			case JavaPackage.METHOD_PARAMETER__MODIFIER:
				return modifier != MODIFIER_EDEFAULT;
			case JavaPackage.METHOD_PARAMETER__HINT:
				return hint != null;
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
		result.append(", modifier: ");
		result.append(modifier);
		result.append(", hint: ");
		result.append(hint);
		result.append(')');

		return result.toString();
	}

}
