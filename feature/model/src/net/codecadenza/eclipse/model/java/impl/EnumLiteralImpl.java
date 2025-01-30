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

import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Enum Literal</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl#getTag <em>Tag</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl#getJavaEnum <em>Java Enum</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class EnumLiteralImpl extends EObjectImpl implements EnumLiteral {
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
	 * The default value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected static final EnumLiteralTagEnumeration TAG_EDEFAULT = EnumLiteralTagEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected EnumLiteralTagEnumeration tag = TAG_EDEFAULT;

	/**
	 * @generated
	 */
	protected EnumLiteralImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JavaPackage.Literals.ENUM_LITERAL;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.ENUM_LITERAL__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getTag()
	 * @generated
	 */
	@Override
	public EnumLiteralTagEnumeration getTag() {
		return tag;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#setTag(net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration)
	 * @generated
	 */
	@Override
	public void setTag(EnumLiteralTagEnumeration newTag) {
		final EnumLiteralTagEnumeration oldTag = tag;
		tag = newTag == null ? TAG_EDEFAULT : newTag;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.ENUM_LITERAL__TAG, oldTag, tag));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum()
	 * @generated
	 */
	@Override
	public JavaEnum getJavaEnum() {
		if (eContainerFeatureID() != JavaPackage.ENUM_LITERAL__JAVA_ENUM)
			return null;

		return (JavaEnum) eInternalContainer();
	}

	/**
	 * @param newJavaEnum
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetJavaEnum(JavaEnum newJavaEnum, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newJavaEnum, JavaPackage.ENUM_LITERAL__JAVA_ENUM, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#setJavaEnum(net.codecadenza.eclipse.model.java.JavaEnum)
	 * @generated
	 */
	@Override
	public void setJavaEnum(JavaEnum newJavaEnum) {
		if (newJavaEnum != eInternalContainer()
				|| (eContainerFeatureID() != JavaPackage.ENUM_LITERAL__JAVA_ENUM && newJavaEnum != null)) {
			if (EcoreUtil.isAncestor(this, newJavaEnum))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newJavaEnum != null)
				msgs = ((InternalEObject) newJavaEnum).eInverseAdd(this, JavaPackage.JAVA_ENUM__ENUMERATION_VALUES, JavaEnum.class, msgs);

			msgs = basicSetJavaEnum(newJavaEnum, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.ENUM_LITERAL__JAVA_ENUM, newJavaEnum, newJavaEnum));
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
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetJavaEnum((JavaEnum) otherEnd, msgs);
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
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				return basicSetJavaEnum(null, msgs);
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
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				return eInternalContainer().eInverseRemove(this, JavaPackage.JAVA_ENUM__ENUMERATION_VALUES, JavaEnum.class, msgs);
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
			case JavaPackage.ENUM_LITERAL__NAME:
				return getName();
			case JavaPackage.ENUM_LITERAL__TAG:
				return getTag();
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				return getJavaEnum();
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
			case JavaPackage.ENUM_LITERAL__NAME:
				setName((String) newValue);
				return;
			case JavaPackage.ENUM_LITERAL__TAG:
				setTag((EnumLiteralTagEnumeration) newValue);
				return;
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				setJavaEnum((JavaEnum) newValue);
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
			case JavaPackage.ENUM_LITERAL__NAME:
				setName(NAME_EDEFAULT);
				return;
			case JavaPackage.ENUM_LITERAL__TAG:
				setTag(TAG_EDEFAULT);
				return;
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				setJavaEnum((JavaEnum) null);
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
			case JavaPackage.ENUM_LITERAL__NAME:
				return name != null;
			case JavaPackage.ENUM_LITERAL__TAG:
				return tag != TAG_EDEFAULT;
			case JavaPackage.ENUM_LITERAL__JAVA_ENUM:
				return getJavaEnum() != null;
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
		result.append(", tag: ");
		result.append(tag);
		result.append(')');

		return result.toString();
	}

}
