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

import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Java Method</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#getComment <em>Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#getJavaType <em>Java Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#getReturnType <em>Return Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#getReturnTypeModifier <em>Return Type Modifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#getMethodParameters <em>Method Parameters</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class JavaMethodImpl extends EObjectImpl implements JavaMethod {
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
	 * The default value of the '{@link #getComment() <em>Comment</em>}' attribute
	 * @see #getComment()
	 * @generated
	 * @ordered
	 */
	protected static final String COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getComment() <em>Comment</em>}' attribute
	 * @see #getComment()
	 * @generated
	 * @ordered
	 */
	protected String comment = COMMENT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getJavaType() <em>Java Type</em>}' reference
	 * @see #getJavaType()
	 * @generated
	 * @ordered
	 */
	protected JavaType javaType;

	/**
	 * The cached value of the '{@link #getReturnType() <em>Return Type</em>}' reference
	 * @see #getReturnType()
	 * @generated
	 * @ordered
	 */
	protected JavaType returnType;

	/**
	 * The default value of the '{@link #getReturnTypeModifier() <em>Return Type Modifier</em>}' attribute
	 * @see #getReturnTypeModifier()
	 * @generated
	 * @ordered
	 */
	protected static final JavaTypeModifierEnumeration RETURN_TYPE_MODIFIER_EDEFAULT = JavaTypeModifierEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getReturnTypeModifier() <em>Return Type Modifier</em>}' attribute
	 * @see #getReturnTypeModifier()
	 * @generated
	 * @ordered
	 */
	protected JavaTypeModifierEnumeration returnTypeModifier = RETURN_TYPE_MODIFIER_EDEFAULT;

	/**
	 * The cached value of the '{@link #getMethodParameters() <em>Method Parameters</em>}' containment reference list
	 * @see #getMethodParameters()
	 * @generated
	 * @ordered
	 */
	protected EList<MethodParameter> methodParameters;

	/**
	 * @generated
	 */
	protected JavaMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JavaPackage.Literals.JAVA_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_METHOD__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getComment()
	 * @generated
	 */
	@Override
	public String getComment() {
		return comment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#setComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setComment(String newComment) {
		final String oldComment = comment;
		comment = newComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_METHOD__COMMENT, oldComment, comment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getJavaType()
	 * @generated
	 */
	@Override
	public JavaType getJavaType() {
		if (javaType != null && javaType.eIsProxy()) {
			final var oldJavaType = (InternalEObject) javaType;
			javaType = (JavaType) eResolveProxy(oldJavaType);

			if (javaType != oldJavaType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, JavaPackage.JAVA_METHOD__JAVA_TYPE, oldJavaType, javaType));
		}

		return javaType;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaType basicGetJavaType() {
		return javaType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#setJavaType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setJavaType(JavaType newJavaType) {
		final JavaType oldJavaType = javaType;
		javaType = newJavaType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_METHOD__JAVA_TYPE, oldJavaType, javaType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getReturnType()
	 * @generated
	 */
	@Override
	public JavaType getReturnType() {
		if (returnType != null && returnType.eIsProxy()) {
			final var oldReturnType = (InternalEObject) returnType;
			returnType = (JavaType) eResolveProxy(oldReturnType);

			if (returnType != oldReturnType && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, JavaPackage.JAVA_METHOD__RETURN_TYPE, oldReturnType, returnType));
		}

		return returnType;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaType basicGetReturnType() {
		return returnType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#setReturnType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setReturnType(JavaType newReturnType) {
		final JavaType oldReturnType = returnType;
		returnType = newReturnType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_METHOD__RETURN_TYPE, oldReturnType, returnType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getReturnTypeModifier()
	 * @generated
	 */
	@Override
	public JavaTypeModifierEnumeration getReturnTypeModifier() {
		return returnTypeModifier;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#setReturnTypeModifier(net.codecadenza.eclipse.model.java.
	 * JavaTypeModifierEnumeration)
	 * @generated
	 */
	@Override
	public void setReturnTypeModifier(JavaTypeModifierEnumeration newReturnTypeModifier) {
		final JavaTypeModifierEnumeration oldReturnTypeModifier = returnTypeModifier;
		returnTypeModifier = newReturnTypeModifier == null ? RETURN_TYPE_MODIFIER_EDEFAULT : newReturnTypeModifier;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_METHOD__RETURN_TYPE_MODIFIER, oldReturnTypeModifier,
					returnTypeModifier));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getMethodParameters()
	 * @generated
	 */
	@Override
	public EList<MethodParameter> getMethodParameters() {
		if (methodParameters == null)
			methodParameters = new EObjectContainmentWithInverseEList<>(MethodParameter.class, this,
					JavaPackage.JAVA_METHOD__METHOD_PARAMETERS, JavaPackage.METHOD_PARAMETER__METHOD);

		return methodParameters;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case JavaPackage.JAVA_METHOD__METHOD_PARAMETERS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getMethodParameters()).basicAdd(otherEnd, msgs);
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
			case JavaPackage.JAVA_METHOD__METHOD_PARAMETERS:
				return ((InternalEList<?>) getMethodParameters()).basicRemove(otherEnd, msgs);
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
			case JavaPackage.JAVA_METHOD__NAME:
				return getName();
			case JavaPackage.JAVA_METHOD__COMMENT:
				return getComment();
			case JavaPackage.JAVA_METHOD__JAVA_TYPE:
				if (resolve)
					return getJavaType();

				return basicGetJavaType();
			case JavaPackage.JAVA_METHOD__RETURN_TYPE:
				if (resolve)
					return getReturnType();

				return basicGetReturnType();
			case JavaPackage.JAVA_METHOD__RETURN_TYPE_MODIFIER:
				return getReturnTypeModifier();
			case JavaPackage.JAVA_METHOD__METHOD_PARAMETERS:
				return getMethodParameters();
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
			case JavaPackage.JAVA_METHOD__NAME:
				setName((String) newValue);
				return;
			case JavaPackage.JAVA_METHOD__COMMENT:
				setComment((String) newValue);
				return;
			case JavaPackage.JAVA_METHOD__JAVA_TYPE:
				setJavaType((JavaType) newValue);
				return;
			case JavaPackage.JAVA_METHOD__RETURN_TYPE:
				setReturnType((JavaType) newValue);
				return;
			case JavaPackage.JAVA_METHOD__RETURN_TYPE_MODIFIER:
				setReturnTypeModifier((JavaTypeModifierEnumeration) newValue);
				return;
			case JavaPackage.JAVA_METHOD__METHOD_PARAMETERS:
				getMethodParameters().clear();
				getMethodParameters().addAll((Collection<? extends MethodParameter>) newValue);
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
			case JavaPackage.JAVA_METHOD__NAME:
				setName(NAME_EDEFAULT);
				return;
			case JavaPackage.JAVA_METHOD__COMMENT:
				setComment(COMMENT_EDEFAULT);
				return;
			case JavaPackage.JAVA_METHOD__JAVA_TYPE:
				setJavaType((JavaType) null);
				return;
			case JavaPackage.JAVA_METHOD__RETURN_TYPE:
				setReturnType((JavaType) null);
				return;
			case JavaPackage.JAVA_METHOD__RETURN_TYPE_MODIFIER:
				setReturnTypeModifier(RETURN_TYPE_MODIFIER_EDEFAULT);
				return;
			case JavaPackage.JAVA_METHOD__METHOD_PARAMETERS:
				getMethodParameters().clear();
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
			case JavaPackage.JAVA_METHOD__NAME:
				return name != null;
			case JavaPackage.JAVA_METHOD__COMMENT:
				return comment != null;
			case JavaPackage.JAVA_METHOD__JAVA_TYPE:
				return javaType != null;
			case JavaPackage.JAVA_METHOD__RETURN_TYPE:
				return returnType != null;
			case JavaPackage.JAVA_METHOD__RETURN_TYPE_MODIFIER:
				return returnTypeModifier != RETURN_TYPE_MODIFIER_EDEFAULT;
			case JavaPackage.JAVA_METHOD__METHOD_PARAMETERS:
				return methodParameters != null && !methodParameters.isEmpty();
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
		result.append(", comment: ");
		result.append(comment);
		result.append(", returnTypeModifier: ");
		result.append(returnTypeModifier);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getFirstParameter()
	 * @generated not
	 */
	@Override
	public MethodParameter getFirstParameter() {
		return getFirstParameter(true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getFirstParameter(boolean)
	 * @generated not
	 */
	@Override
	public MethodParameter getFirstParameter(boolean mustExist) {
		final Optional<MethodParameter> param = getMethodParameters().stream().findFirst();

		if (param.isPresent())
			return param.get();

		if (mustExist)
			throw new IllegalStateException("The method '" + getName() + "' has no parameters!");

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#generateBeginOfJavadocComment(boolean)
	 * @generated not
	 */
	@Override
	public String generateBeginOfJavadocComment(boolean addParameters) {
		final var b = new StringBuilder();
		b.append("/**\n");

		if (getComment() != null && !getComment().isEmpty())
			Stream.of(getComment().split("\n")).forEach(line -> b.append(" * " + line + "\n"));

		if (addParameters)
			getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#generateBeginOfJavadocComment()
	 * @generated not
	 */
	@Override
	public String generateBeginOfJavadocComment() {
		return generateBeginOfJavadocComment(false);
	}

}
