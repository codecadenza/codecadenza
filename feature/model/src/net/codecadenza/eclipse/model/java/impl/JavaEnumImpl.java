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

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_DOMAIN_FOLDER;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.EnumTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Enum</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaEnumImpl#getEnumerationValues <em>Enumeration Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaEnumImpl#getTag <em>Tag</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class JavaEnumImpl extends JavaTypeImpl implements JavaEnum {
	/**
	 * The cached value of the '{@link #getEnumerationValues() <em>Enumeration Values</em>}' containment reference list
	 * @see #getEnumerationValues()
	 * @generated
	 * @ordered
	 */
	protected EList<EnumLiteral> enumerationValues;

	/**
	 * The default value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected static final EnumTagEnumeration TAG_EDEFAULT = EnumTagEnumeration.NONE;
	/**
	 * The cached value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected EnumTagEnumeration tag = TAG_EDEFAULT;

	/**
	 * @generated
	 */
	protected JavaEnumImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JavaPackage.Literals.JAVA_ENUM;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getEnumerationValues()
	 * @generated
	 */
	@Override
	public EList<EnumLiteral> getEnumerationValues() {
		if (enumerationValues == null)
			enumerationValues = new EObjectContainmentWithInverseEList<>(EnumLiteral.class, this,
					JavaPackage.JAVA_ENUM__ENUMERATION_VALUES, JavaPackage.ENUM_LITERAL__JAVA_ENUM);

		return enumerationValues;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getTag()
	 * @generated
	 */
	@Override
	public EnumTagEnumeration getTag() {
		return tag;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#setTag(net.codecadenza.eclipse.model.java.EnumTagEnumeration)
	 * @generated
	 */
	@Override
	public void setTag(EnumTagEnumeration newTag) {
		final EnumTagEnumeration oldTag = tag;
		tag = newTag == null ? TAG_EDEFAULT : newTag;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_ENUM__TAG, oldTag, tag));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case JavaPackage.JAVA_ENUM__ENUMERATION_VALUES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getEnumerationValues()).basicAdd(otherEnd, msgs);
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
			case JavaPackage.JAVA_ENUM__ENUMERATION_VALUES:
				return ((InternalEList<?>) getEnumerationValues()).basicRemove(otherEnd, msgs);
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
			case JavaPackage.JAVA_ENUM__ENUMERATION_VALUES:
				return getEnumerationValues();
			case JavaPackage.JAVA_ENUM__TAG:
				return getTag();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case JavaPackage.JAVA_ENUM__ENUMERATION_VALUES:
				getEnumerationValues().clear();
				getEnumerationValues().addAll((Collection<? extends EnumLiteral>) newValue);
				return;
			case JavaPackage.JAVA_ENUM__TAG:
				setTag((EnumTagEnumeration) newValue);
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
			case JavaPackage.JAVA_ENUM__ENUMERATION_VALUES:
				getEnumerationValues().clear();
				return;
			case JavaPackage.JAVA_ENUM__TAG:
				setTag(TAG_EDEFAULT);
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
			case JavaPackage.JAVA_ENUM__ENUMERATION_VALUES:
				return enumerationValues != null && !enumerationValues.isEmpty();
			case JavaPackage.JAVA_ENUM__TAG:
				return tag != TAG_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (tag: ");
		result.append(tag);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getValidLiteralsTagsOfEnum()
	 * @generated not
	 */
	@Override
	public ArrayList<String> getValidLiteralsTagsOfEnum() {
		final EnumSet<EnumLiteralTagEnumeration> existingTags = EnumSet.noneOf(EnumLiteralTagEnumeration.class);
		final var list = new ArrayList<String>();
		list.add(EnumLiteralTagEnumeration.NONE.getName());

		if (getTag() == EnumTagEnumeration.NONE)
			return list;

		for (final EnumLiteral literal : getEnumerationValues())
			if (literal.getTag() != EnumLiteralTagEnumeration.NONE)
				existingTags.add(literal.getTag());

		// Just add literal tags that belong to enumeration. Check via name!
		for (final EnumLiteralTagEnumeration item : EnumLiteralTagEnumeration.values())
			if (item.getName().startsWith(getTag().getName()) && !existingTags.contains(item))
				list.add(item.getName());

		return list;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.SHARED, name, getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getTypeScriptSourceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getTypeScriptSourceFile() {
		final Project project = getNamespace().getProject();

		if (!project.hasAngularClient())
			return null;

		final var path = ANGULAR_DOMAIN_FOLDER + "/" + name.toLowerCase() + ".enum.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

}
