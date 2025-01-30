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
package net.codecadenza.eclipse.model.db.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * An implementation of the model object '<em><b>DB Column Type</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl#getJavaTypes <em>Java Types</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.impl.DBColumnTypeImpl#isOmitSizeInformation <em>Omit Size Information</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DBColumnTypeImpl extends EObjectImpl implements DBColumnType {
	/**
	 * The cached value of the '{@link #getJavaTypes() <em>Java Types</em>}' reference list
	 * @see #getJavaTypes()
	 * @generated
	 * @ordered
	 */
	protected EList<JavaType> javaTypes;

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
	 * The default value of the '{@link #isOmitSizeInformation() <em>Omit Size Information</em>}' attribute
	 * @see #isOmitSizeInformation()
	 * @generated
	 * @ordered
	 */
	protected static final boolean OMIT_SIZE_INFORMATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isOmitSizeInformation() <em>Omit Size Information</em>}' attribute
	 * @see #isOmitSizeInformation()
	 * @generated
	 * @ordered
	 */
	protected boolean omitSizeInformation = OMIT_SIZE_INFORMATION_EDEFAULT;

	/**
	 * @generated
	 */
	protected DBColumnTypeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DbPackage.Literals.DB_COLUMN_TYPE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN_TYPE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#isOmitSizeInformation()
	 * @generated
	 */
	@Override
	public boolean isOmitSizeInformation() {
		return omitSizeInformation;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#setOmitSizeInformation(boolean)
	 * @generated
	 */
	@Override
	public void setOmitSizeInformation(boolean newOmitSizeInformation) {
		final boolean oldOmitSizeInformation = omitSizeInformation;
		omitSizeInformation = newOmitSizeInformation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DbPackage.DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION,
					oldOmitSizeInformation, omitSizeInformation));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DBColumnType#getJavaTypes()
	 * @generated
	 */
	@Override
	public EList<JavaType> getJavaTypes() {
		if (javaTypes == null)
			javaTypes = new EObjectResolvingEList<>(JavaType.class, this, DbPackage.DB_COLUMN_TYPE__JAVA_TYPES);

		return javaTypes;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DbPackage.DB_COLUMN_TYPE__JAVA_TYPES:
				return getJavaTypes();
			case DbPackage.DB_COLUMN_TYPE__NAME:
				return getName();
			case DbPackage.DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION:
				return isOmitSizeInformation();
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
			case DbPackage.DB_COLUMN_TYPE__JAVA_TYPES:
				getJavaTypes().clear();
				getJavaTypes().addAll((Collection<? extends JavaType>) newValue);
				return;
			case DbPackage.DB_COLUMN_TYPE__NAME:
				setName((String) newValue);
				return;
			case DbPackage.DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION:
				setOmitSizeInformation((Boolean) newValue);
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
			case DbPackage.DB_COLUMN_TYPE__JAVA_TYPES:
				getJavaTypes().clear();
				return;
			case DbPackage.DB_COLUMN_TYPE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DbPackage.DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION:
				setOmitSizeInformation(OMIT_SIZE_INFORMATION_EDEFAULT);
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
			case DbPackage.DB_COLUMN_TYPE__JAVA_TYPES:
				return javaTypes != null && !javaTypes.isEmpty();
			case DbPackage.DB_COLUMN_TYPE__NAME:
				return name != null;
			case DbPackage.DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION:
				return omitSizeInformation != OMIT_SIZE_INFORMATION_EDEFAULT;
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
		result.append(", omitSizeInformation: ");
		result.append(omitSizeInformation);
		result.append(')');

		return result.toString();
	}

}
