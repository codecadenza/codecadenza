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
package net.codecadenza.eclipse.model.project.impl;

import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>Persistence Unit Property</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.PersistenceUnitPropertyImpl#getValue <em>Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.PersistenceUnitPropertyImpl#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class PersistenceUnitPropertyImpl extends EObjectImpl implements PersistenceUnitProperty {
	/**
	 * The default value of the '{@link #getValue() <em>Value</em>}' attribute
	 * @see #getValue()
	 * @generated
	 * @ordered
	 */
	protected static final String VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getValue() <em>Value</em>}' attribute
	 * @see #getValue()
	 * @generated
	 * @ordered
	 */
	protected String value = VALUE_EDEFAULT;

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
	 * @generated
	 */
	protected PersistenceUnitPropertyImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.PERSISTENCE_UNIT_PROPERTY;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty#getValue()
	 * @generated
	 */
	@Override
	public String getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty#setValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setValue(String newValue) {
		final String oldValue = value;
		value = newValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PERSISTENCE_UNIT_PROPERTY__VALUE, oldValue, value));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.PERSISTENCE_UNIT_PROPERTY__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__VALUE:
				return getValue();
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__NAME:
				return getName();
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
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__VALUE:
				setValue((String) newValue);
				return;
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__NAME:
				setName((String) newValue);
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
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__VALUE:
				setValue(VALUE_EDEFAULT);
				return;
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__NAME:
				setName(NAME_EDEFAULT);
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
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__VALUE:
				return value != null;
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY__NAME:
				return name != null;
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
		result.append(" (value: ");
		result.append(value);
		result.append(", name: ");
		result.append(name);
		result.append(')');

		return result.toString();
	}

}
