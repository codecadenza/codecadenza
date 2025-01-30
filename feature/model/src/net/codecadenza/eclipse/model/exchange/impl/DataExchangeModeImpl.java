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
package net.codecadenza.eclipse.model.exchange.impl;

import net.codecadenza.eclipse.model.exchange.DataExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>Data Exchange Mode</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#getMaxObjectsToBeProcessed <em>Max Objects To Be
 * Processed</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public abstract class DataExchangeModeImpl extends EObjectImpl implements DataExchangeMode {
	/**
	 * The default value of the '{@link #getMaxObjectsToBeProcessed() <em>Max Objects To Be Processed</em>}' attribute
	 * @see #getMaxObjectsToBeProcessed()
	 * @generated
	 * @ordered
	 */
	protected static final Integer MAX_OBJECTS_TO_BE_PROCESSED_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMaxObjectsToBeProcessed() <em>Max Objects To Be Processed</em>}' attribute
	 * @see #getMaxObjectsToBeProcessed()
	 * @generated
	 * @ordered
	 */
	protected Integer maxObjectsToBeProcessed = MAX_OBJECTS_TO_BE_PROCESSED_EDEFAULT;

	/**
	 * @generated
	 */
	protected DataExchangeModeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.DATA_EXCHANGE_MODE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMode#getMaxObjectsToBeProcessed()
	 * @generated
	 */
	@Override
	public Integer getMaxObjectsToBeProcessed() {
		return maxObjectsToBeProcessed;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMode#setMaxObjectsToBeProcessed(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setMaxObjectsToBeProcessed(Integer newMaxObjectsToBeProcessed) {
		final Integer oldMaxObjectsToBeProcessed = maxObjectsToBeProcessed;
		maxObjectsToBeProcessed = newMaxObjectsToBeProcessed;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED,
					oldMaxObjectsToBeProcessed, maxObjectsToBeProcessed));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED:
				return getMaxObjectsToBeProcessed();
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
			case ExchangePackage.DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED:
				setMaxObjectsToBeProcessed((Integer) newValue);
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
			case ExchangePackage.DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED:
				setMaxObjectsToBeProcessed(MAX_OBJECTS_TO_BE_PROCESSED_EDEFAULT);
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
			case ExchangePackage.DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED:
				return maxObjectsToBeProcessed != null;
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
		result.append(" (maxObjectsToBeProcessed: ");
		result.append(maxObjectsToBeProcessed);
		result.append(')');

		return result.toString();
	}

}
