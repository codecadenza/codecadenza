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
package net.codecadenza.eclipse.model.service.impl;

import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Asynchronous Invocation</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.AsynchronousInvocationImpl#getDelayInMilliseconds <em>Delay In
 * Milliseconds</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class AsynchronousInvocationImpl extends MethodInvocationImpl implements AsynchronousInvocation {
	/**
	 * The default value of the '{@link #getDelayInMilliseconds() <em>Delay In Milliseconds</em>}' attribute
	 * @see #getDelayInMilliseconds()
	 * @generated
	 * @ordered
	 */
	protected static final Integer DELAY_IN_MILLISECONDS_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDelayInMilliseconds() <em>Delay In Milliseconds</em>}' attribute
	 * @see #getDelayInMilliseconds()
	 * @generated
	 * @ordered
	 */
	protected Integer delayInMilliseconds = DELAY_IN_MILLISECONDS_EDEFAULT;

	/**
	 * @generated
	 */
	protected AsynchronousInvocationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ServicePackage.Literals.ASYNCHRONOUS_INVOCATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.AsynchronousInvocation#getDelayInMilliseconds()
	 * @generated
	 */
	@Override
	public Integer getDelayInMilliseconds() {
		return delayInMilliseconds;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.AsynchronousInvocation#setDelayInMilliseconds(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setDelayInMilliseconds(Integer newDelayInMilliseconds) {
		final Integer oldDelayInMilliseconds = delayInMilliseconds;
		delayInMilliseconds = newDelayInMilliseconds;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS,
					oldDelayInMilliseconds, delayInMilliseconds));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ServicePackage.ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS:
				return getDelayInMilliseconds();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ServicePackage.ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS:
				setDelayInMilliseconds((Integer) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ServicePackage.ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS:
				setDelayInMilliseconds(DELAY_IN_MILLISECONDS_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ServicePackage.ASYNCHRONOUS_INVOCATION__DELAY_IN_MILLISECONDS:
				return delayInMilliseconds != null;
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
		result.append(" (delayInMilliseconds: ");
		result.append(delayInMilliseconds);
		result.append(')');

		return result.toString();
	}

}
