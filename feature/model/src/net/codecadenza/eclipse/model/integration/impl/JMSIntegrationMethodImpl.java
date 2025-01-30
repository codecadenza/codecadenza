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
package net.codecadenza.eclipse.model.integration.impl;

import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>JMS Integration Method</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationMethodImpl#getOperationID <em>Operation ID</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationMethodImpl#isSendResponse <em>Send Response</em>}</li>
 * </ul>
 * @generated
 */
public class JMSIntegrationMethodImpl extends AbstractIntegrationMethodImpl implements JMSIntegrationMethod {
	/**
	 * The default value of the '{@link #getOperationID() <em>Operation ID</em>}' attribute
	 * @see #getOperationID()
	 * @generated
	 * @ordered
	 */
	protected static final String OPERATION_ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getOperationID() <em>Operation ID</em>}' attribute
	 * @see #getOperationID()
	 * @generated
	 * @ordered
	 */
	protected String operationID = OPERATION_ID_EDEFAULT;

	/**
	 * The default value of the '{@link #isSendResponse() <em>Send Response</em>}' attribute
	 * @see #isSendResponse()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SEND_RESPONSE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSendResponse() <em>Send Response</em>}' attribute
	 * @see #isSendResponse()
	 * @generated
	 * @ordered
	 */
	protected boolean sendResponse = SEND_RESPONSE_EDEFAULT;

	/**
	 * @generated
	 */
	protected JMSIntegrationMethodImpl() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.JMS_INTEGRATION_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#getOperationID()
	 * @generated
	 */
	@Override
	public String getOperationID() {
		return operationID;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#setOperationID(java.lang.String)
	 * @generated
	 */
	@Override
	public void setOperationID(String newOperationID) {
		final String oldOperationID = operationID;
		operationID = newOperationID;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.JMS_INTEGRATION_METHOD__OPERATION_ID,
					oldOperationID, operationID));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#isSendResponse()
	 * @generated
	 */
	@Override
	public boolean isSendResponse() {
		return sendResponse;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#setSendResponse(boolean)
	 * @generated
	 */
	@Override
	public void setSendResponse(boolean newSendResponse) {
		final boolean oldSendResponse = sendResponse;
		sendResponse = newSendResponse;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.JMS_INTEGRATION_METHOD__SEND_RESPONSE,
					oldSendResponse, sendResponse));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.JMS_INTEGRATION_METHOD__OPERATION_ID:
				return getOperationID();
			case IntegrationPackage.JMS_INTEGRATION_METHOD__SEND_RESPONSE:
				return isSendResponse();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case IntegrationPackage.JMS_INTEGRATION_METHOD__OPERATION_ID:
				setOperationID((String) newValue);
				return;
			case IntegrationPackage.JMS_INTEGRATION_METHOD__SEND_RESPONSE:
				setSendResponse((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case IntegrationPackage.JMS_INTEGRATION_METHOD__OPERATION_ID:
				setOperationID(OPERATION_ID_EDEFAULT);
				return;
			case IntegrationPackage.JMS_INTEGRATION_METHOD__SEND_RESPONSE:
				setSendResponse(SEND_RESPONSE_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case IntegrationPackage.JMS_INTEGRATION_METHOD__OPERATION_ID:
				return operationID != null;
			case IntegrationPackage.JMS_INTEGRATION_METHOD__SEND_RESPONSE:
				return sendResponse != SEND_RESPONSE_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (operationID: ");
		result.append(operationID);
		result.append(", sendResponse: ");
		result.append(sendResponse);
		result.append(')');
		return result.toString();
	}

}
