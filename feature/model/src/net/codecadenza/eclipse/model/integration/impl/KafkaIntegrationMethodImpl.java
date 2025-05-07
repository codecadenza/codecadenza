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
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Kafka Integration Method</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl#getRequestSchemaName <em>Request Schema
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl#getResponseSchemaName <em>Response Schema
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl#isUseDedicatedPartition() <em>Use
 * Dedicated Partition</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationMethodImpl#isSendResponse <em>Send
 * Response</em>}</li>
 * </ul>
 * @generated
 */
public class KafkaIntegrationMethodImpl extends AbstractIntegrationMethodImpl implements KafkaIntegrationMethod {
	/**
	 * The default value of the '{@link #getRequestSchemaName() <em>Request Schema Name</em>}' attribute
	 * @see #getRequestSchemaName()
	 * @generated
	 * @ordered
	 */
	protected static final String REQUEST_SCHEMA_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRequestSchemaName() <em>Request Schema Name</em>}' attribute
	 * @see #getRequestSchemaName()
	 * @generated
	 * @ordered
	 */
	protected String requestSchemaName = REQUEST_SCHEMA_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getResponseSchemaName() <em>Response Schema Name</em>}' attribute
	 * @see #getResponseSchemaName()
	 * @generated
	 * @ordered
	 */
	protected static final String RESPONSE_SCHEMA_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getResponseSchemaName() <em>Response Schema Name</em>}' attribute
	 * @see #getResponseSchemaName()
	 * @generated
	 * @ordered
	 */
	protected String responseSchemaName = RESPONSE_SCHEMA_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #isUseDedicatedPartition() <em>Use Dedicated Partition</em>}' attribute
	 * @see #isUseDedicatedPartition()
	 * @generated
	 * @ordered
	 */
	protected static final boolean USE_DEDICATED_PARTITION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUseDedicatedPartition() <em>Use Dedicated Partition</em>}' attribute
	 * @see #isUseDedicatedPartition()
	 * @generated
	 * @ordered
	 */
	protected boolean useDedicatedPartition = USE_DEDICATED_PARTITION_EDEFAULT;

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
	protected KafkaIntegrationMethodImpl() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.KAFKA_INTEGRATION_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getRequestSchemaName()
	 * @generated
	 */
	@Override
	public String getRequestSchemaName() {
		return requestSchemaName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#setRequestSchemaName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setRequestSchemaName(String newRequestSchemaName) {
		final String oldRequestSchemaName = requestSchemaName;
		requestSchemaName = newRequestSchemaName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME,
					oldRequestSchemaName, requestSchemaName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getResponseSchemaName()
	 * @generated
	 */
	@Override
	public String getResponseSchemaName() {
		return responseSchemaName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#setResponseSchemaName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setResponseSchemaName(String newResponseSchemaName) {
		final String oldResponseSchemaName = responseSchemaName;
		responseSchemaName = newResponseSchemaName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME,
					oldResponseSchemaName, responseSchemaName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isUseDedicatedPartition()
	 * @generated
	 */
	@Override
	public boolean isUseDedicatedPartition() {
		return useDedicatedPartition;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#setUseDedicatedPartition(boolean)
	 * @generated
	 */
	@Override
	public void setUseDedicatedPartition(boolean newUseDedicatedPartition) {
		final boolean oldUseDedicatedPartition = useDedicatedPartition;
		useDedicatedPartition = newUseDedicatedPartition;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION,
					oldUseDedicatedPartition, useDedicatedPartition));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isSendResponse()
	 * @generated
	 */
	@Override
	public boolean isSendResponse() {
		return sendResponse;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#setSendResponse(boolean)
	 * @generated
	 */
	@Override
	public void setSendResponse(boolean newSendResponse) {
		final boolean oldSendResponse = sendResponse;
		sendResponse = newSendResponse;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_METHOD__SEND_RESPONSE,
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
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME:
				return getRequestSchemaName();
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME:
				return getResponseSchemaName();
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION:
				return isUseDedicatedPartition();
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__SEND_RESPONSE:
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
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME:
				setRequestSchemaName((String) newValue);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME:
				setResponseSchemaName((String) newValue);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION:
				setUseDedicatedPartition((Boolean) newValue);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__SEND_RESPONSE:
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
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME:
				setRequestSchemaName(REQUEST_SCHEMA_NAME_EDEFAULT);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME:
				setResponseSchemaName(RESPONSE_SCHEMA_NAME_EDEFAULT);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION:
				setUseDedicatedPartition(USE_DEDICATED_PARTITION_EDEFAULT);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__SEND_RESPONSE:
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
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME:
				return requestSchemaName != null;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME:
				return responseSchemaName != null;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION:
				return useDedicatedPartition != USE_DEDICATED_PARTITION_EDEFAULT;
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD__SEND_RESPONSE:
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
		result.append(" (requestSchemaName: ");
		result.append(requestSchemaName);
		result.append(", responseSchemaName: ");
		result.append(responseSchemaName);
		result.append(", useDedicatedPartition: ");
		result.append(useDedicatedPartition);
		result.append(", sendResponse: ");
		result.append(sendResponse);
		result.append(')');

		return result.toString();
	}

}
