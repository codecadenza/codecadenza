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
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Kafka Integration Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl#getRequestTopic <em>Request Topic</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl#getResponseTopic <em>Response
 * Topic</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.KafkaIntegrationBeanImpl#getConsumerGroup <em>Consumer
 * Group</em>}</li>
 * </ul>
 * @generated
 */
public class KafkaIntegrationBeanImpl extends AbstractIntegrationBeanImpl implements KafkaIntegrationBean {
	/**
	 * The default value of the '{@link #getRequestTopic() <em>Request Topic</em>}' attribute
	 * @see #getRequestTopic()
	 * @generated
	 * @ordered
	 */
	protected static final String REQUEST_TOPIC_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRequestTopic() <em>Request Topic</em>}' attribute
	 * @see #getRequestTopic()
	 * @generated
	 * @ordered
	 */
	protected String requestTopic = REQUEST_TOPIC_EDEFAULT;

	/**
	 * The default value of the '{@link #getResponseTopic() <em>Response Topic</em>}' attribute
	 * @see #getResponseTopic()
	 * @generated
	 * @ordered
	 */
	protected static final String RESPONSE_TOPIC_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getResponseTopic() <em>Response Topic</em>}' attribute
	 * @see #getResponseTopic()
	 * @generated
	 * @ordered
	 */
	protected String responseTopic = RESPONSE_TOPIC_EDEFAULT;

	/**
	 * The default value of the '{@link #getConsumerGroup() <em>Consumer Group</em>}' attribute
	 * @see #getConsumerGroup()
	 * @generated
	 * @ordered
	 */
	protected static final String CONSUMER_GROUP_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getConsumerGroup() <em>Consumer Group</em>}' attribute
	 * @see #getConsumerGroup()
	 * @generated
	 * @ordered
	 */
	protected String consumerGroup = CONSUMER_GROUP_EDEFAULT;

	/**
	 * @generated
	 */
	protected KafkaIntegrationBeanImpl() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.KAFKA_INTEGRATION_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getRequestTopic()
	 * @generated
	 */
	@Override
	public String getRequestTopic() {
		return requestTopic;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#setRequestTopic(java.lang.String)
	 * @generated
	 */
	@Override
	public void setRequestTopic(String newRequestTopic) {
		final String oldRequestTopic = requestTopic;
		requestTopic = newRequestTopic;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC,
					oldRequestTopic, requestTopic));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getResponseTopic()
	 * @generated
	 */
	@Override
	public String getResponseTopic() {
		return responseTopic;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#setResponseTopic(java.lang.String)
	 * @generated
	 */
	@Override
	public void setResponseTopic(String newResponseTopic) {
		final String oldResponseTopic = responseTopic;
		responseTopic = newResponseTopic;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC,
					oldResponseTopic, responseTopic));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getConsumerGroup()
	 * @generated
	 */
	@Override
	public String getConsumerGroup() {
		return consumerGroup;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#setConsumerGroup(java.lang.String)
	 * @generated
	 */
	@Override
	public void setConsumerGroup(String newConsumerGroup) {
		final String oldConsumerGroup = consumerGroup;
		consumerGroup = newConsumerGroup;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP,
					oldConsumerGroup, consumerGroup));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC:
				return getRequestTopic();
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC:
				return getResponseTopic();
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP:
				return getConsumerGroup();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC:
				setRequestTopic((String) newValue);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC:
				setResponseTopic((String) newValue);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP:
				setConsumerGroup((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC:
				setRequestTopic(REQUEST_TOPIC_EDEFAULT);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC:
				setResponseTopic(RESPONSE_TOPIC_EDEFAULT);
				return;
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP:
				setConsumerGroup(CONSUMER_GROUP_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC:
				return requestTopic != null;
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC:
				return responseTopic != null;
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP:
				return consumerGroup != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (requestTopic: ");
		result.append(requestTopic);
		result.append(", responseTopic: ");
		result.append(responseTopic);
		result.append(", consumerGroup: ");
		result.append(consumerGroup);
		result.append(')');

		return result.toString();
	}

}
