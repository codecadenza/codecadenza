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
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSResource;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>JMS Integration Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationBeanImpl#getRequestDestination <em>Request
 * Destination</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.JMSIntegrationBeanImpl#getResponseDestination <em>Response
 * Destination</em>}</li>
 * </ul>
 * @generated
 */
public class JMSIntegrationBeanImpl extends AbstractIntegrationBeanImpl implements JMSIntegrationBean {
	/**
	 * The cached value of the '{@link #getRequestDestination() <em>Request Destination</em>}' containment reference
	 * @see #getRequestDestination()
	 * @generated
	 * @ordered
	 */
	protected JMSResource requestDestination;

	/**
	 * The cached value of the '{@link #getResponseDestination() <em>Response Destination</em>}' containment reference
	 * @see #getResponseDestination()
	 * @generated
	 * @ordered
	 */
	protected JMSResource responseDestination;

	/**
	 * @generated
	 */
	protected JMSIntegrationBeanImpl() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.JMS_INTEGRATION_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getRequestDestination()
	 * @generated
	 */
	@Override
	public JMSResource getRequestDestination() {
		return requestDestination;
	}

	/**
	 * @param newRequestDestination
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetRequestDestination(JMSResource newRequestDestination, NotificationChain msgs) {
		final JMSResource oldRequestDestination = requestDestination;
		requestDestination = newRequestDestination;

		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
					IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION, oldRequestDestination, newRequestDestination);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#
	 * setRequestDestination(net.codecadenza.eclipse.model.integration.JMSResource)
	 * @generated
	 */
	@Override
	public void setRequestDestination(JMSResource newRequestDestination) {
		if (newRequestDestination != requestDestination) {
			NotificationChain msgs = null;

			if (requestDestination != null)
				msgs = ((InternalEObject) requestDestination).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION, null, msgs);

			if (newRequestDestination != null)
				msgs = ((InternalEObject) newRequestDestination).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION, null, msgs);

			msgs = basicSetRequestDestination(newRequestDestination, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION,
					newRequestDestination, newRequestDestination));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getResponseDestination()
	 * @generated
	 */
	@Override
	public JMSResource getResponseDestination() {
		return responseDestination;
	}

	/**
	 * @param newResponseDestination
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetResponseDestination(JMSResource newResponseDestination, NotificationChain msgs) {
		final JMSResource oldResponseDestination = responseDestination;
		responseDestination = newResponseDestination;

		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
					IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION, oldResponseDestination, newResponseDestination);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#
	 * setResponseDestination(net.codecadenza.eclipse.model.integration.JMSResource)
	 * @generated
	 */
	@Override
	public void setResponseDestination(JMSResource newResponseDestination) {
		if (newResponseDestination != responseDestination) {
			NotificationChain msgs = null;

			if (responseDestination != null)
				msgs = ((InternalEObject) responseDestination).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION, null, msgs);

			if (newResponseDestination != null)
				msgs = ((InternalEObject) newResponseDestination).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION, null, msgs);

			msgs = basicSetResponseDestination(newResponseDestination, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION,
					newResponseDestination, newResponseDestination));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eInverseRemove(org.eclipse.emf.ecore.
	 * InternalEObject, int, org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION:
				return basicSetRequestDestination(null, msgs);
			case IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION:
				return basicSetResponseDestination(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION:
				return getRequestDestination();
			case IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION:
				return getResponseDestination();
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
			case IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION:
				setRequestDestination((JMSResource) newValue);
				return;
			case IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION:
				setResponseDestination((JMSResource) newValue);
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
			case IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION:
				setRequestDestination((JMSResource) null);
				return;
			case IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION:
				setResponseDestination((JMSResource) null);
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
			case IntegrationPackage.JMS_INTEGRATION_BEAN__REQUEST_DESTINATION:
				return requestDestination != null;
			case IntegrationPackage.JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION:
				return responseDestination != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean#isSendResponse()
	 * @generated not
	 */
	@Override
	public boolean isSendResponse() {
		return getMethods().stream().map(JMSIntegrationMethod.class::cast).anyMatch(JMSIntegrationMethod::isSendResponse);
	}

}
