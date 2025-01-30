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
import net.codecadenza.eclipse.model.integration.JMSResource;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>JMS Resource</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.JMSResourceImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.JMSResourceImpl#isTopic <em>Topic</em>}</li>
 * </ul>
 * @generated
 */
public class JMSResourceImpl extends EObjectImpl implements JMSResource {
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
	 * The default value of the '{@link #isTopic() <em>Topic</em>}' attribute
	 * @see #isTopic()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TOPIC_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isTopic() <em>Topic</em>}' attribute
	 * @see #isTopic()
	 * @generated
	 * @ordered
	 */
	protected boolean topic = TOPIC_EDEFAULT;

	/**
	 * @generated
	 */
	protected JMSResourceImpl() {

	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.JMS_RESOURCE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSResource#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSResource#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.JMS_RESOURCE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSResource#isTopic()
	 * @generated
	 */
	@Override
	public boolean isTopic() {
		return topic;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.JMSResource#setTopic(boolean)
	 * @generated
	 */
	@Override
	public void setTopic(boolean newTopic) {
		final boolean oldTopic = topic;
		topic = newTopic;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.JMS_RESOURCE__TOPIC, oldTopic, topic));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.JMS_RESOURCE__NAME:
				return getName();
			case IntegrationPackage.JMS_RESOURCE__TOPIC:
				return isTopic();
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
			case IntegrationPackage.JMS_RESOURCE__NAME:
				setName((String) newValue);
				return;
			case IntegrationPackage.JMS_RESOURCE__TOPIC:
				setTopic((Boolean) newValue);
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
			case IntegrationPackage.JMS_RESOURCE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case IntegrationPackage.JMS_RESOURCE__TOPIC:
				setTopic(TOPIC_EDEFAULT);
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
			case IntegrationPackage.JMS_RESOURCE__NAME:
				return name != null;
			case IntegrationPackage.JMS_RESOURCE__TOPIC:
				return topic != TOPIC_EDEFAULT;
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

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", topic: ");
		result.append(topic);
		result.append(')');
		return result.toString();
	}

}
