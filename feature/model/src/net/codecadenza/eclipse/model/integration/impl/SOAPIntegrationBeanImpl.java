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
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>SOAP Integration Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl#getServiceName <em>Service Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl#getPortName <em>Port Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl#isRpcStype <em>Rpc Stype</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl#isBareParameterStyle <em>Bare Parameter
 * Style</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationBeanImpl#getPortTypeName <em>Port Type Name</em>}</li>
 * </ul>
 * @generated
 */
public class SOAPIntegrationBeanImpl extends AbstractIntegrationBeanImpl implements SOAPIntegrationBean {
	/**
	 * The default value of the '{@link #getServiceName() <em>Service Name</em>}' attribute
	 * @see #getServiceName()
	 * @generated
	 * @ordered
	 */
	protected static final String SERVICE_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getServiceName() <em>Service Name</em>}' attribute
	 * @see #getServiceName()
	 * @generated
	 * @ordered
	 */
	protected String serviceName = SERVICE_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getPortName() <em>Port Name</em>}' attribute
	 * @see #getPortName()
	 * @generated
	 * @ordered
	 */
	protected static final String PORT_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPortName() <em>Port Name</em>}' attribute
	 * @see #getPortName()
	 * @generated
	 * @ordered
	 */
	protected String portName = PORT_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #isRpcStype() <em>Rpc Stype</em>}' attribute
	 * @see #isRpcStype()
	 * @generated
	 * @ordered
	 */
	protected static final boolean RPC_STYPE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRpcStype() <em>Rpc Stype</em>}' attribute
	 * @see #isRpcStype()
	 * @generated
	 * @ordered
	 */
	protected boolean rpcStype = RPC_STYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #isBareParameterStyle() <em>Bare Parameter Style</em>}' attribute
	 * @see #isBareParameterStyle()
	 * @generated
	 * @ordered
	 */
	protected static final boolean BARE_PARAMETER_STYLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isBareParameterStyle() <em>Bare Parameter Style</em>}' attribute
	 * @see #isBareParameterStyle()
	 * @generated
	 * @ordered
	 */
	protected boolean bareParameterStyle = BARE_PARAMETER_STYLE_EDEFAULT;

	/**
	 * The default value of the '{@link #getPortTypeName() <em>Port Type Name</em>}' attribute
	 * @see #getPortTypeName()
	 * @generated
	 * @ordered
	 */
	protected static final String PORT_TYPE_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPortTypeName() <em>Port Type Name</em>}' attribute
	 * @see #getPortTypeName()
	 * @generated
	 * @ordered
	 */
	protected String portTypeName = PORT_TYPE_NAME_EDEFAULT;

	/**
	 * @generated
	 */
	protected SOAPIntegrationBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.SOAP_INTEGRATION_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getServiceName()
	 * @generated
	 */
	@Override
	public String getServiceName() {
		return serviceName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#setServiceName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setServiceName(String newServiceName) {
		final String oldServiceName = serviceName;
		serviceName = newServiceName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_BEAN__SERVICE_NAME,
					oldServiceName, serviceName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortName()
	 * @generated
	 */
	@Override
	public String getPortName() {
		return portName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#setPortName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPortName(String newPortName) {
		final String oldPortName = portName;
		portName = newPortName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_NAME, oldPortName,
					portName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isRpcStype()
	 * @generated
	 */
	@Override
	public boolean isRpcStype() {
		return rpcStype;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#setRpcStype(boolean)
	 * @generated
	 */
	@Override
	public void setRpcStype(boolean newRpcStype) {
		final boolean oldRpcStype = rpcStype;
		rpcStype = newRpcStype;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_BEAN__RPC_STYPE, oldRpcStype,
					rpcStype));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isBareParameterStyle()
	 * @generated
	 */
	@Override
	public boolean isBareParameterStyle() {
		return bareParameterStyle;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#setBareParameterStyle(boolean)
	 * @generated
	 */
	@Override
	public void setBareParameterStyle(boolean newBareParameterStyle) {
		final boolean oldBareParameterStyle = bareParameterStyle;
		bareParameterStyle = newBareParameterStyle;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE,
					oldBareParameterStyle, bareParameterStyle));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortTypeName()
	 * @generated
	 */
	@Override
	public String getPortTypeName() {
		return portTypeName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#setPortTypeName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPortTypeName(String newPortTypeName) {
		final String oldPortTypeName = portTypeName;
		portTypeName = newPortTypeName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME,
					oldPortTypeName, portTypeName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__SERVICE_NAME:
				return getServiceName();
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_NAME:
				return getPortName();
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__RPC_STYPE:
				return isRpcStype();
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE:
				return isBareParameterStyle();
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME:
				return getPortTypeName();
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
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__SERVICE_NAME:
				setServiceName((String) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_NAME:
				setPortName((String) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__RPC_STYPE:
				setRpcStype((Boolean) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE:
				setBareParameterStyle((Boolean) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME:
				setPortTypeName((String) newValue);
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
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__SERVICE_NAME:
				setServiceName(SERVICE_NAME_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_NAME:
				setPortName(PORT_NAME_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__RPC_STYPE:
				setRpcStype(RPC_STYPE_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE:
				setBareParameterStyle(BARE_PARAMETER_STYLE_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME:
				setPortTypeName(PORT_TYPE_NAME_EDEFAULT);
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
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__SERVICE_NAME:
				return serviceName != null;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_NAME:
				return portName != null;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__RPC_STYPE:
				return rpcStype != RPC_STYPE_EDEFAULT;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE:
				return bareParameterStyle != BARE_PARAMETER_STYLE_EDEFAULT;
			case IntegrationPackage.SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME:
				return portTypeName != null;
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

		final var result = new StringBuilder(super.toString());
		result.append(" (serviceName: ");
		result.append(serviceName);
		result.append(", portName: ");
		result.append(portName);
		result.append(", rpcStype: ");
		result.append(rpcStype);
		result.append(", bareParameterStyle: ");
		result.append(bareParameterStyle);
		result.append(", portTypeName: ");
		result.append(portTypeName);
		result.append(')');

		return result.toString();
	}

}
