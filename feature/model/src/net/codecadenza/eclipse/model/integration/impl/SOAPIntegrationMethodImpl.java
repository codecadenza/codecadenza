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
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>SOAP Integration Method</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl#getOperationName <em>Operation
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl#isAddParameterAnnotations <em>Add Parameter
 * Annotations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl#getReturnValueName <em>Return Value
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.SOAPIntegrationMethodImpl#getReturnValuePartName <em>Return Value
 * Part Name</em>}</li>
 * </ul>
 * @generated
 */
public class SOAPIntegrationMethodImpl extends AbstractIntegrationMethodImpl implements SOAPIntegrationMethod {
	/**
	 * The default value of the '{@link #getOperationName() <em>Operation Name</em>}' attribute
	 * @see #getOperationName()
	 * @generated
	 * @ordered
	 */
	protected static final String OPERATION_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getOperationName() <em>Operation Name</em>}' attribute
	 * @see #getOperationName()
	 * @generated
	 * @ordered
	 */
	protected String operationName = OPERATION_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #isAddParameterAnnotations() <em>Add Parameter Annotations</em>}' attribute
	 * @see #isAddParameterAnnotations()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADD_PARAMETER_ANNOTATIONS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAddParameterAnnotations() <em>Add Parameter Annotations</em>}' attribute
	 * @see #isAddParameterAnnotations()
	 * @generated
	 * @ordered
	 */
	protected boolean addParameterAnnotations = ADD_PARAMETER_ANNOTATIONS_EDEFAULT;

	/**
	 * The default value of the '{@link #getReturnValueName() <em>Return Value Name</em>}' attribute
	 * @see #getReturnValueName()
	 * @generated
	 * @ordered
	 */
	protected static final String RETURN_VALUE_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getReturnValueName() <em>Return Value Name</em>}' attribute
	 * @see #getReturnValueName()
	 * @generated
	 * @ordered
	 */
	protected String returnValueName = RETURN_VALUE_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getReturnValuePartName() <em>Return Value Part Name</em>}' attribute
	 * @see #getReturnValuePartName()
	 * @generated
	 * @ordered
	 */
	protected static final String RETURN_VALUE_PART_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getReturnValuePartName() <em>Return Value Part Name</em>}' attribute
	 * @see #getReturnValuePartName()
	 * @generated
	 * @ordered
	 */
	protected String returnValuePartName = RETURN_VALUE_PART_NAME_EDEFAULT;

	/**
	 * @generated
	 */
	protected SOAPIntegrationMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.SOAP_INTEGRATION_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getOperationName()
	 * @generated
	 */
	@Override
	public String getOperationName() {
		return operationName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#setOperationName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setOperationName(String newOperationName) {
		final String oldOperationName = operationName;
		operationName = newOperationName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_METHOD__OPERATION_NAME,
					oldOperationName, operationName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#isAddParameterAnnotations()
	 * @generated
	 */
	@Override
	public boolean isAddParameterAnnotations() {
		return addParameterAnnotations;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#setAddParameterAnnotations(boolean)
	 * @generated
	 */
	@Override
	public void setAddParameterAnnotations(boolean newAddParameterAnnotations) {
		final boolean oldAddParameterAnnotations = addParameterAnnotations;
		addParameterAnnotations = newAddParameterAnnotations;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS,
					oldAddParameterAnnotations, addParameterAnnotations));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValueName()
	 * @generated
	 */
	@Override
	public String getReturnValueName() {
		return returnValueName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#setReturnValueName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setReturnValueName(String newReturnValueName) {
		final String oldReturnValueName = returnValueName;
		returnValueName = newReturnValueName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME,
					oldReturnValueName, returnValueName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValuePartName()
	 * @generated
	 */
	@Override
	public String getReturnValuePartName() {
		return returnValuePartName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#setReturnValuePartName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setReturnValuePartName(String newReturnValuePartName) {
		final String oldReturnValuePartName = returnValuePartName;
		returnValuePartName = newReturnValuePartName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME,
					oldReturnValuePartName, returnValuePartName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__OPERATION_NAME:
				return getOperationName();
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS:
				return isAddParameterAnnotations();
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME:
				return getReturnValueName();
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME:
				return getReturnValuePartName();
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
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__OPERATION_NAME:
				setOperationName((String) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS:
				setAddParameterAnnotations((Boolean) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME:
				setReturnValueName((String) newValue);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME:
				setReturnValuePartName((String) newValue);
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
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__OPERATION_NAME:
				setOperationName(OPERATION_NAME_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS:
				setAddParameterAnnotations(ADD_PARAMETER_ANNOTATIONS_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME:
				setReturnValueName(RETURN_VALUE_NAME_EDEFAULT);
				return;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME:
				setReturnValuePartName(RETURN_VALUE_PART_NAME_EDEFAULT);
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
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__OPERATION_NAME:
				return operationName != null;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS:
				return addParameterAnnotations != ADD_PARAMETER_ANNOTATIONS_EDEFAULT;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME:
				return returnValueName != null;
			case IntegrationPackage.SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME:
				return returnValuePartName != null;
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

		final var result = new StringBuilder(super.toString());
		result.append(" (operationName: ");
		result.append(operationName);
		result.append(", addParameterAnnotations: ");
		result.append(addParameterAnnotations);
		result.append(", returnValueName: ");
		result.append(returnValueName);
		result.append(", returnValuePartName: ");
		result.append(returnValuePartName);
		result.append(')');

		return result.toString();
	}

}
