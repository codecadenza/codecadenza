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

import java.util.Optional;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>REST Integration Method</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl#getPath <em>Path</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl#getHttpMethod <em>Http Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl#getInputType <em>Input Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationMethodImpl#getOutputType <em>Output Type</em>}</li>
 * </ul>
 * @generated
 */
public class RESTIntegrationMethodImpl extends AbstractIntegrationMethodImpl implements RESTIntegrationMethod {
	/**
	 * The default value of the '{@link #getPath() <em>Path</em>}' attribute
	 * @see #getPath()
	 * @generated
	 * @ordered
	 */
	protected static final String PATH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPath() <em>Path</em>}' attribute
	 * @see #getPath()
	 * @generated
	 * @ordered
	 */
	protected String path = PATH_EDEFAULT;

	/**
	 * The default value of the '{@link #getHttpMethod() <em>Http Method</em>}' attribute
	 * @see #getHttpMethod()
	 * @generated
	 * @ordered
	 */
	protected static final HttpMethodEnumeration HTTP_METHOD_EDEFAULT = HttpMethodEnumeration.GET;

	/**
	 * The cached value of the '{@link #getHttpMethod() <em>Http Method</em>}' attribute
	 * @see #getHttpMethod()
	 * @generated
	 * @ordered
	 */
	protected HttpMethodEnumeration httpMethod = HTTP_METHOD_EDEFAULT;

	/**
	 * The default value of the '{@link #getInputType() <em>Input Type</em>}' attribute
	 * @see #getInputType()
	 * @generated
	 * @ordered
	 */
	protected static final MediaTypeEnumeration INPUT_TYPE_EDEFAULT = MediaTypeEnumeration.XML;

	/**
	 * The cached value of the '{@link #getInputType() <em>Input Type</em>}' attribute
	 * @see #getInputType()
	 * @generated
	 * @ordered
	 */
	protected MediaTypeEnumeration inputType = INPUT_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getOutputType() <em>Output Type</em>}' attribute
	 * @see #getOutputType()
	 * @generated
	 * @ordered
	 */
	protected static final MediaTypeEnumeration OUTPUT_TYPE_EDEFAULT = MediaTypeEnumeration.XML;

	/**
	 * The cached value of the '{@link #getOutputType() <em>Output Type</em>}' attribute
	 * @see #getOutputType()
	 * @generated
	 * @ordered
	 */
	protected MediaTypeEnumeration outputType = OUTPUT_TYPE_EDEFAULT;

	/**
	 * @generated
	 */
	protected RESTIntegrationMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.REST_INTEGRATION_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getPath()
	 * @generated
	 */
	@Override
	public String getPath() {
		return path;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#setPath(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPath(String newPath) {
		final String oldPath = path;
		path = newPath;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.REST_INTEGRATION_METHOD__PATH, oldPath, path));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getHttpMethod()
	 * @generated
	 */
	@Override
	public HttpMethodEnumeration getHttpMethod() {
		return httpMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#setHttpMethod(net.codecadenza.eclipse.model.integration.
	 * HttpMethodEnumeration)
	 * @generated
	 */
	@Override
	public void setHttpMethod(HttpMethodEnumeration newHttpMethod) {
		final HttpMethodEnumeration oldHttpMethod = httpMethod;
		httpMethod = newHttpMethod == null ? HTTP_METHOD_EDEFAULT : newHttpMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.REST_INTEGRATION_METHOD__HTTP_METHOD,
					oldHttpMethod, httpMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getInputType()
	 * @generated
	 */
	@Override
	public MediaTypeEnumeration getInputType() {
		return inputType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#setInputType(net.codecadenza.eclipse.model.integration.
	 * MediaTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setInputType(MediaTypeEnumeration newInputType) {
		final MediaTypeEnumeration oldInputType = inputType;
		inputType = newInputType == null ? INPUT_TYPE_EDEFAULT : newInputType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.REST_INTEGRATION_METHOD__INPUT_TYPE, oldInputType,
					inputType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getOutputType()
	 * @generated
	 */
	@Override
	public MediaTypeEnumeration getOutputType() {
		return outputType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#setOutputType(net.codecadenza.eclipse.model.integration.
	 * MediaTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setOutputType(MediaTypeEnumeration newOutputType) {
		final MediaTypeEnumeration oldOutputType = outputType;
		outputType = newOutputType == null ? OUTPUT_TYPE_EDEFAULT : newOutputType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.REST_INTEGRATION_METHOD__OUTPUT_TYPE,
					oldOutputType, outputType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.REST_INTEGRATION_METHOD__PATH:
				return getPath();
			case IntegrationPackage.REST_INTEGRATION_METHOD__HTTP_METHOD:
				return getHttpMethod();
			case IntegrationPackage.REST_INTEGRATION_METHOD__INPUT_TYPE:
				return getInputType();
			case IntegrationPackage.REST_INTEGRATION_METHOD__OUTPUT_TYPE:
				return getOutputType();
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
			case IntegrationPackage.REST_INTEGRATION_METHOD__PATH:
				setPath((String) newValue);
				return;
			case IntegrationPackage.REST_INTEGRATION_METHOD__HTTP_METHOD:
				setHttpMethod((HttpMethodEnumeration) newValue);
				return;
			case IntegrationPackage.REST_INTEGRATION_METHOD__INPUT_TYPE:
				setInputType((MediaTypeEnumeration) newValue);
				return;
			case IntegrationPackage.REST_INTEGRATION_METHOD__OUTPUT_TYPE:
				setOutputType((MediaTypeEnumeration) newValue);
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
			case IntegrationPackage.REST_INTEGRATION_METHOD__PATH:
				setPath(PATH_EDEFAULT);
				return;
			case IntegrationPackage.REST_INTEGRATION_METHOD__HTTP_METHOD:
				setHttpMethod(HTTP_METHOD_EDEFAULT);
				return;
			case IntegrationPackage.REST_INTEGRATION_METHOD__INPUT_TYPE:
				setInputType(INPUT_TYPE_EDEFAULT);
				return;
			case IntegrationPackage.REST_INTEGRATION_METHOD__OUTPUT_TYPE:
				setOutputType(OUTPUT_TYPE_EDEFAULT);
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
			case IntegrationPackage.REST_INTEGRATION_METHOD__PATH:
				return path != null;
			case IntegrationPackage.REST_INTEGRATION_METHOD__HTTP_METHOD:
				return httpMethod != HTTP_METHOD_EDEFAULT;
			case IntegrationPackage.REST_INTEGRATION_METHOD__INPUT_TYPE:
				return inputType != INPUT_TYPE_EDEFAULT;
			case IntegrationPackage.REST_INTEGRATION_METHOD__OUTPUT_TYPE:
				return outputType != OUTPUT_TYPE_EDEFAULT;
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
		result.append(" (path: ");
		result.append(path);
		result.append(", httpMethod: ");
		result.append(httpMethod);
		result.append(", inputType: ");
		result.append(inputType);
		result.append(", outputType: ");
		result.append(outputType);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getContentParameter()
	 * @generated not
	 */
	@Override
	public Optional<IntegrationMethodParameter> getContentParameter() {
		return getIntegrationParameters().stream()
				.filter(e -> !e.isPathParameter() && !e.isQueryParameter() && !e.isResponseParameter()).findFirst();
	}

}
