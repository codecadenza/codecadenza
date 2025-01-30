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

import java.util.function.Function;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>REST Integration Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.RESTIntegrationBeanImpl#getPath <em>Path</em>}</li>
 * </ul>
 * @generated
 */
public class RESTIntegrationBeanImpl extends AbstractIntegrationBeanImpl implements RESTIntegrationBean {
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
	 * @generated
	 */
	protected RESTIntegrationBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.REST_INTEGRATION_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPath()
	 * @generated
	 */
	@Override
	public String getPath() {
		return path;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean#setPath(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPath(String newPath) {
		final String oldPath = path;
		path = newPath;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.REST_INTEGRATION_BEAN__PATH, oldPath, path));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case IntegrationPackage.REST_INTEGRATION_BEAN__PATH:
				return getPath();
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
			case IntegrationPackage.REST_INTEGRATION_BEAN__PATH:
				setPath((String) newValue);
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
			case IntegrationPackage.REST_INTEGRATION_BEAN__PATH:
				setPath(PATH_EDEFAULT);
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
			case IntegrationPackage.REST_INTEGRATION_BEAN__PATH:
				return path != null;
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
		result.append(" (path: ");
		result.append(path);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean#
	 * convertToMediaType(net.codecadenza.eclipse.model.integration.MediaTypeEnumeration)
	 * @generated not
	 */
	@Override
	public String convertToMediaType(MediaTypeEnumeration mediaType) {
		if (mediaType == MediaTypeEnumeration.JSON)
			return "MediaType.APPLICATION_JSON";
		else if (mediaType == MediaTypeEnumeration.XML)
			return "MediaType.APPLICATION_XML";
		else if (mediaType == MediaTypeEnumeration.TEXT)
			return "MediaType.TEXT_PLAIN";
		else if (mediaType == MediaTypeEnumeration.BINARY)
			return "MediaType.APPLICATION_OCTET_STREAM";

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPrimaryConsumedMediaType()
	 * @generated not
	 */
	@Override
	public String getPrimaryConsumedMediaType() {
		return getPrimaryMediaType(RESTIntegrationMethod::getInputType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPrimaryProducedMediaType()
	 * @generated not
	 */
	@Override
	public String getPrimaryProducedMediaType() {
		return getPrimaryMediaType(RESTIntegrationMethod::getOutputType);
	}

	/**
	 * Determine the primary media type (either JSON or XML) used by this REST integration bean
	 * @param mediaTypeFunction a function that controls if either the consumed or the produced media type should be checked
	 * @return the primary media type or null if no primary media type should be declared
	 * @generated not
	 */
	private String getPrimaryMediaType(Function<RESTIntegrationMethod, MediaTypeEnumeration> mediaTypeFunction) {
		final var json = getMethods().stream().map(RESTIntegrationMethod.class::cast).map(mediaTypeFunction)
				.filter(type -> type == MediaTypeEnumeration.JSON).count();
		final var xml = getMethods().stream().map(RESTIntegrationMethod.class::cast).map(mediaTypeFunction)
				.filter(type -> type == MediaTypeEnumeration.XML).count();

		if (json >= xml && json > 0)
			return convertToMediaType(MediaTypeEnumeration.JSON);
		else if (xml > 0)
			return convertToMediaType(MediaTypeEnumeration.XML);

		return null;
	}

}
