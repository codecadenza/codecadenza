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
package net.codecadenza.eclipse.model.integration;

import java.util.Optional;

/**
 * A representation of the model object '<em><b>REST Integration Method</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getPath <em>Path</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getHttpMethod <em>Http Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getInputType <em>Input Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getOutputType <em>Output Type</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod()
 * @model
 * @generated
 */
public interface RESTIntegrationMethod extends AbstractIntegrationMethod {
	/**
	 * Return the value of the '<em><b>Path</b></em>' attribute
	 * @return the value of the '<em>Path</em>' attribute
	 * @see #setPath(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_Path()
	 * @model
	 * @generated
	 */
	String getPath();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getPath <em>Path</em>}' attribute
	 * @param value the new value of the '<em>Path</em>' attribute
	 * @see #getPath()
	 * @generated
	 */
	void setPath(String value);

	/**
	 * Return the value of the '<em><b>Http Method</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.integration.HttpMethodEnumeration}.
	 * @return the value of the '<em>Http Method</em>' attribute
	 * @see net.codecadenza.eclipse.model.integration.HttpMethodEnumeration
	 * @see #setHttpMethod(HttpMethodEnumeration)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_HttpMethod()
	 * @model
	 * @generated
	 */
	HttpMethodEnumeration getHttpMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getHttpMethod <em>Http
	 * Method</em>}' attribute
	 * @param value the new value of the '<em>Http Method</em>' attribute
	 * @see net.codecadenza.eclipse.model.integration.HttpMethodEnumeration
	 * @see #getHttpMethod()
	 * @generated
	 */
	void setHttpMethod(HttpMethodEnumeration value);

	/**
	 * Return the value of the '<em><b>Input Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.integration.MediaTypeEnumeration}.
	 * @return the value of the '<em>Input Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
	 * @see #setInputType(MediaTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_InputType()
	 * @model
	 * @generated
	 */
	MediaTypeEnumeration getInputType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getInputType <em>Input
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Input Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
	 * @see #getInputType()
	 * @generated
	 */
	void setInputType(MediaTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Output Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.integration.MediaTypeEnumeration}.
	 * @return the value of the '<em>Output Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
	 * @see #setOutputType(MediaTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_OutputType()
	 * @model
	 * @generated
	 */
	MediaTypeEnumeration getOutputType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod#getOutputType <em>Output
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Output Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.integration.MediaTypeEnumeration
	 * @see #getOutputType()
	 * @generated
	 */
	void setOutputType(MediaTypeEnumeration value);

	/**
	 * @return the optional content parameter
	 * @generated not
	 */
	Optional<IntegrationMethodParameter> getContentParameter();

}
