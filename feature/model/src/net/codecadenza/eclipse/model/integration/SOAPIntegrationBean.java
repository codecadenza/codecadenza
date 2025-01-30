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

/**
 * A representation of the model object '<em><b>SOAP Integration Bean</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getServiceName <em>Service Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortName <em>Port Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isRpcStype <em>Rpc Stype</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isBareParameterStyle <em>Bare Parameter
 * Style</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortTypeName <em>Port Type Name</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean()
 * @model
 * @generated
 */
public interface SOAPIntegrationBean extends AbstractIntegrationBean {
	/**
	 * Return the value of the '<em><b>Service Name</b></em>' attribute
	 * @return the value of the '<em>Service Name</em>' attribute
	 * @see #setServiceName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_ServiceName()
	 * @model
	 * @generated
	 */
	String getServiceName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getServiceName <em>Service
	 * Name</em>}' attribute
	 * @param value the new value of the '<em>Service Name</em>' attribute
	 * @see #getServiceName()
	 * @generated
	 */
	void setServiceName(String value);

	/**
	 * Return the value of the '<em><b>Port Name</b></em>' attribute
	 * @return the value of the '<em>Port Name</em>' attribute
	 * @see #setPortName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_PortName()
	 * @model
	 * @generated
	 */
	String getPortName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortName <em>Port Name</em>}'
	 * attribute
	 * @param value the new value of the '<em>Port Name</em>' attribute
	 * @see #getPortName()
	 * @generated
	 */
	void setPortName(String value);

	/**
	 * Return the value of the '<em><b>Rpc Stype</b></em>' attribute
	 * @return the value of the '<em>Rpc Stype</em>' attribute
	 * @see #setRpcStype(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_RpcStype()
	 * @model
	 * @generated
	 */
	boolean isRpcStype();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isRpcStype <em>Rpc Stype</em>}'
	 * attribute
	 * @param value the new value of the '<em>Rpc Stype</em>' attribute
	 * @see #isRpcStype()
	 * @generated
	 */
	void setRpcStype(boolean value);

	/**
	 * Return the value of the '<em><b>Bare Parameter Style</b></em>' attribute
	 * @return the value of the '<em>Bare Parameter Style</em>' attribute
	 * @see #setBareParameterStyle(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_BareParameterStyle()
	 * @model
	 * @generated
	 */
	boolean isBareParameterStyle();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#isBareParameterStyle <em>Bare
	 * Parameter Style</em>}' attribute
	 * @param value the new value of the '<em>Bare Parameter Style</em>' attribute
	 * @see #isBareParameterStyle()
	 * @generated
	 */
	void setBareParameterStyle(boolean value);

	/**
	 * Return the value of the '<em><b>Port Type Name</b></em>' attribute
	 * @return the value of the '<em>Port Type Name</em>' attribute
	 * @see #setPortTypeName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_PortTypeName()
	 * @model
	 * @generated
	 */
	String getPortTypeName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean#getPortTypeName <em>Port Type
	 * Name</em>}' attribute
	 * @param value the new value of the '<em>Port Type Name</em>' attribute
	 * @see #getPortTypeName()
	 * @generated
	 */
	void setPortTypeName(String value);

}
