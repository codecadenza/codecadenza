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
 * A representation of the model object '<em><b>JMS Integration Method</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#getOperationID <em>Operation ID</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#isSendResponse <em>Send Response</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationMethod()
 * @model
 * @generated
 */
public interface JMSIntegrationMethod extends AbstractIntegrationMethod {
	/**
	 * Return the value of the '<em><b>Operation ID</b></em>' attribute
	 * @return the value of the '<em>Operation ID</em>' attribute
	 * @see #setOperationID(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationMethod_OperationID()
	 * @model
	 * @generated
	 */
	String getOperationID();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#getOperationID <em> Operation
	 * ID</em>}' attribute
	 * @param value the new value of the '<em>Operation ID</em>' attribute
	 * @see #getOperationID()
	 * @generated
	 */
	void setOperationID(String value);

	/**
	 * Return the value of the '<em><b>Send Response</b></em>' attribute
	 * @return the value of the '<em>Send Response</em>' attribute
	 * @see #setSendResponse(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationMethod_SendResponse()
	 * @model
	 * @generated
	 */
	boolean isSendResponse();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod#isSendResponse <em>Send
	 * Response</em>}' attribute
	 * @param value the new value of the '<em>Send Response</em>' attribute
	 * @see #isSendResponse()
	 * @generated
	 */
	void setSendResponse(boolean value);

}
