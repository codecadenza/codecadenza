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
 * A representation of the model object '<em><b>JMS Integration Bean</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getRequestDestination <em>Request
 * Destination</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getResponseDestination <em>Response
 * Destination</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationBean()
 * @model
 * @generated
 */
public interface JMSIntegrationBean extends AbstractIntegrationBean {
	/**
	 * Return the value of the '<em><b>Request Destination</b></em>' containment reference
	 * @return the value of the '<em>Request Destination</em>' containment reference
	 * @see #setRequestDestination(JMSResource)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationBean_RequestDestination()
	 * @model containment="true"
	 * @generated
	 */
	JMSResource getRequestDestination();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getRequestDestination <em>Request
	 * Destination</em>}' containment reference
	 * @param value the new value of the '<em>Request Destination</em>' containment reference
	 * @see #getRequestDestination()
	 * @generated
	 */
	void setRequestDestination(JMSResource value);

	/**
	 * Return the value of the '<em><b>Response Destination</b></em>' containment reference
	 * @return the value of the '<em>Response Destination</em>' containment reference
	 * @see #setResponseDestination(JMSResource)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationBean_ResponseDestination()
	 * @model containment="true"
	 * @generated
	 */
	JMSResource getResponseDestination();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean#getResponseDestination <em>Response
	 * Destination</em>}' containment reference
	 * @param value the new value of the '<em>Response Destination</em>' containment reference
	 * @see #getResponseDestination()
	 * @generated
	 */
	void setResponseDestination(JMSResource value);

	/**
	 * @return true if at least one method should send a response
	 * @generated not
	 */
	boolean isSendResponse();

}
