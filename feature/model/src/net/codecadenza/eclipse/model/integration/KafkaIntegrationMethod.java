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
 * A representation of the model object '<em><b>Kafka Integration Method</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getRequestSchemaName <em>Request Schema
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getResponseSchemaName <em>Response Schema
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isUseDedicatedPartition() <em>Use Dedicated
 * Partition</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isSendResponse <em>Send Response</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod()
 * @model
 * @generated
 */
public interface KafkaIntegrationMethod extends AbstractIntegrationMethod {
	/**
	 * Return the value of the '<em><b>Request Schema Name</b></em>' attribute
	 * @return the value of the '<em>Request Schema Name</em>' attribute
	 * @see #setRequestSchemaName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_RequestSchemaName()
	 * @model
	 * @generated
	 */
	String getRequestSchemaName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getRequestSchemaName <em>Request
	 * Schema Name</em>}' attribute
	 * @param value the new value of the '<em>Request Schema Name</em>' attribute
	 * @see #getRequestSchemaName()
	 * @generated
	 */
	void setRequestSchemaName(String value);

	/**
	 * Return the value of the '<em><b>Response Schema Name</b></em>' attribute
	 * @return the value of the '<em>Response Schema Name</em>' attribute
	 * @see #setResponseSchemaName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_ResponseSchemaName()
	 * @model
	 * @generated
	 */
	String getResponseSchemaName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#getResponseSchemaName
	 * <em>Response Schema Name</em>}' attribute
	 * @param value the new value of the '<em>Response Schema Name</em>' attribute
	 * @see #getResponseSchemaName()
	 * @generated
	 */
	void setResponseSchemaName(String value);

	/**
	 * Return the value of the '<em><b>Use Dedicated Partition</b></em>' attribute
	 * @return the value of the '<em>Use Dedicated Partition</em>' attribute
	 * @see #setUseDedicatedPartition(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_UseDedicatedPartition()
	 * @model
	 * @generated
	 */
	boolean isUseDedicatedPartition();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isUseDedicatedPartition()
	 * <em>Use Dedicated Partition </em>}' attribute
	 * @param value the new value of the '<em>Use Dedicated Partition</em>' attribute
	 * @see #isUseDedicatedPartition()
	 * @generated
	 */
	void setUseDedicatedPartition(boolean value);

	/**
	 * Return the value of the '<em><b>Send Response</b></em>' attribute
	 * @return the value of the '<em>Send Response</em>' attribute
	 * @see #setSendResponse(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_SendResponse()
	 * @model
	 * @generated
	 */
	boolean isSendResponse();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod#isSendResponse <em>Send
	 * Response</em>}' attribute
	 * @param value the new value of the '<em>Send Response</em>' attribute
	 * @see #isSendResponse()
	 * @generated
	 */
	void setSendResponse(boolean value);

}
