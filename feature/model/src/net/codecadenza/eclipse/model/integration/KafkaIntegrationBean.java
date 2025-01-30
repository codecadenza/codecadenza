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
 * A representation of the model object '<em><b>Kafka Integration Bean</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getRequestTopic <em>Request Topic</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getResponseTopic <em>Response Topic</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getConsumerGroup <em>Consumer Group</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean()
 * @model
 * @generated
 */
public interface KafkaIntegrationBean extends AbstractIntegrationBean {
	/**
	 * Return the value of the '<em><b>Request Topic</b></em>' attribute
	 * @return the value of the '<em>Request Topic</em>' attribute
	 * @see #setRequestTopic(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean_RequestTopic()
	 * @model
	 * @generated
	 */
	String getRequestTopic();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getRequestTopic <em>Request
	 * Topic</em>}' attribute
	 * @param value the new value of the '<em>Request Topic</em>' attribute
	 * @see #getRequestTopic()
	 * @generated
	 */
	void setRequestTopic(String value);

	/**
	 * Return the value of the '<em><b>Response Topic</b></em>' attribute
	 * @return the value of the '<em>Response Topic</em>' attribute
	 * @see #setResponseTopic(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean_ResponseTopic()
	 * @model
	 * @generated
	 */
	String getResponseTopic();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getResponseTopic <em>Response
	 * Topic</em>}' attribute
	 * @param value the new value of the '<em>Response Topic</em>' attribute
	 * @see #getResponseTopic()
	 * @generated
	 */
	void setResponseTopic(String value);

	/**
	 * Return the value of the '<em><b>Consumer Group</b></em>' attribute
	 * @return the value of the '<em>Consumer Group</em>' attribute
	 * @see #setConsumerGroup(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean_ConsumerGroup()
	 * @model
	 * @generated
	 */
	String getConsumerGroup();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean#getConsumerGroup <em>Consumer
	 * Group</em>}' attribute
	 * @param value the new value of the '<em>Consumer Group</em>' attribute
	 * @see #getConsumerGroup()
	 * @generated
	 */
	void setConsumerGroup(String value);

}
