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

import org.eclipse.emf.ecore.EFactory;

/**
 * The <b>Factory</b> for the model. It provides a create method for each non-abstract class of the model.
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage
 * @generated
 */
public interface IntegrationFactory extends EFactory {
	/**
	 * The singleton instance of the factory
	 * @generated
	 */
	IntegrationFactory eINSTANCE = net.codecadenza.eclipse.model.integration.impl.IntegrationFactoryImpl.init();

	/**
	 * Return a new object of class '<em>SOAP Integration Bean</em>'
	 * @return a new object of class '<em>SOAP Integration Bean</em>'
	 * @generated
	 */
	SOAPIntegrationBean createSOAPIntegrationBean();

	/**
	 * Return a new object of class '<em>REST Integration Bean</em>'
	 * @return a new object of class '<em>REST Integration Bean</em>'
	 * @generated
	 */
	RESTIntegrationBean createRESTIntegrationBean();

	/**
	 * Return a new object of class '<em>Abstract Integration Bean</em>'
	 * @return a new object of class '<em>Abstract Integration Bean</em>'
	 * @generated
	 */
	AbstractIntegrationBean createAbstractIntegrationBean();

	/**
	 * Return a new object of class '<em>SOAP Integration Method</em>'
	 * @return a new object of class '<em>SOAP Integration Method</em>'
	 * @generated
	 */
	SOAPIntegrationMethod createSOAPIntegrationMethod();

	/**
	 * Return a new object of class '<em>REST Integration Method</em>'
	 * @return a new object of class '<em>REST Integration Method</em>'
	 * @generated
	 */
	RESTIntegrationMethod createRESTIntegrationMethod();

	/**
	 * Return a new object of class '<em>Abstract Integration Method</em>'
	 * @return a new object of class '<em>Abstract Integration Method</em>'
	 * @generated
	 */
	AbstractIntegrationMethod createAbstractIntegrationMethod();

	/**
	 * Return a new object of class '<em>RMI Integration Method</em>'
	 * @return a new object of class '<em>RMI Integration Method</em>'
	 * @generated
	 */
	RMIIntegrationMethod createRMIIntegrationMethod();

	/**
	 * Return a new object of class '<em>RMI Integration Bean</em>'
	 * @return a new object of class '<em>RMI Integration Bean</em>'
	 * @generated
	 */
	RMIIntegrationBean createRMIIntegrationBean();

	/**
	 * Return a new object of class '<em>Kafka Integration Bean</em>'
	 * @return a new object of class '<em>Kafka Integration Bean</em>'
	 * @generated
	 */
	KafkaIntegrationBean createKafkaIntegrationBean();

	/**
	 * Return a new object of class '<em>Kafka Integration Method</em>'
	 * @return a new object of class '<em>Kafka Integration Method</em>'
	 * @generated
	 */
	KafkaIntegrationMethod createKafkaIntegrationMethod();

	/**
	 * Return a new object of class '<em>JMS Integration Method</em>'
	 * @return a new object of class '<em>JMS Integration Method</em>'
	 * @generated
	 */
	JMSIntegrationMethod createJMSIntegrationMethod();

	/**
	 * Return a new object of class '<em>JMS Integration Bean</em>'
	 * @return a new object of class '<em>JMS Integration Bean</em>'
	 * @generated
	 */
	JMSIntegrationBean createJMSIntegrationBean();

	/**
	 * Return a new object of class '<em>JMS Resource</em>'
	 * @return a new object of class '<em>JMS Resource</em>'
	 * @generated
	 */
	JMSResource createJMSResource();

	/**
	 * Return the package supported by this factory
	 * @return the package supported by this factory
	 * @generated
	 */
	IntegrationPackage getIntegrationPackage();

}
