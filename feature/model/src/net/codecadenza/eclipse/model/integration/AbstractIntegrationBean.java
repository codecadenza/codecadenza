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

import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.service.ServiceBean;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Abstract Integration Bean</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getMethods <em>Methods</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getClientClassName <em>Client Class Name</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean()
 * @model
 * @generated
 */
public interface AbstractIntegrationBean extends ServiceBean {
	/**
	 * Return the value of the '<em><b>Methods</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean <em>Integration Bean</em>}'.
	 * @return the value of the '<em>Methods</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean_Methods()
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean
	 * @model opposite="integrationBean" containment="true"
	 * @generated
	 */
	EList<AbstractIntegrationMethod> getMethods();

	/**
	 * Return the value of the '<em><b>Client Class Name</b></em>' attribute
	 * @return the value of the '<em>Client Class Name</em>' attribute
	 * @see #setClientClassName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean_ClientClassName()
	 * @model
	 * @generated
	 */
	String getClientClassName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getClientClassName <em>Client
	 * Class Name</em>}' attribute
	 * @param value the new value of the '<em>Client Class Name</em>' attribute
	 * @see #getClientClassName()
	 * @generated
	 */
	void setClientClassName(String value);

	/**
	 * Return the value of the '<em><b>Producer Class Name</b></em>' attribute
	 * @return the value of the '<em>Producer Class Name</em>' attribute
	 * @see #setProducerClassName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean_ProducerClassName()
	 * @model
	 * @generated
	 */
	String getProducerClassName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getProducerClassName
	 * <em>Producer Class Name</em>}' attribute
	 * @param value the new value of the '<em>Producer Class Name</em>' attribute
	 * @see #getProducerClassName()
	 * @generated
	 */
	void setProducerClassName(String value);

	/**
	 * @return the integration technology used by this service
	 * @generated not
	 */
	IntegrationTechnology getIntegrationTechnology();

	/**
	 * @return the module this integration bean belongs to
	 * @throws IllegalStateException if the integration module could not be found
	 * @generated not
	 */
	IntegrationModule getIntegrationModule();

	/**
	 * @return the internal representation of the service bean source file
	 * @generated not
	 */
	JavaFile getServiceBeanSourceFile();

	/**
	 * @return the internal representation of the service end-point interface source file
	 * @generated not
	 */
	JavaFile getSEISourceFile();

	/**
	 * @return the internal representation of the integration client source file
	 * @generated not
	 */
	JavaFile getClientSourceFile();

	/**
	 * @return the internal representation of the integration service producer
	 * @generated not
	 */
	JavaFile getProducerSourceFile();

}
