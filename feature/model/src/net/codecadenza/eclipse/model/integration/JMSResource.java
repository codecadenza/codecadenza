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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>JMS Resource</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.JMSResource#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.JMSResource#isTopic <em>Topic</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSResource()
 * @model
 * @generated
 */
public interface JMSResource extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSResource_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.JMSResource#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Topic</b></em>' attribute
	 * @return the value of the '<em>Topic</em>' attribute
	 * @see #setTopic(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSResource_Topic()
	 * @model
	 * @generated
	 */
	boolean isTopic();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.JMSResource#isTopic <em>Topic</em>}' attribute
	 * @param value the new value of the '<em>Topic</em>' attribute
	 * @see #isTopic()
	 * @generated
	 */
	void setTopic(boolean value);

}
