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
package net.codecadenza.eclipse.model.project;

import net.codecadenza.eclipse.model.java.Namespace;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Integration Module</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.IntegrationModule#getTechnology <em>Technology</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.IntegrationModule#getProject <em>Project</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.IntegrationModule#getNamespace <em>Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.IntegrationModule#isAddSecurityHandler <em>Add Security Handler</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule()
 * @model
 * @generated
 */
public interface IntegrationModule extends EObject {
	/**
	 * Return the value of the '<em><b>Technology</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.IntegrationTechnology}.
	 * @return the value of the '<em>Technology</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.IntegrationTechnology
	 * @see #setTechnology(IntegrationTechnology)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_Technology()
	 * @model
	 * @generated
	 */
	IntegrationTechnology getTechnology();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getTechnology <em>Technology</em>}'
	 * attribute
	 * @param value the new value of the '<em>Technology</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.IntegrationTechnology
	 * @see #getTechnology()
	 * @generated
	 */
	void setTechnology(IntegrationTechnology value);

	/**
	 * Return the value of the '<em><b>Project</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.Project#getIntegrationModules <em>Integration Modules</em>}'.
	 * @return the value of the '<em>Project</em>' container reference
	 * @see #setProject(Project)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_Project()
	 * @see net.codecadenza.eclipse.model.project.Project#getIntegrationModules
	 * @model opposite="integrationModules" transient="false"
	 * @generated
	 */
	Project getProject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getProject <em>Project</em>}' container
	 * reference
	 * @param value the new value of the '<em>Project</em>' container reference
	 * @see #getProject()
	 * @generated
	 */
	void setProject(Project value);

	/**
	 * Return the value of the '<em><b>Namespace</b></em>' reference
	 * @return the value of the '<em>Namespace</em>' reference
	 * @see #setNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_Namespace()
	 * @model
	 * @generated
	 */
	Namespace getNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.IntegrationModule#getNamespace <em>Namespace</em>}'
	 * reference
	 * @param value the new value of the '<em>Namespace</em>' reference
	 * @see #getNamespace()
	 * @generated
	 */
	void setNamespace(Namespace value);

	/**
	 * Return the value of the '<em><b>Add Security Handler</b></em>' attribute
	 * @return the value of the '<em>Add Security Handler</em>' attribute
	 * @see #setAddSecurityHandler(boolean)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_AddSecurityHandler()
	 * @model
	 * @generated
	 */
	boolean isAddSecurityHandler();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.IntegrationModule#isAddSecurityHandler <em>Add Security
	 * Handler</em>}' attribute
	 * @param value the new value of the '<em>Add Security Handler</em>' attribute
	 * @see #isAddSecurityHandler()
	 * @generated
	 */
	void setAddSecurityHandler(boolean value);

	/**
	 * Return the value of the '<em><b>Add Producers</b></em>' attribute
	 * @return the value of the '<em>Add Producers</em>' attribute
	 * @see #setAddProducers(boolean)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationModule_AddProducers()
	 * @model
	 * @generated
	 */
	boolean isAddProducers();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.IntegrationModule#isAddProducers <em>Add Producers</em>}'
	 * attribute
	 * @param value the new value of the '<em>Add Producers</em>' attribute
	 * @see #isAddProducers()
	 * @generated
	 */
	void setAddProducers(boolean value);

	/**
	 * @return true if the module contains an artifact for integration clients
	 * @generated not
	 */
	boolean hasClientArtifact();

	/**
	 * @return true if the module contains a service end-point artifact
	 * @generated not
	 */
	boolean hasSEIArtifact();

	/**
	 * @return the name of the credentials provider
	 * @generated not
	 */
	String getCredentialsProviderName();

	/**
	 * @return the name of the file service
	 * @generated not
	 */
	String getFileServiceName();

	/**
	 * @return the name of the file service client
	 * @generated not
	 */
	String getFileServiceClientName();

	/**
	 * @return the name of the file service producer
	 * @generated not
	 */
	String getFileServiceProducerName();

}
