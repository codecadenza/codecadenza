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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Build Artifact</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.BuildArtifact#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.BuildArtifact#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.BuildArtifact#getContainedArtifacts <em>Contained Artifacts</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.BuildArtifact#getProject <em>Project</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact()
 * @model
 * @generated
 */
public interface BuildArtifact extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.BuildArtifactType}.
	 * @return the value of the '<em>Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.BuildArtifactType
	 * @see #setType(BuildArtifactType)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_Type()
	 * @model
	 * @generated
	 */
	BuildArtifactType getType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getType <em>Type</em>}' attribute
	 * @param value the new value of the '<em>Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.project.BuildArtifactType
	 * @see #getType()
	 * @generated
	 */
	void setType(BuildArtifactType value);

	/**
	 * Return the value of the '<em><b>Contained Artifacts</b></em>' attribute list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.BuildArtifactType}. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.project.BuildArtifactType}.
	 * @return the value of the '<em>Contained Artifacts</em>' attribute list
	 * @see net.codecadenza.eclipse.model.project.BuildArtifactType
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_ContainedArtifacts()
	 * @model
	 * @generated
	 */
	EList<BuildArtifactType> getContainedArtifacts();

	/**
	 * Return the value of the '<em><b>Project</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.Project#getBuildConfiguration <em>Build Configuration</em>}'.
	 * @return the value of the '<em>Project</em>' container reference
	 * @see #setProject(Project)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifact_Project()
	 * @see net.codecadenza.eclipse.model.project.Project#getBuildConfiguration
	 * @model opposite="buildConfiguration" transient="false"
	 * @generated
	 */
	Project getProject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.BuildArtifact#getProject <em>Project</em>}' container
	 * reference
	 * @param value the new value of the '<em>Project</em>' container reference
	 * @see #getProject()
	 * @generated
	 */
	void setProject(Project value);

	/**
	 * @return the name of the corresponding module
	 * @generated not
	 */
	String getModuleName();

	/**
	 * @return true if the artifact is only used for tests
	 * @generated not
	 */
	boolean isTestArtifact();

}
