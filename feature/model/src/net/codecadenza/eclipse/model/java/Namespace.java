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
package net.codecadenza.eclipse.model.java;

import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Namespace</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.Namespace#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.Namespace#getParent <em>Parent</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.Namespace#getChildNamespaces <em>Child Namespaces</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.Namespace#getJavaTypes <em>Java Types</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.Namespace#getProject <em>Project</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace()
 * @model
 * @generated
 */
public interface Namespace extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.Namespace#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Parent</b></em>' reference
	 * @return the value of the '<em>Parent</em>' reference
	 * @see #setParent(Namespace)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_Parent()
	 * @model
	 * @generated
	 */
	Namespace getParent();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.Namespace#getParent <em>Parent</em>}' reference
	 * @param value the new value of the '<em>Parent</em>' reference
	 * @see #getParent()
	 * @generated
	 */
	void setParent(Namespace value);

	/**
	 * Return the value of the '<em><b>Child Namespaces</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.Namespace}.
	 * @return the value of the '<em>Child Namespaces</em>' reference list
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_ChildNamespaces()
	 * @model
	 * @generated
	 */
	EList<Namespace> getChildNamespaces();

	/**
	 * Return the value of the '<em><b>Java Types</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.JavaType}. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.java.JavaType#getNamespace <em>Namespace</em>}'.
	 * @return the value of the '<em>Java Types</em>' reference list
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_JavaTypes()
	 * @see net.codecadenza.eclipse.model.java.JavaType#getNamespace
	 * @model opposite="namespace"
	 * @generated
	 */
	EList<JavaType> getJavaTypes();

	/**
	 * Return the value of the '<em><b>Project</b></em>' reference
	 * @return the value of the '<em>Project</em>' reference
	 * @see #setProject(Project)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_Project()
	 * @model
	 * @generated
	 */
	Project getProject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.Namespace#getProject <em>Project</em>}' reference
	 * @param value the new value of the '<em>Project</em>' reference
	 * @see #getProject()
	 * @generated
	 */
	void setProject(Project value);

}
