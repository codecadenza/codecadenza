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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Role</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.Role#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Role#isAdminRole <em>Admin Role</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Role#isReadonlyRole <em>Readonly Role</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole()
 * @model
 * @generated
 */
public interface Role extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Role#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Admin Role</b></em>' attribute
	 * @return the value of the '<em>Admin Role</em>' attribute
	 * @see #setAdminRole(boolean)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole_AdminRole()
	 * @model
	 * @generated
	 */
	boolean isAdminRole();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Role#isAdminRole <em>Admin Role</em>}' attribute
	 * @param value the new value of the '<em>Admin Role</em>' attribute
	 * @see #isAdminRole()
	 * @generated
	 */
	void setAdminRole(boolean value);

	/**
	 * Return the value of the '<em><b>Readonly Role</b></em>' attribute
	 * @return the value of the '<em>Readonly Role</em>' attribute
	 * @see #setReadonlyRole(boolean)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getRole_ReadonlyRole()
	 * @model
	 * @generated
	 */
	boolean isReadonlyRole();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Role#isReadonlyRole <em>Readonly Role</em>}' attribute
	 * @param value the new value of the '<em>Readonly Role</em>' attribute
	 * @see #isReadonlyRole()
	 * @generated
	 */
	void setReadonlyRole(boolean value);

}
