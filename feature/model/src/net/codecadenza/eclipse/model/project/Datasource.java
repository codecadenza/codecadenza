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
 * A representation of the model object '<em><b>Datasource</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.Datasource#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Datasource#getConnectionURL <em>Connection URL</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Datasource#getUserName <em>User Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Datasource#getPassword <em>Password</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Datasource#getDriverName <em>Driver Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.Datasource#getDriverList <em>Driver List</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource()
 * @model
 * @generated
 */
public interface Datasource extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Datasource#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Connection URL</b></em>' attribute
	 * @return the value of the '<em>Connection URL</em>' attribute
	 * @see #setConnectionURL(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_ConnectionURL()
	 * @model
	 * @generated
	 */
	String getConnectionURL();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Datasource#getConnectionURL <em>Connection URL</em>}'
	 * attribute
	 * @param value the new value of the '<em>Connection URL</em>' attribute
	 * @see #getConnectionURL()
	 * @generated
	 */
	void setConnectionURL(String value);

	/**
	 * Return the value of the '<em><b>User Name</b></em>' attribute
	 * @return the value of the '<em>User Name</em>' attribute
	 * @see #setUserName(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_UserName()
	 * @model
	 * @generated
	 */
	String getUserName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Datasource#getUserName <em>User Name</em>}' attribute
	 * @param value the new value of the '<em>User Name</em>' attribute
	 * @see #getUserName()
	 * @generated
	 */
	void setUserName(String value);

	/**
	 * Return the value of the '<em><b>Password</b></em>' attribute
	 * @return the value of the '<em>Password</em>' attribute
	 * @see #setPassword(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_Password()
	 * @model
	 * @generated
	 */
	String getPassword();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Datasource#getPassword <em>Password</em>}' attribute
	 * @param value the new value of the '<em>Password</em>' attribute
	 * @see #getPassword()
	 * @generated
	 */
	void setPassword(String value);

	/**
	 * Return the value of the '<em><b>Driver Name</b></em>' attribute
	 * @return the value of the '<em>Driver Name</em>' attribute
	 * @see #setDriverName(String)
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_DriverName()
	 * @model
	 * @generated
	 */
	String getDriverName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.project.Datasource#getDriverName <em>Driver Name</em>}' attribute
	 * @param value the new value of the '<em>Driver Name</em>' attribute
	 * @see #getDriverName()
	 * @generated
	 */
	void setDriverName(String value);

	/**
	 * Return the value of the '<em><b>Driver List</b></em>' attribute list. The list contents are of type {@link java.lang.String}.
	 * @return the value of the '<em>Driver List</em>' attribute list
	 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getDatasource_DriverList()
	 * @model
	 * @generated
	 */
	EList<String> getDriverList();

}
