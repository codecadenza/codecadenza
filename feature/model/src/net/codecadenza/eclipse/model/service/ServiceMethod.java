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
package net.codecadenza.eclipse.model.service;

import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Service Method</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.ServiceMethod#getQueryStatement <em>Query Statement</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ServiceMethod#getPermissionMode <em>Permission Mode</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ServiceMethod#getTransactionType <em>Transaction Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ServiceMethod#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation <em>Method Invocation</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.ServiceMethod#getCustomStatement <em>Custom Statement</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod()
 * @model abstract="true"
 * @generated
 */
public interface ServiceMethod extends JavaMethod {
	/**
	 * Return the value of the '<em><b>Query Statement</b></em>' attribute
	 * @return the value of the '<em>Query Statement</em>' attribute
	 * @see #setQueryStatement(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_QueryStatement()
	 * @model
	 * @generated
	 */
	String getQueryStatement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getQueryStatement <em>Query Statement</em>}'
	 * attribute
	 * @param value the new value of the '<em>Query Statement</em>' attribute
	 * @see #getQueryStatement()
	 * @generated
	 */
	void setQueryStatement(String value);

	/**
	 * Return the value of the '<em><b>Permission Mode</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.repository.PermissionModeEnumeration}.
	 * @return the value of the '<em>Permission Mode</em>' attribute
	 * @see net.codecadenza.eclipse.model.repository.PermissionModeEnumeration
	 * @see #setPermissionMode(PermissionModeEnumeration)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_PermissionMode()
	 * @model
	 * @generated
	 */
	PermissionModeEnumeration getPermissionMode();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getPermissionMode <em>Permission Mode</em>}'
	 * attribute
	 * @param value the new value of the '<em>Permission Mode</em>' attribute
	 * @see net.codecadenza.eclipse.model.repository.PermissionModeEnumeration
	 * @see #getPermissionMode()
	 * @generated
	 */
	void setPermissionMode(PermissionModeEnumeration value);

	/**
	 * Return the value of the '<em><b>Transaction Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration}.
	 * @return the value of the '<em>Transaction Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration
	 * @see #setTransactionType(TransactionTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_TransactionType()
	 * @model
	 * @generated
	 */
	TransactionTypeEnumeration getTransactionType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getTransactionType <em>Transaction
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Transaction Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration
	 * @see #getTransactionType()
	 * @generated
	 */
	void setTransactionType(TransactionTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Roles</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.Role}.
	 * @return the value of the '<em>Roles</em>' reference list
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_Roles()
	 * @model
	 * @generated
	 */
	EList<Role> getRoles();

	/**
	 * Return the value of the '<em><b>Method Invocation</b></em>' containment reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.service.MethodInvocation#getMethod <em>Method</em>}'.
	 * @return the value of the '<em>Method Invocation</em>' containment reference
	 * @see #setMethodInvocation(MethodInvocation)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_MethodInvocation()
	 * @see net.codecadenza.eclipse.model.service.MethodInvocation#getMethod
	 * @model opposite="method" containment="true"
	 * @generated
	 */
	MethodInvocation getMethodInvocation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation <em>Method
	 * Invocation</em>}' containment reference
	 * @param value the new value of the '<em>Method Invocation</em>' containment reference
	 * @see #getMethodInvocation()
	 * @generated
	 */
	void setMethodInvocation(MethodInvocation value);

	/**
	 * Return the value of the '<em><b>Custom Statement</b></em>' attribute
	 * @return the value of the '<em>Custom Statement</em>' attribute
	 * @see #setCustomStatement(String)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getServiceMethod_CustomStatement()
	 * @model
	 * @generated
	 */
	String getCustomStatement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.ServiceMethod#getCustomStatement <em>Custom
	 * Statement</em>}' attribute
	 * @param value the new value of the '<em>Custom Statement</em>' attribute
	 * @see #getCustomStatement()
	 * @generated
	 */
	void setCustomStatement(String value);

}
