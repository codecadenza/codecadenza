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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Method Invocation</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.MethodInvocation#getMethod <em>Method</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.service.ServicePackage#getMethodInvocation()
 * @model abstract="true"
 * @generated
 */
public interface MethodInvocation extends EObject {
	/**
	 * Return the value of the '<em><b>Service Method</b></em>' container reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation <em>Method Invocation</em>}'.
	 * @return the value of the '<em>Service Method</em>' container reference
	 * @see #setMethod(ServiceMethod)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getMethodInvocation_Method()
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation
	 * @model opposite="methodInvocation" transient="false"
	 * @generated
	 */
	ServiceMethod getMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.MethodInvocation#getMethod <em>Service Method</em>}'
	 * container reference
	 * @param value the new value of the '<em>Service Method</em>' container reference
	 * @see #getMethod()
	 * @generated
	 */
	void setMethod(ServiceMethod value);

}
