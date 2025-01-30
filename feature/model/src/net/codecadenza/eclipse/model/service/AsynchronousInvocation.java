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

/**
 * A representation of the model object '<em><b>Asynchronous Invocation</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.AsynchronousInvocation#getDelayInMilliseconds <em>Delay In
 * Milliseconds</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.service.ServicePackage#getAsynchronousInvocation()
 * @model
 * @generated
 */
public interface AsynchronousInvocation extends MethodInvocation {
	/**
	 * Return the value of the '<em><b>Delay In Milliseconds</b></em>' attribute
	 * @return the value of the '<em>Delay In Milliseconds</em>' attribute
	 * @see #setDelayInMilliseconds(Integer)
	 * @see net.codecadenza.eclipse.model.service.ServicePackage#getAsynchronousInvocation_DelayInMilliseconds()
	 * @model
	 * @generated
	 */
	Integer getDelayInMilliseconds();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.service.AsynchronousInvocation#getDelayInMilliseconds <em>Delay In
	 * Milliseconds</em>}' attribute
	 * @param value the new value of the '<em>Delay In Milliseconds</em>' attribute
	 * @see #getDelayInMilliseconds()
	 * @generated
	 */
	void setDelayInMilliseconds(Integer value);

}
