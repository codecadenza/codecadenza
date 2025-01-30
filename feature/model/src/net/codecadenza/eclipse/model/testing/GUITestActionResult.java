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
package net.codecadenza.eclipse.model.testing;

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>GUI Test Action Result</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getMessageText <em>Message Text</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getStatus <em>Status</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction <em>Test Action</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getComponentType <em>Component Type</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult()
 * @model
 * @generated
 */
public interface GUITestActionResult extends EObject {
	/**
	 * Return the value of the '<em><b>Message Text</b></em>' attribute
	 * @return the value of the '<em>Message Text</em>' attribute
	 * @see #setMessageText(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_MessageText()
	 * @model
	 * @generated
	 */
	String getMessageText();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getMessageText <em>Message Text</em>}'
	 * attribute
	 * @param value the new value of the '<em>Message Text</em>' attribute
	 * @see #getMessageText()
	 * @generated
	 */
	void setMessageText(String value);

	/**
	 * Return the value of the '<em><b>Status</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.testing.GUITestActionStatus}.
	 * @return the value of the '<em>Status</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionStatus
	 * @see #setStatus(GUITestActionStatus)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_Status()
	 * @model
	 * @generated
	 */
	GUITestActionStatus getStatus();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getStatus <em>Status</em>}' attribute
	 * @param value the new value of the '<em>Status</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionStatus
	 * @see #getStatus()
	 * @generated
	 */
	void setStatus(GUITestActionStatus value);

	/**
	 * Return the value of the '<em><b>Test Action</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult <em>Action Result</em>}'.
	 * @return the value of the '<em>Test Action</em>' container reference
	 * @see #setTestAction(GUITestAction)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_TestAction()
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult
	 * @model opposite="actionResult" transient="false"
	 * @generated
	 */
	GUITestAction getTestAction();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction <em>Test Action</em>}'
	 * container reference
	 * @param value the new value of the '<em>Test Action</em>' container reference
	 * @see #getTestAction()
	 * @generated
	 */
	void setTestAction(GUITestAction value);

	/**
	 * Return the value of the '<em><b>Component Type</b></em>' attribute
	 * @return the value of the '<em>Component Type</em>' attribute
	 * @see #setComponentType(GUITestActionResultComponentType)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_ComponentType()
	 * @model
	 * @generated
	 */
	GUITestActionResultComponentType getComponentType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getComponentType <em>Component
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Driver</em>' attribute
	 * @see #getComponentType()
	 * @generated
	 */
	void setComponentType(GUITestActionResultComponentType value);

}
