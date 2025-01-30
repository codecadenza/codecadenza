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
package net.codecadenza.eclipse.model.client;

import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.project.Role;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Form Action</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getDescription <em>Description</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getForm <em>Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getPanel <em>Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getTargetForm <em>Target Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getBoundaryMethod <em>Boundary Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getType <em>Action Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormAction#getLabel <em>Label</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction()
 * @model
 * @generated
 */
public interface FormAction extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Description</b></em>' attribute
	 * @return the value of the '<em>Description</em>' attribute
	 * @see #setDescription(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Description()
	 * @model
	 * @generated
	 */
	String getDescription();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getDescription <em>Description</em>}' attribute
	 * @param value the new value of the '<em>Description</em>' attribute
	 * @see #getDescription()
	 * @generated
	 */
	void setDescription(String value);

	/**
	 * Return the value of the '<em><b>Form</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.Form#getActions <em>Actions</em>}'.
	 * @return the value of the '<em>Form</em>' container reference
	 * @see #setForm(Form)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Form()
	 * @see net.codecadenza.eclipse.model.client.Form#getActions
	 * @model opposite="actions"
	 * @generated
	 */
	Form getForm();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getForm <em>Form</em>}' container reference
	 * @param value the new value of the '<em>Form</em>' container reference
	 * @see #getForm()
	 * @generated
	 */
	void setForm(Form value);

	/**
	 * Return the value of the '<em><b>Panel</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormPanel#getActions <em>Actions</em>}'.
	 * @return the value of the '<em>Panel</em>' container reference
	 * @see #setPanel(FormPanel)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Panel()
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getActions
	 * @model opposite="actions"
	 * @generated
	 */
	FormPanel getPanel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getPanel <em>Panel</em>}' container reference
	 * @param value the new value of the '<em>Panel</em>' container reference
	 * @see #getPanel()
	 * @generated
	 */
	void setPanel(FormPanel value);

	/**
	 * Return the value of the '<em><b>Target Form</b></em>' reference
	 * @return the value of the '<em>Target Form</em>' reference
	 * @see #setTargetForm(Form)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_TargetForm()
	 * @model
	 * @generated
	 */
	Form getTargetForm();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getTargetForm <em>Target Form</em>}' reference
	 * @param value the new value of the '<em>Target Form</em>' reference
	 * @see #getTargetForm()
	 * @generated
	 */
	void setTargetForm(Form value);

	/**
	 * Return the value of the '<em><b>Boundary Method</b></em>' reference
	 * @return the value of the '<em>Boundary Method</em>' reference
	 * @see #setBoundaryMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_BoundaryMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getBoundaryMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getBoundaryMethod <em>Boundary Method</em>}'
	 * reference
	 * @param value the new value of the '<em>Boundary Method</em>' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	void setBoundaryMethod(BoundaryMethod value);

	/**
	 * Return the value of the '<em><b>Roles</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.Role}.
	 * @return the value of the '<em>Roles</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Roles()
	 * @model
	 * @generated
	 */
	EList<Role> getRoles();

	/**
	 * Return the value of the '<em><b>Action Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.client.ActionType}.
	 * @return the value of the '<em>Action Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.ActionType
	 * @see #setType(ActionType)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Type()
	 * @model
	 * @generated
	 */
	ActionType getType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getType <em>Action Type</em>}' attribute
	 * @param value the new value of the '<em>Action Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.ActionType
	 * @see #getType()
	 * @generated
	 */
	void setType(ActionType value);

	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormAction#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

}
