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

import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Form Group</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getGroupOrder <em>Group Order</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getParentGroup <em>Parent Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getChildGroups <em>Child Groups</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getForms <em>Forms</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getPanels <em>Panels</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormGroup#getProject <em>Project</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup()
 * @model
 * @generated
 */
public interface FormGroup extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormGroup#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Group Order</b></em>' attribute
	 * @return the value of the '<em>Group Order</em>' attribute
	 * @see #setGroupOrder(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_GroupOrder()
	 * @model
	 * @generated
	 */
	int getGroupOrder();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormGroup#getGroupOrder <em>Group Order</em>}' attribute
	 * @param value the new value of the '<em>Group Order</em>' attribute
	 * @see #getGroupOrder()
	 * @generated
	 */
	void setGroupOrder(int value);

	/**
	 * Return the value of the '<em><b>Parent Group</b></em>' reference
	 * @return the value of the '<em>Parent Group</em>' reference
	 * @see #setParentGroup(FormGroup)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_ParentGroup()
	 * @model
	 * @generated
	 */
	FormGroup getParentGroup();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormGroup#getParentGroup <em>Parent Group</em>}' reference
	 * @param value the new value of the '<em>Parent Group</em>' reference
	 * @see #getParentGroup()
	 * @generated
	 */
	void setParentGroup(FormGroup value);

	/**
	 * Return the value of the '<em><b>Child Groups</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormGroup}.
	 * @return the value of the '<em>Child Groups</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_ChildGroups()
	 * @model containment="true"
	 * @generated
	 */
	EList<FormGroup> getChildGroups();

	/**
	 * Return the value of the '<em><b>Forms</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.Form}. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.client.Form#getFormGroup <em>Form Group</em>}'.
	 * @return the value of the '<em>Forms</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Forms()
	 * @see net.codecadenza.eclipse.model.client.Form#getFormGroup
	 * @model opposite="formGroup"
	 * @generated
	 */
	EList<Form> getForms();

	/**
	 * Return the value of the '<em><b>Panels</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormPanel}. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.client.FormPanel#getFormGroup <em>Form Group</em>}'.
	 * @return the value of the '<em>Panels</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Panels()
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFormGroup
	 * @model opposite="formGroup"
	 * @generated
	 */
	EList<FormPanel> getPanels();

	/**
	 * Return the value of the '<em><b>Roles</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.Role}.
	 * @return the value of the '<em>Roles</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Roles()
	 * @model
	 * @generated
	 */
	EList<Role> getRoles();

	/**
	 * Return the value of the '<em><b>Project</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.project.Project#getFormGroups <em>Form Groups</em>}'.
	 * @return the value of the '<em>Project</em>' reference
	 * @see #setProject(Project)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Project()
	 * @see net.codecadenza.eclipse.model.project.Project#getFormGroups
	 * @model opposite="formGroups"
	 * @generated
	 */
	Project getProject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormGroup#getProject <em>Project</em>}' reference
	 * @param value the new value of the '<em>Project</em>' reference
	 * @see #getProject()
	 * @generated
	 */
	void setProject(Project value);

	/**
	 * @return the project this form group belongs to
	 * @generated not
	 */
	Project findProject();

}
