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
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Form</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getFormType <em>Form Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getFormGroup <em>Form Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getTitle <em>Title</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#isModal <em>Modal</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#isResizable <em>Resizable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#isTitleArea <em>Title Area</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#isOpenEditAfterCreate <em>Open Edit After Create</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getHeight <em>Height</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getWidth <em>Width</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getFormPanels <em>Form Panels</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getActions <em>Actions</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getDTO <em>DTO</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.Form#getBoundaryMethod <em>Boundary Method</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm()
 * @model
 * @generated
 */
public interface Form extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Form Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.client.FormTypeEnumeration}.
	 * @return the value of the '<em>Form Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.FormTypeEnumeration
	 * @see #setFormType(FormTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_FormType()
	 * @model
	 * @generated
	 */
	FormTypeEnumeration getFormType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getFormType <em>Form Type</em>}' attribute
	 * @param value the new value of the '<em>Form Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.FormTypeEnumeration
	 * @see #getFormType()
	 * @generated
	 */
	void setFormType(FormTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Form Group</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormGroup#getForms <em>Forms</em>}'.
	 * @return the value of the '<em>Form Group</em>' reference
	 * @see #setFormGroup(FormGroup)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_FormGroup()
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getForms
	 * @model opposite="forms"
	 * @generated
	 */
	FormGroup getFormGroup();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getFormGroup <em>Form Group</em>}' reference
	 * @param value the new value of the '<em>Form Group</em>' reference
	 * @see #getFormGroup()
	 * @generated
	 */
	void setFormGroup(FormGroup value);

	/**
	 * Return the value of the '<em><b>Title</b></em>' attribute
	 * @return the value of the '<em>Title</em>' attribute
	 * @see #setTitle(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Title()
	 * @model
	 * @generated
	 */
	String getTitle();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getTitle <em>Title</em>}' attribute
	 * @param value the new value of the '<em>Title</em>' attribute
	 * @see #getTitle()
	 * @generated
	 */
	void setTitle(String value);

	/**
	 * Return the value of the '<em><b>Modal</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Modal</em>' attribute
	 * @see #setModal(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Modal()
	 * @model default="false"
	 * @generated
	 */
	boolean isModal();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#isModal <em>Modal</em>}' attribute
	 * @param value the new value of the '<em>Modal</em>' attribute
	 * @see #isModal()
	 * @generated
	 */
	void setModal(boolean value);

	/**
	 * Return the value of the '<em><b>Resizable</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Resizable</em>' attribute
	 * @see #setResizable(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Resizable()
	 * @model default="true"
	 * @generated
	 */
	boolean isResizable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#isResizable <em>Resizable</em>}' attribute
	 * @param value the new value of the '<em>Resizable</em>' attribute
	 * @see #isResizable()
	 * @generated
	 */
	void setResizable(boolean value);

	/**
	 * Return the value of the '<em><b>Title Area</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Title Area</em>' attribute
	 * @see #setTitleArea(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_TitleArea()
	 * @model default="true"
	 * @generated
	 */
	boolean isTitleArea();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#isTitleArea <em>Title Area</em>}' attribute
	 * @param value the new value of the '<em>Title Area</em>' attribute
	 * @see #isTitleArea()
	 * @generated
	 */
	void setTitleArea(boolean value);

	/**
	 * Return the value of the '<em><b>Open Edit After Create</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Open Edit After Create</em>' attribute
	 * @see #setOpenEditAfterCreate(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_OpenEditAfterCreate()
	 * @model default="false"
	 * @generated
	 */
	boolean isOpenEditAfterCreate();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#isOpenEditAfterCreate <em>Open Edit After
	 * Create</em>}' attribute
	 * @param value the new value of the '<em>Open Edit After Create</em>' attribute
	 * @see #isOpenEditAfterCreate()
	 * @generated
	 */
	void setOpenEditAfterCreate(boolean value);

	/**
	 * Return the value of the '<em><b>Height</b></em>' attribute
	 * @return the value of the '<em>Height</em>' attribute
	 * @see #setHeight(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Height()
	 * @model
	 * @generated
	 */
	int getHeight();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getHeight <em>Height</em>}' attribute
	 * @param value the new value of the '<em>Height</em>' attribute
	 * @see #getHeight()
	 * @generated
	 */
	void setHeight(int value);

	/**
	 * Return the value of the '<em><b>Width</b></em>' attribute
	 * @return the value of the '<em>Width</em>' attribute
	 * @see #setWidth(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Width()
	 * @model
	 * @generated
	 */
	int getWidth();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getWidth <em>Width</em>}' attribute
	 * @param value the new value of the '<em>Width</em>' attribute
	 * @see #getWidth()
	 * @generated
	 */
	void setWidth(int value);

	/**
	 * Return the value of the '<em><b>Form Panels</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormPanel}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormPanel#getForm <em>Form</em>}'.
	 * @return the value of the '<em>Form Panels</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_FormPanels()
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getForm
	 * @model opposite="form"
	 * @generated
	 */
	EList<FormPanel> getFormPanels();

	/**
	 * Return the value of the '<em><b>Actions</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormAction}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormAction#getForm <em>Form</em>}'.
	 * @return the value of the '<em>Actions</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Actions()
	 * @see net.codecadenza.eclipse.model.client.FormAction#getForm
	 * @model opposite="form" containment="true"
	 * @generated
	 */
	EList<FormAction> getActions();

	/**
	 * Return the value of the '<em><b>DTO</b></em>' reference
	 * @return the value of the '<em>DTO</em>' reference
	 * @see #setDTO(DTOBean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_DTO()
	 * @model
	 * @generated
	 */
	DTOBean getDTO();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getDTO <em>DTO</em>}' reference
	 * @param value the new value of the '<em>DTO</em>' reference
	 * @see #getDTO()
	 * @generated
	 */
	void setDTO(DTOBean value);

	/**
	 * Return the value of the '<em><b>Domain Object</b></em>' reference
	 * @return the value of the '<em>Domain Object</em>' reference
	 * @see #setDomainObject(DomainObject)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_DomainObject()
	 * @model
	 * @generated
	 */
	DomainObject getDomainObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getDomainObject <em>Domain Object</em>}' reference
	 * @param value the new value of the '<em>Domain Object</em>' reference
	 * @see #getDomainObject()
	 * @generated
	 */
	void setDomainObject(DomainObject value);

	/**
	 * Return the value of the '<em><b>Roles</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.project.Role}.
	 * @return the value of the '<em>Roles</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Roles()
	 * @model
	 * @generated
	 */
	EList<Role> getRoles();

	/**
	 * Return the value of the '<em><b>Boundary Method</b></em>' reference
	 * @return the value of the '<em>Boundary Method</em>' reference
	 * @see #setBoundaryMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_BoundaryMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getBoundaryMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.Form#getBoundaryMethod <em>Boundary Method</em>}' reference
	 * @param value the new value of the '<em>Boundary Method</em>' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	void setBoundaryMethod(BoundaryMethod value);

	/**
	 * @return the lower-case name of the form
	 * @generated not
	 */
	String getLowerCaseName();

	/**
	 * @return a list containing all form fields of this form
	 * @generated not
	 */
	EList<FormField> getAllFormFields();

	/**
	 * Rearrange all form fields
	 * @generated not
	 */
	void rearrangeFields();

	/**
	 * @return the internal representation of the form source file
	 * @generated not
	 */
	JavaFile getSourceFile();

	/**
	 * @return the internal representation of the source file for an Angular application
	 * @generated not
	 */
	WorkspaceFile getTypeScriptSourceFile();

	/**
	 * @return the internal representation of the page object source file
	 * @generated not
	 */
	JavaFile getPageObjectSourceFile();

	/**
	 * @return the resource path for the form. An empty string will be returned if the form doesn't belong to a web application!
	 * @generated not
	 */
	String getResourcePath();

	/**
	 * @return the corresponding file that contains the visual representation of a form. It returns null if the given technology
	 *         doesn't support the separation of user interface and respective logic! Note that the returned object doesn't contain
	 *         the file content!
	 * @generated not
	 */
	WorkspaceFile getUserInterfaceFile();

	/**
	 * @return the form panel of this view form
	 * @throws IllegalStateException if a form panel doesn't exist
	 * @generated not
	 */
	FormPanel getViewFormPanel();
}
