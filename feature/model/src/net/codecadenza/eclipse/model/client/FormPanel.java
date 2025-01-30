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
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Form Panel</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getColumnCount <em>Column Count</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getRowIndex <em>Row Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#isVerticalspan <em>Verticalspan</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#isHorizontalSpan <em>Horizontal Span</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getFormGroup <em>Form Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getForm <em>Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#isDrawBorder <em>Draw Border</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getBasePanel <em>Base Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getFormTable <em>Form Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getFields <em>Fields</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getActions <em>Actions</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getDTO <em>DTO</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getBoundaryMethod <em>Boundary Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormPanel#getAssociation <em>Association</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel()
 * @model
 * @generated
 */
public interface FormPanel extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Column Count</b></em>' attribute
	 * @return the value of the '<em>Column Count</em>' attribute
	 * @see #setColumnCount(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_ColumnCount()
	 * @model
	 * @generated
	 */
	int getColumnCount();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getColumnCount <em>Column Count</em>}' attribute
	 * @param value the new value of the '<em>Column Count</em>' attribute
	 * @see #getColumnCount()
	 * @generated
	 */
	void setColumnCount(int value);

	/**
	 * Return the value of the '<em><b>Col Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Col Index</em>' attribute
	 * @see #setColIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_ColIndex()
	 * @model default="1"
	 * @generated
	 */
	int getColIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getColIndex <em>Col Index</em>}' attribute
	 * @param value the new value of the '<em>Col Index</em>' attribute
	 * @see #getColIndex()
	 * @generated
	 */
	void setColIndex(int value);

	/**
	 * Return the value of the '<em><b>Row Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Row Index</em>' attribute
	 * @see #setRowIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_RowIndex()
	 * @model default="1"
	 * @generated
	 */
	int getRowIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getRowIndex <em>Row Index</em>}' attribute
	 * @param value the new value of the '<em>Row Index</em>' attribute
	 * @see #getRowIndex()
	 * @generated
	 */
	void setRowIndex(int value);

	/**
	 * Return the value of the '<em><b>Verticalspan</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Verticalspan</em>' attribute
	 * @see #setVerticalspan(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Verticalspan()
	 * @model default="false"
	 * @generated
	 */
	boolean isVerticalspan();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#isVerticalspan <em>Verticalspan</em>}' attribute
	 * @param value the new value of the '<em>Verticalspan</em>' attribute
	 * @see #isVerticalspan()
	 * @generated
	 */
	void setVerticalspan(boolean value);

	/**
	 * Return the value of the '<em><b>Horizontal Span</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Horizontal Span</em>' attribute
	 * @see #setHorizontalSpan(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_HorizontalSpan()
	 * @model default="false"
	 * @generated
	 */
	boolean isHorizontalSpan();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#isHorizontalSpan <em>Horizontal Span</em>}'
	 * attribute
	 * @param value the new value of the '<em>Horizontal Span</em>' attribute
	 * @see #isHorizontalSpan()
	 * @generated
	 */
	void setHorizontalSpan(boolean value);

	/**
	 * Return the value of the '<em><b>Form Group</b></em>' reference It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormGroup#getPanels <em>Panels</em>}'.
	 * @return the value of the '<em>Form Group</em>' reference
	 * @see #setFormGroup(FormGroup)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_FormGroup()
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getPanels
	 * @model opposite="panels"
	 * @generated
	 */
	FormGroup getFormGroup();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getFormGroup <em>Form Group</em>}' reference
	 * @param value the new value of the '<em>Form Group</em>' reference
	 * @see #getFormGroup()
	 * @generated
	 */
	void setFormGroup(FormGroup value);

	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Return the value of the '<em><b>Form</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.Form#getFormPanels <em>Form Panels</em>}'.
	 * @return the value of the '<em>Form</em>' reference
	 * @see #setForm(Form)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Form()
	 * @see net.codecadenza.eclipse.model.client.Form#getFormPanels
	 * @model opposite="formPanels"
	 * @generated
	 */
	Form getForm();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getForm <em>Form</em>}' reference
	 * @param value the new value of the '<em>Form</em>' reference
	 * @see #getForm()
	 * @generated
	 */
	void setForm(Form value);

	/**
	 * Return the value of the '<em><b>Draw Border</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Draw Border</em>' attribute
	 * @see #setDrawBorder(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_DrawBorder()
	 * @model default="true"
	 * @generated
	 */
	boolean isDrawBorder();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#isDrawBorder <em>Draw Border</em>}' attribute
	 * @param value the new value of the '<em>Draw Border</em>' attribute
	 * @see #isDrawBorder()
	 * @generated
	 */
	void setDrawBorder(boolean value);

	/**
	 * Return the value of the '<em><b>Base Panel</b></em>' reference
	 * @return the value of the '<em>Base Panel</em>' reference
	 * @see #setBasePanel(FormPanel)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_BasePanel()
	 * @model
	 * @generated
	 */
	FormPanel getBasePanel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getBasePanel <em>Base Panel</em>}' reference
	 * @param value the new value of the '<em>Base Panel</em>' reference
	 * @see #getBasePanel()
	 * @generated
	 */
	void setBasePanel(FormPanel value);

	/**
	 * Return the value of the '<em><b>Form Table</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormTable#getFormPanel <em>Form Panel</em>}'.
	 * @return the value of the '<em>Form Table</em>' reference
	 * @see #setFormTable(FormTable)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_FormTable()
	 * @see net.codecadenza.eclipse.model.client.FormTable#getFormPanel
	 * @model opposite="formPanel"
	 * @generated
	 */
	FormTable getFormTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getFormTable <em>Form Table</em>}' reference
	 * @param value the new value of the '<em>Form Table</em>' reference
	 * @see #getFormTable()
	 * @generated
	 */
	void setFormTable(FormTable value);

	/**
	 * Return the value of the '<em><b>Fields</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormField}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormField#getPanel <em>Panel</em>}'.
	 * @return the value of the '<em>Fields</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Fields()
	 * @see net.codecadenza.eclipse.model.client.FormField#getPanel
	 * @model opposite="panel" containment="true"
	 * @generated
	 */
	EList<FormField> getFields();

	/**
	 * Return the value of the '<em><b>Actions</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.FormAction}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormAction#getPanel <em>Panel</em>}'.
	 * @return the value of the '<em>Actions</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Actions()
	 * @see net.codecadenza.eclipse.model.client.FormAction#getPanel
	 * @model opposite="panel" containment="true"
	 * @generated
	 */
	EList<FormAction> getActions();

	/**
	 * Return the value of the '<em><b>DTO</b></em>' reference
	 * @return the value of the '<em>DTO</em>' reference
	 * @see #setDTO(DTOBean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_DTO()
	 * @model
	 * @generated
	 */
	DTOBean getDTO();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getDTO <em>DTO</em>}' reference
	 * @param value the new value of the '<em>DTO</em>' reference
	 * @see #getDTO()
	 * @generated
	 */
	void setDTO(DTOBean value);

	/**
	 * Return the value of the '<em><b>Boundary Method</b></em>' reference
	 * @return the value of the '<em>Boundary Method</em>' reference
	 * @see #setBoundaryMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_BoundaryMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getBoundaryMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getBoundaryMethod <em>Boundary Method</em>}'
	 * reference
	 * @param value the new value of the '<em>Boundary Method</em>' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	void setBoundaryMethod(BoundaryMethod value);

	/**
	 * Return the value of the '<em><b>Association</b></em>' reference
	 * @return the value of the '<em>Association</em>' reference
	 * @see #setAssociation(AbstractDomainAssociation)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Association()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormPanel#getAssociation <em>Association</em>}' reference
	 * @param value the new value of the '<em>Association</em>' reference
	 * @see #getAssociation()
	 * @generated
	 */
	void setAssociation(AbstractDomainAssociation value);

	/**
	 * Rearrange all form fields
	 * @generated not
	 */
	void rearrangeFields();

	/**
	 * @return the internal representation of the form panel source file
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
	 * @return the corresponding file that contains the visual representation of a grid panel. It returns null if the given
	 *         technology doesn't support the separation of user interface and respective logic! Note that the returned object
	 *         doesn't contain the file content!
	 * @generated not
	 */
	WorkspaceFile getUserInterfaceFile();
}
