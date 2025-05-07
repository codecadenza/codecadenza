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

import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Form Field</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getRowIndex <em>Row Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#isVisible <em>Visible</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#isSpanCols <em>Span Cols</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#isReadonly <em>Readonly</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#isMandatory <em>Mandatory</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getFieldType <em>Field Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getPanel <em>Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getWidth <em>Width</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getDefaultValue <em>Default Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getListOfValues <em>List Of Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#getDTOAttribute <em>DTO Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormField#isAddFormLinkToLabel <em>Add Form Link To Label</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField()
 * @model
 * @generated
 */
public interface FormField extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Col Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Col Index</em>' attribute
	 * @see #setColIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_ColIndex()
	 * @model default="1"
	 * @generated
	 */
	int getColIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getColIndex <em>Col Index</em>}' attribute
	 * @param value the new value of the '<em>Col Index</em>' attribute
	 * @see #getColIndex()
	 * @generated
	 */
	void setColIndex(int value);

	/**
	 * Return the value of the '<em><b>Row Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Row Index</em>' attribute
	 * @see #setRowIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_RowIndex()
	 * @model default="1"
	 * @generated
	 */
	int getRowIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getRowIndex <em>Row Index</em>}' attribute
	 * @param value the new value of the '<em>Row Index</em>' attribute
	 * @see #getRowIndex()
	 * @generated
	 */
	void setRowIndex(int value);

	/**
	 * Return the value of the '<em><b>Visible</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Visible</em>' attribute
	 * @see #setVisible(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Visible()
	 * @model default="true"
	 * @generated
	 */
	boolean isVisible();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#isVisible <em>Visible</em>}' attribute
	 * @param value the new value of the '<em>Visible</em>' attribute
	 * @see #isVisible()
	 * @generated
	 */
	void setVisible(boolean value);

	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Return the value of the '<em><b>Span Cols</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Span Cols</em>' attribute
	 * @see #setSpanCols(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_SpanCols()
	 * @model default="false"
	 * @generated
	 */
	boolean isSpanCols();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#isSpanCols <em>Span Cols</em>}' attribute
	 * @param value the new value of the '<em>Span Cols</em>' attribute
	 * @see #isSpanCols()
	 * @generated
	 */
	void setSpanCols(boolean value);

	/**
	 * Return the value of the '<em><b>Readonly</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Readonly</em>' attribute
	 * @see #setReadonly(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Readonly()
	 * @model default="false"
	 * @generated
	 */
	boolean isReadonly();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#isReadonly <em>Readonly</em>}' attribute
	 * @param value the new value of the '<em>Readonly</em>' attribute
	 * @see #isReadonly()
	 * @generated
	 */
	void setReadonly(boolean value);

	/**
	 * Return the value of the '<em><b>Mandatory</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Mandatory</em>' attribute
	 * @see #setMandatory(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Mandatory()
	 * @model default="true"
	 * @generated
	 */
	boolean isMandatory();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#isMandatory <em>Mandatory</em>}' attribute
	 * @param value the new value of the '<em>Mandatory</em>' attribute
	 * @see #isMandatory()
	 * @generated
	 */
	void setMandatory(boolean value);

	/**
	 * Return the value of the '<em><b>Field Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration}.
	 * @return the value of the '<em>Field Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration
	 * @see #setFieldType(FormFieldTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_FieldType()
	 * @model
	 * @generated
	 */
	FormFieldTypeEnumeration getFieldType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getFieldType <em>Field Type</em>}' attribute
	 * @param value the new value of the '<em>Field Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration
	 * @see #getFieldType()
	 * @generated
	 */
	void setFieldType(FormFieldTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Panel</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormPanel#getFields <em>Fields</em>}'.
	 * @return the value of the '<em>Panel</em>' container reference
	 * @see #setPanel(FormPanel)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Panel()
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFields
	 * @model opposite="fields"
	 * @generated
	 */
	FormPanel getPanel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getPanel <em>Panel</em>}' container reference
	 * @param value the new value of the '<em>Panel</em>' container reference
	 * @see #getPanel()
	 * @generated
	 */
	void setPanel(FormPanel value);

	/**
	 * Return the value of the '<em><b>Width</b></em>' attribute. The default value is <code>"0"</code>.
	 * @return the value of the '<em>Width</em>' attribute
	 * @see #setWidth(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Width()
	 * @model default="0"
	 * @generated
	 */
	int getWidth();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getWidth <em>Width</em>}' attribute
	 * @param value the new value of the '<em>Width</em>' attribute
	 * @see #getWidth()
	 * @generated
	 */
	void setWidth(int value);

	/**
	 * Return the value of the '<em><b>Default Value</b></em>' attribute
	 * @return the value of the '<em>Default Value</em>' attribute
	 * @see #setDefaultValue(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_DefaultValue()
	 * @model
	 * @generated
	 */
	String getDefaultValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getDefaultValue <em>Default Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Default Value</em>' attribute
	 * @see #getDefaultValue()
	 * @generated
	 */
	void setDefaultValue(String value);

	/**
	 * Return the value of the '<em><b>List Of Values</b></em>' reference
	 * @return the value of the '<em>List Of Values</em>' reference
	 * @see #setListOfValues(Form)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_ListOfValues()
	 * @model
	 * @generated
	 */
	Form getListOfValues();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getListOfValues <em>List Of Values</em>}'
	 * reference
	 * @param value the new value of the '<em>List Of Values</em>' reference
	 * @see #getListOfValues()
	 * @generated
	 */
	void setListOfValues(Form value);

	/**
	 * Return the value of the '<em><b>DTO Attribute</b></em>' reference
	 * @return the value of the '<em>DTO Attribute</em>' reference
	 * @see #setDTOAttribute(DTOBeanAttribute)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_DTOAttribute()
	 * @model
	 * @generated
	 */
	DTOBeanAttribute getDTOAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#getDTOAttribute <em>DTO Attribute</em>}'
	 * reference
	 * @param value the new value of the '<em>DTO Attribute</em>' reference
	 * @see #getDTOAttribute()
	 * @generated
	 */
	void setDTOAttribute(DTOBeanAttribute value);

	/**
	 * Return the value of the '<em><b>Add Form Link To Label</b></em>' attribute
	 * @return the value of the '<em>Add Form Link To Label</em>' attribute
	 * @see #setAddFormLinkToLabel(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_AddFormLinkToLabel()
	 * @model
	 * @generated
	 */
	boolean isAddFormLinkToLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormField#isAddFormLinkToLabel <em>Add Form Link To
	 * Label</em>}' attribute
	 * @param value the new value of the '<em>Add Form Link To Label</em>' attribute
	 * @see #isAddFormLinkToLabel()
	 * @generated
	 */
	void setAddFormLinkToLabel(boolean value);

	/**
	 * @return true if the field isn't visible or if it has no visual representation
	 * @generated not
	 */
	boolean isHidden();

	/**
	 * Search for a suitable list-of-values form
	 * @return a list-of-values form. Null will be returned if no corresponding form could be found or if it must not be used in
	 *         this context!
	 * @generated not
	 */
	Form findListOfValues();

	/**
	 * Check if an empty column in the field's grid layout must be filled
	 * @return true if either the previous or the next column must be filled
	 * @generated not
	 */
	boolean fillEmptyGridColumn();

	/**
	 * @return an expression that converts the field's default value string into the respective Java type
	 * @throws IllegalStateException if the Java type of this field is not supported
	 * @generated not
	 */
	String getConvertedDefaultValue();

}
