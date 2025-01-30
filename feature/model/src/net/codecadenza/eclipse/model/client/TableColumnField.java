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
 * A representation of the model object '<em><b>Table Column Field</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#isVisible <em>Visible</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#isIdentifier <em>Identifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getFormTable <em>Form Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getWidth <em>Width</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getTitle <em>Title</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getLovForm <em>Lov Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getFieldType <em>Field Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#isAssociationRef <em>Association Ref</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#isSearchable <em>Searchable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TableColumnField#getDTOAttribute <em>DTO Attribute</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField()
 * @model
 * @generated
 */
public interface TableColumnField extends EObject {
	/**
	 * Return the value of the '<em><b>Col Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Col Index</em>' attribute
	 * @see #setColIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_ColIndex()
	 * @model default="1"
	 * @generated
	 */
	int getColIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getColIndex <em>Col Index</em>}' attribute
	 * @param value the new value of the '<em>Col Index</em>' attribute
	 * @see #getColIndex()
	 * @generated
	 */
	void setColIndex(int value);

	/**
	 * Return the value of the '<em><b>Visible</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Visible</em>' attribute
	 * @see #setVisible(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Visible()
	 * @model default="true"
	 * @generated
	 */
	boolean isVisible();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#isVisible <em>Visible</em>}' attribute
	 * @param value the new value of the '<em>Visible</em>' attribute
	 * @see #isVisible()
	 * @generated
	 */
	void setVisible(boolean value);

	/**
	 * Return the value of the '<em><b>Identifier</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Identifier</em>' attribute
	 * @see #setIdentifier(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Identifier()
	 * @model default="false"
	 * @generated
	 */
	boolean isIdentifier();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#isIdentifier <em>Identifier</em>}'
	 * attribute
	 * @param value the new value of the '<em>Identifier</em>' attribute
	 * @see #isIdentifier()
	 * @generated
	 */
	void setIdentifier(boolean value);

	/**
	 * Return the value of the '<em><b>Form Table</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormTable#getFields <em>Fields</em>} '.
	 * @return the value of the '<em>Form Table</em>' container reference
	 * @see #setFormTable(FormTable)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_FormTable()
	 * @see net.codecadenza.eclipse.model.client.FormTable#getFields
	 * @model opposite="fields"
	 * @generated
	 */
	FormTable getFormTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getFormTable <em>Form Table</em>}'
	 * container reference
	 * @param value the new value of the '<em>Form Table</em>' container reference
	 * @see #getFormTable()
	 * @generated
	 */
	void setFormTable(FormTable value);

	/**
	 * Return the value of the '<em><b>Width</b></em>' attribute. The default value is <code>"100"</code>.
	 * @return the value of the '<em>Width</em>' attribute
	 * @see #setWidth(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Width()
	 * @model default="100"
	 * @generated
	 */
	int getWidth();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getWidth <em>Width</em>}' attribute
	 * @param value the new value of the '<em>Width</em>' attribute
	 * @see #getWidth()
	 * @generated
	 */
	void setWidth(int value);

	/**
	 * Return the value of the '<em><b>Title</b></em>' attribute
	 * @return the value of the '<em>Title</em>' attribute
	 * @see #setTitle(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Title()
	 * @model
	 * @generated
	 */
	String getTitle();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getTitle <em>Title</em>}' attribute
	 * @param value the new value of the '<em>Title</em>' attribute
	 * @see #getTitle()
	 * @generated
	 */
	void setTitle(String value);

	/**
	 * Return the value of the '<em><b>Lov Form</b></em>' reference
	 * @return the value of the '<em>Lov Form</em>' reference
	 * @see #setLovForm(Form)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_LovForm()
	 * @model
	 * @generated
	 */
	Form getLovForm();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getLovForm <em>Lov Form</em>}' reference
	 * @param value the new value of the '<em>Lov Form</em>' reference
	 * @see #getLovForm()
	 * @generated
	 */
	void setLovForm(Form value);

	/**
	 * Return the value of the '<em><b>Field Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration}
	 * @return the value of the '<em>Field Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration
	 * @see #setFieldType(TableColumnFieldTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_FieldType()
	 * @model
	 * @generated
	 */
	TableColumnFieldTypeEnumeration getFieldType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getFieldType <em>Field Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Field Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration
	 * @see #getFieldType()
	 * @generated
	 */
	void setFieldType(TableColumnFieldTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Association Ref</b></em>' attribute
	 * @return the value of the '<em>Association Ref</em>' attribute
	 * @see #setAssociationRef(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_AssociationRef()
	 * @model
	 * @generated
	 */
	boolean isAssociationRef();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#isAssociationRef <em>Association Ref</em>}'
	 * attribute
	 * @param value the new value of the '<em>Association Ref</em>' attribute
	 * @see #isAssociationRef()
	 * @generated
	 */
	void setAssociationRef(boolean value);

	/**
	 * Return the value of the '<em><b>Searchable</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Searchable</em>' attribute
	 * @see #setSearchable(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Searchable()
	 * @model default="true"
	 * @generated
	 */
	boolean isSearchable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#isSearchable <em>Searchable</em>}'
	 * attribute
	 * @param value the new value of the '<em>Searchable</em>' attribute
	 * @see #isSearchable()
	 * @generated
	 */
	void setSearchable(boolean value);

	/**
	 * Return the value of the '<em><b>DTO Attribute</b></em>' reference
	 * @return the value of the '<em>DTO Attribute</em>' reference
	 * @see #setDTOAttribute(DTOBeanAttribute)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_DTOAttribute()
	 * @model
	 * @generated
	 */
	DTOBeanAttribute getDTOAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TableColumnField#getDTOAttribute <em>DTO Attribute</em>}'
	 * reference
	 * @param value the new value of the '<em>DTO Attribute</em>' reference
	 * @see #getDTOAttribute()
	 * @generated
	 */
	void setDTOAttribute(DTOBeanAttribute value);

	/**
	 * @return true if the field type is either 'DATE', 'GREGORIAN_CALENDAR', 'LOCAL_DATE' or 'LOCAL_DATE_TIME'
	 * @generated not
	 */
	boolean hasTemporalType();

	/**
	 * @return true if a temporal type uses a date format (without time)
	 * @generated not
	 */
	boolean hasDateFormat();

}
