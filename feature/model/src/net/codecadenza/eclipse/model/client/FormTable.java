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

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Form Table</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getRowIndex <em>Row Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getSpanCols <em>Span Cols</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getSpanRows <em>Span Rows</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#isVerticalSpan <em>Vertical Span</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#isHorizontalSpan <em>Horizontal Span</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getFormPanel <em>Form Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getFields <em>Fields</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.FormTable#getAssociation <em>Association</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable()
 * @model
 * @generated
 */
public interface FormTable extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Col Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Col Index</em>' attribute
	 * @see #setColIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_ColIndex()
	 * @model default="1"
	 * @generated
	 */
	int getColIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getColIndex <em>Col Index</em>}' attribute
	 * @param value the new value of the '<em>Col Index</em>' attribute
	 * @see #getColIndex()
	 * @generated
	 */
	void setColIndex(int value);

	/**
	 * Return the value of the '<em><b>Row Index</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Row Index</em>' attribute
	 * @see #setRowIndex(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_RowIndex()
	 * @model default="1"
	 * @generated
	 */
	int getRowIndex();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getRowIndex <em>Row Index</em>}' attribute
	 * @param value the new value of the '<em>Row Index</em>' attribute
	 * @see #getRowIndex()
	 * @generated
	 */
	void setRowIndex(int value);

	/**
	 * Return the value of the '<em><b>Span Cols</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Span Cols</em>' attribute
	 * @see #setSpanCols(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_SpanCols()
	 * @model default="1"
	 * @generated
	 */
	int getSpanCols();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getSpanCols <em>Span Cols</em>}' attribute
	 * @param value the new value of the '<em>Span Cols</em>' attribute
	 * @see #getSpanCols()
	 * @generated
	 */
	void setSpanCols(int value);

	/**
	 * Return the value of the '<em><b>Span Rows</b></em>' attribute. The default value is <code>"1"</code>.
	 * @return the value of the '<em>Span Rows</em>' attribute
	 * @see #setSpanRows(int)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_SpanRows()
	 * @model default="1"
	 * @generated
	 */
	int getSpanRows();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getSpanRows <em>Span Rows</em>}' attribute
	 * @param value the new value of the '<em>Span Rows</em>' attribute
	 * @see #getSpanRows()
	 * @generated
	 */
	void setSpanRows(int value);

	/**
	 * Return the value of the '<em><b>Vertical Span</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Vertical Span</em>' attribute
	 * @see #setVerticalSpan(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_VerticalSpan()
	 * @model default="true"
	 * @generated
	 */
	boolean isVerticalSpan();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#isVerticalSpan <em>Vertical Span</em>}' attribute
	 * @param value the new value of the '<em>Vertical Span</em>' attribute
	 * @see #isVerticalSpan()
	 * @generated
	 */
	void setVerticalSpan(boolean value);

	/**
	 * Return the value of the '<em><b>Horizontal Span</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Horizontal Span</em>' attribute
	 * @see #setHorizontalSpan(boolean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_HorizontalSpan()
	 * @model default="true"
	 * @generated
	 */
	boolean isHorizontalSpan();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#isHorizontalSpan <em>Horizontal Span</em>}'
	 * attribute
	 * @param value the new value of the '<em>Horizontal Span</em>' attribute
	 * @see #isHorizontalSpan()
	 * @generated
	 */
	void setHorizontalSpan(boolean value);

	/**
	 * Return the value of the '<em><b>Form Panel</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.FormPanel#getFormTable <em>Form Table</em>}'.
	 * @return the value of the '<em>Form Panel</em>' reference
	 * @see #setFormPanel(FormPanel)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_FormPanel()
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFormTable
	 * @model opposite="formTable"
	 * @generated
	 */
	FormPanel getFormPanel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getFormPanel <em>Form Panel</em>}' reference
	 * @param value the new value of the '<em>Form Panel</em>' reference
	 * @see #getFormPanel()
	 * @generated
	 */
	void setFormPanel(FormPanel value);

	/**
	 * Return the value of the '<em><b>Fields</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.TableColumnField}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.TableColumnField#getFormTable <em>Form Table</em>}'.
	 * @return the value of the '<em>Fields</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_Fields()
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getFormTable
	 * @model opposite="formTable" containment="true"
	 * @generated
	 */
	EList<TableColumnField> getFields();

	/**
	 * Return the value of the '<em><b>Association</b></em>' reference
	 * @return the value of the '<em>Association</em>' reference
	 * @see #setAssociation(AbstractDomainAssociation)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_Association()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.FormTable#getAssociation <em>Association</em>}' reference
	 * @param value the new value of the '<em>Association</em>' reference
	 * @see #getAssociation()
	 * @generated
	 */
	void setAssociation(AbstractDomainAssociation value);

	/**
	 * @return true if the table contains visible boolean fields
	 * @generated not
	 */
	boolean hasVisibleBooleanFields();

}
