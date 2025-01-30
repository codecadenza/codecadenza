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

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.TableColumnField;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>GUI Test Data</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestData#getFormField <em>Form Field</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestData#getTableColumnField <em>Table Column Field</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestData#getNewValue <em>New Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestData#getExpectedValue <em>Expected Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestData#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestData#getTestAction <em>Test Action</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData()
 * @model
 * @generated
 */
public interface GUITestData extends EObject {
	/**
	 * Return the value of the '<em><b>Form Field</b></em>' reference
	 * @return the value of the '<em>Form Field</em>' reference
	 * @see #setFormField(FormField)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_FormField()
	 * @model
	 * @generated
	 */
	FormField getFormField();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getFormField <em>Form Field</em>}' reference
	 * @param value the new value of the '<em>Form Field</em>' reference
	 * @see #getFormField()
	 * @generated
	 */
	void setFormField(FormField value);

	/**
	 * Return the value of the '<em><b>Table Column Field</b></em>' reference
	 * @return the value of the '<em>Table Column Field</em>' reference
	 * @see #setTableColumnField(TableColumnField)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_TableColumnField()
	 * @model
	 * @generated
	 */
	TableColumnField getTableColumnField();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getTableColumnField <em>Table Column
	 * Field</em>}' reference
	 * @param value the new value of the '<em>Table Column Field</em>' reference
	 * @see #getTableColumnField()
	 * @generated
	 */
	void setTableColumnField(TableColumnField value);

	/**
	 * Return the value of the '<em><b>New Value</b></em>' attribute
	 * @return the value of the '<em>New Value</em>' attribute
	 * @see #setNewValue(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_NewValue()
	 * @model
	 * @generated
	 */
	String getNewValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getNewValue <em>New Value</em>}' attribute
	 * @param value the new value of the '<em>New Value</em>' attribute
	 * @see #getNewValue()
	 * @generated
	 */
	void setNewValue(String value);

	/**
	 * Return the value of the '<em><b>Expected Value</b></em>' attribute
	 * @return the value of the '<em>Expected Value</em>' attribute
	 * @see #setExpectedValue(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_ExpectedValue()
	 * @model
	 * @generated
	 */
	String getExpectedValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getExpectedValue <em>Expected Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Expected Value</em>' attribute
	 * @see #getExpectedValue()
	 * @generated
	 */
	void setExpectedValue(String value);

	/**
	 * Return the value of the '<em><b>Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.testing.GUITestDataType}.
	 * @return the value of the '<em>Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.GUITestDataType
	 * @see #setType(GUITestDataType)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_Type()
	 * @model
	 * @generated
	 */
	GUITestDataType getType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getType <em>Type</em>}' attribute
	 * @param value the new value of the '<em>Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.GUITestDataType
	 * @see #getType()
	 * @generated
	 */
	void setType(GUITestDataType value);

	/**
	 * Return the value of the '<em><b>Test Action</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestData <em>Test Data</em>}'.
	 * @return the value of the '<em>Test Action</em>' container reference
	 * @see #setTestAction(GUITestAction)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_TestAction()
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestData
	 * @model opposite="testData" transient="false"
	 * @generated
	 */
	GUITestAction getTestAction();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getTestAction <em>Test Action</em>}' container
	 * reference
	 * @param value the new value of the '<em>Test Action</em>' container reference
	 * @see #getTestAction()
	 * @generated
	 */
	void setTestAction(GUITestAction value);

	/**
	 * Return the value of the '<em><b>Filter Value</b></em>' attribute
	 * @return the value of the '<em>Filter Value</em>' attribute
	 * @see #setFilterValue(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_FilterValue()
	 * @model
	 * @generated
	 */
	String getFilterValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestData#getFilterValue <em>Filter Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Filter Value</em>' attribute
	 * @see #getFilterValue()
	 * @generated
	 */
	void setFilterValue(String value);

}
