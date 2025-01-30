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

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>GUI Test Action</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getComment <em>Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getForm <em>Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTargetForm <em>Target Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getFormAction <em>Form Action</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getFormPanel <em>Form Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestData <em>Test Data</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult <em>Action Result</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase <em>Test Case</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getDelayBefore <em>Delay Before</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.GUITestAction#getDelayAfter <em>Delay After</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction()
 * @model
 * @generated
 */
public interface GUITestAction extends EObject {
	/**
	 * Return the value of the '<em><b>Comment</b></em>' attribute
	 * @return the value of the '<em>Comment</em>' attribute
	 * @see #setComment(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_Comment()
	 * @model
	 * @generated
	 */
	String getComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getComment <em>Comment</em>}' attribute
	 * @param value the new value of the '<em>Comment</em>' attribute
	 * @see #getComment()
	 * @generated
	 */
	void setComment(String value);

	/**
	 * Return the value of the '<em><b>Form</b></em>' reference
	 * @return the value of the '<em>Form</em>' reference
	 * @see #setForm(Form)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_Form()
	 * @model
	 * @generated
	 */
	Form getForm();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getForm <em>Form</em>}' reference
	 * @param value the new value of the '<em>Form</em>' reference
	 * @see #getForm()
	 * @generated
	 */
	void setForm(Form value);

	/**
	 * Return the value of the '<em><b>Target Form</b></em>' reference
	 * @return the value of the '<em>Target Form</em>' reference
	 * @see #setTargetForm(Form)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_TargetForm()
	 * @model
	 * @generated
	 */
	Form getTargetForm();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTargetForm <em>Target Form</em>}'
	 * reference
	 * @param value the new value of the '<em>Target Form</em>' reference
	 * @see #getTargetForm()
	 * @generated
	 */
	void setTargetForm(Form value);

	/**
	 * Return the value of the '<em><b>Form Action</b></em>' reference
	 * @return the value of the '<em>Form Action</em>' reference
	 * @see #setFormAction(FormAction)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_FormAction()
	 * @model
	 * @generated
	 */
	FormAction getFormAction();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getFormAction <em>Form Action</em>}'
	 * reference
	 * @param value the new value of the '<em>Form Action</em>' reference
	 * @see #getFormAction()
	 * @generated
	 */
	void setFormAction(FormAction value);

	/**
	 * Return the value of the '<em><b>Form Panel</b></em>' reference
	 * @return the value of the '<em>Form Panel</em>' reference
	 * @see #setFormPanel(FormPanel)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_FormPanel()
	 * @model
	 * @generated
	 */
	FormPanel getFormPanel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getFormPanel <em>Form Panel</em>}' reference
	 * @param value the new value of the '<em>Form Panel</em>' reference
	 * @see #getFormPanel()
	 * @generated
	 */
	void setFormPanel(FormPanel value);

	/**
	 * Return the value of the '<em><b>Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.testing.GUITestActionType}.
	 * @return the value of the '<em>Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionType
	 * @see #setType(GUITestActionType)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_Type()
	 * @model
	 * @generated
	 */
	GUITestActionType getType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getType <em>Type</em>}' attribute
	 * @param value the new value of the '<em>Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionType
	 * @see #getType()
	 * @generated
	 */
	void setType(GUITestActionType value);

	/**
	 * Return the value of the '<em><b>Test Data</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.GUITestData}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestData#getTestAction <em>Test Action</em>}'.
	 * @return the value of the '<em>Test Data</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_TestData()
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getTestAction
	 * @model opposite="testAction" containment="true"
	 * @generated
	 */
	EList<GUITestData> getTestData();

	/**
	 * Return the value of the '<em><b>Action Result</b></em>' containment reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction <em>Test Action</em>}'.
	 * @return the value of the '<em>Action Result</em>' containment reference
	 * @see #setActionResult(GUITestActionResult)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_ActionResult()
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction
	 * @model opposite="testAction" containment="true"
	 * @generated
	 */
	GUITestActionResult getActionResult();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult <em>Action Result</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Action Result</em>' containment reference
	 * @see #getActionResult()
	 * @generated
	 */
	void setActionResult(GUITestActionResult value);

	/**
	 * Return the value of the '<em><b>Test Case</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.GUITestCase#getTestActions <em>Test Actions</em>}'.
	 * @return the value of the '<em>Test Case</em>' container reference
	 * @see #setTestCase(GUITestCase)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_TestCase()
	 * @see net.codecadenza.eclipse.model.testing.GUITestCase#getTestActions
	 * @model opposite="testActions" transient="false"
	 * @generated
	 */
	GUITestCase getTestCase();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase <em>Test Case</em>}' container
	 * reference
	 * @param value the new value of the '<em>Test Case</em>' container reference
	 * @see #getTestCase()
	 * @generated
	 */
	void setTestCase(GUITestCase value);

	/**
	 * Return the value of the '<em><b>Delay Before</b></em>' attribute
	 * @return the value of the '<em>Delay Before</em>' attribute
	 * @see #setDelayBefore(Integer)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_DelayBefore()
	 * @model
	 * @generated
	 */
	Integer getDelayBefore();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getDelayBefore <em>Delay Before</em>}'
	 * attribute
	 * @param value the new value of the '<em>Delay Before</em>' attribute
	 * @see #getDelayBefore()
	 * @generated
	 */
	void setDelayBefore(Integer value);

	/**
	 * Return the value of the '<em><b>Delay After</b></em>' attribute
	 * @return the value of the '<em>Delay After</em>' attribute
	 * @see #setDelayAfter(Integer)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_DelayAfter()
	 * @model
	 * @generated
	 */
	Integer getDelayAfter();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.GUITestAction#getDelayAfter <em>Delay After</em>}'
	 * attribute
	 * @param value the new value of the '<em>Delay After</em>' attribute
	 * @see #getDelayAfter()
	 * @generated
	 */
	void setDelayAfter(Integer value);

	/**
	 * Determine if the test action basically needs test data
	 * @return true if test data must be added to the respective resource file
	 * @generated not
	 */
	boolean needsTestData();

	/**
	 * Search for a test data object with the given type
	 * @param type
	 * @return the first test data object from the respective list or null if no test data object of this type has been found
	 * @generated not
	 */
	GUITestData getTestDataByType(GUITestDataType type);

}
