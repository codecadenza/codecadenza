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
package net.codecadenza.eclipse.model.client.util;

import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.client.ClientPackage
 * @generated
 */
public class ClientSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ClientPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public ClientSwitch() {
		if (modelPackage == null)
			modelPackage = ClientPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#isSwitchFor(org.eclipse.emf.ecore.EPackage)
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#doSwitch(int, org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case ClientPackage.FORM: {
				final var form = (Form) theEObject;
				T result = caseForm(form);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.FORM_ACTION: {
				final var formAction = (FormAction) theEObject;
				T result = caseFormAction(formAction);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.FORM_FIELD: {
				final var formField = (FormField) theEObject;
				T result = caseFormField(formField);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.FORM_GROUP: {
				final var formGroup = (FormGroup) theEObject;
				T result = caseFormGroup(formGroup);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.FORM_PANEL: {
				final var formPanel = (FormPanel) theEObject;
				T result = caseFormPanel(formPanel);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.FORM_TABLE: {
				final var formTable = (FormTable) theEObject;
				T result = caseFormTable(formTable);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.TABLE_COLUMN_FIELD: {
				final var tableColumnField = (TableColumnField) theEObject;
				T result = caseTableColumnField(tableColumnField);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.TREE_VIEW: {
				final var treeView = (TreeView) theEObject;
				T result = caseTreeView(treeView);

				if (result == null)
					result = caseForm(treeView);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.TREE_VIEW_ITEM: {
				final var treeViewItem = (TreeViewItem) theEObject;
				T result = caseTreeViewItem(treeViewItem);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.TREE_SEARCH_ITEM: {
				final var treeSearchItem = (TreeSearchItem) theEObject;
				T result = caseTreeSearchItem(treeSearchItem);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ClientPackage.TREE_NODE: {
				final var treeNode = (TreeNode) theEObject;
				T result = caseTreeNode(treeNode);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Form</em>'. This implementation returns null; returning a
	 * non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Form</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseForm(Form object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Form Action</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Form Action</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFormAction(FormAction object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Form Field</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Form Field</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFormField(FormField object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Form Group</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Form Group</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFormGroup(FormGroup object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Form Panel</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Form Panel</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFormPanel(FormPanel object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Form Table</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Form Table</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFormTable(FormTable object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Table Column Field</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Table Column Field</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseTableColumnField(TableColumnField object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Tree View</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Tree View</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseTreeView(TreeView object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Tree View Item</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Tree View Item</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseTreeViewItem(TreeViewItem object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Tree Search Item</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Tree Search Item</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseTreeSearchItem(TreeSearchItem object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Tree Node</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Tree Node</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseTreeNode(TreeNode object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#defaultCase(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

}
