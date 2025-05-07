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
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.client.ClientPackage
 * @generated
 */
public class ClientAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ClientPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public ClientAdapterFactory() {
		if (modelPackage == null)
			modelPackage = ClientPackage.eINSTANCE;
	}

	/**
	 * Return whether this factory is applicable for the type of the object. This implementation returns true if the object is
	 * either the model's package or is an instance object of the model.
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage)
			return true;

		if (object instanceof final EObject eObject)
			return eObject.eClass().getEPackage() == modelPackage;

		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * @generated
	 */
	protected ClientSwitch<Adapter> modelSwitch = new ClientSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseForm(net.codecadenza.eclipse.model.client.Form)
		 */
		@Override
		public Adapter caseForm(Form object) {
			return createFormAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseFormAction(net.codecadenza.eclipse.model.client.FormAction)
		 */
		@Override
		public Adapter caseFormAction(FormAction object) {
			return createFormActionAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseFormField(net.codecadenza.eclipse.model.client.FormField)
		 */
		@Override
		public Adapter caseFormField(FormField object) {
			return createFormFieldAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseFormGroup(net.codecadenza.eclipse.model.client.FormGroup)
		 */
		@Override
		public Adapter caseFormGroup(FormGroup object) {
			return createFormGroupAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseFormPanel(net.codecadenza.eclipse.model.client.FormPanel)
		 */
		@Override
		public Adapter caseFormPanel(FormPanel object) {
			return createFormPanelAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseFormTable(net.codecadenza.eclipse.model.client.FormTable)
		 */
		@Override
		public Adapter caseFormTable(FormTable object) {
			return createFormTableAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseTableColumnField(net.codecadenza.eclipse.model.client.
		 * TableColumnField)
		 */
		@Override
		public Adapter caseTableColumnField(TableColumnField object) {
			return createTableColumnFieldAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseTreeView(net.codecadenza.eclipse.model.client.TreeView)
		 */
		@Override
		public Adapter caseTreeView(TreeView object) {
			return createTreeViewAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseTreeViewItem(net.codecadenza.eclipse.model.client.
		 * TreeViewItem)
		 */
		@Override
		public Adapter caseTreeViewItem(TreeViewItem object) {
			return createTreeViewItemAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseTreeSearchItem(net.codecadenza.eclipse.model.client.
		 * TreeSearchItem)
		 */
		@Override
		public Adapter caseTreeSearchItem(TreeSearchItem object) {
			return createTreeSearchItemAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#caseTreeNode(net.codecadenza.eclipse.model.client.TreeNode)
		 */
		@Override
		public Adapter caseTreeNode(TreeNode object) {
			return createTreeNodeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.client.util.ClientSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public Adapter defaultCase(EObject object) {
			return createEObjectAdapter();
		}
	};

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#createAdapter(org.eclipse.emf.common.notify.Notifier)
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.Form <em>Form</em>}'. This default
	 * implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will catch all
	 * the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.Form
	 * @generated
	 */
	public Adapter createFormAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.FormAction <em>Form Action</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.FormAction
	 * @generated
	 */
	public Adapter createFormActionAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.FormField <em>Form Field</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.FormField
	 * @generated
	 */
	public Adapter createFormFieldAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.FormGroup <em>Form Group</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.FormGroup
	 * @generated
	 */
	public Adapter createFormGroupAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.FormPanel <em>Form Panel</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.FormPanel
	 * @generated
	 */
	public Adapter createFormPanelAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.FormTable <em>Form Table</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.FormTable
	 * @generated
	 */
	public Adapter createFormTableAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.TableColumnField <em>Table Column
	 * Field</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.TableColumnField
	 * @generated
	 */
	public Adapter createTableColumnFieldAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.TreeView <em>Tree View</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.TreeView
	 * @generated
	 */
	public Adapter createTreeViewAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.TreeViewItem <em>Tree View
	 * Item</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem
	 * @generated
	 */
	public Adapter createTreeViewItemAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.TreeSearchItem <em>Tree Search
	 * Item</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.TreeSearchItem
	 * @generated
	 */
	public Adapter createTreeSearchItemAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.client.TreeNode <em>Tree Node</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.client.TreeNode
	 * @generated
	 */
	public Adapter createTreeNodeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for the default case. This default implementation returns null.
	 * @return the new adapter
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

}
