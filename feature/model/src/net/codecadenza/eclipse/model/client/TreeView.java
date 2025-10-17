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
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Tree View</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeView#getRootTreeItem <em>Root Tree Item</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeView#getQuickSearchItems <em>Quick Search Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeView#getAdvancedSearchItems <em>Advanced Search Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeView#getCountMethod <em>Count Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeView#getRecursiveMethod <em>Recursive Method</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView()
 * @model
 * @generated
 */
public interface TreeView extends Form {
	/**
	 * Return the value of the '<em><b>Root Tree Item</b></em>' containment reference
	 * @return the value of the '<em>Root Tree Item</em>' containment reference
	 * @see #setRootTreeItem(TreeViewItem)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_RootTreeItem()
	 * @model containment="true"
	 * @generated
	 */
	TreeViewItem getRootTreeItem();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeView#getRootTreeItem <em>Root Tree Item</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Root Tree Item</em>' containment reference
	 * @see #getRootTreeItem()
	 * @generated
	 */
	void setRootTreeItem(TreeViewItem value);

	/**
	 * Return the value of the '<em><b>Quick Search Items</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.TreeSearchItem}.
	 * @return the value of the '<em>Quick Search Items</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_QuickSearchItems()
	 * @model containment="true"
	 * @generated
	 */
	EList<TreeSearchItem> getQuickSearchItems();

	/**
	 * Return the value of the '<em><b>Advanced Search Items</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.TreeSearchItem}.
	 * @return the value of the '<em>Advanced Search Items</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_AdvancedSearchItems()
	 * @model containment="true"
	 * @generated
	 */
	EList<TreeSearchItem> getAdvancedSearchItems();

	/**
	 * Return the value of the '<em><b>Count Method</b></em>' reference
	 * @return the value of the '<em>Count Method</em>' reference
	 * @see #setCountMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_CountMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getCountMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeView#getCountMethod <em>Count Method</em>}' reference
	 * @param value the new value of the '<em>Count Method</em>' reference
	 * @see #getCountMethod()
	 * @generated
	 */
	void setCountMethod(BoundaryMethod value);

	/**
	 * Return the value of the '<em><b>Recursive Method</b></em>' reference
	 * @return the value of the '<em>Recursive Method</em>' reference
	 * @see #setRecursiveMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_RecursiveMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getRecursiveMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeView#getRecursiveMethod <em>Recursive Method</em>}'
	 * reference
	 * @param value the new value of the '<em>Recursive Method</em>' reference
	 * @see #getRecursiveMethod()
	 * @generated
	 */
	void setRecursiveMethod(BoundaryMethod value);

	/**
	 * @return a list containing all sub-items of a tree view
	 * @generated not
	 */
	EList<TreeViewItem> getAllSubTreeItems();

	/**
	 * @return true if a search object is necessary
	 * @generated not
	 */
	boolean needsSearchObject();

}
