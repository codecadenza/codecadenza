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
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Tree View Item</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem <em>Parent Item</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getChildren <em>Children</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDataFetchMethod <em>Data Fetch Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDropMethod <em>Drop Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDisplayAttributes <em>Display Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getNodes <em>Nodes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getItemDTO <em>Item DTO</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeViewItem#getInvisibleAttributes <em>Invisible Attributes</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem()
 * @model
 * @generated
 */
public interface TreeViewItem extends EObject {
	/**
	 * Return the value of the '<em><b>Parent Item</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getChildren <em>Children</em>}'.
	 * @return the value of the '<em>Parent Item</em>' container reference
	 * @see #setParentItem(TreeViewItem)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_ParentItem()
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getChildren
	 * @model opposite="children" transient="false"
	 * @generated
	 */
	TreeViewItem getParentItem();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem <em>Parent Item</em>}' container
	 * reference
	 * @param value the new value of the '<em>Parent Item</em>' container reference
	 * @see #getParentItem()
	 * @generated
	 */
	void setParentItem(TreeViewItem value);

	/**
	 * Return the value of the '<em><b>Children</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.TreeViewItem}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem <em>Parent Item</em>}'.
	 * @return the value of the '<em>Children</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Children()
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem
	 * @model opposite="parentItem" containment="true"
	 * @generated
	 */
	EList<TreeViewItem> getChildren();

	/**
	 * Return the value of the '<em><b>Data Fetch Method</b></em>' reference
	 * @return the value of the '<em>Data Fetch Method</em>' reference
	 * @see #setDataFetchMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_DataFetchMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getDataFetchMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDataFetchMethod <em>Data Fetch Method</em>}'
	 * reference
	 * @param value the new value of the '<em>Data Fetch Method</em>' reference
	 * @see #getDataFetchMethod()
	 * @generated
	 */
	void setDataFetchMethod(BoundaryMethod value);

	/**
	 * Return the value of the '<em><b>Drop Method</b></em>' reference
	 * @return the value of the '<em>Drop Method</em>' reference
	 * @see #setDropMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_DropMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getDropMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDropMethod <em>Drop Method</em>}' reference
	 * @param value the new value of the '<em>Drop Method</em>' reference
	 * @see #getDropMethod()
	 * @generated
	 */
	void setDropMethod(BoundaryMethod value);

	/**
	 * Return the value of the '<em><b>Display Attributes</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute}.
	 * @return the value of the '<em>Display Attributes</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_DisplayAttributes()
	 * @model
	 * @generated
	 */
	EList<DTOBeanAttribute> getDisplayAttributes();

	/**
	 * Return the value of the '<em><b>Association</b></em>' reference
	 * @return the value of the '<em>Association</em>' reference
	 * @see #setAssociation(AbstractDomainAssociation)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Association()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getAssociation <em>Association</em>}' reference
	 * @param value the new value of the '<em>Association</em>' reference
	 * @see #getAssociation()
	 * @generated
	 */
	void setAssociation(AbstractDomainAssociation value);

	/**
	 * Return the value of the '<em><b>Nodes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.client.TreeNode}.
	 * @return the value of the '<em>Nodes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Nodes()
	 * @model containment="true"
	 * @generated
	 */
	EList<TreeNode> getNodes();

	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Return the value of the '<em><b>Item DTO</b></em>' reference
	 * @return the value of the '<em>Item DTO</em>' reference
	 * @see #setItemDTO(DTOBean)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_ItemDTO()
	 * @model
	 * @generated
	 */
	DTOBean getItemDTO();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getItemDTO <em>Item DTO</em>}' reference
	 * @param value the new value of the '<em>Item DTO</em>' reference
	 * @see #getItemDTO()
	 * @generated
	 */
	void setItemDTO(DTOBean value);

	/**
	 * Return the value of the '<em><b>Invisible Attributes</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute}.
	 * @return the value of the '<em>Invisible Attributes</em>' reference list
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_InvisibleAttributes()
	 * @model
	 * @generated
	 */
	EList<DTOBeanAttribute> getInvisibleAttributes();

	/**
	 * @return true if this item is the root item of the tree view
	 * @generated not
	 */
	boolean isRootItem();

}
