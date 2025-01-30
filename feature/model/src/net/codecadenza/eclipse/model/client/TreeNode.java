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
 * A representation of the model object '<em><b>Tree Node</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeNode#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.TreeNode#getDTOAttribute <em>DTO Attribute</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeNode()
 * @model
 * @generated
 */
public interface TreeNode extends EObject {
	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeNode_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeNode#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Return the value of the '<em><b>DTO Attribute</b></em>' reference
	 * @return the value of the '<em>DTO Attribute</em>' reference
	 * @see #setDTOAttribute(DTOBeanAttribute)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeNode_DTOAttribute()
	 * @model
	 * @generated
	 */
	DTOBeanAttribute getDTOAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.client.TreeNode#getDTOAttribute <em>DTO Attribute</em>}' reference
	 * @param value the new value of the '<em>DTO Attribute</em>' reference
	 * @see #getDTOAttribute()
	 * @generated
	 */
	void setDTOAttribute(DTOBeanAttribute value);

}
