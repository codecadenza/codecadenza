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
package net.codecadenza.eclipse.model.exchange;

import net.codecadenza.eclipse.model.mapping.MappingObject;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Mapping Object</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isDeleteAllItems <em>Delete All Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isAddNewItems <em>Add New Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isUpdateExistingItems <em>Update Existing
 * Items</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject()
 * @model
 * @generated
 */
public interface ExchangeMappingObject extends MappingObject {
	/**
	 * Return the value of the '<em><b>Attributes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject <em>Exchange Mapping
	 * Object</em>}'.
	 * @return the value of the '<em>Attributes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_Attributes()
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject
	 * @model opposite="exchangeMappingObject" containment="true"
	 * @generated
	 */
	EList<ExchangeMappingAttribute> getAttributes();

	/**
	 * Return the value of the '<em><b>Delete All Items</b></em>' attribute
	 * @return the value of the '<em>Delete All Items</em>' attribute
	 * @see #setDeleteAllItems(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_DeleteAllItems()
	 * @model
	 * @generated
	 */
	boolean isDeleteAllItems();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isDeleteAllItems <em>Delete All
	 * Items</em>}' attribute
	 * @param value the new value of the '<em>Delete All Items</em>' attribute
	 * @see #isDeleteAllItems()
	 * @generated
	 */
	void setDeleteAllItems(boolean value);

	/**
	 * Return the value of the '<em><b>Add New Items</b></em>' attribute
	 * @return the value of the '<em>Add New Items</em>' attribute
	 * @see #setAddNewItems(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_AddNewItems()
	 * @model
	 * @generated
	 */
	boolean isAddNewItems();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isAddNewItems <em>Add New
	 * Items</em>}' attribute
	 * @param value the new value of the '<em>Add New Items</em>' attribute
	 * @see #isAddNewItems()
	 * @generated
	 */
	void setAddNewItems(boolean value);

	/**
	 * Return the value of the '<em><b>Update Existing Items</b></em>' attribute
	 * @return the value of the '<em>Update Existing Items</em>' attribute
	 * @see #setUpdateExistingItems(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_UpdateExistingItems()
	 * @model
	 * @generated
	 */
	boolean isUpdateExistingItems();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isUpdateExistingItems <em>Update
	 * Existing Items</em>}' attribute
	 * @param value the new value of the '<em>Update Existing Items</em>' attribute
	 * @see #isUpdateExistingItems()
	 * @generated
	 */
	void setUpdateExistingItems(boolean value);

	/**
	 * @return the primary key attribute of this exchange mapping object
	 * @generated not
	 */
	ExchangeMappingAttribute getPKAttribute();

	/**
	 * @return the attribute that represents the display attribute, or null if no display attribute exists!
	 * @generated not
	 */
	ExchangeMappingAttribute getDisplayAttribute();

	/**
	 * @return the data exchange element that is mapped to this mapping object
	 * @generated not
	 */
	DataExchangeElement getDataExchangeElement();

	/**
	 * @return the client ID attribute for this exchange mapping object, or null if an appropriate attribute has not been found!
	 * @generated not
	 */
	ExchangeMappingAttribute getClientAttribute();

}
