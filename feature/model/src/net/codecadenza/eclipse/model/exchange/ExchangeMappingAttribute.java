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

import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;

/**
 * A representation of the model object '<em><b>Mapping Attribute</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject <em>Exchange Mapping
 * Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isInsertable <em>Insertable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdatable <em>Updatable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getSelectionListStatement <em>Selection List
 * Statement</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isDeleteAllItems <em>Delete All Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isAddNewItems <em>Add New Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isJoinAttribute <em>Join Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdateExistingItems <em>Update Existing
 * Items</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute()
 * @model
 * @generated
 */
public interface ExchangeMappingAttribute extends MappingAttribute {
	/**
	 * Return the value of the '<em><b>Exchange Mapping Object</b></em>' container reference. It is bidirectional and its opposite
	 * is ' {@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getAttributes <em>Attributes</em>}'.
	 * @return the value of the '<em>Exchange Mapping Object</em>' container reference
	 * @see #setExchangeMappingObject(ExchangeMappingObject)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_ExchangeMappingObject()
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getAttributes
	 * @model opposite="attributes" transient="false"
	 * @generated
	 */
	ExchangeMappingObject getExchangeMappingObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject
	 * <em>Exchange Mapping Object</em>}' container reference
	 * @param value the new value of the '<em>Exchange Mapping Object</em>' container reference
	 * @see #getExchangeMappingObject()
	 * @generated
	 */
	void setExchangeMappingObject(ExchangeMappingObject value);

	/**
	 * Return the value of the '<em><b>Insertable</b></em>' attribute
	 * @return the value of the '<em>Insertable</em>' attribute
	 * @see #setInsertable(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_Insertable()
	 * @model
	 * @generated
	 */
	boolean isInsertable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isInsertable
	 * <em>Insertable</em>}' attribute
	 * @param value the new value of the '<em>Insertable</em>' attribute
	 * @see #isInsertable()
	 * @generated
	 */
	void setInsertable(boolean value);

	/**
	 * Return the value of the '<em><b>Updatable</b></em>' attribute
	 * @return the value of the '<em>Updatable</em>' attribute
	 * @see #setUpdatable(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_Updatable()
	 * @model
	 * @generated
	 */
	boolean isUpdatable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdatable <em>Updatable</em>}'
	 * attribute
	 * @param value the new value of the '<em>Updatable</em>' attribute
	 * @see #isUpdatable()
	 * @generated
	 */
	void setUpdatable(boolean value);

	/**
	 * Return the value of the '<em><b>Selection List Statement</b></em>' attribute
	 * @return the value of the '<em>Selection List Statement</em>' attribute
	 * @see #setSelectionListStatement(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_SelectionListStatement()
	 * @model
	 * @generated
	 */
	String getSelectionListStatement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getSelectionListStatement
	 * <em>Selection List Statement</em>}' attribute
	 * @param value the new value of the '<em>Selection List Statement</em>' attribute
	 * @see #getSelectionListStatement()
	 * @generated
	 */
	void setSelectionListStatement(String value);

	/**
	 * Return the value of the '<em><b>Delete All Items</b></em>' attribute
	 * @return the value of the '<em>Delete All Items</em>' attribute
	 * @see #setDeleteAllItems(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_DeleteAllItems()
	 * @model
	 * @generated
	 */
	boolean isDeleteAllItems();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isDeleteAllItems <em>Delete All
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
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_AddNewItems()
	 * @model
	 * @generated
	 */
	boolean isAddNewItems();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isAddNewItems <em>Add New
	 * Items</em>}' attribute
	 * @param value the new value of the '<em>Add New Items</em>' attribute
	 * @see #isAddNewItems()
	 * @generated
	 */
	void setAddNewItems(boolean value);

	/**
	 * Return the value of the '<em><b>Join Attribute</b></em>' attribute
	 * @return the value of the '<em>Join Attribute</em>' attribute
	 * @see #setJoinAttribute(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_JoinAttribute()
	 * @model
	 * @generated
	 */
	boolean isJoinAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isJoinAttribute <em>Join
	 * Attribute</em>}' attribute
	 * @param value the new value of the '<em>Join Attribute</em>' attribute
	 * @see #isJoinAttribute()
	 * @generated
	 */
	void setJoinAttribute(boolean value);

	/**
	 * Return the value of the '<em><b>Update Existing Items</b></em>' attribute
	 * @return the value of the '<em>Update Existing Items</em>' attribute
	 * @see #setUpdateExistingItems(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_UpdateExistingItems()
	 * @model
	 * @generated
	 */
	boolean isUpdateExistingItems();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdateExistingItems <em>Update
	 * Existing Items</em>}' attribute
	 * @param value the new value of the '<em>Update Existing Items</em>' attribute
	 * @see #isUpdateExistingItems()
	 * @generated
	 */
	void setUpdateExistingItems(boolean value);

	/**
	 * @return the data exchange element this attribute is mapped to
	 * @param directMappingOnly
	 * @generated not
	 */
	DataExchangeElement getDataExchangeElement(boolean directMappingOnly);

	/**
	 * @return the data exchange attribute this attribute is mapped to
	 * @generated not
	 */
	DataExchangeAttribute getDataExchangeAttribute();

	/**
	 * @return the basic type this attribute is mapped to
	 * @generated not
	 */
	JavaType getJavaType();

}
