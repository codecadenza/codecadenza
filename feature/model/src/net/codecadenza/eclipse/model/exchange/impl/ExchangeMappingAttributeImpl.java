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
package net.codecadenza.eclipse.model.exchange.impl;

import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Mapping Attribute</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#getExchangeMappingObject <em>Exchange
 * Mapping Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#isInsertable <em>Insertable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#isUpdatable <em>Updatable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#getSelectionListStatement <em>Selection List
 * Statement</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#isDeleteAllItems <em>Delete All
 * Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#isAddNewItems <em>Add New Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#isJoinAttribute <em>Join
 * Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl#isUpdateExistingItems <em>Update Existing
 * Items</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class ExchangeMappingAttributeImpl extends MappingAttributeImpl implements ExchangeMappingAttribute {
	/**
	 * The default value of the '{@link #isInsertable() <em>Insertable</em>}' attribute
	 * @see #isInsertable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean INSERTABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isInsertable() <em>Insertable</em>}' attribute
	 * @see #isInsertable()
	 * @generated
	 * @ordered
	 */
	protected boolean insertable = INSERTABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isUpdatable() <em>Updatable</em>}' attribute
	 * @see #isUpdatable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean UPDATABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUpdatable() <em>Updatable</em>}' attribute
	 * @see #isUpdatable()
	 * @generated
	 * @ordered
	 */
	protected boolean updatable = UPDATABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #getSelectionListStatement() <em>Selection List Statement</em>}' attribute
	 * @see #getSelectionListStatement()
	 * @generated
	 * @ordered
	 */
	protected static final String SELECTION_LIST_STATEMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSelectionListStatement() <em>Selection List Statement</em>}' attribute
	 * @see #getSelectionListStatement()
	 * @generated
	 * @ordered
	 */
	protected String selectionListStatement = SELECTION_LIST_STATEMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #isDeleteAllItems() <em>Delete All Items</em>}' attribute
	 * @see #isDeleteAllItems()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DELETE_ALL_ITEMS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDeleteAllItems() <em>Delete All Items</em>}' attribute
	 * @see #isDeleteAllItems()
	 * @generated
	 * @ordered
	 */
	protected boolean deleteAllItems = DELETE_ALL_ITEMS_EDEFAULT;

	/**
	 * The default value of the '{@link #isAddNewItems() <em>Add New Items</em>}' attribute
	 * @see #isAddNewItems()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADD_NEW_ITEMS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAddNewItems() <em>Add New Items</em>}' attribute
	 * @see #isAddNewItems()
	 * @generated
	 * @ordered
	 */
	protected boolean addNewItems = ADD_NEW_ITEMS_EDEFAULT;

	/**
	 * The default value of the '{@link #isJoinAttribute() <em>Join Attribute</em>}' attribute
	 * @see #isJoinAttribute()
	 * @generated
	 * @ordered
	 */
	protected static final boolean JOIN_ATTRIBUTE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isJoinAttribute() <em>Join Attribute</em>}' attribute
	 * @see #isJoinAttribute()
	 * @generated
	 * @ordered
	 */
	protected boolean joinAttribute = JOIN_ATTRIBUTE_EDEFAULT;

	/**
	 * The default value of the '{@link #isUpdateExistingItems() <em>Update Existing Items</em>}' attribute
	 * @see #isUpdateExistingItems()
	 * @generated
	 * @ordered
	 */
	protected static final boolean UPDATE_EXISTING_ITEMS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUpdateExistingItems() <em>Update Existing Items</em>}' attribute
	 * @see #isUpdateExistingItems()
	 * @generated
	 * @ordered
	 */
	protected boolean updateExistingItems = UPDATE_EXISTING_ITEMS_EDEFAULT;

	/**
	 * @generated
	 */
	protected ExchangeMappingAttributeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.EXCHANGE_MAPPING_ATTRIBUTE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject()
	 * @generated
	 */
	@Override
	public ExchangeMappingObject getExchangeMappingObject() {
		if (eContainerFeatureID() != ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT)
			return null;

		return (ExchangeMappingObject) eInternalContainer();
	}

	/**
	 * @param newExchangeMappingObject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetExchangeMappingObject(ExchangeMappingObject newExchangeMappingObject, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newExchangeMappingObject,
				ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setExchangeMappingObject(net.codecadenza.eclipse.model.
	 * exchange.ExchangeMappingObject)
	 * @generated
	 */
	@Override
	public void setExchangeMappingObject(ExchangeMappingObject newExchangeMappingObject) {
		if (newExchangeMappingObject != eInternalContainer()
				|| (eContainerFeatureID() != ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT
						&& newExchangeMappingObject != null)) {
			if (EcoreUtil.isAncestor(this, newExchangeMappingObject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newExchangeMappingObject != null)
				msgs = ((InternalEObject) newExchangeMappingObject).eInverseAdd(this, ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES,
						ExchangeMappingObject.class, msgs);

			msgs = basicSetExchangeMappingObject(newExchangeMappingObject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT,
					newExchangeMappingObject, newExchangeMappingObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isInsertable()
	 * @generated
	 */
	@Override
	public boolean isInsertable() {
		return insertable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setInsertable(boolean)
	 * @generated
	 */
	@Override
	public void setInsertable(boolean newInsertable) {
		final boolean oldInsertable = insertable;
		insertable = newInsertable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE, oldInsertable,
					insertable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdatable()
	 * @generated
	 */
	@Override
	public boolean isUpdatable() {
		return updatable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setUpdatable(boolean)
	 * @generated
	 */
	@Override
	public void setUpdatable(boolean newUpdatable) {
		final boolean oldUpdatable = updatable;
		updatable = newUpdatable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE, oldUpdatable,
					updatable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getSelectionListStatement()
	 * @generated
	 */
	@Override
	public String getSelectionListStatement() {
		return selectionListStatement;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setSelectionListStatement(java.lang.String)
	 * @generated
	 */
	@Override
	public void setSelectionListStatement(String newSelectionListStatement) {
		final String oldSelectionListStatement = selectionListStatement;
		selectionListStatement = newSelectionListStatement;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT,
					oldSelectionListStatement, selectionListStatement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isDeleteAllItems()
	 * @generated
	 */
	@Override
	public boolean isDeleteAllItems() {
		return deleteAllItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setDeleteAllItems(boolean)
	 * @generated
	 */
	@Override
	public void setDeleteAllItems(boolean newDeleteAllItems) {
		final boolean oldDeleteAllItems = deleteAllItems;
		deleteAllItems = newDeleteAllItems;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS,
					oldDeleteAllItems, deleteAllItems));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isAddNewItems()
	 * @generated
	 */
	@Override
	public boolean isAddNewItems() {
		return addNewItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setAddNewItems(boolean)
	 * @generated
	 */
	@Override
	public void setAddNewItems(boolean newAddNewItems) {
		final boolean oldAddNewItems = addNewItems;
		addNewItems = newAddNewItems;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS,
					oldAddNewItems, addNewItems));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isJoinAttribute()
	 * @generated
	 */
	@Override
	public boolean isJoinAttribute() {
		return joinAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setJoinAttribute(boolean)
	 * @generated
	 */
	@Override
	public void setJoinAttribute(boolean newJoinAttribute) {
		final boolean oldJoinAttribute = joinAttribute;
		joinAttribute = newJoinAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE,
					oldJoinAttribute, joinAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdateExistingItems()
	 * @generated
	 */
	@Override
	public boolean isUpdateExistingItems() {
		return updateExistingItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#setUpdateExistingItems(boolean)
	 * @generated
	 */
	@Override
	public void setUpdateExistingItems(boolean newUpdateExistingItems) {
		final boolean oldUpdateExistingItems = updateExistingItems;
		updateExistingItems = newUpdateExistingItems;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS,
					oldUpdateExistingItems, updateExistingItems));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetExchangeMappingObject((ExchangeMappingObject) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				return basicSetExchangeMappingObject(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				return eInternalContainer().eInverseRemove(this, ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES,
						ExchangeMappingObject.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				return getExchangeMappingObject();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE:
				return isInsertable();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE:
				return isUpdatable();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT:
				return getSelectionListStatement();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS:
				return isDeleteAllItems();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS:
				return isAddNewItems();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE:
				return isJoinAttribute();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS:
				return isUpdateExistingItems();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				setExchangeMappingObject((ExchangeMappingObject) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE:
				setInsertable((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE:
				setUpdatable((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT:
				setSelectionListStatement((String) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS:
				setDeleteAllItems((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS:
				setAddNewItems((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE:
				setJoinAttribute((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS:
				setUpdateExistingItems((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				setExchangeMappingObject((ExchangeMappingObject) null);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE:
				setInsertable(INSERTABLE_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE:
				setUpdatable(UPDATABLE_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT:
				setSelectionListStatement(SELECTION_LIST_STATEMENT_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS:
				setDeleteAllItems(DELETE_ALL_ITEMS_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS:
				setAddNewItems(ADD_NEW_ITEMS_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE:
				setJoinAttribute(JOIN_ATTRIBUTE_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS:
				setUpdateExistingItems(UPDATE_EXISTING_ITEMS_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT:
				return getExchangeMappingObject() != null;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE:
				return insertable != INSERTABLE_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE:
				return updatable != UPDATABLE_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT:
				return selectionListStatement != null;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS:
				return deleteAllItems != DELETE_ALL_ITEMS_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS:
				return addNewItems != ADD_NEW_ITEMS_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE:
				return joinAttribute != JOIN_ATTRIBUTE_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS:
				return updateExistingItems != UPDATE_EXISTING_ITEMS_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (insertable: ");
		result.append(insertable);
		result.append(", updatable: ");
		result.append(updatable);
		result.append(", selectionListStatement: ");
		result.append(selectionListStatement);
		result.append(", deleteAllItems: ");
		result.append(deleteAllItems);
		result.append(", addNewItems: ");
		result.append(addNewItems);
		result.append(", joinAttribute: ");
		result.append(joinAttribute);
		result.append(", updateExistingItems: ");
		result.append(updateExistingItems);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getDataExchangeAttribute()
	 * @generated not
	 */
	@Override
	public DataExchangeAttribute getDataExchangeAttribute() {
		final DataExchangeElement element = getExchangeMappingObject().getDataExchangeElement();

		if (element == null)
			return null;

		final DataExchangeElement rootElement = element.getRootElement();

		return rootElement.getAllAttributes().stream()
				.filter(exchangeAttribute -> this.equals(exchangeAttribute.getMappingAttribute())).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getDataExchangeElement(boolean)
	 * @generated not
	 */
	@Override
	public DataExchangeElement getDataExchangeElement(boolean directMappingOnly) {
		final ExchangeMappingObject mappingObject = getExchangeMappingObject();
		final DataExchangeElement element = mappingObject.getDataExchangeElement();

		if (element == null)
			return null;

		final DataExchangeElement rootElement = element.getRootElement();

		for (final DataExchangeElement exchangeElement : rootElement.getAllElements())
			if (exchangeElement.getMappingAttribute() != null && exchangeElement.getMappingAttribute().equals(this))
				return exchangeElement;

		if (!directMappingOnly) {
			for (final DataExchangeElement exchangeElement : rootElement.getAllElements()) {
				if (!exchangeElement.isContainer() || exchangeElement.getMappingObject() == null)
					continue;

				if (exchangeElement.getMappingObject().equals(mappingObject))
					return exchangeElement;
			}
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getJavaType()
	 * @generated not
	 */
	@Override
	public JavaType getJavaType() {
		if (getDomainAttribute() != null)
			return getDomainAttribute().getJavaType();

		if (getMappingType() != null)
			return getMappingType();

		return getDataExchangeElement(true).getMappingObject();
	}

}
