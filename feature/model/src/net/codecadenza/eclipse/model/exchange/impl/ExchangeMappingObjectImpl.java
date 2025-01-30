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

import java.util.Collection;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Mapping Object</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl#isDeleteAllItems <em>Delete All
 * Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl#isAddNewItems <em>Add New Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl#isUpdateExistingItems <em>Update Existing
 * Items</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class ExchangeMappingObjectImpl extends MappingObjectImpl implements ExchangeMappingObject {
	/**
	 * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference list
	 * @see #getAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<ExchangeMappingAttribute> attributes;

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
	protected ExchangeMappingObjectImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.EXCHANGE_MAPPING_OBJECT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getAttributes()
	 * @generated
	 */
	@Override
	public EList<ExchangeMappingAttribute> getAttributes() {
		if (attributes == null)
			attributes = new EObjectContainmentWithInverseEList<>(ExchangeMappingAttribute.class, this,
					ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES,
					ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT);

		return attributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isDeleteAllItems()
	 * @generated
	 */
	@Override
	public boolean isDeleteAllItems() {
		return deleteAllItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#setDeleteAllItems(boolean)
	 * @generated
	 */
	@Override
	public void setDeleteAllItems(boolean newDeleteAllItems) {
		final boolean oldDeleteAllItems = deleteAllItems;
		deleteAllItems = newDeleteAllItems;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS,
					oldDeleteAllItems, deleteAllItems));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isAddNewItems()
	 * @generated
	 */
	@Override
	public boolean isAddNewItems() {
		return addNewItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#setAddNewItems(boolean)
	 * @generated
	 */
	@Override
	public void setAddNewItems(boolean newAddNewItems) {
		final boolean oldAddNewItems = addNewItems;
		addNewItems = newAddNewItems;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS,
					oldAddNewItems, addNewItems));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isUpdateExistingItems()
	 * @generated
	 */
	@Override
	public boolean isUpdateExistingItems() {
		return updateExistingItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#setUpdateExistingItems(boolean)
	 * @generated
	 */
	@Override
	public void setUpdateExistingItems(boolean newUpdateExistingItems) {
		final boolean oldUpdateExistingItems = updateExistingItems;
		updateExistingItems = newUpdateExistingItems;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS,
					oldUpdateExistingItems, updateExistingItems));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getAttributes()).basicAdd(otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES:
				return ((InternalEList<?>) getAttributes()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES:
				return getAttributes();
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS:
				return isDeleteAllItems();
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS:
				return isAddNewItems();
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS:
				return isUpdateExistingItems();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES:
				getAttributes().clear();
				getAttributes().addAll((Collection<? extends ExchangeMappingAttribute>) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS:
				setDeleteAllItems((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS:
				setAddNewItems((Boolean) newValue);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS:
				setUpdateExistingItems((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES:
				getAttributes().clear();
				return;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS:
				setDeleteAllItems(DELETE_ALL_ITEMS_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS:
				setAddNewItems(ADD_NEW_ITEMS_EDEFAULT);
				return;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS:
				setUpdateExistingItems(UPDATE_EXISTING_ITEMS_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ATTRIBUTES:
				return attributes != null && !attributes.isEmpty();
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS:
				return deleteAllItems != DELETE_ALL_ITEMS_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS:
				return addNewItems != ADD_NEW_ITEMS_EDEFAULT;
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS:
				return updateExistingItems != UPDATE_EXISTING_ITEMS_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (deleteAllItems: ");
		result.append(deleteAllItems);
		result.append(", addNewItems: ");
		result.append(addNewItems);
		result.append(", updateExistingItems: ");
		result.append(updateExistingItems);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getPKAttribute()
	 * @generated not
	 */
	@Override
	public ExchangeMappingAttribute getPKAttribute() {
		for (final ExchangeMappingAttribute attr : getAttributes())
			if (attr.getDomainAttribute() != null
					&& (attr.getAssociation() == null || attr.getAssociation().getTarget().equals(getDomainObject()))
					&& attr.getDomainAttribute().equals(getDomainObject().getPKAttribute()))
				return attr;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getDisplayAttribute()
	 * @generated not
	 */
	@Override
	public ExchangeMappingAttribute getDisplayAttribute() {
		final DomainAttribute displayAttribute = this.getDomainObject().getDisplayAttribute();

		if (displayAttribute == null)
			return null;

		for (final ExchangeMappingAttribute attr : getAttributes())
			if (attr.getDomainAttribute() != null
					&& (attr.getAssociation() == null || attr.getAssociation().getTarget().equals(getDomainObject()))
					&& attr.getDomainAttribute().equals(displayAttribute))
				return attr;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.getDataExchangeElement#getExchangeElement()
	 * @generated not
	 */
	@Override
	public DataExchangeElement getDataExchangeElement() {
		final Project project = getNamespace().getProject();

		for (final Namespace ns : project.getExchangeNamespace().getChildNamespaces())
			for (final JavaType t : ns.getJavaTypes()) {
				if (!(t instanceof final DataExchangeServiceBean bean))
					continue;

				for (final DataExchangeMethod m : bean.getDataExchangeMethods())
					for (final DataExchangeElement element : m.getRootElement(true).getAllElements())
						if (element.getMappingObject() != null && element.getMappingObject().equals(this))
							return element;
			}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getClientAttribute()
	 * @generated not
	 */
	@Override
	public ExchangeMappingAttribute getClientAttribute() {
		final DomainObject clientDomainObject = getDomainObject().getNamespace().getProject()
				.getDomainObjectByTag(DomainTagEnumeration.CLIENT);

		if (clientDomainObject == null || !getDomainObject().isMandated())
			return null;

		for (final ExchangeMappingAttribute mappingAttr : getAttributes()) {
			if (mappingAttr.getAssociation() == null || mappingAttr.getDomainAttribute() == null)
				continue;

			if (mappingAttr.getAssociation().getTarget().equals(clientDomainObject) && mappingAttr.getDomainAttribute().isPk())
				return mappingAttr;
		}

		return null;
	}

}
