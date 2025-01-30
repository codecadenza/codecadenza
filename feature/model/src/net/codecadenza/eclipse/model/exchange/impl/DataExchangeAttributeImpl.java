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
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Data Exchange Attribute</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getValueListEntries <em>Value List
 * Entries</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#isVisible <em>Visible</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#isOptional <em>Optional</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getAttributeOrder <em>Attribute
 * Order</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#isReadonly <em>Readonly</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getElement <em>Element</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getMappingAttribute <em>Mapping
 * Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getDataType <em>Data Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#getFormat <em>Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#isDisableExternalMapping <em>Disable External
 * Mapping</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl#isUsedForCustomQuery <em>Used For Custom
 * Query</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DataExchangeAttributeImpl extends EObjectImpl implements DataExchangeAttribute {
	/**
	 * The cached value of the '{@link #getValueListEntries() <em>Value List Entries</em>}' containment reference list
	 * @see #getValueListEntries()
	 * @generated
	 * @ordered
	 */
	protected EList<ValueListEntry> valueListEntries;

	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #isVisible() <em>Visible</em>}' attribute
	 * @see #isVisible()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VISIBLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isVisible() <em>Visible</em>}' attribute
	 * @see #isVisible()
	 * @generated
	 * @ordered
	 */
	protected boolean visible = VISIBLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isOptional() <em>Optional</em>}' attribute
	 * @see #isOptional()
	 * @generated
	 * @ordered
	 */
	protected static final boolean OPTIONAL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isOptional() <em>Optional</em>}' attribute
	 * @see #isOptional()
	 * @generated
	 * @ordered
	 */
	protected boolean optional = OPTIONAL_EDEFAULT;

	/**
	 * The default value of the '{@link #getAttributeOrder() <em>Attribute Order</em>}' attribute
	 * @see #getAttributeOrder()
	 * @generated
	 * @ordered
	 */
	protected static final Integer ATTRIBUTE_ORDER_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAttributeOrder() <em>Attribute Order</em>}' attribute
	 * @see #getAttributeOrder()
	 * @generated
	 * @ordered
	 */
	protected Integer attributeOrder = ATTRIBUTE_ORDER_EDEFAULT;

	/**
	 * The default value of the '{@link #isReadonly() <em>Readonly</em>}' attribute
	 * @see #isReadonly()
	 * @generated
	 * @ordered
	 */
	protected static final boolean READONLY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isReadonly() <em>Readonly</em>}' attribute
	 * @see #isReadonly()
	 * @generated
	 * @ordered
	 */
	protected boolean readonly = READONLY_EDEFAULT;

	/**
	 * The cached value of the '{@link #getMappingAttribute() <em>Mapping Attribute</em>}' reference
	 * @see #getMappingAttribute()
	 * @generated
	 * @ordered
	 */
	protected ExchangeMappingAttribute mappingAttribute;

	/**
	 * The cached value of the '{@link #getDataType() <em>Data Type</em>}' reference
	 * @see #getDataType()
	 * @generated
	 * @ordered
	 */
	protected JavaType dataType;

	/**
	 * The default value of the '{@link #getFormat() <em>Format</em>}' attribute
	 * @see #getFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFormat() <em>Format</em>}' attribute
	 * @see #getFormat()
	 * @generated
	 * @ordered
	 */
	protected String format = FORMAT_EDEFAULT;

	/**
	 * The default value of the '{@link #isDisableExternalMapping() <em>Disable External Mapping</em>}' attribute
	 * @see #isDisableExternalMapping()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DISABLE_EXTERNAL_MAPPING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDisableExternalMapping() <em>Disable External Mapping</em>}' attribute
	 * @see #isDisableExternalMapping()
	 * @generated
	 * @ordered
	 */
	protected boolean disableExternalMapping = DISABLE_EXTERNAL_MAPPING_EDEFAULT;

	/**
	 * The default value of the '{@link #isUsedForCustomQuery() <em>Used For Custom Query</em>}' attribute
	 * @see #isUsedForCustomQuery()
	 * @generated
	 * @ordered
	 */
	protected static final boolean USED_FOR_CUSTOM_QUERY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUsedForCustomQuery() <em>Used For Custom Query</em>}' attribute
	 * @see #isUsedForCustomQuery()
	 * @generated
	 * @ordered
	 */
	protected boolean usedForCustomQuery = USED_FOR_CUSTOM_QUERY_EDEFAULT;

	/**
	 * @generated
	 */
	protected DataExchangeAttributeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.DATA_EXCHANGE_ATTRIBUTE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getValueListEntries()
	 * @generated
	 */
	@Override
	public EList<ValueListEntry> getValueListEntries() {
		if (valueListEntries == null)
			valueListEntries = new EObjectContainmentEList<>(ValueListEntry.class, this,
					ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES);

		return valueListEntries;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isVisible()
	 * @generated
	 */
	@Override
	public boolean isVisible() {
		return visible;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setVisible(boolean)
	 * @generated
	 */
	@Override
	public void setVisible(boolean newVisible) {
		final boolean oldVisible = visible;
		visible = newVisible;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VISIBLE, oldVisible, visible));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isOptional()
	 * @generated
	 */
	@Override
	public boolean isOptional() {
		return optional;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setOptional(boolean)
	 */
	@Override
	public void setOptional(boolean newOptional) {
		final boolean oldOptional = optional;
		optional = newOptional;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__OPTIONAL, oldOptional,
					optional));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getAttributeOrder()
	 * @generated
	 */
	@Override
	public Integer getAttributeOrder() {
		return attributeOrder;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setAttributeOrder(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setAttributeOrder(Integer newAttributeOrder) {
		final Integer oldAttributeOrder = attributeOrder;
		attributeOrder = newAttributeOrder;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER,
					oldAttributeOrder, attributeOrder));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isReadonly()
	 */
	@Override
	public boolean isReadonly() {
		return readonly;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setReadonly(boolean)
	 * @generated
	 */
	@Override
	public void setReadonly(boolean newReadonly) {
		final boolean oldReadonly = readonly;
		readonly = newReadonly;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__READONLY, oldReadonly,
					readonly));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement()
	 * @generated
	 */
	@Override
	public DataExchangeElement getElement() {
		if (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT)
			return null;

		return (DataExchangeElement) eInternalContainer();
	}

	/**
	 * @param newElement
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetElement(DataExchangeElement newElement, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newElement, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setElement(net.codecadenza.eclipse.model.exchange.
	 * DataExchangeElement)
	 * @generated
	 */
	@Override
	public void setElement(DataExchangeElement newElement) {
		if (newElement != eInternalContainer()
				|| (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT && newElement != null)) {
			if (EcoreUtil.isAncestor(this, newElement))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newElement != null)
				msgs = ((InternalEObject) newElement).eInverseAdd(this, ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES,
						DataExchangeElement.class, msgs);

			msgs = basicSetElement(newElement, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT, newElement,
					newElement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getMappingAttribute()
	 * @generated
	 */
	@Override
	public ExchangeMappingAttribute getMappingAttribute() {
		if (mappingAttribute != null && mappingAttribute.eIsProxy()) {
			final var oldMappingAttribute = (InternalEObject) mappingAttribute;
			mappingAttribute = (ExchangeMappingAttribute) eResolveProxy(oldMappingAttribute);

			if (mappingAttribute != oldMappingAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE,
						oldMappingAttribute, mappingAttribute));
		}

		return mappingAttribute;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public ExchangeMappingAttribute basicGetMappingAttribute() {
		return mappingAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#
	 * setMappingAttribute(net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute)
	 * @generated
	 */
	@Override
	public void setMappingAttribute(ExchangeMappingAttribute newMappingAttribute) {
		final ExchangeMappingAttribute oldMappingAttribute = mappingAttribute;
		mappingAttribute = newMappingAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE,
					oldMappingAttribute, mappingAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getDataType()
	 * @generated
	 */
	@Override
	public JavaType getDataType() {
		if (dataType != null && dataType.eIsProxy()) {
			final var oldDataType = (InternalEObject) dataType;
			dataType = (JavaType) eResolveProxy(oldDataType);

			if (dataType != oldDataType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE, oldDataType,
						dataType));
		}

		return dataType;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaType basicGetDataType() {
		return dataType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setDataType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setDataType(JavaType newDataType) {
		final JavaType oldDataType = dataType;
		dataType = newDataType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE, oldDataType,
					dataType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getFormat()
	 * @generated
	 */
	@Override
	public String getFormat() {
		return format;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setFormat(String newFormat) {
		final String oldFormat = format;
		format = newFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__FORMAT, oldFormat, format));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isDisableExternalMapping()
	 * @generated
	 */
	@Override
	public boolean isDisableExternalMapping() {
		return disableExternalMapping;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setDisableExternalMapping(boolean)
	 * @generated
	 */
	@Override
	public void setDisableExternalMapping(boolean newDisableExternalMapping) {
		final boolean oldDisableExternalMapping = disableExternalMapping;
		disableExternalMapping = newDisableExternalMapping;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING,
					oldDisableExternalMapping, disableExternalMapping));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isUsedForCustomQuery()
	 */
	@Override
	public boolean isUsedForCustomQuery() {
		return usedForCustomQuery;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#setUsedForCustomQuery(boolean)
	 * @generated
	 */
	@Override
	public void setUsedForCustomQuery(boolean newUsedForCustomQuery) {
		final boolean oldUsedForCustomQuery = usedForCustomQuery;
		usedForCustomQuery = newUsedForCustomQuery;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY,
					oldUsedForCustomQuery, usedForCustomQuery));
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
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetElement((DataExchangeElement) otherEnd, msgs);
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
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES:
				return ((InternalEList<?>) getValueListEntries()).basicRemove(otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				return basicSetElement(null, msgs);
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
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				return eInternalContainer().eInverseRemove(this, ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES,
						DataExchangeElement.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES:
				return getValueListEntries();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__NAME:
				return getName();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VISIBLE:
				return isVisible();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__OPTIONAL:
				return isOptional();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER:
				return getAttributeOrder();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__READONLY:
				return isReadonly();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				return getElement();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE:
				if (resolve)
					return getMappingAttribute();

				return basicGetMappingAttribute();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE:
				if (resolve)
					return getDataType();

				return basicGetDataType();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__FORMAT:
				return getFormat();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING:
				return isDisableExternalMapping();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY:
				return isUsedForCustomQuery();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES:
				getValueListEntries().clear();
				getValueListEntries().addAll((Collection<? extends ValueListEntry>) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__NAME:
				setName((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VISIBLE:
				setVisible((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__OPTIONAL:
				setOptional((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER:
				setAttributeOrder((Integer) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__READONLY:
				setReadonly((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				setElement((DataExchangeElement) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE:
				setMappingAttribute((ExchangeMappingAttribute) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE:
				setDataType((JavaType) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__FORMAT:
				setFormat((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING:
				setDisableExternalMapping((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY:
				setUsedForCustomQuery((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES:
				getValueListEntries().clear();
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VISIBLE:
				setVisible(VISIBLE_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__OPTIONAL:
				setOptional(OPTIONAL_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER:
				setAttributeOrder(ATTRIBUTE_ORDER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__READONLY:
				setReadonly(READONLY_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				setElement((DataExchangeElement) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE:
				setMappingAttribute((ExchangeMappingAttribute) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE:
				setDataType((JavaType) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__FORMAT:
				setFormat(FORMAT_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING:
				setDisableExternalMapping(DISABLE_EXTERNAL_MAPPING_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY:
				setUsedForCustomQuery(USED_FOR_CUSTOM_QUERY_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES:
				return valueListEntries != null && !valueListEntries.isEmpty();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__NAME:
				return name != null;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__VISIBLE:
				return visible != VISIBLE_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__OPTIONAL:
				return optional != OPTIONAL_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER:
				return attributeOrder != null;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__READONLY:
				return readonly != READONLY_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT:
				return getElement() != null;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE:
				return mappingAttribute != null;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE:
				return dataType != null;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__FORMAT:
				return format != null;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING:
				return disableExternalMapping != DISABLE_EXTERNAL_MAPPING_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY:
				return usedForCustomQuery != USED_FOR_CUSTOM_QUERY_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", visible: ");
		result.append(visible);
		result.append(", optional: ");
		result.append(optional);
		result.append(", attributeOrder: ");
		result.append(attributeOrder);
		result.append(", readonly: ");
		result.append(readonly);
		result.append(", format: ");
		result.append(format);
		result.append(", disableExternalMapping: ");
		result.append(disableExternalMapping);
		result.append(", usedForCustomQuery: ");
		result.append(usedForCustomQuery);
		result.append(')');

		return result.toString();
	}

}
