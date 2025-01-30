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
import java.util.HashSet;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Data Exchange Element</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getMinOccurrences <em>Min Occurrences</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getMaxOccurrences <em>Max Occurrences</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getElementOrder <em>Element Order</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getTypeName <em>Type Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getWrapperElementName <em>Wrapper Element
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getDataExchangeMethod <em>Data Exchange
 * Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getValueListEntries <em>Value List
 * Entries</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getMappingAttribute <em>Mapping
 * Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getSubElements <em>Sub Elements</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getParentElement <em>Parent Element</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getDataType <em>Data Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#getMappingObject <em>Mapping Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#isContainer <em>Container</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#isDisableExternalMapping <em>Disable External
 * Mapping</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl#isUsedForCustomQuery <em>Used For Custom
 * Query</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DataExchangeElementImpl extends EObjectImpl implements DataExchangeElement {
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
	 * The default value of the '{@link #getMinOccurrences() <em>Min Occurrences</em>}' attribute
	 * @see #getMinOccurrences()
	 * @generated
	 * @ordered
	 */
	protected static final Integer MIN_OCCURRENCES_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMinOccurrences() <em>Min Occurrences</em>}' attribute
	 * @see #getMinOccurrences()
	 * @generated
	 * @ordered
	 */
	protected Integer minOccurrences = MIN_OCCURRENCES_EDEFAULT;

	/**
	 * The default value of the '{@link #getMaxOccurrences() <em>Max Occurrences</em>}' attribute
	 * @see #getMaxOccurrences()
	 * @generated
	 * @ordered
	 */
	protected static final Integer MAX_OCCURRENCES_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMaxOccurrences() <em>Max Occurrences</em>}' attribute
	 * @see #getMaxOccurrences()
	 * @generated
	 * @ordered
	 */
	protected Integer maxOccurrences = MAX_OCCURRENCES_EDEFAULT;

	/**
	 * The default value of the '{@link #getElementOrder() <em>Element Order</em>}' attribute
	 * @see #getElementOrder()
	 * @generated
	 * @ordered
	 */
	protected static final Integer ELEMENT_ORDER_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getElementOrder() <em>Element Order</em>}' attribute
	 * @see #getElementOrder()
	 * @generated
	 * @ordered
	 */
	protected Integer elementOrder = ELEMENT_ORDER_EDEFAULT;

	/**
	 * The default value of the '{@link #getTypeName() <em>Type Name</em>}' attribute
	 * @see #getTypeName()
	 * @generated
	 * @ordered
	 */
	protected static final String TYPE_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTypeName() <em>Type Name</em>}' attribute
	 * @see #getTypeName()
	 * @generated
	 * @ordered
	 */
	protected String typeName = TYPE_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getWrapperElementName() <em>Wrapper Element Name</em>}' attribute
	 * @see #getWrapperElementName()
	 * @generated
	 * @ordered
	 */
	protected static final String WRAPPER_ELEMENT_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getWrapperElementName() <em>Wrapper Element Name</em>}' attribute
	 * @see #getWrapperElementName()
	 * @generated
	 * @ordered
	 */
	protected String wrapperElementName = WRAPPER_ELEMENT_NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getValueListEntries() <em>Value List Entries</em>}' containment reference list
	 * @see #getValueListEntries()
	 * @generated
	 * @ordered
	 */
	protected EList<ValueListEntry> valueListEntries;

	/**
	 * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference list
	 * @see #getAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<DataExchangeAttribute> attributes;

	/**
	 * The cached value of the '{@link #getMappingAttribute() <em>Mapping Attribute</em>}' reference
	 * @see #getMappingAttribute()
	 * @generated
	 * @ordered
	 */
	protected ExchangeMappingAttribute mappingAttribute;

	/**
	 * The cached value of the '{@link #getSubElements() <em>Sub Elements</em>}' containment reference list
	 * @see #getSubElements()
	 * @generated
	 * @ordered
	 */
	protected EList<DataExchangeElement> subElements;

	/**
	 * The cached value of the '{@link #getDataType() <em>Data Type</em>}' reference
	 * @see #getDataType()
	 * @generated
	 * @ordered
	 */
	protected JavaType dataType;

	/**
	 * The cached value of the '{@link #getMappingObject() <em>Mapping Object</em>}' reference
	 * @see #getMappingObject()
	 * @generated
	 * @ordered
	 */
	protected ExchangeMappingObject mappingObject;

	/**
	 * The default value of the '{@link #isContainer() <em>Container</em>}' attribute
	 * @see #isContainer()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CONTAINER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isContainer() <em>Container</em>}' attribute
	 * @see #isContainer()
	 * @generated
	 * @ordered
	 */
	protected boolean container = CONTAINER_EDEFAULT;

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
	protected DataExchangeElementImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.DATA_EXCHANGE_ELEMENT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMinOccurrences()
	 * @generated
	 */
	@Override
	public Integer getMinOccurrences() {
		return minOccurrences;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setMinOccurrences(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setMinOccurrences(Integer newMinOccurrences) {
		final Integer oldMinOccurrences = minOccurrences;
		minOccurrences = newMinOccurrences;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES,
					oldMinOccurrences, minOccurrences));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMaxOccurrences()
	 * @generated
	 */
	@Override
	public Integer getMaxOccurrences() {
		return maxOccurrences;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setMaxOccurrences(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setMaxOccurrences(Integer newMaxOccurrences) {
		final Integer oldMaxOccurrences = maxOccurrences;
		maxOccurrences = newMaxOccurrences;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES,
					oldMaxOccurrences, maxOccurrences));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getElementOrder()
	 * @generated
	 */
	@Override
	public Integer getElementOrder() {
		return elementOrder;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setElementOrder(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setElementOrder(Integer newElementOrder) {
		final Integer oldElementOrder = elementOrder;
		elementOrder = newElementOrder;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER, oldElementOrder,
					elementOrder));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getTypeName()
	 * @generated
	 */
	@Override
	public String getTypeName() {
		return typeName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setTypeName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setTypeName(String newTypeName) {
		final String oldTypeName = typeName;
		typeName = newTypeName;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__TYPE_NAME, oldTypeName, typeName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getWrapperElementName()
	 * @generated
	 */
	@Override
	public String getWrapperElementName() {
		return wrapperElementName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setWrapperElementName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setWrapperElementName(String newWrapperElementName) {
		final String oldWrapperElementName = wrapperElementName;
		wrapperElementName = newWrapperElementName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME,
					oldWrapperElementName, wrapperElementName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod()
	 * @generated
	 */
	@Override
	public DataExchangeMethod getDataExchangeMethod() {
		if (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD)
			return null;

		return (DataExchangeMethod) eInternalContainer();
	}

	/**
	 * @param newDataExchangeMethod
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDataExchangeMethod(DataExchangeMethod newDataExchangeMethod, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newDataExchangeMethod,
				ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setDataExchangeMethod(net.codecadenza.eclipse.model.exchange.
	 * DataExchangeMethod)
	 * @generated
	 */
	@Override
	public void setDataExchangeMethod(DataExchangeMethod newDataExchangeMethod) {
		if (newDataExchangeMethod != eInternalContainer()
				|| (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD
						&& newDataExchangeMethod != null)) {
			if (EcoreUtil.isAncestor(this, newDataExchangeMethod))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newDataExchangeMethod != null)
				msgs = ((InternalEObject) newDataExchangeMethod).eInverseAdd(this, ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT,
						DataExchangeMethod.class, msgs);

			msgs = basicSetDataExchangeMethod(newDataExchangeMethod, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD,
					newDataExchangeMethod, newDataExchangeMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getValueListEntries()
	 * @generated
	 */
	@Override
	public EList<ValueListEntry> getValueListEntries() {
		if (valueListEntries == null)
			valueListEntries = new EObjectContainmentEList<>(ValueListEntry.class, this,
					ExchangePackage.DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES);

		return valueListEntries;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAttributes()
	 * @generated
	 */
	@Override
	public EList<DataExchangeAttribute> getAttributes() {
		if (attributes == null)
			attributes = new EObjectContainmentWithInverseEList<>(DataExchangeAttribute.class, this,
					ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES, ExchangePackage.DATA_EXCHANGE_ATTRIBUTE__ELEMENT);

		return attributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingAttribute()
	 * @generated
	 */
	@Override
	public ExchangeMappingAttribute getMappingAttribute() {
		if (mappingAttribute != null && mappingAttribute.eIsProxy()) {
			final var oldMappingAttribute = (InternalEObject) mappingAttribute;
			mappingAttribute = (ExchangeMappingAttribute) eResolveProxy(oldMappingAttribute);

			if (mappingAttribute != oldMappingAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE,
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
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#
	 * setMappingAttribute(net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute)
	 * @generated
	 */
	@Override
	public void setMappingAttribute(ExchangeMappingAttribute newMappingAttribute) {
		final ExchangeMappingAttribute oldMappingAttribute = mappingAttribute;
		mappingAttribute = newMappingAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE,
					oldMappingAttribute, mappingAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getSubElements()
	 * @generated
	 */
	@Override
	public EList<DataExchangeElement> getSubElements() {
		if (subElements == null)
			subElements = new EObjectContainmentWithInverseEList<>(DataExchangeElement.class, this,
					ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS, ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT);

		return subElements;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement()
	 * @generated
	 */
	@Override
	public DataExchangeElement getParentElement() {
		if (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT)
			return null;

		return (DataExchangeElement) eInternalContainer();
	}

	/**
	 * @param newParentElement
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetParentElement(DataExchangeElement newParentElement, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newParentElement, ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setParentElement(net.codecadenza.eclipse.model.exchange.
	 * DataExchangeElement)
	 * @generated
	 */
	@Override
	public void setParentElement(DataExchangeElement newParentElement) {
		if (newParentElement != eInternalContainer()
				|| (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT && newParentElement != null)) {
			if (EcoreUtil.isAncestor(this, newParentElement))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newParentElement != null)
				msgs = ((InternalEObject) newParentElement).eInverseAdd(this, ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS,
						DataExchangeElement.class, msgs);

			msgs = basicSetParentElement(newParentElement, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT,
					newParentElement, newParentElement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataType()
	 * @generated
	 */
	@Override
	public JavaType getDataType() {
		if (dataType != null && dataType.eIsProxy()) {
			final var oldDataType = (InternalEObject) dataType;
			dataType = (JavaType) eResolveProxy(oldDataType);

			if (dataType != oldDataType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_TYPE, oldDataType,
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
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setDataType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setDataType(JavaType newDataType) {
		final JavaType oldDataType = dataType;
		dataType = newDataType;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_TYPE, oldDataType, dataType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingObject()
	 * @generated
	 */
	@Override
	public ExchangeMappingObject getMappingObject() {
		if (mappingObject != null && mappingObject.eIsProxy()) {
			final var oldMappingObject = (InternalEObject) mappingObject;
			mappingObject = (ExchangeMappingObject) eResolveProxy(oldMappingObject);

			if (mappingObject != oldMappingObject && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT,
						oldMappingObject, mappingObject));
		}

		return mappingObject;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public ExchangeMappingObject basicGetMappingObject() {
		return mappingObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setMappingObject(net.codecadenza.eclipse.model.exchange.
	 * ExchangeMappingObject)
	 * @generated
	 */
	@Override
	public void setMappingObject(ExchangeMappingObject newMappingObject) {
		final ExchangeMappingObject oldMappingObject = mappingObject;
		mappingObject = newMappingObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT,
					oldMappingObject, mappingObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#isContainer()
	 * @generated
	 */
	@Override
	public boolean isContainer() {
		return container;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setContainer(boolean)
	 */
	@Override
	public void setContainer(boolean newContainer) {
		final boolean oldContainer = container;
		container = newContainer;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__CONTAINER, oldContainer,
					container));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#isDisableExternalMapping()
	 * @generated
	 */
	@Override
	public boolean isDisableExternalMapping() {
		return disableExternalMapping;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setDisableExternalMapping(boolean)
	 * @generated
	 */
	@Override
	public void setDisableExternalMapping(boolean newDisableExternalMapping) {
		final boolean oldDisableExternalMapping = disableExternalMapping;
		disableExternalMapping = newDisableExternalMapping;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING,
					oldDisableExternalMapping, disableExternalMapping));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#isUsedForCustomQuery()
	 * @generated
	 */
	@Override
	public boolean isUsedForCustomQuery() {
		return usedForCustomQuery;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#setUsedForCustomQuery(boolean)
	 * @generated
	 */
	@Override
	public void setUsedForCustomQuery(boolean newUsedForCustomQuery) {
		final boolean oldUsedForCustomQuery = usedForCustomQuery;
		usedForCustomQuery = newUsedForCustomQuery;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY,
					oldUsedForCustomQuery, usedForCustomQuery));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetDataExchangeMethod((DataExchangeMethod) otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getAttributes()).basicAdd(otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getSubElements()).basicAdd(otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetParentElement((DataExchangeElement) otherEnd, msgs);
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
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				return basicSetDataExchangeMethod(null, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES:
				return ((InternalEList<?>) getValueListEntries()).basicRemove(otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES:
				return ((InternalEList<?>) getAttributes()).basicRemove(otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS:
				return ((InternalEList<?>) getSubElements()).basicRemove(otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				return basicSetParentElement(null, msgs);
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
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				return eInternalContainer().eInverseRemove(this, ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT,
						DataExchangeMethod.class, msgs);
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				return eInternalContainer().eInverseRemove(this, ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS,
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
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__NAME:
				return getName();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES:
				return getMinOccurrences();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES:
				return getMaxOccurrences();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER:
				return getElementOrder();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__TYPE_NAME:
				return getTypeName();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME:
				return getWrapperElementName();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				return getDataExchangeMethod();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES:
				return getValueListEntries();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES:
				return getAttributes();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE:
				if (resolve)
					return getMappingAttribute();

				return basicGetMappingAttribute();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS:
				return getSubElements();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				return getParentElement();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_TYPE:
				if (resolve)
					return getDataType();

				return basicGetDataType();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT:
				if (resolve)
					return getMappingObject();

				return basicGetMappingObject();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__CONTAINER:
				return isContainer();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING:
				return isDisableExternalMapping();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY:
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
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__NAME:
				setName((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES:
				setMinOccurrences((Integer) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES:
				setMaxOccurrences((Integer) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER:
				setElementOrder((Integer) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__TYPE_NAME:
				setTypeName((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME:
				setWrapperElementName((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				setDataExchangeMethod((DataExchangeMethod) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES:
				getValueListEntries().clear();
				getValueListEntries().addAll((Collection<? extends ValueListEntry>) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES:
				getAttributes().clear();
				getAttributes().addAll((Collection<? extends DataExchangeAttribute>) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE:
				setMappingAttribute((ExchangeMappingAttribute) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS:
				getSubElements().clear();
				getSubElements().addAll((Collection<? extends DataExchangeElement>) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				setParentElement((DataExchangeElement) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_TYPE:
				setDataType((JavaType) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT:
				setMappingObject((ExchangeMappingObject) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__CONTAINER:
				setContainer((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING:
				setDisableExternalMapping((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY:
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
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES:
				setMinOccurrences(MIN_OCCURRENCES_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES:
				setMaxOccurrences(MAX_OCCURRENCES_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER:
				setElementOrder(ELEMENT_ORDER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__TYPE_NAME:
				setTypeName(TYPE_NAME_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME:
				setWrapperElementName(WRAPPER_ELEMENT_NAME_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				setDataExchangeMethod((DataExchangeMethod) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES:
				getValueListEntries().clear();
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES:
				getAttributes().clear();
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE:
				setMappingAttribute((ExchangeMappingAttribute) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS:
				getSubElements().clear();
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				setParentElement((DataExchangeElement) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_TYPE:
				setDataType((JavaType) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT:
				setMappingObject((ExchangeMappingObject) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__CONTAINER:
				setContainer(CONTAINER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING:
				setDisableExternalMapping(DISABLE_EXTERNAL_MAPPING_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY:
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
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__NAME:
				return name != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES:
				return minOccurrences != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES:
				return maxOccurrences != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER:
				return elementOrder != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__TYPE_NAME:
				return typeName != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME:
				return wrapperElementName != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD:
				return getDataExchangeMethod() != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES:
				return valueListEntries != null && !valueListEntries.isEmpty();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__ATTRIBUTES:
				return attributes != null && !attributes.isEmpty();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE:
				return mappingAttribute != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS:
				return subElements != null && !subElements.isEmpty();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT:
				return getParentElement() != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_TYPE:
				return dataType != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT:
				return mappingObject != null;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__CONTAINER:
				return container != CONTAINER_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING:
				return disableExternalMapping != DISABLE_EXTERNAL_MAPPING_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY:
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
		result.append(", minOccurrences: ");
		result.append(minOccurrences);
		result.append(", maxOccurrences: ");
		result.append(maxOccurrences);
		result.append(", elementOrder: ");
		result.append(elementOrder);
		result.append(", typeName: ");
		result.append(typeName);
		result.append(", wrapperElementName: ");
		result.append(wrapperElementName);
		result.append(", container: ");
		result.append(container);
		result.append(", disableExternalMapping: ");
		result.append(disableExternalMapping);
		result.append(", usedForCustomQuery: ");
		result.append(usedForCustomQuery);
		result.append(')');

		return result.toString();
	}

	/**
	 * @param parentElement
	 * @return all attributes of this element including attributes of all sub-elements
	 * @generated not
	 */
	private BasicEList<DataExchangeAttribute> getAllAttributes(DataExchangeElement parentElement) {
		final var allAttributes = new BasicEList<DataExchangeAttribute>();

		parentElement.getSubElements().forEach(element -> {
			allAttributes.addAll(element.getAttributes());
			allAttributes.addAll(getAllAttributes(element));
		});

		return allAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAllAttributes()
	 * @generated not
	 */
	@Override
	public BasicEList<DataExchangeAttribute> getAllAttributes() {
		final var allAttributes = new BasicEList<DataExchangeAttribute>();

		// Add attributes of this (root) element!
		allAttributes.addAll(getAttributes());

		getSubElements().forEach(element -> {
			allAttributes.addAll(element.getAttributes());
			allAttributes.addAll(getAllAttributes(element));
		});

		return allAttributes;
	}

	/**
	 * @param parentElement
	 * @return all elements this element including elements of all sub-elements
	 * @generated not
	 */
	private BasicEList<DataExchangeElement> getAllElements(DataExchangeElement parentElement) {
		final var allElements = new BasicEList<DataExchangeElement>();

		parentElement.getSubElements().forEach(element -> {
			allElements.add(element);
			allElements.addAll(getAllElements(element));
		});

		return allElements;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAllElements()
	 * @generated not
	 */
	@Override
	public BasicEList<DataExchangeElement> getAllElements() {
		final var allElements = new BasicEList<DataExchangeElement>();

		// Add this (root) element!
		allElements.add(this);

		getSubElements().forEach(element -> {
			allElements.add(element);
			allElements.addAll(getAllElements(element));
		});

		return allElements;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getRootElement()
	 * @generated not
	 */
	@Override
	public DataExchangeElement getRootElement() {
		if (getParentElement() == null)
			return this;

		DataExchangeElement currentParent = getParentElement();

		while (true) {
			if (currentParent.getParentElement() == null)
				return currentParent;

			currentParent = currentParent.getParentElement();
		}
	}

	/**
	 * @param element
	 * @generated not
	 * @throws IllegalStateException if the given data exchange element (or one of its children) contains duplicate attribute and
	 *           element names
	 */
	private void checkElementStructure(DataExchangeElement element) {
		final var attributeNameSet = new HashSet<String>();
		final var elementNameSet = new HashSet<String>();

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attributeNameSet.contains(attr.getName()))
				throw new IllegalStateException("An attribute with the name '" + attr.getName() + "' already exists!");

			attributeNameSet.add(attr.getName());
		}

		for (final DataExchangeElement subElement : element.getSubElements()) {
			if (elementNameSet.contains(subElement.getName()))
				throw new IllegalStateException("An element with the name '" + subElement.getName() + "' already exists!");

			// The names of all attributes and sub-elements must be unique!
			if (attributeNameSet.contains(subElement.getName()))
				throw new IllegalStateException("An attribute with the name '" + subElement.getName() + "' already exists!");

			elementNameSet.add(subElement.getName());

			checkElementStructure(subElement);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#checkElementStructure()
	 * @generated not
	 */
	@Override
	public void checkElementStructure() {
		checkElementStructure(this);
	}

}
