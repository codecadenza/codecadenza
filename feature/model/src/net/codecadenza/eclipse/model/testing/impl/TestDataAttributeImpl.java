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
package net.codecadenza.eclipse.model.testing.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Test Data Attribute</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getMappingAttribute <em>Mapping
 * Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getValue <em>Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#isTrackValue <em>Track Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getOperator <em>Operator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getReferencedAttribute <em>Referenced
 * Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getReferencedObjects <em>Referenced
 * Objects</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getMappingType <em>Mapping Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getExpectedSize <em>Expected Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getExpectedSizeOperator <em>Expected Size
 * Operator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataAttributeImpl#getId <em>Id</em>}</li>
 * </ul>
 * @generated
 */
public class TestDataAttributeImpl extends EObjectImpl implements TestDataAttribute {
	/**
	 * The cached value of the '{@link #getMappingAttribute() <em>Mapping Attribute</em>}' reference
	 * @see #getMappingAttribute()
	 * @generated
	 * @ordered
	 */
	protected MappingAttribute mappingAttribute;

	/**
	 * The default value of the '{@link #getValue() <em>Value</em>}' attribute
	 * @see #getValue()
	 * @generated
	 * @ordered
	 */
	protected static final String VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getValue() <em>Value</em>}' attribute
	 * @see #getValue()
	 * @generated
	 * @ordered
	 */
	protected String value = VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #isTrackValue() <em>Track Value</em>}' attribute
	 * @see #isTrackValue()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TRACK_VALUE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isTrackValue() <em>Track Value</em>}' attribute
	 * @see #isTrackValue()
	 * @generated
	 * @ordered
	 */
	protected boolean trackValue = TRACK_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getOperator() <em>Operator</em>}' attribute
	 * @see #getOperator()
	 * @generated
	 * @ordered
	 */
	protected static final AssertionOperator OPERATOR_EDEFAULT = AssertionOperator.NONE;

	/**
	 * The cached value of the '{@link #getOperator() <em>Operator</em>}' attribute
	 * @see #getOperator()
	 * @generated
	 * @ordered
	 */
	protected AssertionOperator operator = OPERATOR_EDEFAULT;

	/**
	 * The cached value of the '{@link #getReferencedAttribute() <em>Referenced Attribute</em>}' reference
	 * @see #getReferencedAttribute()
	 * @generated
	 * @ordered
	 */
	protected TestDataAttribute referencedAttribute;

	/**
	 * The cached value of the '{@link #getReferencedObjects() <em>Referenced Objects</em>}' containment reference list
	 * @see #getReferencedObjects()
	 * @generated
	 * @ordered
	 */
	protected EList<TestDataObject> referencedObjects;

	/**
	 * The cached value of the '{@link #getMappingType() <em>Mapping Type</em>}' reference
	 * @see #getMappingType()
	 * @generated
	 * @ordered
	 */
	protected JavaType mappingType;

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
	 * The default value of the '{@link #getExpectedSize() <em>Expected Size</em>}' attribute
	 * @see #getExpectedSize()
	 * @generated
	 * @ordered
	 */
	protected static final Integer EXPECTED_SIZE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getExpectedSize() <em>Expected Size</em>}' attribute
	 * @see #getExpectedSize()
	 * @generated
	 * @ordered
	 */
	protected Integer expectedSize = EXPECTED_SIZE_EDEFAULT;

	/**
	 * The default value of the '{@link #getExpectedSizeOperator() <em>Expected Size Operator</em>}' attribute
	 * @see #getExpectedSizeOperator()
	 * @generated
	 * @ordered
	 */
	protected static final AssertionOperator EXPECTED_SIZE_OPERATOR_EDEFAULT = AssertionOperator.NONE;

	/**
	 * The cached value of the '{@link #getExpectedSizeOperator() <em>Expected Size Operator</em>}' attribute
	 * @see #getExpectedSizeOperator()
	 * @generated
	 * @ordered
	 */
	protected AssertionOperator expectedSizeOperator = EXPECTED_SIZE_OPERATOR_EDEFAULT;

	/**
	 * The default value of the '{@link #getId() <em>Id</em>}' attribute
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected static final String ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getId() <em>Id</em>}' attribute
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected String id = ID_EDEFAULT;

	/**
	 * @generated
	 */
	protected TestDataAttributeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.TEST_DATA_ATTRIBUTE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingAttribute()
	 * @generated
	 */
	@Override
	public MappingAttribute getMappingAttribute() {
		if (mappingAttribute != null && mappingAttribute.eIsProxy()) {
			final var oldMappingAttribute = (InternalEObject) mappingAttribute;
			mappingAttribute = (MappingAttribute) eResolveProxy(oldMappingAttribute);

			if ((mappingAttribute != oldMappingAttribute) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE,
						oldMappingAttribute, mappingAttribute));
		}

		return mappingAttribute;
	}

	/**
	 * @return the mapping attribute
	 * @generated
	 */
	public MappingAttribute basicGetMappingAttribute() {
		return mappingAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#
	 * setMappingAttribute(net.codecadenza.eclipse.model.mapping.MappingAttribute)
	 * @generated
	 */
	@Override
	public void setMappingAttribute(MappingAttribute newMappingAttribute) {
		final MappingAttribute oldMappingAttribute = mappingAttribute;
		mappingAttribute = newMappingAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE,
					oldMappingAttribute, mappingAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getValue()
	 * @generated
	 */
	@Override
	public String getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setValue(String newValue) {
		final String oldValue = value;
		value = newValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__VALUE, oldValue, value));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#isTrackValue()
	 * @generated
	 */
	@Override
	public boolean isTrackValue() {
		return trackValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setTrackValue(boolean)
	 * @generated
	 */
	@Override
	public void setTrackValue(boolean newTrackValue) {
		final boolean oldTrackValue = trackValue;
		trackValue = newTrackValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__TRACK_VALUE, oldTrackValue,
					trackValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getOperator()
	 * @generated
	 */
	@Override
	public AssertionOperator getOperator() {
		return operator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#
	 * setOperator(net.codecadenza.eclipse.model.testing.AssertionOperator)
	 * @generated
	 */
	@Override
	public void setOperator(AssertionOperator newOperator) {
		final AssertionOperator oldOperator = operator;
		operator = newOperator == null ? OPERATOR_EDEFAULT : newOperator;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__OPERATOR, oldOperator, operator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedAttribute()
	 * @generated
	 */
	@Override
	public TestDataAttribute getReferencedAttribute() {
		if (referencedAttribute != null && referencedAttribute.eIsProxy()) {
			final var oldReferencedAttribute = (InternalEObject) referencedAttribute;
			referencedAttribute = (TestDataAttribute) eResolveProxy(oldReferencedAttribute);

			if ((referencedAttribute != oldReferencedAttribute) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE,
						oldReferencedAttribute, referencedAttribute));
		}

		return referencedAttribute;
	}

	/**
	 * @return the test data attribute
	 * @generated
	 */
	public TestDataAttribute basicGetReferencedAttribute() {
		return referencedAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#
	 * setReferencedAttribute(net.codecadenza.eclipse.model.testing.TestDataAttribute)
	 * @generated
	 */
	@Override
	public void setReferencedAttribute(TestDataAttribute newReferencedAttribute) {
		final TestDataAttribute oldReferencedAttribute = referencedAttribute;
		referencedAttribute = newReferencedAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE,
					oldReferencedAttribute, referencedAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedObjects()
	 * @generated
	 */
	@Override
	public EList<TestDataObject> getReferencedObjects() {
		if (referencedObjects == null)
			referencedObjects = new EObjectContainmentEList<>(TestDataObject.class, this,
					TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS);

		return referencedObjects;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingType()
	 * @generated
	 */
	@Override
	public JavaType getMappingType() {
		if (mappingType != null && mappingType.eIsProxy()) {
			final InternalEObject oldMappingType = (InternalEObject) mappingType;
			mappingType = (JavaType) eResolveProxy(oldMappingType);

			if (mappingType != oldMappingType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_TYPE,
						oldMappingType, mappingType));
		}

		return mappingType;
	}

	/**
	 * @return the mapping type
	 * @generated
	 */
	public JavaType basicGetMappingType() {
		return mappingType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setMappingType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setMappingType(JavaType newMappingType) {
		final JavaType oldMappingType = mappingType;
		mappingType = newMappingType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_TYPE, oldMappingType,
					mappingType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSize()
	 * @generated
	 */
	@Override
	public Integer getExpectedSize() {
		return expectedSize;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setExpectedSize(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setExpectedSize(Integer newExpectedSize) {
		final Integer oldExpectedSize = expectedSize;
		expectedSize = newExpectedSize;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE, oldExpectedSize,
					expectedSize));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSizeOperator()
	 * @generated
	 */
	@Override
	public AssertionOperator getExpectedSizeOperator() {
		return expectedSizeOperator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setExpectedSizeOperator(net.codecadenza.eclipse.model.testing.
	 * AssertionOperator)
	 * @generated
	 */
	@Override
	public void setExpectedSizeOperator(AssertionOperator newExpectedSizeOperator) {
		final AssertionOperator oldExpectedSizeOperator = expectedSizeOperator;
		expectedSizeOperator = newExpectedSizeOperator == null ? EXPECTED_SIZE_OPERATOR_EDEFAULT : newExpectedSizeOperator;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR,
					oldExpectedSizeOperator, expectedSizeOperator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getId()
	 * @generated
	 */
	@Override
	public String getId() {
		return id;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#setId(java.lang.String)
	 * @generated
	 */
	@Override
	public void setId(String newId) {
		final String oldId = id;
		id = newId;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_ATTRIBUTE__ID, oldId, id));
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
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS:
				return ((InternalEList<?>) getReferencedObjects()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE:
				if (resolve)
					return getMappingAttribute();

				return basicGetMappingAttribute();
			case TestingPackage.TEST_DATA_ATTRIBUTE__VALUE:
				return getValue();
			case TestingPackage.TEST_DATA_ATTRIBUTE__TRACK_VALUE:
				return isTrackValue();
			case TestingPackage.TEST_DATA_ATTRIBUTE__OPERATOR:
				return getOperator();
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE:
				if (resolve)
					return getReferencedAttribute();

				return basicGetReferencedAttribute();
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS:
				return getReferencedObjects();
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_TYPE:
				if (resolve)
					return getMappingType();

				return basicGetMappingType();
			case TestingPackage.TEST_DATA_ATTRIBUTE__NAME:
				return getName();
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE:
				return getExpectedSize();
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR:
				return getExpectedSizeOperator();
			case TestingPackage.TEST_DATA_ATTRIBUTE__ID:
				return getId();
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
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE:
				setMappingAttribute((MappingAttribute) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__VALUE:
				setValue((String) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__TRACK_VALUE:
				setTrackValue((Boolean) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__OPERATOR:
				setOperator((AssertionOperator) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE:
				setReferencedAttribute((TestDataAttribute) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS:
				getReferencedObjects().clear();
				getReferencedObjects().addAll((Collection<? extends TestDataObject>) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_TYPE:
				setMappingType((JavaType) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__NAME:
				setName((String) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE:
				setExpectedSize((Integer) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR:
				setExpectedSizeOperator((AssertionOperator) newValue);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__ID:
				setId((String) newValue);
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
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE:
				setMappingAttribute((MappingAttribute) null);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__VALUE:
				setValue(VALUE_EDEFAULT);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__TRACK_VALUE:
				setTrackValue(TRACK_VALUE_EDEFAULT);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__OPERATOR:
				setOperator(OPERATOR_EDEFAULT);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE:
				setReferencedAttribute((TestDataAttribute) null);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS:
				getReferencedObjects().clear();
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_TYPE:
				setMappingType((JavaType) null);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE:
				setExpectedSize(EXPECTED_SIZE_EDEFAULT);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR:
				setExpectedSizeOperator(EXPECTED_SIZE_OPERATOR_EDEFAULT);
				return;
			case TestingPackage.TEST_DATA_ATTRIBUTE__ID:
				setId(ID_EDEFAULT);
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
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_ATTRIBUTE:
				return mappingAttribute != null;
			case TestingPackage.TEST_DATA_ATTRIBUTE__VALUE:
				return value != null;
			case TestingPackage.TEST_DATA_ATTRIBUTE__TRACK_VALUE:
				return trackValue != TRACK_VALUE_EDEFAULT;
			case TestingPackage.TEST_DATA_ATTRIBUTE__OPERATOR:
				return operator != OPERATOR_EDEFAULT;
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_ATTRIBUTE:
				return referencedAttribute != null;
			case TestingPackage.TEST_DATA_ATTRIBUTE__REFERENCED_OBJECTS:
				return referencedObjects != null && !referencedObjects.isEmpty();
			case TestingPackage.TEST_DATA_ATTRIBUTE__MAPPING_TYPE:
				return mappingType != null;
			case TestingPackage.TEST_DATA_ATTRIBUTE__NAME:
				return name != null;
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE:
				return expectedSize != null;
			case TestingPackage.TEST_DATA_ATTRIBUTE__EXPECTED_SIZE_OPERATOR:
				return expectedSizeOperator != EXPECTED_SIZE_OPERATOR_EDEFAULT;
			case TestingPackage.TEST_DATA_ATTRIBUTE__ID:
				return id != null;
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

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (value: ");
		result.append(value);
		result.append(", trackValue: ");
		result.append(trackValue);
		result.append(", operator: ");
		result.append(operator);
		result.append(", name: ");
		result.append(name);
		result.append(", expectedSize: ");
		result.append(expectedSize);
		result.append(", expectedSizeOperator: ");
		result.append(expectedSizeOperator);
		result.append(", id: ");
		result.append(id);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getJavaType()
	 * @generated not
	 */
	@Override
	public JavaType getJavaType() {
		if (getMappingAttribute() != null) {
			if (getMappingAttribute().getMappingType() != null)
				return getMappingAttribute().getMappingType();
			else if (getMappingAttribute().getDomainAttribute() != null)
				return getMappingAttribute().getDomainAttribute().getJavaType();
			else if (getMappingAttribute() instanceof final DTOBeanAttribute dtoAttribute)
				return dtoAttribute.getReferencedDTOBean();
			else if (getMappingAttribute() instanceof final ExchangeMappingAttribute exchangeAttribute)
				return exchangeAttribute.getJavaType();
		}

		if (getMappingType() != null)
			return getMappingType();

		// In the case of either a search or a count operation the type (either SearchInput or SearchInputField) is not defined in the
		// project meta-data and must be handled in a special way!
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#getLabel()
	 * @generated not
	 */
	@Override
	public String getLabel() {
		if (getMappingAttribute() != null) {
			final boolean listField = isMappedToList();
			final var label = new StringBuilder();

			if (listField)
				label.append("List<");

			if (getJavaType() != null)
				label.append(getJavaType().getName());

			if (listField)
				label.append(">");

			label.append(" ");
			label.append(getMappingAttribute().getName());

			return label.toString();
		}
		else if (getMappingType() != null && getName() != null)
			return getMappingType().getName() + " " + getName();

		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#isMandatory()
	 * @generated not
	 */
	@Override
	public boolean isMandatory() {
		if (getMappingType() != null)
			return getMappingType().isPrimitive();

		if (getMappingAttribute().getMappingType() != null)
			return !getMappingAttribute().getMappingType().isPrimitive();
		else if (getMappingAttribute().getDomainAttribute() != null)
			return !getMappingAttribute().getDomainAttribute().getDomainAttributeValidator().isNullable();
		else if (getMappingAttribute() instanceof final DTOBeanAttribute dtoAttribute && dtoAttribute.getReferencedDTOBean() != null
				&& dtoAttribute.getAssociation() != null && dtoAttribute.getAssociation() instanceof final ManyToOneAssociation mto)
			return !mto.isOptional();
		else if (getMappingAttribute() instanceof final ExchangeMappingAttribute exchangeAttribute
				&& exchangeAttribute.getAssociation() != null
				&& exchangeAttribute.getAssociation() instanceof final ManyToOneAssociation mto)
			return !mto.isOptional();

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#skip()
	 * @generated not
	 */
	@Override
	public boolean skip() {
		return getValue() == null && getExpectedSize() == null && getOperator() == AssertionOperator.NONE
				&& getReferencedObjects().isEmpty() && getReferencedAttribute() == null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#isMappedToElementCollection()
	 * @generated not
	 */
	@Override
	public boolean isMappedToElementCollection() {
		return getMappingAttribute() != null && getMappingAttribute().getDomainAttribute() != null
				&& getMappingAttribute().getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#isMappedToFile()
	 * @generated not
	 */
	@Override
	public boolean isMappedToFile() {
		return getMappingAttribute() != null && getMappingAttribute().getDomainAttribute() != null
				&& (getMappingAttribute().getDomainAttribute().getJavaType().isByteArray()
						|| getMappingAttribute().getDomainAttribute().getTag() == AttributeTagEnumeration.DOCUMENT_REF);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#isMappedToList()
	 * @generated not
	 */
	@Override
	public boolean isMappedToList() {
		if (getMappingAttribute() == null)
			return false;

		return getMappingAttribute().getDomainAttribute() == null
				&& (getMappingAttribute().getAssociation() instanceof ManyToManyAssociation
						|| getMappingAttribute().getAssociation() instanceof OneToManyAssociation
						|| getMappingAttribute().getModifier() == JavaTypeModifierEnumeration.LIST);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataAttribute#
	 * isReferenceAllowed(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated not
	 */
	@Override
	public boolean isReferenceAllowed(DomainObject domainObject) {
		if (getMappingAttribute() == null || getMappingAttribute().getDomainAttribute() == null)
			return domainObject.getPKAttribute().getJavaType().equals(getJavaType());

		final DomainObject referencedDomainObject = getMappingAttribute().getDomainAttribute().getDomainObject();
		final Collection<DomainObject> inheritanceTree = domainObject.getFullInheritanceTree();

		return inheritanceTree.contains(referencedDomainObject) && getMappingAttribute().getAssociation() == null
				&& getMappingAttribute().getDomainAttribute().isPk();
	}

}
