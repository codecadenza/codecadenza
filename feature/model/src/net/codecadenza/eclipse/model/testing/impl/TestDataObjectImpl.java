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
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
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
 * An implementation of the model object '<em><b>Test Data Object</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl#getMappingObject <em>Mapping Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.TestDataObjectImpl#getReferencedObject <em>Referenced Object</em>}</li>
 * </ul>
 * @generated
 */
public class TestDataObjectImpl extends EObjectImpl implements TestDataObject {
	/**
	 * The cached value of the '{@link #getMappingObject() <em>Mapping Object</em>}' reference
	 * @see #getMappingObject()
	 * @generated
	 * @ordered
	 */
	protected MappingObject mappingObject;

	/**
	 * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference list
	 * @see #getAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<TestDataAttribute> attributes;

	/**
	 * The cached value of the '{@link #getReferencedObject() <em>Referenced Object</em>}' reference
	 * @see #getReferencedObject()
	 * @generated
	 * @ordered
	 */
	protected TestDataObject referencedObject;

	/**
	 * @generated
	 */
	protected TestDataObjectImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.TEST_DATA_OBJECT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getMappingObject()
	 * @generated
	 */
	@Override
	public MappingObject getMappingObject() {
		if (mappingObject != null && mappingObject.eIsProxy()) {
			final var oldMappingObject = (InternalEObject) mappingObject;

			mappingObject = (MappingObject) eResolveProxy(oldMappingObject);

			if ((mappingObject != oldMappingObject) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.TEST_DATA_OBJECT__MAPPING_OBJECT,
						oldMappingObject, mappingObject));
		}

		return mappingObject;
	}

	/**
	 * @return the mapping object
	 * @generated
	 */
	public MappingObject basicGetMappingObject() {
		return mappingObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#
	 * setMappingObject(net.codecadenza.eclipse.model.mapping.MappingObject)
	 * @generated
	 */
	@Override
	public void setMappingObject(MappingObject newMappingObject) {
		final MappingObject oldMappingObject = mappingObject;
		mappingObject = newMappingObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_OBJECT__MAPPING_OBJECT, oldMappingObject,
					mappingObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getAttributes()
	 * @generated
	 */
	@Override
	public EList<TestDataAttribute> getAttributes() {
		if (attributes == null)
			attributes = new EObjectContainmentEList<>(TestDataAttribute.class, this, TestingPackage.TEST_DATA_OBJECT__ATTRIBUTES);

		return attributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getReferencedObject()
	 * @generated
	 */
	@Override
	public TestDataObject getReferencedObject() {
		if (referencedObject != null && referencedObject.eIsProxy()) {
			final var oldReferencedObject = (InternalEObject) referencedObject;
			referencedObject = (TestDataObject) eResolveProxy(oldReferencedObject);

			if ((referencedObject != oldReferencedObject) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.TEST_DATA_OBJECT__REFERENCED_OBJECT,
						oldReferencedObject, referencedObject));
		}

		return referencedObject;
	}

	/**
	 * @return the referenced object
	 * @generated
	 */
	public TestDataObject basicGetReferencedObject() {
		return referencedObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#
	 * setReferencedObject(net.codecadenza.eclipse.model.testing.TestDataObject)
	 * @generated
	 */
	@Override
	public void setReferencedObject(TestDataObject newReferencedObject) {
		final TestDataObject oldReferencedObject = referencedObject;
		referencedObject = newReferencedObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.TEST_DATA_OBJECT__REFERENCED_OBJECT,
					oldReferencedObject, referencedObject));
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
			case TestingPackage.TEST_DATA_OBJECT__ATTRIBUTES:
				return ((InternalEList<?>) getAttributes()).basicRemove(otherEnd, msgs);
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
			case TestingPackage.TEST_DATA_OBJECT__MAPPING_OBJECT:
				if (resolve)
					return getMappingObject();

				return basicGetMappingObject();
			case TestingPackage.TEST_DATA_OBJECT__ATTRIBUTES:
				return getAttributes();
			case TestingPackage.TEST_DATA_OBJECT__REFERENCED_OBJECT:
				if (resolve)
					return getReferencedObject();

				return basicGetReferencedObject();
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
			case TestingPackage.TEST_DATA_OBJECT__MAPPING_OBJECT:
				setMappingObject((MappingObject) newValue);
				return;
			case TestingPackage.TEST_DATA_OBJECT__ATTRIBUTES:
				getAttributes().clear();
				getAttributes().addAll((Collection<? extends TestDataAttribute>) newValue);
				return;
			case TestingPackage.TEST_DATA_OBJECT__REFERENCED_OBJECT:
				setReferencedObject((TestDataObject) newValue);
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
			case TestingPackage.TEST_DATA_OBJECT__MAPPING_OBJECT:
				setMappingObject((MappingObject) null);
				return;
			case TestingPackage.TEST_DATA_OBJECT__ATTRIBUTES:
				getAttributes().clear();
				return;
			case TestingPackage.TEST_DATA_OBJECT__REFERENCED_OBJECT:
				setReferencedObject((TestDataObject) null);
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
			case TestingPackage.TEST_DATA_OBJECT__MAPPING_OBJECT:
				return mappingObject != null;
			case TestingPackage.TEST_DATA_OBJECT__ATTRIBUTES:
				return attributes != null && !attributes.isEmpty();
			case TestingPackage.TEST_DATA_OBJECT__REFERENCED_OBJECT:
				return referencedObject != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getPKAttribute()
	 * @generated not
	 */
	@Override
	public TestDataAttribute getPKAttribute() {
		if (getMappingObject() == null)
			return null;

		final DomainAttribute pkAttr = getMappingObject().getDomainObject().getPKAttribute();

		return getAttributes().stream()
				.filter(a -> a.getMappingAttribute() != null && a.getMappingAttribute().getAssociation() == null
						&& a.getMappingAttribute().getDomainAttribute() != null
						&& a.getMappingAttribute().getDomainAttribute().equals(pkAttr))
				.findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getDisplayAttribute()
	 * @generated not
	 */
	@Override
	public TestDataAttribute getDisplayAttribute() {
		if (getMappingObject() == null)
			return null;

		final DomainAttribute displayAttr = getMappingObject().getDomainObject().getDisplayAttribute();

		if (displayAttr == null)
			return null;

		return getAttributes().stream()
				.filter(a -> a.getMappingAttribute() != null && a.getMappingAttribute().getAssociation() == null
						&& a.getMappingAttribute().getDomainAttribute() != null
						&& a.getMappingAttribute().getDomainAttribute().equals(displayAttr))
				.findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestDataObject#getAttributeByName(java.lang.String)
	 * @generated not
	 */
	@Override
	public TestDataAttribute getAttributeByName(String name) {
		return getAttributes().stream().filter(a -> a.getName().equals(name)).findFirst().orElse(null);
	}

}
