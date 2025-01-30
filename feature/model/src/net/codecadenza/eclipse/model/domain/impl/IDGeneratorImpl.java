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
package net.codecadenza.eclipse.model.domain.impl;

import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>ID Generator</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl#getBlockSize <em>Block Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl#getInitialValue <em>Initial Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl#getGeneratorType <em>Generator Type</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class IDGeneratorImpl extends EObjectImpl implements IDGenerator {
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
	 * The default value of the '{@link #getBlockSize() <em>Block Size</em>}' attribute
	 * @see #getBlockSize()
	 * @generated
	 * @ordered
	 */
	protected static final int BLOCK_SIZE_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getBlockSize() <em>Block Size</em>}' attribute
	 * @see #getBlockSize()
	 * @generated
	 * @ordered
	 */
	protected int blockSize = BLOCK_SIZE_EDEFAULT;

	/**
	 * The default value of the '{@link #getInitialValue() <em>Initial Value</em>}' attribute
	 * @see #getInitialValue()
	 * @generated
	 * @ordered
	 */
	protected static final int INITIAL_VALUE_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getInitialValue() <em>Initial Value</em>}' attribute
	 * @see #getInitialValue()
	 * @generated
	 * @ordered
	 */
	protected int initialValue = INITIAL_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getGeneratorType() <em>Generator Type</em>}' attribute
	 * @see #getGeneratorType()
	 * @generated
	 * @ordered
	 */
	protected static final IDGeneratorTypeEnumeration GENERATOR_TYPE_EDEFAULT = IDGeneratorTypeEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getGeneratorType() <em>Generator Type</em>}' attribute
	 * @see #getGeneratorType()
	 * @generated
	 * @ordered
	 */
	protected IDGeneratorTypeEnumeration generatorType = GENERATOR_TYPE_EDEFAULT;

	/**
	 * @generated
	 */
	protected IDGeneratorImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.ID_GENERATOR;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ID_GENERATOR__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getBlockSize()
	 * @generated
	 */
	@Override
	public int getBlockSize() {
		return blockSize;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#setBlockSize(int)
	 * @generated
	 */
	@Override
	public void setBlockSize(int newBlockSize) {
		final int oldBlockSize = blockSize;
		blockSize = newBlockSize;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ID_GENERATOR__BLOCK_SIZE, oldBlockSize, blockSize));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getInitialValue()
	 * @generated
	 */
	@Override
	public int getInitialValue() {
		return initialValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#setInitialValue(int)
	 * @generated
	 */
	@Override
	public void setInitialValue(int newInitialValue) {
		final int oldInitialValue = initialValue;
		initialValue = newInitialValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ID_GENERATOR__INITIAL_VALUE, oldInitialValue,
					initialValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getGeneratorType()
	 * @generated
	 */
	@Override
	public IDGeneratorTypeEnumeration getGeneratorType() {
		return generatorType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#setGeneratorType(net.codecadenza.eclipse.model.domain.
	 * IDGeneratorTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setGeneratorType(IDGeneratorTypeEnumeration newGeneratorType) {
		final IDGeneratorTypeEnumeration oldGeneratorType = generatorType;
		generatorType = newGeneratorType == null ? GENERATOR_TYPE_EDEFAULT : newGeneratorType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ID_GENERATOR__GENERATOR_TYPE, oldGeneratorType,
					generatorType));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.ID_GENERATOR__NAME:
				return getName();
			case DomainPackage.ID_GENERATOR__BLOCK_SIZE:
				return getBlockSize();
			case DomainPackage.ID_GENERATOR__INITIAL_VALUE:
				return getInitialValue();
			case DomainPackage.ID_GENERATOR__GENERATOR_TYPE:
				return getGeneratorType();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DomainPackage.ID_GENERATOR__NAME:
				setName((String) newValue);
				return;
			case DomainPackage.ID_GENERATOR__BLOCK_SIZE:
				setBlockSize((Integer) newValue);
				return;
			case DomainPackage.ID_GENERATOR__INITIAL_VALUE:
				setInitialValue((Integer) newValue);
				return;
			case DomainPackage.ID_GENERATOR__GENERATOR_TYPE:
				setGeneratorType((IDGeneratorTypeEnumeration) newValue);
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
			case DomainPackage.ID_GENERATOR__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DomainPackage.ID_GENERATOR__BLOCK_SIZE:
				setBlockSize(BLOCK_SIZE_EDEFAULT);
				return;
			case DomainPackage.ID_GENERATOR__INITIAL_VALUE:
				setInitialValue(INITIAL_VALUE_EDEFAULT);
				return;
			case DomainPackage.ID_GENERATOR__GENERATOR_TYPE:
				setGeneratorType(GENERATOR_TYPE_EDEFAULT);
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
			case DomainPackage.ID_GENERATOR__NAME:
				return name != null;
			case DomainPackage.ID_GENERATOR__BLOCK_SIZE:
				return blockSize != BLOCK_SIZE_EDEFAULT;
			case DomainPackage.ID_GENERATOR__INITIAL_VALUE:
				return initialValue != INITIAL_VALUE_EDEFAULT;
			case DomainPackage.ID_GENERATOR__GENERATOR_TYPE:
				return generatorType != GENERATOR_TYPE_EDEFAULT;
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
		result.append(", blockSize: ");
		result.append(blockSize);
		result.append(", initialValue: ");
		result.append(initialValue);
		result.append(", generatorType: ");
		result.append(generatorType);
		result.append(')');

		return result.toString();
	}

}
