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
package net.codecadenza.eclipse.model.client.impl;

import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>Tree Node</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeNodeImpl#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeNodeImpl#getDTOAttribute <em>DTO Attribute</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class TreeNodeImpl extends EObjectImpl implements TreeNode {
	/**
	 * The default value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected String label = LABEL_EDEFAULT;

	/**
	 * The cached value of the '{@link #getDTOAttribute() <em>DTO Attribute</em>}' reference
	 * @see #getDTOAttribute()
	 * @generated
	 * @ordered
	 */
	protected DTOBeanAttribute dTOAttribute;

	/**
	 * @generated
	 */
	protected TreeNodeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.TREE_NODE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeNode#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeNode#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_NODE__LABEL, oldLabel, label));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeNode#getDTOAttribute()
	 * @generated
	 */
	@Override
	public DTOBeanAttribute getDTOAttribute() {
		if (dTOAttribute != null && dTOAttribute.eIsProxy()) {
			final var oldDTOAttribute = (InternalEObject) dTOAttribute;
			dTOAttribute = (DTOBeanAttribute) eResolveProxy(oldDTOAttribute);

			if (dTOAttribute != oldDTOAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_NODE__DTO_ATTRIBUTE, oldDTOAttribute,
						dTOAttribute));
		}

		return dTOAttribute;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DTOBeanAttribute basicGetDTOAttribute() {
		return dTOAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeNode#setDTOAttribute(net.codecadenza.eclipse.model.dto.DTOBeanAttribute)
	 * @generated
	 */
	@Override
	public void setDTOAttribute(DTOBeanAttribute newDTOAttribute) {
		final DTOBeanAttribute oldDTOAttribute = dTOAttribute;
		dTOAttribute = newDTOAttribute;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_NODE__DTO_ATTRIBUTE, oldDTOAttribute, dTOAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ClientPackage.TREE_NODE__LABEL:
				return getLabel();
			case ClientPackage.TREE_NODE__DTO_ATTRIBUTE:
				if (resolve)
					return getDTOAttribute();

				return basicGetDTOAttribute();
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
			case ClientPackage.TREE_NODE__LABEL:
				setLabel((String) newValue);
				return;
			case ClientPackage.TREE_NODE__DTO_ATTRIBUTE:
				setDTOAttribute((DTOBeanAttribute) newValue);
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
			case ClientPackage.TREE_NODE__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case ClientPackage.TREE_NODE__DTO_ATTRIBUTE:
				setDTOAttribute((DTOBeanAttribute) null);
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
			case ClientPackage.TREE_NODE__LABEL:
				return label != null;
			case ClientPackage.TREE_NODE__DTO_ATTRIBUTE:
				return dTOAttribute != null;
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
		result.append(" (label: ");
		result.append(label);
		result.append(')');

		return result.toString();
	}

}
