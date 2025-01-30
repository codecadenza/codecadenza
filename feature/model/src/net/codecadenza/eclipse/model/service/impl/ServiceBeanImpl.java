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
package net.codecadenza.eclipse.model.service.impl;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.impl.JavaTypeImpl;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Service Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#getInterfaceName <em>Interface Name</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public abstract class ServiceBeanImpl extends JavaTypeImpl implements ServiceBean {
	/**
	 * The cached value of the '{@link #getDomainObject() <em>Domain Object</em>}' reference
	 * @see #getDomainObject()
	 * @generated
	 * @ordered
	 */
	protected DomainObject domainObject;

	/**
	 * The default value of the '{@link #getInterfaceName() <em>Interface Name</em>}' attribute
	 * @see #getInterfaceName()
	 * @generated
	 * @ordered
	 */
	protected static final String INTERFACE_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getInterfaceName() <em>Interface Name</em>}' attribute
	 * @see #getInterfaceName()
	 * @generated
	 * @ordered
	 */
	protected String interfaceName = INTERFACE_NAME_EDEFAULT;

	/**
	 * @generated
	 */
	protected ServiceBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ServicePackage.Literals.SERVICE_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceBean#getDomainObject()
	 * @generated
	 */
	@Override
	public DomainObject getDomainObject() {
		if (domainObject != null && domainObject.eIsProxy()) {
			final var oldDomainObject = (InternalEObject) domainObject;
			domainObject = (DomainObject) eResolveProxy(oldDomainObject);

			if (domainObject != oldDomainObject && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT, oldDomainObject,
						domainObject));
		}

		return domainObject;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainObject basicGetDomainObject() {
		return domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceBean#setDomainObject(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setDomainObject(DomainObject newDomainObject) {
		final DomainObject oldDomainObject = domainObject;
		domainObject = newDomainObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT, oldDomainObject,
					domainObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceBean#getInterfaceName()
	 * @generated
	 */
	@Override
	public String getInterfaceName() {
		return interfaceName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceBean#setInterfaceName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setInterfaceName(String newInterfaceName) {
		final String oldInterfaceName = interfaceName;
		interfaceName = newInterfaceName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_BEAN__INTERFACE_NAME, oldInterfaceName,
					interfaceName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT:
				if (resolve)
					return getDomainObject();

				return basicGetDomainObject();
			case ServicePackage.SERVICE_BEAN__INTERFACE_NAME:
				return getInterfaceName();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT:
				setDomainObject((DomainObject) newValue);
				return;
			case ServicePackage.SERVICE_BEAN__INTERFACE_NAME:
				setInterfaceName((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT:
				setDomainObject((DomainObject) null);
				return;
			case ServicePackage.SERVICE_BEAN__INTERFACE_NAME:
				setInterfaceName(INTERFACE_NAME_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT:
				return domainObject != null;
			case ServicePackage.SERVICE_BEAN__INTERFACE_NAME:
				return interfaceName != null;
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
		result.append(" (interfaceName: ");
		result.append(interfaceName);
		result.append(')');

		return result.toString();
	}

}
