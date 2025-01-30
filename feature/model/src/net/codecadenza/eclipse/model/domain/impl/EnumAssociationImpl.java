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

import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.java.JavaEnum;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Enum Association</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.EnumAssociationImpl#getSource <em>Source</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.EnumAssociationImpl#getTarget <em>Target</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class EnumAssociationImpl extends EObjectImpl implements EnumAssociation {
	/**
	 * The cached value of the '{@link #getTarget() <em>Target</em>}' reference
	 * @see #getTarget()
	 * @generated
	 * @ordered
	 */
	protected JavaEnum target;

	/**
	 * The cached value of the '{@link #getDomainAttribute() <em>Domain Attribute</em>}' reference
	 * @see #getDomainAttribute()
	 * @generated
	 * @ordered
	 */
	protected DomainAttribute domainAttribute;

	/**
	 * @generated
	 */
	protected EnumAssociationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.ENUM_ASSOCIATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getSource()
	 * @generated
	 */
	@Override
	public DomainObject getSource() {
		if (eContainerFeatureID() != DomainPackage.ENUM_ASSOCIATION__SOURCE)
			return null;

		return (DomainObject) eInternalContainer();
	}

	/**
	 * @param newSource
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetSource(DomainObject newSource, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newSource, DomainPackage.ENUM_ASSOCIATION__SOURCE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#setSource(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setSource(DomainObject newSource) {
		if (newSource != eInternalContainer()
				|| (eContainerFeatureID() != DomainPackage.ENUM_ASSOCIATION__SOURCE && newSource != null)) {
			if (EcoreUtil.isAncestor(this, newSource))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newSource != null)
				msgs = ((InternalEObject) newSource).eInverseAdd(this, DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS, DomainObject.class,
						msgs);

			msgs = basicSetSource(newSource, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ENUM_ASSOCIATION__SOURCE, newSource, newSource));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getTarget()
	 * @generated
	 */
	@Override
	public JavaEnum getTarget() {
		if (target != null && target.eIsProxy()) {
			final var oldTarget = (InternalEObject) target;
			target = (JavaEnum) eResolveProxy(oldTarget);

			if (target != oldTarget && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.ENUM_ASSOCIATION__TARGET, oldTarget, target));
		}

		return target;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaEnum basicGetTarget() {
		return target;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#setTarget(net.codecadenza.eclipse.model.java.JavaEnum)
	 * @generated
	 */
	@Override
	public void setTarget(JavaEnum newTarget) {
		final JavaEnum oldTarget = target;
		target = newTarget;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ENUM_ASSOCIATION__TARGET, oldTarget, target));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getDomainAttribute()
	 * @generated
	 */
	@Override
	public DomainAttribute getDomainAttribute() {
		if (domainAttribute != null && domainAttribute.eIsProxy()) {
			final InternalEObject oldDomainAttribute = (InternalEObject) domainAttribute;

			domainAttribute = (DomainAttribute) eResolveProxy(oldDomainAttribute);

			if ((domainAttribute != oldDomainAttribute) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE,
						oldDomainAttribute, domainAttribute));
		}

		return domainAttribute;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainAttribute basicGetDomainAttribute() {
		return domainAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#
	 * setDomainAttribute(net.codecadenza.eclipse.model.domain.DomainAttribute)
	 * @generated
	 */
	@Override
	public void setDomainAttribute(DomainAttribute newDomainAttribute) {
		final DomainAttribute oldDomainAttribute = domainAttribute;
		domainAttribute = newDomainAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE, oldDomainAttribute,
					domainAttribute));
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetSource((DomainObject) otherEnd, msgs);
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				return basicSetSource(null, msgs);
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				return eInternalContainer().eInverseRemove(this, DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS, DomainObject.class,
						msgs);
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				return getSource();
			case DomainPackage.ENUM_ASSOCIATION__TARGET:
				if (resolve)
					return getTarget();

				return basicGetTarget();
			case DomainPackage.ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE:
				if (resolve)
					return getDomainAttribute();

				return basicGetDomainAttribute();
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				setSource((DomainObject) newValue);
				return;
			case DomainPackage.ENUM_ASSOCIATION__TARGET:
				setTarget((JavaEnum) newValue);
				return;
			case DomainPackage.ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) newValue);
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				setSource((DomainObject) null);
				return;
			case DomainPackage.ENUM_ASSOCIATION__TARGET:
				setTarget((JavaEnum) null);
				return;
			case DomainPackage.ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) null);
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
			case DomainPackage.ENUM_ASSOCIATION__SOURCE:
				return getSource() != null;
			case DomainPackage.ENUM_ASSOCIATION__TARGET:
				return target != null;
			case DomainPackage.ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE:
				return domainAttribute != null;
		}

		return super.eIsSet(featureID);
	}

}
