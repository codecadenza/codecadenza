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

import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Inheritance</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainInheritanceImpl#getSource <em>Source</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainInheritanceImpl#getTarget <em>Target</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DomainInheritanceImpl extends EObjectImpl implements DomainInheritance {
	/**
	 * The cached value of the '{@link #getTarget() <em>Target</em>}' reference
	 * @see #getTarget()
	 * @generated
	 * @ordered
	 */
	protected DomainObject target;

	/**
	 * @generated
	 */
	protected DomainInheritanceImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.DOMAIN_INHERITANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#getSource()
	 * @generated
	 */
	@Override
	public DomainObject getSource() {
		if (eContainerFeatureID() != DomainPackage.DOMAIN_INHERITANCE__SOURCE)
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
		msgs = eBasicSetContainer((InternalEObject) newSource, DomainPackage.DOMAIN_INHERITANCE__SOURCE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#setSource(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setSource(DomainObject newSource) {
		if (newSource != eInternalContainer()
				|| (eContainerFeatureID() != DomainPackage.DOMAIN_INHERITANCE__SOURCE && newSource != null)) {
			if (EcoreUtil.isAncestor(this, newSource))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newSource != null)
				msgs = ((InternalEObject) newSource).eInverseAdd(this, DomainPackage.DOMAIN_OBJECT__INHERITANCE, DomainObject.class,
						msgs);

			msgs = basicSetSource(newSource, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_INHERITANCE__SOURCE, newSource, newSource));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#getTarget()
	 * @generated
	 */
	@Override
	public DomainObject getTarget() {
		if (target != null && target.eIsProxy()) {
			final var oldTarget = (InternalEObject) target;
			target = (DomainObject) eResolveProxy(oldTarget);

			if (target != oldTarget && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.DOMAIN_INHERITANCE__TARGET, oldTarget, target));
		}

		return target;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainObject basicGetTarget() {
		return target;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#setTarget(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setTarget(DomainObject newTarget) {
		final DomainObject oldTarget = target;
		target = newTarget;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_INHERITANCE__TARGET, oldTarget, target));
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
				return eInternalContainer().eInverseRemove(this, DomainPackage.DOMAIN_OBJECT__INHERITANCE, DomainObject.class, msgs);
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
				return getSource();
			case DomainPackage.DOMAIN_INHERITANCE__TARGET:
				if (resolve)
					return getTarget();

				return basicGetTarget();
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
				setSource((DomainObject) newValue);
				return;
			case DomainPackage.DOMAIN_INHERITANCE__TARGET:
				setTarget((DomainObject) newValue);
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
				setSource((DomainObject) null);
				return;
			case DomainPackage.DOMAIN_INHERITANCE__TARGET:
				setTarget((DomainObject) null);
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
			case DomainPackage.DOMAIN_INHERITANCE__SOURCE:
				return getSource() != null;
			case DomainPackage.DOMAIN_INHERITANCE__TARGET:
				return target != null;
		}

		return super.eIsSet(featureID);
	}

}
