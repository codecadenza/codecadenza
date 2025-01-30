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
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * An implementation of the model object '<em><b>Association Controller</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl#getQueryAttributes <em>Query
 * Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl#getPersistAttributes <em>Persist
 * Attributes</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class AssociationControllerImpl extends EObjectImpl implements AssociationController {
	/**
	 * The cached value of the '{@link #getQueryAttributes() <em>Query Attributes</em>}' reference list
	 * @see #getQueryAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<ExchangeMappingAttribute> queryAttributes;

	/**
	 * The cached value of the '{@link #getAssociation() <em>Association</em>}' reference
	 * @see #getAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation association;

	/**
	 * The cached value of the '{@link #getPersistAttributes() <em>Persist Attributes</em>}' reference list
	 * @see #getPersistAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<ExchangeMappingAttribute> persistAttributes;

	/**
	 * @generated
	 */
	protected AssociationControllerImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.ASSOCIATION_CONTROLLER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#getQueryAttributes()
	 * @generated
	 */
	@Override
	public EList<ExchangeMappingAttribute> getQueryAttributes() {
		if (queryAttributes == null)
			queryAttributes = new EObjectResolvingEList<>(ExchangeMappingAttribute.class, this,
					ExchangePackage.ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES);

		return queryAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.ASSOCIATION_CONTROLLER__ASSOCIATION,
						oldAssociation, association));
		}

		return association;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public AbstractDomainAssociation basicGetAssociation() {
		return association;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.ASSOCIATION_CONTROLLER__ASSOCIATION, oldAssociation,
					association));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#getPersistAttributes()
	 * @generated
	 */
	@Override
	public EList<ExchangeMappingAttribute> getPersistAttributes() {
		if (persistAttributes == null)
			persistAttributes = new EObjectResolvingEList<>(ExchangeMappingAttribute.class, this,
					ExchangePackage.ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES);

		return persistAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES:
				return getQueryAttributes();
			case ExchangePackage.ASSOCIATION_CONTROLLER__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
			case ExchangePackage.ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES:
				return getPersistAttributes();
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
			case ExchangePackage.ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES:
				getQueryAttributes().clear();
				getQueryAttributes().addAll((Collection<? extends ExchangeMappingAttribute>) newValue);
				return;
			case ExchangePackage.ASSOCIATION_CONTROLLER__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
				return;
			case ExchangePackage.ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES:
				getPersistAttributes().clear();
				getPersistAttributes().addAll((Collection<? extends ExchangeMappingAttribute>) newValue);
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
			case ExchangePackage.ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES:
				getQueryAttributes().clear();
				return;
			case ExchangePackage.ASSOCIATION_CONTROLLER__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
				return;
			case ExchangePackage.ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES:
				getPersistAttributes().clear();
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
			case ExchangePackage.ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES:
				return queryAttributes != null && !queryAttributes.isEmpty();
			case ExchangePackage.ASSOCIATION_CONTROLLER__ASSOCIATION:
				return association != null;
			case ExchangePackage.ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES:
				return persistAttributes != null && !persistAttributes.isEmpty();
		}

		return super.eIsSet(featureID);
	}

}
