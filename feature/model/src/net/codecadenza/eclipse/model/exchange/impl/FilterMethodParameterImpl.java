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
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.java.impl.MethodParameterImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * An implementation of the model object '<em><b>Filter Method Parameter</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl#getDomainAttribute <em>Domain
 * Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl#getAssociationList <em>Association
 * List</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl#getOperator <em>Operator</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FilterMethodParameterImpl extends MethodParameterImpl implements FilterMethodParameter {
	/**
	 * The cached value of the '{@link #getDomainAttribute() <em>Domain Attribute</em>}' reference
	 * @see #getDomainAttribute()
	 * @generated
	 * @ordered
	 */
	protected DomainAttribute domainAttribute;

	/**
	 * The cached value of the '{@link #getAssociation() <em>Association</em>}' reference
	 * @see #getAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation association;

	/**
	 * The cached value of the '{@link #getAssociationList() <em>Association List</em>}' reference list.
	 * @see #getAssociationList()
	 * @generated
	 * @ordered
	 */
	protected EList<AbstractDomainAssociation> associationList;

	/**
	 * The default value of the '{@link #getOperator() <em>Operator</em>}' attribute
	 * @see #getOperator()
	 * @generated
	 * @ordered
	 */
	protected static final String OPERATOR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getOperator() <em>Operator</em>}' attribute
	 * @see #getOperator()
	 * @generated
	 * @ordered
	 */
	protected String operator = OPERATOR_EDEFAULT;

	/**
	 * @generated
	 */
	protected FilterMethodParameterImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.FILTER_METHOD_PARAMETER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getDomainAttribute()
	 * @generated
	 */
	@Override
	public DomainAttribute getDomainAttribute() {
		if (domainAttribute != null && domainAttribute.eIsProxy()) {
			final var oldDomainAttribute = (InternalEObject) domainAttribute;
			domainAttribute = (DomainAttribute) eResolveProxy(oldDomainAttribute);

			if (domainAttribute != oldDomainAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE,
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
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#setDomainAttribute(net.codecadenza.eclipse.model.domain.
	 * DomainAttribute)
	 * @generated
	 */
	@Override
	public void setDomainAttribute(DomainAttribute newDomainAttribute) {
		final DomainAttribute oldDomainAttribute = domainAttribute;
		domainAttribute = newDomainAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE,
					oldDomainAttribute, domainAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION,
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
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION, oldAssociation,
					association));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociationList()
	 * @generated
	 */
	@Override
	public EList<AbstractDomainAssociation> getAssociationList() {
		if (associationList == null)
			associationList = new EObjectResolvingEList<>(AbstractDomainAssociation.class, this,
					ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION_LIST);

		return associationList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getOperator()
	 * @generated
	 */
	@Override
	public String getOperator() {
		return operator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#setOperator(java.lang.String)
	 * @generated
	 */
	@Override
	public void setOperator(String newOperator) {
		final String oldOperator = operator;
		operator = newOperator;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILTER_METHOD_PARAMETER__OPERATOR, oldOperator,
					operator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE:
				if (resolve)
					return getDomainAttribute();

				return basicGetDomainAttribute();
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION_LIST:
				return getAssociationList();
			case ExchangePackage.FILTER_METHOD_PARAMETER__OPERATOR:
				return getOperator();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) newValue);
				return;
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
				return;
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION_LIST:
				getAssociationList().clear();
				getAssociationList().addAll((Collection<? extends AbstractDomainAssociation>) newValue);
				return;
			case ExchangePackage.FILTER_METHOD_PARAMETER__OPERATOR:
				setOperator((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) null);
				return;
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
				return;
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION_LIST:
				getAssociationList().clear();
				return;
			case ExchangePackage.FILTER_METHOD_PARAMETER__OPERATOR:
				setOperator(OPERATOR_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE:
				return domainAttribute != null;
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION:
				return association != null;
			case ExchangePackage.FILTER_METHOD_PARAMETER__ASSOCIATION_LIST:
				return associationList != null && !associationList.isEmpty();
			case ExchangePackage.FILTER_METHOD_PARAMETER__OPERATOR:
				return operator != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (operator: ");
		result.append(operator);
		result.append(')');

		return result.toString();
	}

}
