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
package net.codecadenza.eclipse.model.repository.impl;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.impl.MethodParameterImpl;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Repository Method Parameter</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodParameterImpl#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodParameterImpl#getAttribute <em>Domain
 * Attribute</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class RepositoryMethodParameterImpl extends MethodParameterImpl implements RepositoryMethodParameter {
	/**
	 * The cached value of the '{@link #getAssociation() <em>Association</em>}' reference
	 * @see #getAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation association;

	/**
	 * The cached value of the '{@link #getAttribute() <em>Domain Attribute</em>}' reference
	 * @see #getAttribute()
	 * @generated
	 * @ordered
	 */
	protected DomainAttribute attribute;

	/**
	 * @generated
	 */
	protected RepositoryMethodParameterImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RepositoryPackage.Literals.REPOSITORY_METHOD_PARAMETER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ASSOCIATION,
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
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ASSOCIATION,
					oldAssociation, association));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#getAttribute()
	 * @generated
	 */
	@Override
	public DomainAttribute getAttribute() {
		if (attribute != null && attribute.eIsProxy()) {
			final var oldAttribute = (InternalEObject) attribute;
			attribute = (DomainAttribute) eResolveProxy(oldAttribute);

			if (attribute != oldAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ATTRIBUTE,
						oldAttribute, attribute));
		}

		return attribute;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainAttribute basicGetAttribute() {
		return attribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodParameter#setAttribute(net.codecadenza.eclipse.model.domain.
	 * DomainAttribute)
	 * @generated
	 */
	@Override
	public void setAttribute(DomainAttribute newAttribute) {
		final DomainAttribute oldAttribute = attribute;
		attribute = newAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ATTRIBUTE,
					oldAttribute, attribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ATTRIBUTE:
				if (resolve)
					return getAttribute();

				return basicGetAttribute();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
				return;
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ATTRIBUTE:
				setAttribute((DomainAttribute) newValue);
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
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
				return;
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ATTRIBUTE:
				setAttribute((DomainAttribute) null);
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
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ASSOCIATION:
				return association != null;
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER__ATTRIBUTE:
				return attribute != null;
		}

		return super.eIsSet(featureID);
	}

}
