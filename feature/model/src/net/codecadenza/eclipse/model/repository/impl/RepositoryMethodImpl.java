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

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Repository Method</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl#getRepository <em>Repository</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl#getMethodType <em>Method Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.repository.impl.RepositoryMethodImpl#getHint <em>Hint</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class RepositoryMethodImpl extends ServiceMethodImpl implements RepositoryMethod {
	/**
	 * The default value of the '{@link #getMethodType() <em>Method Type</em>}' attribute
	 * @see #getMethodType()
	 * @generated
	 * @ordered
	 */
	protected static final RepositoryMethodTypeEnumeration METHOD_TYPE_EDEFAULT = RepositoryMethodTypeEnumeration.FIND_ALL;

	/**
	 * The cached value of the '{@link #getMethodType() <em>Method Type</em>}' attribute
	 * @see #getMethodType()
	 * @generated
	 * @ordered
	 */
	protected RepositoryMethodTypeEnumeration methodType = METHOD_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getHint() <em>Hint</em>}' attribute
	 * @see #getHint()
	 * @generated
	 * @ordered
	 */
	protected static final String HINT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getHint() <em>Hint</em>}' attribute
	 * @see #getHint()
	 * @generated
	 * @ordered
	 */
	protected String hint = HINT_EDEFAULT;

	/**
	 * @generated
	 */
	protected RepositoryMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RepositoryPackage.Literals.REPOSITORY_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository()
	 * @generated
	 */
	@Override
	public Repository getRepository() {
		if (eContainerFeatureID() != RepositoryPackage.REPOSITORY_METHOD__REPOSITORY)
			return null;

		return (Repository) eInternalContainer();
	}

	/**
	 * @param newRepository
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetRepository(Repository newRepository, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newRepository, RepositoryPackage.REPOSITORY_METHOD__REPOSITORY, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#setRepository(net.codecadenza.eclipse.model.repository.
	 * Repository)
	 * @generated
	 */
	@Override
	public void setRepository(Repository newRepository) {
		if (newRepository != eInternalContainer()
				|| (eContainerFeatureID() != RepositoryPackage.REPOSITORY_METHOD__REPOSITORY && newRepository != null)) {
			if (EcoreUtil.isAncestor(this, newRepository))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newRepository != null)
				msgs = ((InternalEObject) newRepository).eInverseAdd(this, RepositoryPackage.REPOSITORY__REPOSITORY_METHODS,
						Repository.class, msgs);

			msgs = basicSetRepository(newRepository, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RepositoryPackage.REPOSITORY_METHOD__REPOSITORY, newRepository,
					newRepository));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getMethodType()
	 * @generated
	 */
	@Override
	public RepositoryMethodTypeEnumeration getMethodType() {
		return methodType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#setMethodType(net.codecadenza.eclipse.model.repository.
	 * RepositoryMethodTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setMethodType(RepositoryMethodTypeEnumeration newMethodType) {
		final RepositoryMethodTypeEnumeration oldMethodType = methodType;
		methodType = newMethodType == null ? METHOD_TYPE_EDEFAULT : newMethodType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RepositoryPackage.REPOSITORY_METHOD__METHOD_TYPE, oldMethodType,
					methodType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getHint()
	 * @generated
	 */
	@Override
	public String getHint() {
		return hint;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#setHint(java.lang.String)
	 * @generated
	 */
	@Override
	public void setHint(String newHint) {
		final String oldHint = hint;
		hint = newHint;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RepositoryPackage.REPOSITORY_METHOD__HINT, oldHint, hint));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetRepository((Repository) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				return basicSetRepository(null, msgs);
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
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				return eInternalContainer().eInverseRemove(this, RepositoryPackage.REPOSITORY__REPOSITORY_METHODS, Repository.class,
						msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				return getRepository();
			case RepositoryPackage.REPOSITORY_METHOD__METHOD_TYPE:
				return getMethodType();
			case RepositoryPackage.REPOSITORY_METHOD__HINT:
				return getHint();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				setRepository((Repository) newValue);
				return;
			case RepositoryPackage.REPOSITORY_METHOD__METHOD_TYPE:
				setMethodType((RepositoryMethodTypeEnumeration) newValue);
				return;
			case RepositoryPackage.REPOSITORY_METHOD__HINT:
				setHint((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				setRepository((Repository) null);
				return;
			case RepositoryPackage.REPOSITORY_METHOD__METHOD_TYPE:
				setMethodType(METHOD_TYPE_EDEFAULT);
				return;
			case RepositoryPackage.REPOSITORY_METHOD__HINT:
				setHint(HINT_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case RepositoryPackage.REPOSITORY_METHOD__REPOSITORY:
				return getRepository() != null;
			case RepositoryPackage.REPOSITORY_METHOD__METHOD_TYPE:
				return methodType != METHOD_TYPE_EDEFAULT;
			case RepositoryPackage.REPOSITORY_METHOD__HINT:
				return hint != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (methodType: ");
		result.append(methodType);
		result.append(", hint: ");
		result.append(hint);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#addUniqueCheck()
	 * @generated not
	 */
	@Override
	public boolean addUniqueCheck() {
		if (getMethodType() != RepositoryMethodTypeEnumeration.PERSIST && getMethodType() != RepositoryMethodTypeEnumeration.COPY
				&& getMethodType() != RepositoryMethodTypeEnumeration.CHANGE_PARENT
				&& getMethodType() != RepositoryMethodTypeEnumeration.MERGE)
			return false;

		for (final RepositoryMethod m : getRepository().getRepositoryMethods()) {
			if (getMethodType() == RepositoryMethodTypeEnumeration.PERSIST
					&& m.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY)
				return true;

			if (getMethodType() == RepositoryMethodTypeEnumeration.MERGE
					&& m.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
				return true;
		}

		if (getMethodType() == RepositoryMethodTypeEnumeration.COPY) {
			final RepositoryMethod persistMethod = getRepository().getMethodByType(RepositoryMethodTypeEnumeration.PERSIST);

			if (persistMethod != null && persistMethod.addUniqueCheck())
				return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#addUserParam()
	 * @generated not
	 */
	@Override
	public boolean addUserParam() {
		final Project project = getRepository().getNamespace().getProject();
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		if (getMethodType() != RepositoryMethodTypeEnumeration.COPY)
			return false;

		return logOnDTO != null && logOnBoundary != null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#isGenerationOmitted()
	 * @generated not
	 */
	@Override
	public boolean isGenerationOmitted() {
		// It makes no sense to generate PERSIST and MERGE methods that don't perform unique key checks!
		if ((getMethodType() == RepositoryMethodTypeEnumeration.PERSIST || getMethodType() == RepositoryMethodTypeEnumeration.MERGE)
				&& !addUniqueCheck())
			return true;

		return getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_ID
				|| getMethodType() == RepositoryMethodTypeEnumeration.COUNT
				|| getMethodType() == RepositoryMethodTypeEnumeration.COUNT_ALL
				|| getMethodType() == RepositoryMethodTypeEnumeration.FIND_ALL
				|| getMethodType() == RepositoryMethodTypeEnumeration.FIND_BY_ID
				|| getMethodType() == RepositoryMethodTypeEnumeration.FIND_EXISTING
				|| getMethodType() == RepositoryMethodTypeEnumeration.DELETE
				|| getMethodType() == RepositoryMethodTypeEnumeration.DELETE_ALL
				|| getMethodType() == RepositoryMethodTypeEnumeration.SEARCH;
	}

}
