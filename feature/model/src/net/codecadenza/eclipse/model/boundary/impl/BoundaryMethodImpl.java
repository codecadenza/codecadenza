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
package net.codecadenza.eclipse.model.boundary.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Boundary Method</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl#getBoundaryBean <em>Boundary Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl#getMethodType <em>Method Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl#getDomainAttribute <em>Domain Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl#getDataFetchType <em>Data Fetch Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryMethodImpl#getServiceMethod <em>Service Method</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class BoundaryMethodImpl extends ServiceMethodImpl implements BoundaryMethod {
	/**
	 * A list of all boundary method types that should not be considered when generating a facade
	 * @generated not
	 */
	private static final List<BoundaryMethodTypeEnumeration> VIRTUAL_METHOD_TYPES = Arrays.asList(
			BoundaryMethodTypeEnumeration.COUNT_ALL, BoundaryMethodTypeEnumeration.DELETE_ALL,
			BoundaryMethodTypeEnumeration.EXISTS_BY_ID, BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY,
			BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID, BoundaryMethodTypeEnumeration.FIND_EXISTING,
			BoundaryMethodTypeEnumeration.FIND_BY_ID, BoundaryMethodTypeEnumeration.FIND_BY_OBJECT,
			BoundaryMethodTypeEnumeration.GET_ASSOCIATION, BoundaryMethodTypeEnumeration.SERVICE_CALL,
			BoundaryMethodTypeEnumeration.SAVE);

	/**
	 * A list of all boundary method types where the actual DTO return type should be used when generating a facade
	 * @generated not
	 */
	private static final List<BoundaryMethodTypeEnumeration> USE_DTO_METHOD_TYPES = Arrays.asList(
			BoundaryMethodTypeEnumeration.SEARCH, BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES,
			BoundaryMethodTypeEnumeration.LOG_ON, BoundaryMethodTypeEnumeration.FIND_ALL, BoundaryMethodTypeEnumeration.FIND_BY_PARENT,
			BoundaryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY, BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY);

	/**
	 * The default value of the '{@link #getMethodType() <em>Method Type</em>}' attribute
	 * @see #getMethodType()
	 * @generated
	 * @ordered
	 */
	protected static final BoundaryMethodTypeEnumeration METHOD_TYPE_EDEFAULT = BoundaryMethodTypeEnumeration.DELETE;

	/**
	 * The cached value of the '{@link #getMethodType() <em>Method Type</em>}' attribute
	 * @see #getMethodType()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethodTypeEnumeration methodType = METHOD_TYPE_EDEFAULT;

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
	 * The default value of the '{@link #getDataFetchType() <em>Data Fetch Type</em>}' attribute
	 * @see #getDataFetchType()
	 * @generated
	 * @ordered
	 */
	protected static final BoundaryMethodDataFetchType DATA_FETCH_TYPE_EDEFAULT = BoundaryMethodDataFetchType.DEFAULT;

	/**
	 * The cached value of the '{@link #getDataFetchType() <em>Data Fetch Type</em>}' attribute
	 * @see #getDataFetchType()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethodDataFetchType dataFetchType = DATA_FETCH_TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getServiceMethod() <em>Service Method</em>}' reference
	 * @see #getServiceMethod()
	 * @generated
	 * @ordered
	 */
	protected ServiceMethod serviceMethod;

	/**
	 * @generated
	 */
	protected BoundaryMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return BoundaryPackage.Literals.BOUNDARY_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean()
	 * @generated
	 */
	@Override
	public BoundaryBean getBoundaryBean() {
		if (eContainerFeatureID() != BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN)
			return null;

		return (BoundaryBean) eInternalContainer();
	}

	/**
	 * @param newBoundaryBean
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetBoundaryBean(BoundaryBean newBoundaryBean, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newBoundaryBean, BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#setBoundaryBean(net.codecadenza.eclipse.model.boundary.BoundaryBean)
	 * @generated
	 */
	@Override
	public void setBoundaryBean(BoundaryBean newBoundaryBean) {
		if (newBoundaryBean != eInternalContainer()
				|| (eContainerFeatureID() != BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN && newBoundaryBean != null)) {
			if (EcoreUtil.isAncestor(this, newBoundaryBean))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newBoundaryBean != null)
				msgs = ((InternalEObject) newBoundaryBean).eInverseAdd(this, BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS,
						BoundaryBean.class, msgs);

			msgs = basicSetBoundaryBean(newBoundaryBean, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN, newBoundaryBean,
					newBoundaryBean));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getMethodType()
	 * @generated
	 */
	@Override
	public BoundaryMethodTypeEnumeration getMethodType() {
		return methodType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#setMethodType(net.codecadenza.eclipse.model.boundary.
	 * BoundaryMethodTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setMethodType(BoundaryMethodTypeEnumeration newMethodType) {
		final BoundaryMethodTypeEnumeration oldMethodType = methodType;
		methodType = newMethodType == null ? METHOD_TYPE_EDEFAULT : newMethodType;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_METHOD__METHOD_TYPE, oldMethodType, methodType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDomainAttribute()
	 * @generated
	 */
	@Override
	public DomainAttribute getDomainAttribute() {
		if (domainAttribute != null && domainAttribute.eIsProxy()) {
			final var oldDomainAttribute = (InternalEObject) domainAttribute;
			domainAttribute = (DomainAttribute) eResolveProxy(oldDomainAttribute);

			if (domainAttribute != oldDomainAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, BoundaryPackage.BOUNDARY_METHOD__DOMAIN_ATTRIBUTE,
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
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#setDomainAttribute(net.codecadenza.eclipse.model.domain.
	 * DomainAttribute)
	 * @generated
	 */
	@Override
	public void setDomainAttribute(DomainAttribute newDomainAttribute) {
		final DomainAttribute oldDomainAttribute = domainAttribute;
		domainAttribute = newDomainAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_METHOD__DOMAIN_ATTRIBUTE, oldDomainAttribute,
					domainAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, BoundaryPackage.BOUNDARY_METHOD__ASSOCIATION, oldAssociation,
						association));
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
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_METHOD__ASSOCIATION, oldAssociation,
					association));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDataFetchType()
	 * @generated
	 */
	@Override
	public BoundaryMethodDataFetchType getDataFetchType() {
		return dataFetchType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#setDataFetchType(net.codecadenza.eclipse.model.boundary.
	 * BoundaryMethodDataFetchType)
	 * @generated
	 */
	@Override
	public void setDataFetchType(BoundaryMethodDataFetchType newDataFetchType) {
		final BoundaryMethodDataFetchType oldDataFetchType = dataFetchType;
		dataFetchType = newDataFetchType == null ? DATA_FETCH_TYPE_EDEFAULT : newDataFetchType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_METHOD__DATA_FETCH_TYPE, oldDataFetchType,
					dataFetchType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getServiceMethod()
	 * @generated
	 */
	@Override
	public ServiceMethod getServiceMethod() {
		if (serviceMethod != null && serviceMethod.eIsProxy()) {
			final var oldServiceMethod = (InternalEObject) serviceMethod;
			serviceMethod = (ServiceMethod) eResolveProxy(oldServiceMethod);

			if (serviceMethod != oldServiceMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, BoundaryPackage.BOUNDARY_METHOD__SERVICE_METHOD,
						oldServiceMethod, serviceMethod));
		}

		return serviceMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public ServiceMethod basicGetServiceMethod() {
		return serviceMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#setServiceMethod(net.codecadenza.eclipse.model.service.
	 * ServiceMethod)
	 * @generated
	 */
	@Override
	public void setServiceMethod(ServiceMethod newServiceMethod) {
		final ServiceMethod oldServiceMethod = serviceMethod;
		serviceMethod = newServiceMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_METHOD__SERVICE_METHOD, oldServiceMethod,
					serviceMethod));
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetBoundaryBean((BoundaryBean) otherEnd, msgs);
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				return basicSetBoundaryBean(null, msgs);
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				return eInternalContainer().eInverseRemove(this, BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS, BoundaryBean.class,
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				return getBoundaryBean();
			case BoundaryPackage.BOUNDARY_METHOD__METHOD_TYPE:
				return getMethodType();
			case BoundaryPackage.BOUNDARY_METHOD__DOMAIN_ATTRIBUTE:
				if (resolve)
					return getDomainAttribute();

				return basicGetDomainAttribute();
			case BoundaryPackage.BOUNDARY_METHOD__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
			case BoundaryPackage.BOUNDARY_METHOD__DATA_FETCH_TYPE:
				return getDataFetchType();
			case BoundaryPackage.BOUNDARY_METHOD__SERVICE_METHOD:
				if (resolve)
					return getServiceMethod();

				return basicGetServiceMethod();
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				setBoundaryBean((BoundaryBean) newValue);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__METHOD_TYPE:
				setMethodType((BoundaryMethodTypeEnumeration) newValue);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) newValue);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__DATA_FETCH_TYPE:
				setDataFetchType((BoundaryMethodDataFetchType) newValue);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__SERVICE_METHOD:
				setServiceMethod((ServiceMethod) newValue);
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				setBoundaryBean((BoundaryBean) null);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__METHOD_TYPE:
				setMethodType(METHOD_TYPE_EDEFAULT);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) null);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__DATA_FETCH_TYPE:
				setDataFetchType(DATA_FETCH_TYPE_EDEFAULT);
				return;
			case BoundaryPackage.BOUNDARY_METHOD__SERVICE_METHOD:
				setServiceMethod((ServiceMethod) null);
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
			case BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN:
				return getBoundaryBean() != null;
			case BoundaryPackage.BOUNDARY_METHOD__METHOD_TYPE:
				return methodType != METHOD_TYPE_EDEFAULT;
			case BoundaryPackage.BOUNDARY_METHOD__DOMAIN_ATTRIBUTE:
				return domainAttribute != null;
			case BoundaryPackage.BOUNDARY_METHOD__ASSOCIATION:
				return association != null;
			case BoundaryPackage.BOUNDARY_METHOD__DATA_FETCH_TYPE:
				return dataFetchType != DATA_FETCH_TYPE_EDEFAULT;
			case BoundaryPackage.BOUNDARY_METHOD__SERVICE_METHOD:
				return serviceMethod != null;
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
		result.append(", dataFetchType: ");
		result.append(dataFetchType);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#addUniqueCheck()
	 * @generated not
	 */
	@Override
	public boolean addUniqueCheck() {
		if (!(serviceMethod instanceof final RepositoryMethod repositoryMethod))
			return false;

		if (getMethodType() != BoundaryMethodTypeEnumeration.CREATE && getMethodType() != BoundaryMethodTypeEnumeration.COPY
				&& getMethodType() != BoundaryMethodTypeEnumeration.CHANGE_PARENT
				&& getMethodType() != BoundaryMethodTypeEnumeration.UPDATE)
			return false;

		for (final RepositoryMethod m : getBoundaryBean().getRepository().getRepositoryMethods()) {
			final RepositoryMethodTypeEnumeration repositoryMethodType = repositoryMethod.getMethodType();

			if (repositoryMethodType == RepositoryMethodTypeEnumeration.PERSIST
					&& m.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY)
				return true;

			if (repositoryMethodType == RepositoryMethodTypeEnumeration.MERGE
					&& m.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
				return true;

			if (repositoryMethodType == RepositoryMethodTypeEnumeration.CHANGE_PARENT
					&& m.getMethodType() == RepositoryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
				return true;
		}

		if (getMethodType() == BoundaryMethodTypeEnumeration.COPY) {
			final RepositoryMethod persistMethod = getBoundaryBean().getRepository()
					.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST);

			if (persistMethod != null && persistMethod.addUniqueCheck())
				return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#isUsedByIntegrationMethod()
	 * @generated not
	 */
	@Override
	public boolean isUsedByIntegrationMethod() {
		for (final IntegrationModule module : this.getBoundaryBean().getNamespace().getProject().getIntegrationModules())
			for (final JavaType type : module.getNamespace().getJavaTypes()) {
				final var integrationBean = (AbstractIntegrationBean) type;

				if (integrationBean.getMethods().stream().anyMatch(e -> e.getBoundaryMethod().equals(this)))
					return true;
			}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#useDTOReturnType()
	 * @generated not
	 */
	@Override
	public boolean useDTOReturnType() {
		final Project project = getBoundaryBean().getNamespace().getProject();

		if (project.isBoundaryMode() || !(getReturnType() instanceof DTOBean))
			return false;

		return USE_DTO_METHOD_TYPES.contains(methodType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#isVirtual()
	 * @generated not
	 */
	@Override
	public boolean isVirtual() {
		if (getBoundaryBean().getNamespace().getProject().isBoundaryMode())
			return false;

		return VIRTUAL_METHOD_TYPES.contains(methodType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getSearchMethod()
	 * @generated not
	 */
	@Override
	public BoundaryMethod getSearchMethod() {
		if (getMethodType() != BoundaryMethodTypeEnumeration.COUNT || getQueryStatement() == null)
			return null;

		// Extract all 'SEARCH' methods
		final Stream<BoundaryMethod> methodStream = getBoundaryBean().getBoundaryMethods().stream();
		final List<BoundaryMethod> searchMethods = methodStream.filter(e -> e.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH)
				.toList();

		// A corresponding 'SEARCH' method has the same query statement!
		final Optional<BoundaryMethod> boundaryMethod = searchMethods.stream()
				.filter(e -> getQueryStatement().trim().equals(e.getQueryStatement().trim())).findFirst();

		if (boundaryMethod.isPresent())
			return boundaryMethod.get();

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getAdditionalFilterAttribute()
	 * @generated not
	 */
	@Override
	public DomainAttribute getAdditionalFilterAttribute() {
		if (getDataFetchType() == BoundaryMethodDataFetchType.CLIENT)
			return getBoundaryBean().getNamespace().getProject().getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
		else if (getDataFetchType() == BoundaryMethodDataFetchType.USER)
			return getBoundaryBean().getNamespace().getProject().getDomainObjectByTag(DomainTagEnumeration.USER).getPKAttribute();

		if ((getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER
				|| getMethodType() == BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES)
				&& getBoundaryBean().getDomainObject().isMandated())
			return getBoundaryBean().getNamespace().getProject().getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();

		if (getMethodType() == BoundaryMethodTypeEnumeration.COPY) {
			final var repositoryMethod = (RepositoryMethod) getServiceMethod();

			if (repositoryMethod.addUserParam())
				return getBoundaryBean().getNamespace().getProject().getApplicationLogOnDTO().getPKAttribute().getDomainAttribute();
		}

		return null;
	}

}
