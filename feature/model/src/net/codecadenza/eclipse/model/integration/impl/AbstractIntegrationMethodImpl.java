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
package net.codecadenza.eclipse.model.integration.impl;

import static net.codecadenza.eclipse.shared.Constants.INTEGRATION_SEARCH_PARAM_TYPE;
import static net.codecadenza.eclipse.shared.Constants.PARAM_LOGGED_ON_USER;
import static net.codecadenza.eclipse.shared.Constants.SEARCH_PARAM_NAME;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Abstract Integration Method</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#getIntegrationBean <em>Integration
 * Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#getBoundaryMethod <em>Boundary
 * Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationMethodImpl#isStartNewThread <em>Start New
 * Thread</em>}</li>
 * </ul>
 * @generated
 */
public class AbstractIntegrationMethodImpl extends ServiceMethodImpl implements AbstractIntegrationMethod {
	private static final String COMMENT_USER = "the id of the user";
	private static final String COMMENT_CLIENT = "the id of the client";
	private static final String COMMENT_SEARCH = "the search configuration object";

	/**
	 * The cached value of the '{@link #getBoundaryMethod() <em>Boundary Method</em>}' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod boundaryMethod;

	/**
	 * The default value of the '{@link #isStartNewThread() <em>Start New Thread</em>}' attribute
	 * @see #isStartNewThread()
	 * @generated
	 * @ordered
	 */
	protected static final boolean START_NEW_THREAD_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isStartNewThread() <em>Start New Thread</em>}' attribute
	 * @see #isStartNewThread()
	 * @generated
	 * @ordered
	 */
	protected boolean startNewThread = START_NEW_THREAD_EDEFAULT;

	/**
	 * @generated
	 */
	protected AbstractIntegrationMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.ABSTRACT_INTEGRATION_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean()
	 * @generated
	 */
	@Override
	public AbstractIntegrationBean getIntegrationBean() {
		if (eContainerFeatureID() != IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN)
			return null;

		return (AbstractIntegrationBean) eInternalContainer();
	}

	/**
	 * @param newIntegrationBean
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetIntegrationBean(AbstractIntegrationBean newIntegrationBean, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newIntegrationBean,
				IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#setIntegrationBean(net.codecadenza.eclipse.model.
	 * integration.AbstractIntegrationBean)
	 * @generated
	 */
	@Override
	public void setIntegrationBean(AbstractIntegrationBean newIntegrationBean) {
		if (newIntegrationBean != eInternalContainer()
				|| (eContainerFeatureID() != IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN
						&& newIntegrationBean != null)) {
			if (EcoreUtil.isAncestor(this, newIntegrationBean))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newIntegrationBean != null)
				msgs = ((InternalEObject) newIntegrationBean).eInverseAdd(this, IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS,
						AbstractIntegrationBean.class, msgs);

			msgs = basicSetIntegrationBean(newIntegrationBean, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN,
					newIntegrationBean, newIntegrationBean));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getBoundaryMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getBoundaryMethod() {
		if (boundaryMethod != null && boundaryMethod.eIsProxy()) {
			final var oldBoundaryMethod = (InternalEObject) boundaryMethod;
			boundaryMethod = (BoundaryMethod) eResolveProxy(oldBoundaryMethod);

			if (boundaryMethod != oldBoundaryMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD,
						oldBoundaryMethod, boundaryMethod));
		}

		return boundaryMethod;
	}

	/**
	 * @return the boundary method
	 * @generated
	 */
	public BoundaryMethod basicGetBoundaryMethod() {
		return boundaryMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#setBoundaryMethod(net.codecadenza.eclipse.model.
	 * boundary.BoundaryMethod)
	 */
	@Override
	public void setBoundaryMethod(BoundaryMethod newBoundaryMethod) {
		final BoundaryMethod oldBoundaryMethod = boundaryMethod;
		boundaryMethod = newBoundaryMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD,
					oldBoundaryMethod, boundaryMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#isStartNewThread()
	 * @generated
	 */
	@Override
	public boolean isStartNewThread() {
		return startNewThread;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#setStartNewThread(boolean)
	 * @generated
	 */
	@Override
	public void setStartNewThread(boolean newStartNewThread) {
		final boolean oldStartNewThread = startNewThread;
		startNewThread = newStartNewThread;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD,
					oldStartNewThread, startNewThread));
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetIntegrationBean((AbstractIntegrationBean) otherEnd, msgs);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				return basicSetIntegrationBean(null, msgs);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				return eInternalContainer().eInverseRemove(this, IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS,
						AbstractIntegrationBean.class, msgs);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				return getIntegrationBean();
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD:
				if (resolve)
					return getBoundaryMethod();

				return basicGetBoundaryMethod();
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD:
				return isStartNewThread();
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				setIntegrationBean((AbstractIntegrationBean) newValue);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) newValue);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD:
				setStartNewThread((Boolean) newValue);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				setIntegrationBean((AbstractIntegrationBean) null);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) null);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD:
				setStartNewThread(START_NEW_THREAD_EDEFAULT);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN:
				return getIntegrationBean() != null;
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD:
				return boundaryMethod != null;
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD:
				return startNewThread != START_NEW_THREAD_EDEFAULT;
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
		result.append(" (startNewThread: ");
		result.append(startNewThread);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationParameters()
	 * @generated not
	 */
	@Override
	public List<IntegrationMethodParameter> getIntegrationParameters() {
		final var params = new ArrayList<IntegrationMethodParameter>();
		final AbstractIntegrationBean bean = this.getIntegrationBean();
		final Project project = bean.getNamespace().getProject();
		final BoundaryMethodTypeEnumeration methodType = this.getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.SEARCH || methodType == BoundaryMethodTypeEnumeration.COUNT) {
			final var paramSearch = new IntegrationMethodParameter(this, SEARCH_PARAM_NAME, INTEGRATION_SEARCH_PARAM_TYPE);
			paramSearch.setComment(COMMENT_SEARCH);

			params.add(paramSearch);

			if (boundaryMethod.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
				final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
				final String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
						+ clientPkAttr.getUpperCaseName();

				final var paramClient = new IntegrationMethodParameter(this, clientParamName, clientPkAttr.getJavaType().getName());
				paramClient.setComment(COMMENT_CLIENT);
				paramClient.setQueryParameter(true);

				params.add(paramClient);
			}
			else if (boundaryMethod.getDataFetchType() == BoundaryMethodDataFetchType.USER) {
				final DomainAttribute userPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.USER).getPKAttribute();
				final String userParamName = project.getDomainObjectByTag(DomainTagEnumeration.USER).getLowerCaseName()
						+ userPkAttr.getUpperCaseName();

				final var paramUser = new IntegrationMethodParameter(this, userParamName, userPkAttr.getJavaType().getName());
				paramUser.setComment(COMMENT_USER);
				paramUser.setQueryParameter(true);

				params.add(paramUser);
			}
		}
		else
			this.getMethodParameters().forEach(p -> params.add(new IntegrationMethodParameter(this, this.getBoundaryMethod(), p)));

		if (boundaryMethod.getBoundaryBean().getDomainObject().isMandated()
				&& (methodType == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER
						|| methodType == BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES)) {
			final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
			final String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
					+ clientPkAttr.getUpperCaseName();

			final var paramClient = new IntegrationMethodParameter(this, clientParamName, clientPkAttr.getJavaType().getName());
			paramClient.setComment(COMMENT_CLIENT);
			paramClient.setQueryParameter(true);

			params.add(paramClient);
		}

		if (methodType == BoundaryMethodTypeEnumeration.COPY) {
			final var repositoryMethod = (RepositoryMethod) boundaryMethod.getServiceMethod();

			if (repositoryMethod.addUserParam()) {
				final DTOBean logOnDTO = project.getApplicationLogOnDTO();
				final String paramType = logOnDTO.getPKAttribute().getDomainAttribute().getJavaType().getName();

				final var paramUser = new IntegrationMethodParameter(this, PARAM_LOGGED_ON_USER, paramType);
				paramUser.setComment(COMMENT_USER);
				paramUser.setQueryParameter(true);

				params.add(paramUser);
			}
		}

		if (getIntegrationBean().getIntegrationTechnology() == IntegrationTechnology.REST && isStartNewThread()) {
			final var paramResponse = new IntegrationMethodParameter(this, "response", "AsyncResponse");
			paramResponse.setResponseParameter(true);

			params.add(paramResponse);
		}

		return params;
	}

}
