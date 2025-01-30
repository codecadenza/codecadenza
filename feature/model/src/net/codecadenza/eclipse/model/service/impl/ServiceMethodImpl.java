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

import java.util.Collection;
import net.codecadenza.eclipse.model.java.impl.JavaMethodImpl;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;
import net.codecadenza.eclipse.model.service.MethodInvocation;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * An implementation of the model object '<em><b>Service Method</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#getQueryStatement <em>Query Statement</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#getPermissionMode <em>Permission Mode</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#getTransactionType <em>Transaction Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#getMethodInvocation <em>Method Invocation</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#getCustomStatement <em>Custom Statement</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public abstract class ServiceMethodImpl extends JavaMethodImpl implements ServiceMethod {
	/**
	 * The default value of the '{@link #getQueryStatement() <em>Query Statement</em>}' attribute
	 * @see #getQueryStatement()
	 * @generated
	 * @ordered
	 */
	protected static final String QUERY_STATEMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getQueryStatement() <em>Query Statement</em>}' attribute
	 * @see #getQueryStatement()
	 * @generated
	 * @ordered
	 */
	protected String queryStatement = QUERY_STATEMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getPermissionMode() <em>Permission Mode</em>}' attribute
	 * @see #getPermissionMode()
	 * @generated
	 * @ordered
	 */
	protected static final PermissionModeEnumeration PERMISSION_MODE_EDEFAULT = PermissionModeEnumeration.PERMIT_ALL;

	/**
	 * The cached value of the '{@link #getPermissionMode() <em>Permission Mode</em>}' attribute
	 * @see #getPermissionMode()
	 * @generated
	 * @ordered
	 */
	protected PermissionModeEnumeration permissionMode = PERMISSION_MODE_EDEFAULT;

	/**
	 * The default value of the '{@link #getTransactionType() <em>Transaction Type</em>}' attribute
	 * @see #getTransactionType()
	 * @generated
	 * @ordered
	 */
	protected static final TransactionTypeEnumeration TRANSACTION_TYPE_EDEFAULT = TransactionTypeEnumeration.REQUIRES_NEW;

	/**
	 * The cached value of the '{@link #getTransactionType() <em>Transaction Type</em>}' attribute
	 * @see #getTransactionType()
	 * @generated
	 * @ordered
	 */
	protected TransactionTypeEnumeration transactionType = TRANSACTION_TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getRoles() <em>Roles</em>}' reference list
	 * @see #getRoles()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> roles;

	/**
	 * The cached value of the '{@link #getMethodInvocation() <em>Method Invocation</em>}' containment reference
	 * @see #getMethodInvocation()
	 * @generated
	 * @ordered
	 */
	protected MethodInvocation methodInvocation;

	/**
	 * The default value of the '{@link #getCustomStatement() <em>Custom Statement</em>}' attribute
	 * @see #getCustomStatement()
	 * @generated
	 * @ordered
	 */
	protected static final String CUSTOM_STATEMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getCustomStatement() <em>Custom Statement</em>}' attribute
	 * @see #getCustomStatement()
	 * @generated
	 * @ordered
	 */
	protected String customStatement = CUSTOM_STATEMENT_EDEFAULT;

	/**
	 * @generated
	 */
	protected ServiceMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ServicePackage.Literals.SERVICE_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getQueryStatement()
	 * @generated
	 */
	@Override
	public String getQueryStatement() {
		return queryStatement;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#setQueryStatement(java.lang.String)
	 * @generated
	 */
	@Override
	public void setQueryStatement(String newQueryStatement) {
		final String oldQueryStatement = queryStatement;
		queryStatement = newQueryStatement;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_METHOD__QUERY_STATEMENT, oldQueryStatement,
					queryStatement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getPermissionMode()
	 * @generated
	 */
	@Override
	public PermissionModeEnumeration getPermissionMode() {
		return permissionMode;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#setPermissionMode(net.codecadenza.eclipse.model.repository.
	 * PermissionModeEnumeration)
	 * @generated
	 */
	@Override
	public void setPermissionMode(PermissionModeEnumeration newPermissionMode) {
		final PermissionModeEnumeration oldPermissionMode = permissionMode;
		permissionMode = newPermissionMode == null ? PERMISSION_MODE_EDEFAULT : newPermissionMode;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_METHOD__PERMISSION_MODE, oldPermissionMode,
					permissionMode));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getTransactionType()
	 * @generated
	 */
	@Override
	public TransactionTypeEnumeration getTransactionType() {
		return transactionType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#setTransactionType(net.codecadenza.eclipse.model.repository.
	 * TransactionTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setTransactionType(TransactionTypeEnumeration newTransactionType) {
		final TransactionTypeEnumeration oldTransactionType = transactionType;
		transactionType = newTransactionType == null ? TRANSACTION_TYPE_EDEFAULT : newTransactionType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE, oldTransactionType,
					transactionType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getRoles()
	 * @generated
	 */
	@Override
	public EList<Role> getRoles() {
		if (roles == null)
			roles = new EObjectResolvingEList<>(Role.class, this, ServicePackage.SERVICE_METHOD__ROLES);

		return roles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getMethodInvocation()
	 * @generated
	 */
	@Override
	public MethodInvocation getMethodInvocation() {
		return methodInvocation;
	}

	/**
	 * @param newMethodInvocation
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetMethodInvocation(MethodInvocation newMethodInvocation, NotificationChain msgs) {
		final MethodInvocation oldMethodInvocation = methodInvocation;
		methodInvocation = newMethodInvocation;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_METHOD__METHOD_INVOCATION,
					oldMethodInvocation, newMethodInvocation);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#setMethodInvocation(net.codecadenza.eclipse.model.service.
	 * MethodInvocation)
	 * @generated
	 */
	@Override
	public void setMethodInvocation(MethodInvocation newMethodInvocation) {
		if (newMethodInvocation != methodInvocation) {
			NotificationChain msgs = null;

			if (methodInvocation != null)
				msgs = ((InternalEObject) methodInvocation).eInverseRemove(this, ServicePackage.METHOD_INVOCATION__METHOD,
						MethodInvocation.class, msgs);

			if (newMethodInvocation != null)
				msgs = ((InternalEObject) newMethodInvocation).eInverseAdd(this, ServicePackage.METHOD_INVOCATION__METHOD,
						MethodInvocation.class, msgs);

			msgs = basicSetMethodInvocation(newMethodInvocation, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_METHOD__METHOD_INVOCATION, newMethodInvocation,
					newMethodInvocation));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#getCustomStatement()
	 * @generated
	 */
	@Override
	public String getCustomStatement() {
		return customStatement;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod#setCustomStatement(java.lang.String)
	 * @generated
	 */
	@Override
	public void setCustomStatement(String newCustomStatement) {
		final String oldCustomStatement = customStatement;
		customStatement = newCustomStatement;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT, oldCustomStatement,
					customStatement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ServicePackage.SERVICE_METHOD__METHOD_INVOCATION:
				if (methodInvocation != null)
					msgs = ((InternalEObject) methodInvocation).eInverseRemove(this,
							EOPPOSITE_FEATURE_BASE - ServicePackage.SERVICE_METHOD__METHOD_INVOCATION, null, msgs);

				return basicSetMethodInvocation((MethodInvocation) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ServicePackage.SERVICE_METHOD__METHOD_INVOCATION:
				return basicSetMethodInvocation(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ServicePackage.SERVICE_METHOD__QUERY_STATEMENT:
				return getQueryStatement();
			case ServicePackage.SERVICE_METHOD__PERMISSION_MODE:
				return getPermissionMode();
			case ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE:
				return getTransactionType();
			case ServicePackage.SERVICE_METHOD__ROLES:
				return getRoles();
			case ServicePackage.SERVICE_METHOD__METHOD_INVOCATION:
				return getMethodInvocation();
			case ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT:
				return getCustomStatement();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ServicePackage.SERVICE_METHOD__QUERY_STATEMENT:
				setQueryStatement((String) newValue);
				return;
			case ServicePackage.SERVICE_METHOD__PERMISSION_MODE:
				setPermissionMode((PermissionModeEnumeration) newValue);
				return;
			case ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE:
				setTransactionType((TransactionTypeEnumeration) newValue);
				return;
			case ServicePackage.SERVICE_METHOD__ROLES:
				getRoles().clear();
				getRoles().addAll((Collection<? extends Role>) newValue);
				return;
			case ServicePackage.SERVICE_METHOD__METHOD_INVOCATION:
				setMethodInvocation((MethodInvocation) newValue);
				return;
			case ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT:
				setCustomStatement((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ServicePackage.SERVICE_METHOD__QUERY_STATEMENT:
				setQueryStatement(QUERY_STATEMENT_EDEFAULT);
				return;
			case ServicePackage.SERVICE_METHOD__PERMISSION_MODE:
				setPermissionMode(PERMISSION_MODE_EDEFAULT);
				return;
			case ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE:
				setTransactionType(TRANSACTION_TYPE_EDEFAULT);
				return;
			case ServicePackage.SERVICE_METHOD__ROLES:
				getRoles().clear();
				return;
			case ServicePackage.SERVICE_METHOD__METHOD_INVOCATION:
				setMethodInvocation((MethodInvocation) null);
				return;
			case ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT:
				setCustomStatement(CUSTOM_STATEMENT_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ServicePackage.SERVICE_METHOD__QUERY_STATEMENT:
				return queryStatement != null;
			case ServicePackage.SERVICE_METHOD__PERMISSION_MODE:
				return permissionMode != PERMISSION_MODE_EDEFAULT;
			case ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE:
				return transactionType != TRANSACTION_TYPE_EDEFAULT;
			case ServicePackage.SERVICE_METHOD__ROLES:
				return roles != null && !roles.isEmpty();
			case ServicePackage.SERVICE_METHOD__METHOD_INVOCATION:
				return methodInvocation != null;
			case ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT:
				return customStatement != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (queryStatement: ");
		result.append(queryStatement);
		result.append(", permissionMode: ");
		result.append(permissionMode);
		result.append(", transactionType: ");
		result.append(transactionType);
		result.append(", customStatement: ");
		result.append(customStatement);
		result.append(')');

		return result.toString();
	}

}
