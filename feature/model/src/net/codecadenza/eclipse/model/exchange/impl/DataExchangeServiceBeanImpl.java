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
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Data Exchange Service Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeServiceBeanImpl#getDataExchangeMethods <em>Data Exchange
 * Methods</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DataExchangeServiceBeanImpl extends ServiceBeanImpl implements DataExchangeServiceBean {
	/**
	 * The cached value of the '{@link #getDataExchangeMethods() <em>Data Exchange Methods</em>}' containment reference list
	 * @see #getDataExchangeMethods()
	 * @generated
	 * @ordered
	 */
	protected EList<DataExchangeMethod> dataExchangeMethods;

	/**
	 * @generated
	 */
	protected DataExchangeServiceBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.DATA_EXCHANGE_SERVICE_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getDataExchangeMethods()
	 * @generated
	 */
	@Override
	public EList<DataExchangeMethod> getDataExchangeMethods() {
		if (dataExchangeMethods == null)
			dataExchangeMethods = new EObjectContainmentWithInverseEList<>(DataExchangeMethod.class, this,
					ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS,
					ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN);

		return dataExchangeMethods;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getDataExchangeMethods()).basicAdd(otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS:
				return ((InternalEList<?>) getDataExchangeMethods()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS:
				return getDataExchangeMethods();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS:
				getDataExchangeMethods().clear();
				getDataExchangeMethods().addAll((Collection<? extends DataExchangeMethod>) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS:
				getDataExchangeMethods().clear();
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS:
				return dataExchangeMethods != null && !dataExchangeMethods.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.DATA_EXCHANGE, name, getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

}
