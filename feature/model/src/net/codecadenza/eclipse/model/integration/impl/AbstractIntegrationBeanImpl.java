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

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_PRODUCER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SEI;

import java.util.Collection;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RMIIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Abstract Integration Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#getMethods <em>Methods</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.impl.AbstractIntegrationBeanImpl#getClientClassName <em>Client Class
 * Name</em>}</li>
 * </ul>
 * @generated
 */
public class AbstractIntegrationBeanImpl extends ServiceBeanImpl implements AbstractIntegrationBean {
	/**
	 * The cached value of the '{@link #getMethods() <em>Methods</em>}' containment reference list
	 * @see #getMethods()
	 * @generated
	 * @ordered
	 */
	protected EList<AbstractIntegrationMethod> methods;

	/**
	 * The default value of the '{@link #getClientClassName() <em>Client Class Name</em>}' attribute
	 * @see #getClientClassName()
	 * @generated
	 * @ordered
	 */
	protected static final String CLIENT_CLASS_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getClientClassName() <em>Client Class Name</em>}' attribute
	 * @see #getClientClassName()
	 * @generated
	 * @ordered
	 */
	protected String clientClassName = CLIENT_CLASS_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getProducerClassName() <em>Producer Class Name</em>}' attribute
	 * @see #getProducerClassName()
	 * @generated
	 * @ordered
	 */
	protected static final String PRODUCER_CLASS_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getProducerClassName() <em>Producer Class Name</em>}' attribute
	 * @see #getProducerClassName()
	 * @generated
	 * @ordered
	 */
	protected String producerClassName = PRODUCER_CLASS_NAME_EDEFAULT;

	/**
	 * @generated
	 */
	protected AbstractIntegrationBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return IntegrationPackage.Literals.ABSTRACT_INTEGRATION_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getMethods()
	 * @generated
	 */
	@Override
	public EList<AbstractIntegrationMethod> getMethods() {
		if (methods == null)
			methods = new EObjectContainmentWithInverseEList<>(AbstractIntegrationMethod.class, this,
					IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS,
					IntegrationPackage.ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN);

		return methods;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getClientClassName()
	 * @generated
	 */
	@Override
	public String getClientClassName() {
		return clientClassName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#setClientClassName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setClientClassName(String newClientClassName) {
		final String oldClientClassName = clientClassName;
		clientClassName = newClientClassName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME,
					oldClientClassName, clientClassName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getProducerClassName()
	 * @generated
	 */
	@Override
	public String getProducerClassName() {
		return producerClassName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#setProducerClassName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setProducerClassName(String newProducerClassName) {
		final String oldProducerClassName = producerClassName;
		producerClassName = newProducerClassName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME,
					oldProducerClassName, producerClassName));
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getMethods()).basicAdd(otherEnd, msgs);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS:
				return ((InternalEList<?>) getMethods()).basicRemove(otherEnd, msgs);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS:
				return getMethods();
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME:
				return getClientClassName();
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME:
				return getProducerClassName();
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS:
				getMethods().clear();
				getMethods().addAll((Collection<? extends AbstractIntegrationMethod>) newValue);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME:
				setClientClassName((String) newValue);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME:
				setProducerClassName((String) newValue);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS:
				getMethods().clear();
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME:
				setClientClassName(CLIENT_CLASS_NAME_EDEFAULT);
				return;
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME:
				setProducerClassName(PRODUCER_CLASS_NAME_EDEFAULT);
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
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__METHODS:
				return methods != null && !methods.isEmpty();
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME:
				return clientClassName != null;
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME:
				return producerClassName != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (clientClassName: ");
		result.append(clientClassName);
		result.append(", producerClassName: ");
		result.append(producerClassName);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getIntegrationTechnology()
	 * @generated not
	 */
	@Override
	public IntegrationTechnology getIntegrationTechnology() {
		if (this instanceof SOAPIntegrationBean)
			return IntegrationTechnology.SOAP;
		else if (this instanceof RESTIntegrationBean)
			return IntegrationTechnology.REST;
		else if (this instanceof RMIIntegrationBean)
			return IntegrationTechnology.RMI;
		else if (this instanceof KafkaIntegrationBean)
			return IntegrationTechnology.KAFKA;

		return IntegrationTechnology.JMS;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getIntegrationModule()
	 * @generated not
	 */
	@Override
	public IntegrationModule getIntegrationModule() {
		final Project project = this.getNamespace().getProject();
		final IntegrationModule module = project.getIntegrationModules().stream().filter(e -> e.getNamespace().equals(getNamespace()))
				.findFirst().orElse(null);

		if (module == null)
			throw new IllegalStateException("The module for the integration bean '" + getName() + "' could not be found!");

		return module;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getServiceBeanSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getServiceBeanSourceFile() {
		final String packageName = getNamespace().toString() + SUB_PACKAGE_BEAN;
		final BuildArtifactType artifactType;

		if (getIntegrationTechnology() == IntegrationTechnology.SOAP)
			artifactType = BuildArtifactType.INTEGRATION_IMP_SOAP;
		else if (getIntegrationTechnology() == IntegrationTechnology.REST)
			artifactType = BuildArtifactType.INTEGRATION_IMP_REST;
		else if (getIntegrationTechnology() == IntegrationTechnology.RMI)
			artifactType = BuildArtifactType.INTEGRATION_IMP_RMI;
		else if (getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			artifactType = BuildArtifactType.INTEGRATION_IMP_KAFKA;
		else
			artifactType = BuildArtifactType.INTEGRATION_IMP_JMS;

		final var javaFile = new JavaFile(getNamespace(), artifactType, name, packageName);
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getSEISourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSEISourceFile() {
		final BuildArtifactType artifactType;
		String packageName = getNamespace().toString();

		if (getIntegrationTechnology() == IntegrationTechnology.SOAP)
			artifactType = BuildArtifactType.INTEGRATION_SEI_SOAP;
		else if (getIntegrationTechnology() == IntegrationTechnology.REST)
			artifactType = BuildArtifactType.INTEGRATION_SEI_REST;
		else if (getIntegrationTechnology() == IntegrationTechnology.RMI)
			artifactType = BuildArtifactType.INTEGRATION_SEI_RMI;
		else if (getIntegrationTechnology() == IntegrationTechnology.KAFKA) {
			artifactType = BuildArtifactType.INTEGRATION_SEI_KAFKA;
			packageName += SUB_PACKAGE_INT_SEI;
		}
		else
			artifactType = BuildArtifactType.INTEGRATION_SEI_JMS;

		final var javaFile = new JavaFile(getNamespace(), artifactType, interfaceName, packageName);
		javaFile.setComment("Service end-point interface for the " + getDomainObject().getLabel() + " "
				+ getIntegrationTechnology().name() + " service");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getClientSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getClientSourceFile() {
		final String packageName = getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;
		final BuildArtifactType artifactType;

		if (getIntegrationTechnology() == IntegrationTechnology.SOAP)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_SOAP;
		else if (getIntegrationTechnology() == IntegrationTechnology.REST)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_REST;
		else if (getIntegrationTechnology() == IntegrationTechnology.RMI)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_RMI;
		else if (getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_KAFKA;
		else
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_JMS;

		final var javaFile = new JavaFile(getNamespace(), artifactType, clientClassName, packageName);
		javaFile.setComment("Client for the " + getDomainObject().getLabel() + " " + getIntegrationTechnology().name() + " service");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getProducerSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getProducerSourceFile() {
		final String packageName = getNamespace().toString() + SUB_PACKAGE_INT_PRODUCER;
		final BuildArtifactType artifactType;

		if (getIntegrationTechnology() == IntegrationTechnology.SOAP)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_SOAP;
		else if (getIntegrationTechnology() == IntegrationTechnology.REST)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_REST;
		else if (getIntegrationTechnology() == IntegrationTechnology.RMI)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_RMI;
		else if (getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_KAFKA;
		else
			artifactType = BuildArtifactType.INTEGRATION_CLIENT_JMS;

		final var javaFile = new JavaFile(getNamespace(), artifactType, producerClassName, packageName);
		javaFile
				.setComment("Producer for the " + getDomainObject().getLabel() + " " + getIntegrationTechnology().name() + " service");

		return javaFile;
	}

}
