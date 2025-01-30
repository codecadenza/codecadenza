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
package net.codecadenza.eclipse.model.project.impl;

import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Integration Module</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl#getTechnology <em>Technology</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl#getProject <em>Project</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl#getNamespace <em>Namespace</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.IntegrationModuleImpl#isAddSecurityHandler <em>Add Security
 * Handler</em>}</li>
 * </ul>
 * @generated
 */
public class IntegrationModuleImpl extends EObjectImpl implements IntegrationModule {
	/**
	 * The default value of the '{@link #getTechnology() <em>Technology</em>}' attribute
	 * @see #getTechnology()
	 * @generated
	 * @ordered
	 */
	protected static final IntegrationTechnology TECHNOLOGY_EDEFAULT = IntegrationTechnology.REST;

	/**
	 * The cached value of the '{@link #getTechnology() <em>Technology</em>}' attribute
	 * @see #getTechnology()
	 * @generated
	 * @ordered
	 */
	protected IntegrationTechnology technology = TECHNOLOGY_EDEFAULT;

	/**
	 * The cached value of the '{@link #getNamespace() <em>Namespace</em>}' reference
	 * @see #getNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace namespace;

	/**
	 * The default value of the '{@link #isAddSecurityHandler() <em>Add Security Handler</em>}' attribute
	 * @see #isAddSecurityHandler()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADD_SECURITY_HANDLER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAddSecurityHandler() <em>Add Security Handler</em>}' attribute
	 * @see #isAddSecurityHandler()
	 * @generated
	 * @ordered
	 */
	protected boolean addSecurityHandler = ADD_SECURITY_HANDLER_EDEFAULT;

	/**
	 * The default value of the '{@link #isAddProducers() <em>Add Producers</em>}' attribute
	 * @see #isAddProducers()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADD_PRODUCERS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAddProducers() <em>Add Producers</em>}' attribute
	 * @see #isAddProducers()
	 * @generated
	 * @ordered
	 */
	protected boolean addProducers = ADD_PRODUCERS_EDEFAULT;

	/**
	 * @generated
	 */
	protected IntegrationModuleImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.INTEGRATION_MODULE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getTechnology()
	 * @generated
	 */
	@Override
	public IntegrationTechnology getTechnology() {
		return technology;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#setTechnology(net.codecadenza.eclipse.model.project.
	 * IntegrationTechnology)
	 * @generated
	 */
	@Override
	public void setTechnology(IntegrationTechnology newTechnology) {
		final IntegrationTechnology oldTechnology = technology;
		technology = newTechnology == null ? TECHNOLOGY_EDEFAULT : newTechnology;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.INTEGRATION_MODULE__TECHNOLOGY, oldTechnology,
					technology));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getProject()
	 * @generated
	 */
	@Override
	public Project getProject() {
		if (eContainerFeatureID() != ProjectPackage.INTEGRATION_MODULE__PROJECT)
			return null;

		return (Project) eInternalContainer();
	}

	/**
	 * @param newProject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetProject(Project newProject, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newProject, ProjectPackage.INTEGRATION_MODULE__PROJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#setProject(net.codecadenza.eclipse.model.project.Project)
	 * @generated
	 */
	@Override
	public void setProject(Project newProject) {
		if (newProject != eInternalContainer()
				|| (eContainerFeatureID() != ProjectPackage.INTEGRATION_MODULE__PROJECT && newProject != null)) {
			if (EcoreUtil.isAncestor(this, newProject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newProject != null)
				msgs = ((InternalEObject) newProject).eInverseAdd(this, ProjectPackage.PROJECT__INTEGRATION_MODULES, Project.class, msgs);

			msgs = basicSetProject(newProject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.INTEGRATION_MODULE__PROJECT, newProject, newProject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getNamespace()
	 * @generated
	 */
	@Override
	public Namespace getNamespace() {
		if (namespace != null && namespace.eIsProxy()) {
			final var oldNamespace = (InternalEObject) namespace;
			namespace = (Namespace) eResolveProxy(oldNamespace);

			if (namespace != oldNamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ProjectPackage.INTEGRATION_MODULE__NAMESPACE, oldNamespace,
						namespace));
		}

		return namespace;
	}

	/**
	 * @return the namespace
	 * @generated
	 */
	public Namespace basicGetNamespace() {
		return namespace;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#setNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setNamespace(Namespace newNamespace) {
		final Namespace oldNamespace = namespace;
		namespace = newNamespace;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ProjectPackage.INTEGRATION_MODULE__NAMESPACE, oldNamespace, namespace));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#isAddSecurityHandler()
	 * @generated
	 */
	@Override
	public boolean isAddSecurityHandler() {
		return addSecurityHandler;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#setAddSecurityHandler(boolean)
	 * @generated
	 */
	@Override
	public void setAddSecurityHandler(boolean newAddSecurityHandler) {
		final boolean oldAddSecurityHandler = addSecurityHandler;
		addSecurityHandler = newAddSecurityHandler;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.INTEGRATION_MODULE__ADD_SECURITY_HANDLER,
					oldAddSecurityHandler, addSecurityHandler));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#isAddProducers()
	 * @generated
	 */
	@Override
	public boolean isAddProducers() {
		return addProducers;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#setAddProducers(boolean)
	 * @generated
	 */
	@Override
	public void setAddProducers(boolean newAddProducers) {
		final boolean oldAddProducers = addProducers;
		addProducers = newAddProducers;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.INTEGRATION_MODULE__ADD_PRODUCERS, oldAddProducers,
					addProducers));
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
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetProject((Project) otherEnd, msgs);
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
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				return basicSetProject(null, msgs);
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
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				return eInternalContainer().eInverseRemove(this, ProjectPackage.PROJECT__INTEGRATION_MODULES, Project.class, msgs);
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
			case ProjectPackage.INTEGRATION_MODULE__TECHNOLOGY:
				return getTechnology();
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				return getProject();
			case ProjectPackage.INTEGRATION_MODULE__ADD_SECURITY_HANDLER:
				return isAddSecurityHandler();
			case ProjectPackage.INTEGRATION_MODULE__NAMESPACE:
				if (resolve)
					return getNamespace();

				return basicGetNamespace();
			case ProjectPackage.INTEGRATION_MODULE__ADD_PRODUCERS:
				return isAddProducers();
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
			case ProjectPackage.INTEGRATION_MODULE__TECHNOLOGY:
				setTechnology((IntegrationTechnology) newValue);
				return;
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				setProject((Project) newValue);
				return;
			case ProjectPackage.INTEGRATION_MODULE__ADD_SECURITY_HANDLER:
				setAddSecurityHandler((boolean) newValue);
				return;
			case ProjectPackage.INTEGRATION_MODULE__NAMESPACE:
				setNamespace((Namespace) newValue);
				return;
			case ProjectPackage.INTEGRATION_MODULE__ADD_PRODUCERS:
				setAddProducers((boolean) newValue);
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
			case ProjectPackage.INTEGRATION_MODULE__TECHNOLOGY:
				setTechnology(TECHNOLOGY_EDEFAULT);
				return;
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				setProject((Project) null);
				return;
			case ProjectPackage.INTEGRATION_MODULE__ADD_SECURITY_HANDLER:
				setAddSecurityHandler(ADD_SECURITY_HANDLER_EDEFAULT);
				return;
			case ProjectPackage.INTEGRATION_MODULE__NAMESPACE:
				setNamespace((Namespace) null);
				return;
			case ProjectPackage.INTEGRATION_MODULE__ADD_PRODUCERS:
				setAddProducers(ADD_PRODUCERS_EDEFAULT);
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
			case ProjectPackage.INTEGRATION_MODULE__TECHNOLOGY:
				return technology != TECHNOLOGY_EDEFAULT;
			case ProjectPackage.INTEGRATION_MODULE__PROJECT:
				return getProject() != null;
			case ProjectPackage.INTEGRATION_MODULE__ADD_SECURITY_HANDLER:
				return addSecurityHandler != ADD_SECURITY_HANDLER_EDEFAULT;
			case ProjectPackage.INTEGRATION_MODULE__NAMESPACE:
				return namespace != null;
			case ProjectPackage.INTEGRATION_MODULE__ADD_PRODUCERS:
				return addProducers != ADD_PRODUCERS_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (technology: ");
		result.append(technology);
		result.append(", addSecurityHandler: ");
		result.append(addSecurityHandler);
		result.append(", addProducers: ");
		result.append(addProducers);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#hasClientArtifact()
	 * @generated not
	 */
	@Override
	public boolean hasClientArtifact() {
		if (getTechnology() == IntegrationTechnology.SOAP)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_CLIENT_SOAP);
		else if (getTechnology() == IntegrationTechnology.REST)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_CLIENT_REST);
		else if (getTechnology() == IntegrationTechnology.RMI)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_CLIENT_RMI);
		else if (getTechnology() == IntegrationTechnology.KAFKA)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_CLIENT_KAFKA);
		else if (getTechnology() == IntegrationTechnology.JMS)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_CLIENT_JMS);

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#hasSEIArtifact()
	 * @generated not
	 */
	@Override
	public boolean hasSEIArtifact() {
		if (getTechnology() == IntegrationTechnology.SOAP)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_SEI_SOAP);
		else if (getTechnology() == IntegrationTechnology.REST)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_SEI_REST);
		else if (getTechnology() == IntegrationTechnology.RMI)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_SEI_RMI);
		else if (getTechnology() == IntegrationTechnology.KAFKA)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_SEI_KAFKA);
		else if (getTechnology() == IntegrationTechnology.JMS)
			return getProject().artifactExists(BuildArtifactType.INTEGRATION_SEI_JMS);

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getCredentialsProviderName()
	 * @generated not
	 */
	@Override
	public String getCredentialsProviderName() {
		return getTechnology().getName() + "CredentialsProvider";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getFileServiceName()
	 * @generated not
	 */
	@Override
	public String getFileServiceName() {
		if (getTechnology() == IntegrationTechnology.SOAP)
			return "CustomFile" + getTechnology().getName() + "Service";

		return "File" + getTechnology().getName() + "Service";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getFileServiceClientName()
	 * @generated not
	 */
	@Override
	public String getFileServiceClientName() {
		return "File" + getTechnology().getName() + "Client";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule#getFileServiceProducerName()
	 * @generated not
	 */
	@Override
	public String getFileServiceProducerName() {
		return "File" + getTechnology().getName() + "ServiceProducer";
	}

}
