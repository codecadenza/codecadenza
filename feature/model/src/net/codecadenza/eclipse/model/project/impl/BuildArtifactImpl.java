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

import java.util.Collection;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Build Artifact</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl#getContainedArtifacts <em>Contained
 * Artifacts</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.BuildArtifactImpl#getProject <em>Project</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class BuildArtifactImpl extends EObjectImpl implements BuildArtifact {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final BuildArtifactType TYPE_EDEFAULT = BuildArtifactType.DOMAIN;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected BuildArtifactType type = TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getContainedArtifacts() <em>Contained Artifacts</em>}' attribute list
	 * @see #getContainedArtifacts()
	 * @generated
	 * @ordered
	 */
	protected EList<BuildArtifactType> containedArtifacts;

	/**
	 * @generated
	 */
	protected BuildArtifactImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.BUILD_ARTIFACT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.BUILD_ARTIFACT__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getType()
	 * @generated
	 */
	@Override
	public BuildArtifactType getType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#setType(net.codecadenza.eclipse.model.project.BuildArtifactType)
	 * @generated
	 */
	@Override
	public void setType(BuildArtifactType newType) {
		final BuildArtifactType oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.BUILD_ARTIFACT__TYPE, oldType, type));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getContainedArtifacts()
	 */
	@Override
	public EList<BuildArtifactType> getContainedArtifacts() {
		if (containedArtifacts == null)
			containedArtifacts = new EDataTypeUniqueEList<>(BuildArtifactType.class, this,
					ProjectPackage.BUILD_ARTIFACT__CONTAINED_ARTIFACTS);

		return containedArtifacts;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getProject()
	 * @generated
	 */
	@Override
	public Project getProject() {
		if (eContainerFeatureID() != ProjectPackage.BUILD_ARTIFACT__PROJECT)
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
		msgs = eBasicSetContainer((InternalEObject) newProject, ProjectPackage.BUILD_ARTIFACT__PROJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#setProject(net.codecadenza.eclipse.model.project.Project)
	 * @generated
	 */
	@Override
	public void setProject(Project newProject) {
		if (newProject != eInternalContainer()
				|| (eContainerFeatureID() != ProjectPackage.BUILD_ARTIFACT__PROJECT && newProject != null)) {
			if (EcoreUtil.isAncestor(this, newProject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newProject != null)
				msgs = ((InternalEObject) newProject).eInverseAdd(this, ProjectPackage.PROJECT__BUILD_CONFIGURATION, Project.class, msgs);

			msgs = basicSetProject(newProject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.BUILD_ARTIFACT__PROJECT, newProject, newProject));
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
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
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
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
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
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
				return eInternalContainer().eInverseRemove(this, ProjectPackage.PROJECT__BUILD_CONFIGURATION, Project.class, msgs);
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
			case ProjectPackage.BUILD_ARTIFACT__NAME:
				return getName();
			case ProjectPackage.BUILD_ARTIFACT__TYPE:
				return getType();
			case ProjectPackage.BUILD_ARTIFACT__CONTAINED_ARTIFACTS:
				return getContainedArtifacts();
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
				return getProject();
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
			case ProjectPackage.BUILD_ARTIFACT__NAME:
				setName((String) newValue);
				return;
			case ProjectPackage.BUILD_ARTIFACT__TYPE:
				setType((BuildArtifactType) newValue);
				return;
			case ProjectPackage.BUILD_ARTIFACT__CONTAINED_ARTIFACTS:
				getContainedArtifacts().clear();
				getContainedArtifacts().addAll((Collection<BuildArtifactType>) newValue);
				return;
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
				setProject((Project) newValue);
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
			case ProjectPackage.BUILD_ARTIFACT__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ProjectPackage.BUILD_ARTIFACT__TYPE:
				setType(TYPE_EDEFAULT);
				return;
			case ProjectPackage.BUILD_ARTIFACT__CONTAINED_ARTIFACTS:
				getContainedArtifacts().clear();
				return;
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
				setProject((Project) null);
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
			case ProjectPackage.BUILD_ARTIFACT__NAME:
				return name != null;
			case ProjectPackage.BUILD_ARTIFACT__TYPE:
				return type != TYPE_EDEFAULT;
			case ProjectPackage.BUILD_ARTIFACT__CONTAINED_ARTIFACTS:
				return containedArtifacts != null && !containedArtifacts.isEmpty();
			case ProjectPackage.BUILD_ARTIFACT__PROJECT:
				return getProject() != null;
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
		result.append(" (name: ");
		result.append(name);
		result.append(", type: ");
		result.append(type);
		result.append(", containedArtifacts: ");
		result.append(containedArtifacts);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#getModuleName()
	 * @generated not
	 */
	@Override
	public String getModuleName() {
		return getType().getName().replace('_', '-').toLowerCase();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact#isTestArtifact()
	 * @generated not
	 */
	@Override
	public boolean isTestArtifact() {
		return getType() == BuildArtifactType.INTEGRATION_TEST_REST || getType() == BuildArtifactType.INTEGRATION_TEST_SOAP
				|| getType() == BuildArtifactType.INTEGRATION_TEST_RMI || getType() == BuildArtifactType.INTEGRATION_TEST_KAFKA
				|| getType() == BuildArtifactType.INTEGRATION_TEST_JMS || getType() == BuildArtifactType.SELENIUM_TEST;
	}

}
