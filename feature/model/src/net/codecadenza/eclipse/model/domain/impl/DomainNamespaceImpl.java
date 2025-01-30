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
package net.codecadenza.eclipse.model.domain.impl;

import static net.codecadenza.eclipse.shared.Constants.DIAGRAM_FILE_EXTENSION;

import java.util.Collection;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.impl.NamespaceImpl;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Namespace</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainNamespaceImpl#getDomainObjects <em>Domain Objects</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainNamespaceImpl#getEnumerations <em>Enumerations</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DomainNamespaceImpl extends NamespaceImpl implements DomainNamespace {
	/**
	 * The cached value of the '{@link #getDomainObjects() <em>Domain Objects</em>}' containment reference list
	 * @see #getDomainObjects()
	 * @generated
	 * @ordered
	 */
	protected EList<DomainObject> domainObjects;

	/**
	 * The cached value of the '{@link #getEnumerations() <em>Enumerations</em>}' containment reference list
	 * @see #getEnumerations()
	 * @generated
	 * @ordered
	 */
	protected EList<JavaEnum> enumerations;

	/**
	 * @generated
	 */
	protected DomainNamespaceImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.DOMAIN_NAMESPACE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace#getDomainObjects()
	 * @generated
	 */
	@Override
	public EList<DomainObject> getDomainObjects() {
		if (domainObjects == null)
			domainObjects = new EObjectContainmentEList<>(DomainObject.class, this, DomainPackage.DOMAIN_NAMESPACE__DOMAIN_OBJECTS);

		return domainObjects;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace#getEnumerations()
	 * @generated
	 */
	@Override
	public EList<JavaEnum> getEnumerations() {
		if (enumerations == null)
			enumerations = new EObjectContainmentEList<>(JavaEnum.class, this, DomainPackage.DOMAIN_NAMESPACE__ENUMERATIONS);

		return enumerations;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.DOMAIN_NAMESPACE__DOMAIN_OBJECTS:
				return ((InternalEList<?>) getDomainObjects()).basicRemove(otherEnd, msgs);
			case DomainPackage.DOMAIN_NAMESPACE__ENUMERATIONS:
				return ((InternalEList<?>) getEnumerations()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.DOMAIN_NAMESPACE__DOMAIN_OBJECTS:
				return getDomainObjects();
			case DomainPackage.DOMAIN_NAMESPACE__ENUMERATIONS:
				return getEnumerations();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DomainPackage.DOMAIN_NAMESPACE__DOMAIN_OBJECTS:
				getDomainObjects().clear();
				getDomainObjects().addAll((Collection<? extends DomainObject>) newValue);
				return;
			case DomainPackage.DOMAIN_NAMESPACE__ENUMERATIONS:
				getEnumerations().clear();
				getEnumerations().addAll((Collection<? extends JavaEnum>) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DomainPackage.DOMAIN_NAMESPACE__DOMAIN_OBJECTS:
				getDomainObjects().clear();
				return;
			case DomainPackage.DOMAIN_NAMESPACE__ENUMERATIONS:
				getEnumerations().clear();
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DomainPackage.DOMAIN_NAMESPACE__DOMAIN_OBJECTS:
				return domainObjects != null && !domainObjects.isEmpty();
			case DomainPackage.DOMAIN_NAMESPACE__ENUMERATIONS:
				return enumerations != null && !enumerations.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace#getDiagramFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getDiagramFile() {
		final String path = "/model/package-" + getName() + "." + DIAGRAM_FILE_EXTENSION;

		return new WorkspaceFile(getProject(), BuildArtifactType.DOMAIN, path, null);
	}

}
