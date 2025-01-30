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
package net.codecadenza.eclipse.model.mapping.impl;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.impl.JavaTypeImpl;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Mapping Object</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#getDomainObject <em>Domain Object</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public abstract class MappingObjectImpl extends JavaTypeImpl implements MappingObject {
	/**
	 * The cached value of the '{@link #getDomainObject() <em>Domain Object</em>}' reference
	 * @see #getDomainObject()
	 * @generated
	 * @ordered
	 */
	protected DomainObject domainObject;

	/**
	 * @generated
	 */
	protected MappingObjectImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return MappingPackage.Literals.MAPPING_OBJECT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingObject#getDomainObject()
	 * @generated
	 */
	@Override
	public DomainObject getDomainObject() {
		if (domainObject != null && domainObject.eIsProxy()) {
			final var oldDomainObject = (InternalEObject) domainObject;
			domainObject = (DomainObject) eResolveProxy(oldDomainObject);

			if (domainObject != oldDomainObject && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT, oldDomainObject,
						domainObject));
		}

		return domainObject;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainObject basicGetDomainObject() {
		return domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingObject#setDomainObject(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setDomainObject(DomainObject newDomainObject) {
		final DomainObject oldDomainObject = domainObject;
		domainObject = newDomainObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT, oldDomainObject,
					domainObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT:
				if (resolve)
					return getDomainObject();

				return basicGetDomainObject();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT:
				setDomainObject((DomainObject) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT:
				setDomainObject((DomainObject) null);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT:
				return domainObject != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingObject#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.DTO, name, getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

}
