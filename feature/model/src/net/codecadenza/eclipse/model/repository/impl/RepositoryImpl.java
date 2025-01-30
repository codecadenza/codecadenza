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

import java.util.Collection;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Repository</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.repository.impl.RepositoryImpl#getRepositoryMethods <em>Repository Methods</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class RepositoryImpl extends ServiceBeanImpl implements Repository {
	/**
	 * The cached value of the '{@link #getRepositoryMethods() <em>Repository Methods</em>}' containment reference list
	 * @see #getRepositoryMethods()
	 * @generated
	 * @ordered
	 */
	protected EList<RepositoryMethod> repositoryMethods;

	/**
	 * @generated
	 */
	protected RepositoryImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RepositoryPackage.Literals.REPOSITORY;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.Repository#getRepositoryMethods()
	 * @generated
	 */
	@Override
	public EList<RepositoryMethod> getRepositoryMethods() {
		if (repositoryMethods == null)
			repositoryMethods = new EObjectContainmentWithInverseEList<>(RepositoryMethod.class, this,
					RepositoryPackage.REPOSITORY__REPOSITORY_METHODS, RepositoryPackage.REPOSITORY_METHOD__REPOSITORY);

		return repositoryMethods;
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
			case RepositoryPackage.REPOSITORY__REPOSITORY_METHODS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getRepositoryMethods()).basicAdd(otherEnd, msgs);
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
			case RepositoryPackage.REPOSITORY__REPOSITORY_METHODS:
				return ((InternalEList<?>) getRepositoryMethods()).basicRemove(otherEnd, msgs);
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
			case RepositoryPackage.REPOSITORY__REPOSITORY_METHODS:
				return getRepositoryMethods();
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
			case RepositoryPackage.REPOSITORY__REPOSITORY_METHODS:
				getRepositoryMethods().clear();
				getRepositoryMethods().addAll((Collection<? extends RepositoryMethod>) newValue);
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
			case RepositoryPackage.REPOSITORY__REPOSITORY_METHODS:
				getRepositoryMethods().clear();
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
			case RepositoryPackage.REPOSITORY__REPOSITORY_METHODS:
				return repositoryMethods != null && !repositoryMethods.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.Repository#getMethodByType(net.codecadenza.eclipse.model.repository.
	 * RepositoryMethodTypeEnumeration)
	 * @generated not
	 */
	@Override
	public RepositoryMethod getMethodByType(RepositoryMethodTypeEnumeration type) {
		return getRepositoryMethods().stream().filter(m -> m.getMethodType() == type).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.Repository#searchMethodsByType(net.codecadenza.eclipse.model.repository.
	 * RepositoryMethodTypeEnumeration, net.codecadenza.eclipse.model.domain.AbstractDomainAssociation)
	 * @generated not
	 */
	@Override
	public RepositoryMethod getMethodByTypeAndAssociation(RepositoryMethodTypeEnumeration type,
			AbstractDomainAssociation association) {
		final RepositoryMethod repositoryMethod = getMethodByType(type);

		if (repositoryMethod == null)
			return null;

		for (final MethodParameter param : repositoryMethod.getMethodParameters())
			if (param instanceof final RepositoryMethodParameter repositoryMethodParameter
					&& association.equals(repositoryMethodParameter.getAssociation()))
				return repositoryMethod;

		for (final MethodParameter param : repositoryMethod.getMethodParameters())
			if (association.getName().equals(param.getHint()))
				return repositoryMethod;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.Repository#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.REPOSITORY, name, getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

}
