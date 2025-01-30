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

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_SERVICE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;

import java.util.Collection;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
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
 * An implementation of the model object '<em><b>Boundary Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryBeanImpl#getBoundaryMethods <em>Boundary Methods</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.impl.BoundaryBeanImpl#getRepository <em>Repository</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class BoundaryBeanImpl extends ServiceBeanImpl implements BoundaryBean {
	/**
	 * The cached value of the '{@link #getBoundaryMethods() <em>Boundary Methods</em>}' containment reference list
	 * @see #getBoundaryMethods()
	 * @generated
	 * @ordered
	 */
	protected EList<BoundaryMethod> boundaryMethods;

	/**
	 * The cached value of the '{@link #getRepository() <em>Repository</em>}' reference
	 * @see #getRepository()
	 * @generated
	 * @ordered
	 */
	protected Repository repository;

	/**
	 * @generated
	 */
	protected BoundaryBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceBeanImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return BoundaryPackage.Literals.BOUNDARY_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethods()
	 * @generated
	 */
	@Override
	public EList<BoundaryMethod> getBoundaryMethods() {
		if (boundaryMethods == null)
			boundaryMethods = new EObjectContainmentWithInverseEList<>(BoundaryMethod.class, this,
					BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS, BoundaryPackage.BOUNDARY_METHOD__BOUNDARY_BEAN);

		return boundaryMethods;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getRepository()
	 * @generated
	 */
	@Override
	public Repository getRepository() {
		if (repository != null && repository.eIsProxy()) {
			final var oldRepository = (InternalEObject) repository;
			repository = (Repository) eResolveProxy(oldRepository);

			if (repository != oldRepository && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, BoundaryPackage.BOUNDARY_BEAN__REPOSITORY, oldRepository,
						repository));
		}

		return repository;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Repository basicGetRepository() {
		return repository;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#setRepository(net.codecadenza.eclipse.model.repository.Repository)
	 * @generated
	 */
	@Override
	public void setRepository(Repository newRepository) {
		final Repository oldRepository = repository;
		repository = newRepository;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, BoundaryPackage.BOUNDARY_BEAN__REPOSITORY, oldRepository, repository));
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
			case BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getBoundaryMethods()).basicAdd(otherEnd, msgs);
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
			case BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS:
				return ((InternalEList<?>) getBoundaryMethods()).basicRemove(otherEnd, msgs);
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
			case BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS:
				return getBoundaryMethods();
			case BoundaryPackage.BOUNDARY_BEAN__REPOSITORY:
				if (resolve)
					return getRepository();

				return basicGetRepository();
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
			case BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS:
				getBoundaryMethods().clear();
				getBoundaryMethods().addAll((Collection<? extends BoundaryMethod>) newValue);
				return;
			case BoundaryPackage.BOUNDARY_BEAN__REPOSITORY:
				setRepository((Repository) newValue);
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
			case BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS:
				getBoundaryMethods().clear();
				return;
			case BoundaryPackage.BOUNDARY_BEAN__REPOSITORY:
				setRepository((Repository) null);
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
			case BoundaryPackage.BOUNDARY_BEAN__BOUNDARY_METHODS:
				return boundaryMethods != null && !boundaryMethods.isEmpty();
			case BoundaryPackage.BOUNDARY_BEAN__REPOSITORY:
				return repository != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethodByReturnType(net.codecadenza.eclipse.model.java.
	 * JavaType, net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration)
	 * @generated not
	 */
	@Override
	public BoundaryMethod getBoundaryMethodByReturnType(JavaType returnType, BoundaryMethodTypeEnumeration methodType) {
		for (final BoundaryMethod method : getBoundaryMethods()) {
			if (method.getMethodType() != methodType)
				continue;

			if (method.getReturnType().equals(returnType))
				return method;
		}

		throw new IllegalStateException("A boundary method with the given return type doesn't exist!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getBeanSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getBeanSourceFile() {
		final Project project = getNamespace().getProject();
		String packageName = getNamespace().toString();
		String beanName = interfaceName;

		if (project.isAddBoundaryInterface()) {
			packageName += SUB_PACKAGE_BEAN;
			beanName = name;
		}

		final JavaFile javaFile;

		if (project.isBoundaryMode())
			javaFile = new JavaFile(project, BuildArtifactType.BOUNDARY, beanName, packageName);
		else
			javaFile = new JavaFile(project, BuildArtifactType.FACADE, beanName, packageName);

		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getInterfaceSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getInterfaceSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.CLIENT_INTERFACE, interfaceName,
				getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getTypeScriptSourceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getTypeScriptSourceFile() {
		final Project project = getNamespace().getProject();

		if (!project.hasAngularClient())
			return null;

		final var path = ANGULAR_SERVICE_FOLDER + "/" + getDomainObject().getName().toLowerCase() + ".service.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

}
