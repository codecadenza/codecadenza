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
package net.codecadenza.eclipse.model.java.impl;

import java.util.Collection;
import java.util.HashMap;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EObjectWithInverseResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Namespace</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl#getParent <em>Parent</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl#getChildNamespaces <em>Child Namespaces</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl#getJavaTypes <em>Java Types</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl#getProject <em>Project</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class NamespaceImpl extends EObjectImpl implements Namespace {
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
	 * The cached value of the '{@link #getParent() <em>Parent</em>}' reference
	 * @see #getParent()
	 * @generated
	 * @ordered
	 */
	protected Namespace parent;

	/**
	 * The cached value of the '{@link #getChildNamespaces() <em>Child Namespaces</em>}' reference list
	 * @see #getChildNamespaces()
	 * @generated
	 * @ordered
	 */
	protected EList<Namespace> childNamespaces;

	/**
	 * The cached value of the '{@link #getJavaTypes() <em>Java Types</em>}' reference list
	 * @see #getJavaTypes()
	 * @generated
	 * @ordered
	 */
	protected EList<JavaType> javaTypes;

	/**
	 * The cached value of the '{@link #getProject() <em>Project</em>}' reference
	 * @see #getProject()
	 * @generated
	 * @ordered
	 */
	protected Project project;

	/**
	 * @generated
	 */
	protected NamespaceImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JavaPackage.Literals.NAMESPACE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.NAMESPACE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#getParent()
	 * @generated
	 */
	@Override
	public Namespace getParent() {
		if (parent != null && parent.eIsProxy()) {
			final var oldParent = (InternalEObject) parent;
			parent = (Namespace) eResolveProxy(oldParent);

			if (parent != oldParent && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, JavaPackage.NAMESPACE__PARENT, oldParent, parent));
		}

		return parent;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Namespace basicGetParent() {
		return parent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#setParent(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setParent(Namespace newParent) {
		final Namespace oldParent = parent;
		parent = newParent;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.NAMESPACE__PARENT, oldParent, parent));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#getChildNamespaces()
	 * @generated
	 */
	@Override
	public EList<Namespace> getChildNamespaces() {
		if (childNamespaces == null)
			childNamespaces = new EObjectResolvingEList<>(Namespace.class, this, JavaPackage.NAMESPACE__CHILD_NAMESPACES);

		return childNamespaces;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#getJavaTypes()
	 * @generated
	 */
	@Override
	public EList<JavaType> getJavaTypes() {
		if (javaTypes == null)
			javaTypes = new EObjectWithInverseResolvingEList<>(JavaType.class, this, JavaPackage.NAMESPACE__JAVA_TYPES,
					JavaPackage.JAVA_TYPE__NAMESPACE);

		return javaTypes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#getProject()
	 * @generated
	 */
	@Override
	public Project getProject() {
		if (project != null && project.eIsProxy()) {
			final var oldProject = (InternalEObject) project;
			project = (Project) eResolveProxy(oldProject);

			if (project != oldProject && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, JavaPackage.NAMESPACE__PROJECT, oldProject, project));
		}

		return project;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Project basicGetProject() {
		return project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.Namespace#setProject(net.codecadenza.eclipse.model.project.Project)
	 * @generated
	 */
	@Override
	public void setProject(Project newProject) {
		final Project oldProject = project;
		project = newProject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.NAMESPACE__PROJECT, oldProject, project));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case JavaPackage.NAMESPACE__JAVA_TYPES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getJavaTypes()).basicAdd(otherEnd, msgs);
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
			case JavaPackage.NAMESPACE__JAVA_TYPES:
				return ((InternalEList<?>) getJavaTypes()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case JavaPackage.NAMESPACE__NAME:
				return getName();
			case JavaPackage.NAMESPACE__PARENT:
				if (resolve)
					return getParent();

				return basicGetParent();
			case JavaPackage.NAMESPACE__CHILD_NAMESPACES:
				return getChildNamespaces();
			case JavaPackage.NAMESPACE__JAVA_TYPES:
				return getJavaTypes();
			case JavaPackage.NAMESPACE__PROJECT:
				if (resolve)
					return getProject();

				return basicGetProject();
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
			case JavaPackage.NAMESPACE__NAME:
				setName((String) newValue);
				return;
			case JavaPackage.NAMESPACE__PARENT:
				setParent((Namespace) newValue);
				return;
			case JavaPackage.NAMESPACE__CHILD_NAMESPACES:
				getChildNamespaces().clear();
				getChildNamespaces().addAll((Collection<? extends Namespace>) newValue);
				return;
			case JavaPackage.NAMESPACE__JAVA_TYPES:
				getJavaTypes().clear();
				getJavaTypes().addAll((Collection<? extends JavaType>) newValue);
				return;
			case JavaPackage.NAMESPACE__PROJECT:
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
			case JavaPackage.NAMESPACE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case JavaPackage.NAMESPACE__PARENT:
				setParent((Namespace) null);
				return;
			case JavaPackage.NAMESPACE__CHILD_NAMESPACES:
				getChildNamespaces().clear();
				return;
			case JavaPackage.NAMESPACE__JAVA_TYPES:
				getJavaTypes().clear();
				return;
			case JavaPackage.NAMESPACE__PROJECT:
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
			case JavaPackage.NAMESPACE__NAME:
				return name != null;
			case JavaPackage.NAMESPACE__PARENT:
				return parent != null;
			case JavaPackage.NAMESPACE__CHILD_NAMESPACES:
				return childNamespaces != null && !childNamespaces.isEmpty();
			case JavaPackage.NAMESPACE__JAVA_TYPES:
				return javaTypes != null && !javaTypes.isEmpty();
			case JavaPackage.NAMESPACE__PROJECT:
				return project != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		if (parent == null)
			return name;

		final var nameBuilder = new StringBuilder();
		final var namespaceMap = new HashMap<Integer, String>();
		int k = 0;
		Namespace loopParent = parent;

		if (parent.eIsProxy()) {
			final var proxyParent = (InternalEObject) parent;
			loopParent = (Namespace) eResolveProxy(proxyParent);
		}

		namespaceMap.put(k, name);

		while (loopParent != null) {
			namespaceMap.put(++k, loopParent.getName());
			loopParent = loopParent.getParent();
		}

		int i = k;

		while (i >= 0) {
			if (i == k)
				nameBuilder.append(namespaceMap.get(i));
			else
				nameBuilder.append("." + namespaceMap.get(i));

			i--;
		}

		return nameBuilder.toString();
	}

}
