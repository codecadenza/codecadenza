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
package net.codecadenza.eclipse.model.client.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.Role;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EObjectWithInverseResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Form Group</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getGroupOrder <em>Group Order</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getParentGroup <em>Parent Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getChildGroups <em>Child Groups</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getForms <em>Forms</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getPanels <em>Panels</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl#getProject <em>Project</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FormGroupImpl extends EObjectImpl implements FormGroup {
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
	 * The default value of the '{@link #getGroupOrder() <em>Group Order</em>}' attribute
	 * @see #getGroupOrder()
	 * @generated
	 * @ordered
	 */
	protected static final int GROUP_ORDER_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getGroupOrder() <em>Group Order</em>}' attribute
	 * @see #getGroupOrder()
	 * @generated
	 * @ordered
	 */
	protected int groupOrder = GROUP_ORDER_EDEFAULT;

	/**
	 * The cached value of the '{@link #getParentGroup() <em>Parent Group</em>}' reference
	 * @see #getParentGroup()
	 * @generated
	 * @ordered
	 */
	protected FormGroup parentGroup;

	/**
	 * The cached value of the '{@link #getChildGroups() <em>Child Groups</em>}' containment reference list
	 * @see #getChildGroups()
	 * @generated
	 * @ordered
	 */
	protected EList<FormGroup> childGroups;

	/**
	 * The cached value of the '{@link #getForms() <em>Forms</em>}' reference list
	 * @see #getForms()
	 * @generated
	 * @ordered
	 */
	protected EList<Form> forms;

	/**
	 * The cached value of the '{@link #getPanels() <em>Panels</em>}' reference list
	 * @see #getPanels()
	 * @generated
	 * @ordered
	 */
	protected EList<FormPanel> panels;

	/**
	 * The cached value of the '{@link #getRoles() <em>Roles</em>}' reference list
	 * @see #getRoles()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> roles;

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
	protected FormGroupImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.FORM_GROUP;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_GROUP__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getGroupOrder()
	 * @generated
	 */
	@Override
	public int getGroupOrder() {
		return groupOrder;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#setGroupOrder(int)
	 * @generated
	 */
	@Override
	public void setGroupOrder(int newGroupOrder) {
		final int oldGroupOrder = groupOrder;
		groupOrder = newGroupOrder;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_GROUP__GROUP_ORDER, oldGroupOrder, groupOrder));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getParentGroup()
	 * @generated
	 */
	@Override
	public FormGroup getParentGroup() {
		if (parentGroup != null && parentGroup.eIsProxy()) {
			final var oldParentGroup = (InternalEObject) parentGroup;
			parentGroup = (FormGroup) eResolveProxy(oldParentGroup);

			if (parentGroup != oldParentGroup && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_GROUP__PARENT_GROUP, oldParentGroup,
						parentGroup));
		}

		return parentGroup;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public FormGroup basicGetParentGroup() {
		return parentGroup;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#setParentGroup(net.codecadenza.eclipse.model.client.FormGroup)
	 * @generated
	 */
	@Override
	public void setParentGroup(FormGroup newParentGroup) {
		final FormGroup oldParentGroup = parentGroup;
		parentGroup = newParentGroup;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_GROUP__PARENT_GROUP, oldParentGroup, parentGroup));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getChildGroups()
	 */
	@Override
	public EList<FormGroup> getChildGroups() {
		if (childGroups == null)
			childGroups = new EObjectContainmentEList<>(FormGroup.class, this, ClientPackage.FORM_GROUP__CHILD_GROUPS);

		return childGroups;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getForms()
	 * @generated
	 */
	@Override
	public EList<Form> getForms() {
		if (forms == null)
			forms = new EObjectWithInverseResolvingEList<>(Form.class, this, ClientPackage.FORM_GROUP__FORMS,
					ClientPackage.FORM__FORM_GROUP);

		return forms;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getPanels()
	 * @generated
	 */
	@Override
	public EList<FormPanel> getPanels() {
		if (panels == null)
			panels = new EObjectWithInverseResolvingEList<>(FormPanel.class, this, ClientPackage.FORM_GROUP__PANELS,
					ClientPackage.FORM_PANEL__FORM_GROUP);

		return panels;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getRoles()
	 * @generated
	 */
	@Override
	public EList<Role> getRoles() {
		if (roles == null)
			roles = new EObjectResolvingEList<>(Role.class, this, ClientPackage.FORM_GROUP__ROLES);

		return roles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getProject()
	 * @generated
	 */
	@Override
	public Project getProject() {
		if (project != null && project.eIsProxy()) {
			final var oldProject = (InternalEObject) project;
			project = (Project) eResolveProxy(oldProject);

			if (project != oldProject && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_GROUP__PROJECT, oldProject, project));
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

	/**
	 * @param newProject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetProject(Project newProject, NotificationChain msgs) {
		final Project oldProject = project;
		project = newProject;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_GROUP__PROJECT, oldProject,
					newProject);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#setProject(net.codecadenza.eclipse.model.project.Project)
	 * @generated
	 */
	@Override
	public void setProject(Project newProject) {
		if (newProject != project) {
			NotificationChain msgs = null;

			if (project != null)
				msgs = ((InternalEObject) project).eInverseRemove(this, ProjectPackage.PROJECT__FORM_GROUPS, Project.class, msgs);

			if (newProject != null)
				msgs = ((InternalEObject) newProject).eInverseAdd(this, ProjectPackage.PROJECT__FORM_GROUPS, Project.class, msgs);

			msgs = basicSetProject(newProject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_GROUP__PROJECT, newProject, newProject));
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
			case ClientPackage.FORM_GROUP__FORMS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getForms()).basicAdd(otherEnd, msgs);
			case ClientPackage.FORM_GROUP__PANELS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getPanels()).basicAdd(otherEnd, msgs);
			case ClientPackage.FORM_GROUP__PROJECT:
				if (project != null)
					msgs = ((InternalEObject) project).eInverseRemove(this, ProjectPackage.PROJECT__FORM_GROUPS, Project.class, msgs);

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
			case ClientPackage.FORM_GROUP__CHILD_GROUPS:
				return ((InternalEList<?>) getChildGroups()).basicRemove(otherEnd, msgs);
			case ClientPackage.FORM_GROUP__FORMS:
				return ((InternalEList<?>) getForms()).basicRemove(otherEnd, msgs);
			case ClientPackage.FORM_GROUP__PANELS:
				return ((InternalEList<?>) getPanels()).basicRemove(otherEnd, msgs);
			case ClientPackage.FORM_GROUP__PROJECT:
				return basicSetProject(null, msgs);
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
			case ClientPackage.FORM_GROUP__NAME:
				return getName();
			case ClientPackage.FORM_GROUP__GROUP_ORDER:
				return getGroupOrder();
			case ClientPackage.FORM_GROUP__PARENT_GROUP:
				if (resolve)
					return getParentGroup();

				return basicGetParentGroup();
			case ClientPackage.FORM_GROUP__CHILD_GROUPS:
				return getChildGroups();
			case ClientPackage.FORM_GROUP__FORMS:
				return getForms();
			case ClientPackage.FORM_GROUP__PANELS:
				return getPanels();
			case ClientPackage.FORM_GROUP__ROLES:
				return getRoles();
			case ClientPackage.FORM_GROUP__PROJECT:
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
			case ClientPackage.FORM_GROUP__NAME:
				setName((String) newValue);
				return;
			case ClientPackage.FORM_GROUP__GROUP_ORDER:
				setGroupOrder((Integer) newValue);
				return;
			case ClientPackage.FORM_GROUP__PARENT_GROUP:
				setParentGroup((FormGroup) newValue);
				return;
			case ClientPackage.FORM_GROUP__CHILD_GROUPS:
				getChildGroups().clear();
				getChildGroups().addAll((Collection<? extends FormGroup>) newValue);
				return;
			case ClientPackage.FORM_GROUP__FORMS:
				getForms().clear();
				getForms().addAll((Collection<? extends Form>) newValue);
				return;
			case ClientPackage.FORM_GROUP__PANELS:
				getPanels().clear();
				getPanels().addAll((Collection<? extends FormPanel>) newValue);
				return;
			case ClientPackage.FORM_GROUP__ROLES:
				getRoles().clear();
				getRoles().addAll((Collection<? extends Role>) newValue);
				return;
			case ClientPackage.FORM_GROUP__PROJECT:
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
			case ClientPackage.FORM_GROUP__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ClientPackage.FORM_GROUP__GROUP_ORDER:
				setGroupOrder(GROUP_ORDER_EDEFAULT);
				return;
			case ClientPackage.FORM_GROUP__PARENT_GROUP:
				setParentGroup((FormGroup) null);
				return;
			case ClientPackage.FORM_GROUP__CHILD_GROUPS:
				getChildGroups().clear();
				return;
			case ClientPackage.FORM_GROUP__FORMS:
				getForms().clear();
				return;
			case ClientPackage.FORM_GROUP__PANELS:
				getPanels().clear();
				return;
			case ClientPackage.FORM_GROUP__ROLES:
				getRoles().clear();
				return;
			case ClientPackage.FORM_GROUP__PROJECT:
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
			case ClientPackage.FORM_GROUP__NAME:
				return name != null;
			case ClientPackage.FORM_GROUP__GROUP_ORDER:
				return groupOrder != GROUP_ORDER_EDEFAULT;
			case ClientPackage.FORM_GROUP__PARENT_GROUP:
				return parentGroup != null;
			case ClientPackage.FORM_GROUP__CHILD_GROUPS:
				return childGroups != null && !childGroups.isEmpty();
			case ClientPackage.FORM_GROUP__FORMS:
				return forms != null && !forms.isEmpty();
			case ClientPackage.FORM_GROUP__PANELS:
				return panels != null && !panels.isEmpty();
			case ClientPackage.FORM_GROUP__ROLES:
				return roles != null && !roles.isEmpty();
			case ClientPackage.FORM_GROUP__PROJECT:
				return project != null;
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
		result.append(", groupOrder: ");
		result.append(groupOrder);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormGroup#findProject()
	 * @generated not
	 */
	@Override
	public Project findProject() {
		FormGroup currentGroup = this;

		while (currentGroup.getProject() == null)
			currentGroup = currentGroup.getParentGroup();

		return currentGroup.getProject();
	}

}
