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
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Role;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Form Action</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getDescription <em>Description</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getForm <em>Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getPanel <em>Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getTargetForm <em>Target Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getBoundaryMethod <em>Boundary Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getType <em>Action Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl#getLabel <em>Label</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FormActionImpl extends EObjectImpl implements FormAction {
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
	 * The default value of the '{@link #getDescription() <em>Description</em>}' attribute
	 * @see #getDescription()
	 * @generated
	 * @ordered
	 */
	protected static final String DESCRIPTION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDescription() <em>Description</em>}' attribute
	 * @see #getDescription()
	 * @generated
	 * @ordered
	 */
	protected String description = DESCRIPTION_EDEFAULT;

	/**
	 * The cached value of the '{@link #getTargetForm() <em>Target Form</em>}' reference
	 * @see #getTargetForm()
	 * @generated
	 * @ordered
	 */
	protected Form targetForm;

	/**
	 * The cached value of the '{@link #getBoundaryMethod() <em>Boundary Method</em>}' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod boundaryMethod;

	/**
	 * The cached value of the '{@link #getRoles() <em>Roles</em>}' reference list
	 * @see #getRoles()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> roles;

	/**
	 * The default value of the '{@link #getType() <em>Action Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final ActionType TYPE_EDEFAULT = ActionType.CREATE;

	/**
	 * The cached value of the '{@link #getType() <em>Action Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected ActionType type = TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected String label = LABEL_EDEFAULT;

	/**
	 * @generated
	 */
	protected FormActionImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.FORM_ACTION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getName()
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getDescription()
	 * @generated
	 */
	@Override
	public String getDescription() {
		return description;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setDescription(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDescription(String newDescription) {
		final String oldDescription = description;
		description = newDescription;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__DESCRIPTION, oldDescription, description));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getForm()
	 * @generated
	 */
	@Override
	public Form getForm() {
		if (eContainerFeatureID() != ClientPackage.FORM_ACTION__FORM)
			return null;

		return (Form) eInternalContainer();
	}

	/**
	 * @param newForm
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetForm(Form newForm, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newForm, ClientPackage.FORM_ACTION__FORM, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setForm(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setForm(Form newForm) {
		if (newForm != eInternalContainer() || (eContainerFeatureID() != ClientPackage.FORM_ACTION__FORM && newForm != null)) {
			if (EcoreUtil.isAncestor(this, newForm))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newForm != null)
				msgs = ((InternalEObject) newForm).eInverseAdd(this, ClientPackage.FORM__ACTIONS, Form.class, msgs);

			msgs = basicSetForm(newForm, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__FORM, newForm, newForm));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getPanel()
	 * @generated
	 */
	@Override
	public FormPanel getPanel() {
		if (eContainerFeatureID() != ClientPackage.FORM_ACTION__PANEL)
			return null;

		return (FormPanel) eInternalContainer();
	}

	/**
	 * @param newPanel
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetPanel(FormPanel newPanel, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newPanel, ClientPackage.FORM_ACTION__PANEL, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 * @generated
	 */
	@Override
	public void setPanel(FormPanel newPanel) {
		if (newPanel != eInternalContainer() || (eContainerFeatureID() != ClientPackage.FORM_ACTION__PANEL && newPanel != null)) {
			if (EcoreUtil.isAncestor(this, newPanel))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newPanel != null)
				msgs = ((InternalEObject) newPanel).eInverseAdd(this, ClientPackage.FORM_PANEL__ACTIONS, FormPanel.class, msgs);

			msgs = basicSetPanel(newPanel, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__PANEL, newPanel, newPanel));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getTargetForm()
	 * @generated
	 */
	@Override
	public Form getTargetForm() {
		if (targetForm != null && targetForm.eIsProxy()) {
			final var oldTargetForm = (InternalEObject) targetForm;
			targetForm = (Form) eResolveProxy(oldTargetForm);

			if (targetForm != oldTargetForm && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_ACTION__TARGET_FORM, oldTargetForm, targetForm));
		}

		return targetForm;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Form basicGetTargetForm() {
		return targetForm;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setTargetForm(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setTargetForm(Form newTargetForm) {
		final Form oldTargetForm = targetForm;
		targetForm = newTargetForm;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__TARGET_FORM, oldTargetForm, targetForm));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getBoundaryMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getBoundaryMethod() {
		if (boundaryMethod != null && boundaryMethod.eIsProxy()) {
			final var oldBoundaryMethod = (InternalEObject) boundaryMethod;
			boundaryMethod = (BoundaryMethod) eResolveProxy(oldBoundaryMethod);

			if (boundaryMethod != oldBoundaryMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_ACTION__BOUNDARY_METHOD, oldBoundaryMethod,
						boundaryMethod));
		}

		return boundaryMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public BoundaryMethod basicGetBoundaryMethod() {
		return boundaryMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setBoundaryMethod(net.codecadenza.eclipse.model.boundary.BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setBoundaryMethod(BoundaryMethod newBoundaryMethod) {
		final BoundaryMethod oldBoundaryMethod = boundaryMethod;
		boundaryMethod = newBoundaryMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__BOUNDARY_METHOD, oldBoundaryMethod,
					boundaryMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getRoles()
	 * @generated
	 */
	@Override
	public EList<Role> getRoles() {
		if (roles == null)
			roles = new EObjectResolvingEList<>(Role.class, this, ClientPackage.FORM_ACTION__ROLES);

		return roles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getType()
	 */
	@Override
	public ActionType getType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setType(net.codecadenza.eclipse.model.client.ActionType)
	 * @generated
	 */
	@Override
	public void setType(ActionType newType) {
		final ActionType oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__TYPE, oldType, type));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormAction#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_ACTION__LABEL, oldLabel, label));
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
			case ClientPackage.FORM_ACTION__FORM:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetForm((Form) otherEnd, msgs);
			case ClientPackage.FORM_ACTION__PANEL:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetPanel((FormPanel) otherEnd, msgs);
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
			case ClientPackage.FORM_ACTION__FORM:
				return basicSetForm(null, msgs);
			case ClientPackage.FORM_ACTION__PANEL:
				return basicSetPanel(null, msgs);
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
			case ClientPackage.FORM_ACTION__FORM:
				return eInternalContainer().eInverseRemove(this, ClientPackage.FORM__ACTIONS, Form.class, msgs);
			case ClientPackage.FORM_ACTION__PANEL:
				return eInternalContainer().eInverseRemove(this, ClientPackage.FORM_PANEL__ACTIONS, FormPanel.class, msgs);
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
			case ClientPackage.FORM_ACTION__NAME:
				return getName();
			case ClientPackage.FORM_ACTION__DESCRIPTION:
				return getDescription();
			case ClientPackage.FORM_ACTION__FORM:
				return getForm();
			case ClientPackage.FORM_ACTION__PANEL:
				return getPanel();
			case ClientPackage.FORM_ACTION__TARGET_FORM:
				if (resolve)
					return getTargetForm();

				return basicGetTargetForm();
			case ClientPackage.FORM_ACTION__BOUNDARY_METHOD:
				if (resolve)
					return getBoundaryMethod();

				return basicGetBoundaryMethod();
			case ClientPackage.FORM_ACTION__ROLES:
				return getRoles();
			case ClientPackage.FORM_ACTION__TYPE:
				return getType();
			case ClientPackage.FORM_ACTION__LABEL:
				return getLabel();
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
			case ClientPackage.FORM_ACTION__NAME:
				setName((String) newValue);
				return;
			case ClientPackage.FORM_ACTION__DESCRIPTION:
				setDescription((String) newValue);
				return;
			case ClientPackage.FORM_ACTION__FORM:
				setForm((Form) newValue);
				return;
			case ClientPackage.FORM_ACTION__PANEL:
				setPanel((FormPanel) newValue);
				return;
			case ClientPackage.FORM_ACTION__TARGET_FORM:
				setTargetForm((Form) newValue);
				return;
			case ClientPackage.FORM_ACTION__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) newValue);
				return;
			case ClientPackage.FORM_ACTION__ROLES:
				getRoles().clear();
				getRoles().addAll((Collection<? extends Role>) newValue);
				return;
			case ClientPackage.FORM_ACTION__TYPE:
				setType((ActionType) newValue);
				return;
			case ClientPackage.FORM_ACTION__LABEL:
				setLabel((String) newValue);
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
			case ClientPackage.FORM_ACTION__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ClientPackage.FORM_ACTION__DESCRIPTION:
				setDescription(DESCRIPTION_EDEFAULT);
				return;
			case ClientPackage.FORM_ACTION__FORM:
				setForm((Form) null);
				return;
			case ClientPackage.FORM_ACTION__PANEL:
				setPanel((FormPanel) null);
				return;
			case ClientPackage.FORM_ACTION__TARGET_FORM:
				setTargetForm((Form) null);
				return;
			case ClientPackage.FORM_ACTION__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) null);
				return;
			case ClientPackage.FORM_ACTION__ROLES:
				getRoles().clear();
				return;
			case ClientPackage.FORM_ACTION__TYPE:
				setType(TYPE_EDEFAULT);
				return;
			case ClientPackage.FORM_ACTION__LABEL:
				setLabel(LABEL_EDEFAULT);
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
			case ClientPackage.FORM_ACTION__NAME:
				return name != null;
			case ClientPackage.FORM_ACTION__DESCRIPTION:
				return description != null;
			case ClientPackage.FORM_ACTION__FORM:
				return getForm() != null;
			case ClientPackage.FORM_ACTION__PANEL:
				return getPanel() != null;
			case ClientPackage.FORM_ACTION__TARGET_FORM:
				return targetForm != null;
			case ClientPackage.FORM_ACTION__BOUNDARY_METHOD:
				return boundaryMethod != null;
			case ClientPackage.FORM_ACTION__ROLES:
				return roles != null && !roles.isEmpty();
			case ClientPackage.FORM_ACTION__TYPE:
				return type != TYPE_EDEFAULT;
			case ClientPackage.FORM_ACTION__LABEL:
				return label != null;
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
		result.append(", description: ");
		result.append(description);
		result.append(", type: ");
		result.append(type);
		result.append(", label: ");
		result.append(label);
		result.append(')');

		return result.toString();
	}

}
