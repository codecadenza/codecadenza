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

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_PAGE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.PACK_PAGE_OBJECT;
import static net.codecadenza.eclipse.shared.Constants.UI_DIALOG_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_LOV_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_TREE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_VIEW_FOLDER;

import java.util.Collection;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaTestFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EObjectWithInverseResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Form</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getFormType <em>Form Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getFormGroup <em>Form Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getTitle <em>Title</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#isModal <em>Modal</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#isResizable <em>Resizable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#isTitleArea <em>Title Area</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#isOpenEditAfterCreate <em>Open Edit After Create</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getHeight <em>Height</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getWidth <em>Width</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getFormPanels <em>Form Panels</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getActions <em>Actions</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getDTO <em>DTO</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getRoles <em>Roles</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormImpl#getBoundaryMethod <em>Boundary Method</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FormImpl extends EObjectImpl implements Form {
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
	 * The default value of the '{@link #getFormType() <em>Form Type</em>}' attribute
	 * @see #getFormType()
	 * @generated
	 * @ordered
	 */
	protected static final FormTypeEnumeration FORM_TYPE_EDEFAULT = FormTypeEnumeration.UPDATE;

	/**
	 * The cached value of the '{@link #getFormType() <em>Form Type</em>}' attribute
	 * @see #getFormType()
	 * @generated
	 * @ordered
	 */
	protected FormTypeEnumeration formType = FORM_TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getFormGroup() <em>Form Group</em>}' reference
	 * @see #getFormGroup()
	 * @generated
	 * @ordered
	 */
	protected FormGroup formGroup;

	/**
	 * The default value of the '{@link #getTitle() <em>Title</em>}' attribute
	 * @see #getTitle()
	 * @generated
	 * @ordered
	 */
	protected static final String TITLE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTitle() <em>Title</em>}' attribute
	 * @see #getTitle()
	 * @generated
	 * @ordered
	 */
	protected String title = TITLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isModal() <em>Modal</em>}' attribute
	 * @see #isModal()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MODAL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isModal() <em>Modal</em>}' attribute
	 * @see #isModal()
	 * @generated
	 * @ordered
	 */
	protected boolean modal = MODAL_EDEFAULT;

	/**
	 * The default value of the '{@link #isResizable() <em>Resizable</em>}' attribute
	 * @see #isResizable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean RESIZABLE_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isResizable() <em>Resizable</em>}' attribute
	 * @see #isResizable()
	 * @generated
	 * @ordered
	 */
	protected boolean resizable = RESIZABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isTitleArea() <em>Title Area</em>}' attribute
	 * @see #isTitleArea()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TITLE_AREA_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isTitleArea() <em>Title Area</em>}' attribute
	 * @see #isTitleArea()
	 * @generated
	 * @ordered
	 */
	protected boolean titleArea = TITLE_AREA_EDEFAULT;

	/**
	 * The default value of the '{@link #isOpenEditAfterCreate() <em>Open Edit After Create</em>}' attribute
	 * @see #isOpenEditAfterCreate()
	 * @generated
	 * @ordered
	 */
	protected static final boolean OPEN_EDIT_AFTER_CREATE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isOpenEditAfterCreate() <em>Open Edit After Create</em>}' attribute
	 * @see #isOpenEditAfterCreate()
	 * @generated
	 * @ordered
	 */
	protected boolean openEditAfterCreate = OPEN_EDIT_AFTER_CREATE_EDEFAULT;

	/**
	 * The default value of the '{@link #getHeight() <em>Height</em>}' attribute
	 * @see #getHeight()
	 * @generated
	 * @ordered
	 */
	protected static final int HEIGHT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getHeight() <em>Height</em>}' attribute
	 * @see #getHeight()
	 * @generated
	 * @ordered
	 */
	protected int height = HEIGHT_EDEFAULT;

	/**
	 * The default value of the '{@link #getWidth() <em>Width</em>}' attribute
	 * @see #getWidth()
	 * @generated
	 * @ordered
	 */
	protected static final int WIDTH_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getWidth() <em>Width</em>}' attribute
	 * @see #getWidth()
	 * @generated
	 * @ordered
	 */
	protected int width = WIDTH_EDEFAULT;

	/**
	 * The cached value of the '{@link #getFormPanels() <em>Form Panels</em>}' reference list
	 * @see #getFormPanels()
	 * @generated
	 * @ordered
	 */
	protected EList<FormPanel> formPanels;

	/**
	 * The cached value of the '{@link #getActions() <em>Actions</em>}' containment reference list
	 * @see #getActions()
	 * @generated
	 * @ordered
	 */
	protected EList<FormAction> actions;

	/**
	 * The cached value of the '{@link #getDTO() <em>DTO</em>}' reference
	 * @see #getDTO()
	 * @generated
	 * @ordered
	 */
	protected DTOBean dTO;

	/**
	 * The cached value of the '{@link #getDomainObject() <em>Domain Object</em>}' reference
	 * @see #getDomainObject()
	 * @generated
	 * @ordered
	 */
	protected DomainObject domainObject;

	/**
	 * The cached value of the '{@link #getRoles() <em>Roles</em>}' reference list
	 * @see #getRoles()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> roles;

	/**
	 * The cached value of the '{@link #getBoundaryMethod() <em>Boundary Method</em>}' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod boundaryMethod;

	/**
	 * @generated
	 */
	protected FormImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.FORM;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getFormType()
	 * @generated
	 */
	@Override
	public FormTypeEnumeration getFormType() {
		return formType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setFormType(net.codecadenza.eclipse.model.client.FormTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setFormType(FormTypeEnumeration newFormType) {
		final FormTypeEnumeration oldFormType = formType;
		formType = newFormType == null ? FORM_TYPE_EDEFAULT : newFormType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__FORM_TYPE, oldFormType, formType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getFormGroup()
	 * @generated
	 */
	@Override
	public FormGroup getFormGroup() {
		if (formGroup != null && formGroup.eIsProxy()) {
			final var oldFormGroup = (InternalEObject) formGroup;
			formGroup = (FormGroup) eResolveProxy(oldFormGroup);

			if (formGroup != oldFormGroup && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM__FORM_GROUP, oldFormGroup, formGroup));
		}

		return formGroup;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public FormGroup basicGetFormGroup() {
		return formGroup;
	}

	/**
	 * @param newFormGroup
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetFormGroup(FormGroup newFormGroup, NotificationChain msgs) {
		final FormGroup oldFormGroup = formGroup;
		formGroup = newFormGroup;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__FORM_GROUP, oldFormGroup,
					newFormGroup);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setFormGroup(net.codecadenza.eclipse.model.client.FormGroup)
	 * @generated
	 */
	@Override
	public void setFormGroup(FormGroup newFormGroup) {
		if (newFormGroup != formGroup) {
			NotificationChain msgs = null;

			if (formGroup != null)
				msgs = ((InternalEObject) formGroup).eInverseRemove(this, ClientPackage.FORM_GROUP__FORMS, FormGroup.class, msgs);

			if (newFormGroup != null)
				msgs = ((InternalEObject) newFormGroup).eInverseAdd(this, ClientPackage.FORM_GROUP__FORMS, FormGroup.class, msgs);

			msgs = basicSetFormGroup(newFormGroup, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__FORM_GROUP, newFormGroup, newFormGroup));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getTitle()
	 * @generated
	 */
	@Override
	public String getTitle() {
		return title;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setTitle(java.lang.String)
	 * @generated
	 */
	@Override
	public void setTitle(String newTitle) {
		final String oldTitle = title;
		title = newTitle;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__TITLE, oldTitle, title));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#isModal()
	 * @generated
	 */
	@Override
	public boolean isModal() {
		return modal;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setModal(boolean)
	 * @generated
	 */
	@Override
	public void setModal(boolean newModal) {
		final boolean oldModal = modal;
		modal = newModal;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__MODAL, oldModal, modal));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#isResizable()
	 * @generated
	 */
	@Override
	public boolean isResizable() {
		return resizable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setResizable(boolean)
	 * @generated
	 */
	@Override
	public void setResizable(boolean newResizable) {
		final boolean oldResizable = resizable;
		resizable = newResizable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__RESIZABLE, oldResizable, resizable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#isTitleArea()
	 * @generated
	 */
	@Override
	public boolean isTitleArea() {
		return titleArea;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setTitleArea(boolean)
	 * @generated
	 */
	@Override
	public void setTitleArea(boolean newTitleArea) {
		final boolean oldTitleArea = titleArea;
		titleArea = newTitleArea;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__TITLE_AREA, oldTitleArea, titleArea));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#isOpenEditAfterCreate()
	 * @generated
	 */
	@Override
	public boolean isOpenEditAfterCreate() {
		return openEditAfterCreate;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setOpenEditAfterCreate(boolean)
	 * @generated
	 */
	@Override
	public void setOpenEditAfterCreate(boolean newOpenEditAfterCreate) {
		final boolean oldOpenEditAfterCreate = openEditAfterCreate;
		openEditAfterCreate = newOpenEditAfterCreate;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__OPEN_EDIT_AFTER_CREATE, oldOpenEditAfterCreate,
					openEditAfterCreate));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getHeight()
	 * @generated
	 */
	@Override
	public int getHeight() {
		return height;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setHeight(int)
	 * @generated
	 */
	@Override
	public void setHeight(int newHeight) {
		final int oldHeight = height;
		height = newHeight;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__HEIGHT, oldHeight, height));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getWidth()
	 * @generated
	 */
	@Override
	public int getWidth() {
		return width;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setWidth(int)
	 * @generated
	 */
	@Override
	public void setWidth(int newWidth) {
		final int oldWidth = width;
		width = newWidth;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__WIDTH, oldWidth, width));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getFormPanels()
	 * @generated
	 */
	@Override
	public EList<FormPanel> getFormPanels() {
		if (formPanels == null)
			formPanels = new EObjectWithInverseResolvingEList<>(FormPanel.class, this, ClientPackage.FORM__FORM_PANELS,
					ClientPackage.FORM_PANEL__FORM);

		return formPanels;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getActions()
	 * @generated
	 */
	@Override
	public EList<FormAction> getActions() {
		if (actions == null)
			actions = new EObjectContainmentWithInverseEList<>(FormAction.class, this, ClientPackage.FORM__ACTIONS,
					ClientPackage.FORM_ACTION__FORM);

		return actions;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getDTO()
	 * @generated
	 */
	@Override
	public DTOBean getDTO() {
		if (dTO != null && dTO.eIsProxy()) {
			final var oldDTO = (InternalEObject) dTO;
			dTO = (DTOBean) eResolveProxy(oldDTO);

			if (dTO != oldDTO && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM__DTO, oldDTO, dTO));
		}

		return dTO;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DTOBean basicGetDTO() {
		return dTO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#setDTO(net.codecadenza.eclipse.model.dto.DTOBean)
	 * @generated
	 */
	@Override
	public void setDTO(DTOBean newDTO) {
		final DTOBean oldDTO = dTO;
		dTO = newDTO;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__DTO, oldDTO, dTO));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getDomainObject()
	 * @generated
	 */
	@Override
	public DomainObject getDomainObject() {
		if (domainObject != null && domainObject.eIsProxy()) {
			final var oldDomainObject = (InternalEObject) domainObject;
			domainObject = (DomainObject) eResolveProxy(oldDomainObject);

			if (domainObject != oldDomainObject && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM__DOMAIN_OBJECT, oldDomainObject, domainObject));
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
	 * @see net.codecadenza.eclipse.model.client.Form#setDomainObject(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setDomainObject(DomainObject newDomainObject) {
		final DomainObject oldDomainObject = domainObject;
		domainObject = newDomainObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__DOMAIN_OBJECT, oldDomainObject, domainObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getRoles()
	 * @generated
	 */
	@Override
	public EList<Role> getRoles() {
		if (roles == null)
			roles = new EObjectResolvingEList<>(Role.class, this, ClientPackage.FORM__ROLES);

		return roles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getBoundaryMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getBoundaryMethod() {
		if (boundaryMethod != null && boundaryMethod.eIsProxy()) {
			final var oldBoundaryMethod = (InternalEObject) boundaryMethod;
			boundaryMethod = (BoundaryMethod) eResolveProxy(oldBoundaryMethod);

			if (boundaryMethod != oldBoundaryMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM__BOUNDARY_METHOD, oldBoundaryMethod,
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
	 * @see net.codecadenza.eclipse.model.client.Form#setBoundaryMethod(net.codecadenza.eclipse.model.boundary.BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setBoundaryMethod(BoundaryMethod newBoundaryMethod) {
		final BoundaryMethod oldBoundaryMethod = boundaryMethod;
		boundaryMethod = newBoundaryMethod;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.FORM__BOUNDARY_METHOD, oldBoundaryMethod, boundaryMethod));
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
			case ClientPackage.FORM__FORM_GROUP:
				if (formGroup != null)
					msgs = ((InternalEObject) formGroup).eInverseRemove(this, ClientPackage.FORM_GROUP__FORMS, FormGroup.class, msgs);

				return basicSetFormGroup((FormGroup) otherEnd, msgs);
			case ClientPackage.FORM__FORM_PANELS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getFormPanels()).basicAdd(otherEnd, msgs);
			case ClientPackage.FORM__ACTIONS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getActions()).basicAdd(otherEnd, msgs);
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
			case ClientPackage.FORM__FORM_GROUP:
				return basicSetFormGroup(null, msgs);
			case ClientPackage.FORM__FORM_PANELS:
				return ((InternalEList<?>) getFormPanels()).basicRemove(otherEnd, msgs);
			case ClientPackage.FORM__ACTIONS:
				return ((InternalEList<?>) getActions()).basicRemove(otherEnd, msgs);
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
			case ClientPackage.FORM__NAME:
				return getName();
			case ClientPackage.FORM__FORM_TYPE:
				return getFormType();
			case ClientPackage.FORM__FORM_GROUP:
				if (resolve)
					return getFormGroup();

				return basicGetFormGroup();
			case ClientPackage.FORM__TITLE:
				return getTitle();
			case ClientPackage.FORM__MODAL:
				return isModal();
			case ClientPackage.FORM__RESIZABLE:
				return isResizable();
			case ClientPackage.FORM__TITLE_AREA:
				return isTitleArea();
			case ClientPackage.FORM__OPEN_EDIT_AFTER_CREATE:
				return isOpenEditAfterCreate();
			case ClientPackage.FORM__HEIGHT:
				return getHeight();
			case ClientPackage.FORM__WIDTH:
				return getWidth();
			case ClientPackage.FORM__FORM_PANELS:
				return getFormPanels();
			case ClientPackage.FORM__ACTIONS:
				return getActions();
			case ClientPackage.FORM__DTO:
				if (resolve)
					return getDTO();

				return basicGetDTO();
			case ClientPackage.FORM__DOMAIN_OBJECT:
				if (resolve)
					return getDomainObject();

				return basicGetDomainObject();
			case ClientPackage.FORM__ROLES:
				return getRoles();
			case ClientPackage.FORM__BOUNDARY_METHOD:
				if (resolve)
					return getBoundaryMethod();

				return basicGetBoundaryMethod();
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
			case ClientPackage.FORM__NAME:
				setName((String) newValue);
				return;
			case ClientPackage.FORM__FORM_TYPE:
				setFormType((FormTypeEnumeration) newValue);
				return;
			case ClientPackage.FORM__FORM_GROUP:
				setFormGroup((FormGroup) newValue);
				return;
			case ClientPackage.FORM__TITLE:
				setTitle((String) newValue);
				return;
			case ClientPackage.FORM__MODAL:
				setModal((Boolean) newValue);
				return;
			case ClientPackage.FORM__RESIZABLE:
				setResizable((Boolean) newValue);
				return;
			case ClientPackage.FORM__TITLE_AREA:
				setTitleArea((Boolean) newValue);
				return;
			case ClientPackage.FORM__OPEN_EDIT_AFTER_CREATE:
				setOpenEditAfterCreate((Boolean) newValue);
				return;
			case ClientPackage.FORM__HEIGHT:
				setHeight((Integer) newValue);
				return;
			case ClientPackage.FORM__WIDTH:
				setWidth((Integer) newValue);
				return;
			case ClientPackage.FORM__FORM_PANELS:
				getFormPanels().clear();
				getFormPanels().addAll((Collection<? extends FormPanel>) newValue);
				return;
			case ClientPackage.FORM__ACTIONS:
				getActions().clear();
				getActions().addAll((Collection<? extends FormAction>) newValue);
				return;
			case ClientPackage.FORM__DTO:
				setDTO((DTOBean) newValue);
				return;
			case ClientPackage.FORM__DOMAIN_OBJECT:
				setDomainObject((DomainObject) newValue);
				return;
			case ClientPackage.FORM__ROLES:
				getRoles().clear();
				getRoles().addAll((Collection<? extends Role>) newValue);
				return;
			case ClientPackage.FORM__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) newValue);
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
			case ClientPackage.FORM__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ClientPackage.FORM__FORM_TYPE:
				setFormType(FORM_TYPE_EDEFAULT);
				return;
			case ClientPackage.FORM__FORM_GROUP:
				setFormGroup((FormGroup) null);
				return;
			case ClientPackage.FORM__TITLE:
				setTitle(TITLE_EDEFAULT);
				return;
			case ClientPackage.FORM__MODAL:
				setModal(MODAL_EDEFAULT);
				return;
			case ClientPackage.FORM__RESIZABLE:
				setResizable(RESIZABLE_EDEFAULT);
				return;
			case ClientPackage.FORM__TITLE_AREA:
				setTitleArea(TITLE_AREA_EDEFAULT);
				return;
			case ClientPackage.FORM__OPEN_EDIT_AFTER_CREATE:
				setOpenEditAfterCreate(OPEN_EDIT_AFTER_CREATE_EDEFAULT);
				return;
			case ClientPackage.FORM__HEIGHT:
				setHeight(HEIGHT_EDEFAULT);
				return;
			case ClientPackage.FORM__WIDTH:
				setWidth(WIDTH_EDEFAULT);
				return;
			case ClientPackage.FORM__FORM_PANELS:
				getFormPanels().clear();
				return;
			case ClientPackage.FORM__ACTIONS:
				getActions().clear();
				return;
			case ClientPackage.FORM__DTO:
				setDTO((DTOBean) null);
				return;
			case ClientPackage.FORM__DOMAIN_OBJECT:
				setDomainObject((DomainObject) null);
				return;
			case ClientPackage.FORM__ROLES:
				getRoles().clear();
				return;
			case ClientPackage.FORM__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) null);
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
			case ClientPackage.FORM__NAME:
				return name != null;
			case ClientPackage.FORM__FORM_TYPE:
				return formType != FORM_TYPE_EDEFAULT;
			case ClientPackage.FORM__FORM_GROUP:
				return formGroup != null;
			case ClientPackage.FORM__TITLE:
				return title != null;
			case ClientPackage.FORM__MODAL:
				return modal != MODAL_EDEFAULT;
			case ClientPackage.FORM__RESIZABLE:
				return resizable != RESIZABLE_EDEFAULT;
			case ClientPackage.FORM__TITLE_AREA:
				return titleArea != TITLE_AREA_EDEFAULT;
			case ClientPackage.FORM__OPEN_EDIT_AFTER_CREATE:
				return openEditAfterCreate != OPEN_EDIT_AFTER_CREATE_EDEFAULT;
			case ClientPackage.FORM__HEIGHT:
				return height != HEIGHT_EDEFAULT;
			case ClientPackage.FORM__WIDTH:
				return width != WIDTH_EDEFAULT;
			case ClientPackage.FORM__FORM_PANELS:
				return formPanels != null && !formPanels.isEmpty();
			case ClientPackage.FORM__ACTIONS:
				return actions != null && !actions.isEmpty();
			case ClientPackage.FORM__DTO:
				return dTO != null;
			case ClientPackage.FORM__DOMAIN_OBJECT:
				return domainObject != null;
			case ClientPackage.FORM__ROLES:
				return roles != null && !roles.isEmpty();
			case ClientPackage.FORM__BOUNDARY_METHOD:
				return boundaryMethod != null;
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
		result.append(", formType: ");
		result.append(formType);
		result.append(", title: ");
		result.append(title);
		result.append(", modal: ");
		result.append(modal);
		result.append(", resizable: ");
		result.append(resizable);
		result.append(", titleArea: ");
		result.append(titleArea);
		result.append(", openEditAfterCreate: ");
		result.append(openEditAfterCreate);
		result.append(", height: ");
		result.append(height);
		result.append(", width: ");
		result.append(width);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getLowerCaseName()
	 * @generated not
	 */
	@Override
	public String getLowerCaseName() {
		if (name == null || name.isEmpty())
			return "";

		return name.substring(0, 1).toLowerCase() + name.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getAllFormFields()
	 * @generated not
	 */
	@Override
	public EList<FormField> getAllFormFields() {
		final var fields = new BasicEList<FormField>();

		getFormPanels().forEach(panel -> fields.addAll(panel.getFields()));

		return fields;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#rearrangeFields()
	 * @generated not
	 */
	@Override
	public void rearrangeFields() {
		// Rearrange fields of all panels
		getFormPanels().forEach(FormPanel::rearrangeFields);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final Project project = getDomainObject().getNamespace().getProject();
		String packageName = project.getClientNamespace().toString();

		if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW)
			packageName += PACK_CLIENT_VIEW;
		else if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD
				|| formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			packageName += PACK_CLIENT_DLG;
		else if (formType == FormTypeEnumeration.LOV)
			packageName += PACK_CLIENT_LOV;
		else if (formType == FormTypeEnumeration.TREE_VIEW)
			packageName += PACK_CLIENT_TREE;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, name, packageName);
		javaFile.setComment(title);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getTypeScriptSourceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getTypeScriptSourceFile() {
		final Project project = getDomainObject().getNamespace().getProject();

		if (!project.hasAngularClient())
			return null;

		final String domainObjectName = domainObject.getName().toLowerCase();
		final var path = ANGULAR_PAGE_FOLDER + "/" + domainObjectName + "/" + getName().toLowerCase() + ".ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getPageObjectSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getPageObjectSourceFile() {
		final Project project = getDomainObject().getNamespace().getProject();
		final AbstractTestModule testModule = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);
		final String packageName = testModule.getNamespace().toString() + PACK_PAGE_OBJECT;
		final var comment = "Page object class that is mapped to relative resource path '" + getResourcePath() + "'";

		final var javaFile = new JavaTestFile(project, BuildArtifactType.SELENIUM_TEST, name, packageName);
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getResourcePath()
	 * @generated not
	 */
	@Override
	public String getResourcePath() {
		final Project project = getDomainObject().getNamespace().getProject();

		if (project.hasAngularClient())
			return "/" + domainObject.getName().toLowerCase() + "/" + name.toLowerCase();

		var resourcePath = UI_DIALOG_FOLDER;

		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW)
			resourcePath = UI_VIEW_FOLDER;

		resourcePath += "/" + name;

		if (project.hasJSFClient())
			resourcePath += ".jsf";

		return resourcePath;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getUserInterfaceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getUserInterfaceFile() {
		final Project project = getDomainObject().getNamespace().getProject();
		String path = null;

		if (project.hasJSFClient()) {
			if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW)
				path = project.getWebAppFolder() + UI_VIEW_FOLDER + "/" + name + ".xhtml";
			else if (formType == FormTypeEnumeration.TREE_VIEW)
				path = project.getWebAppFolder() + UI_TREE_FOLDER + "/" + name + ".xhtml";
			else if (formType == FormTypeEnumeration.LOV)
				path = project.getWebAppFolder() + UI_LOV_FOLDER + "/" + name + ".xhtml";
			else
				path = project.getWebAppFolder() + UI_DIALOG_FOLDER + "/" + name + ".xhtml";
		}
		else if (project.hasAngularClient() && formType != FormTypeEnumeration.SIMPLE_VIEW
				&& formType != FormTypeEnumeration.SEARCHABLE_VIEW && formType != FormTypeEnumeration.LOV)
			path = ANGULAR_PAGE_FOLDER + "/" + domainObject.getName().toLowerCase() + "/" + name.toLowerCase() + ".html";

		return path != null ? new WorkspaceFile(project, BuildArtifactType.GUI, path, null) : null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.Form#getViewFormPanel()
	 * @generated not
	 */
	@Override
	public FormPanel getViewFormPanel() {
		final FormPanel viewFormPanel = getFormPanels().stream().findFirst().orElse(null);

		if (viewFormPanel == null)
			throw new IllegalStateException("The form panel of the view '" + getName() + "' could not be found!");

		return viewFormPanel;
	}

}
