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
package net.codecadenza.eclipse.model.testing.impl;

import java.util.Collection;
import java.util.Optional;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>GUI Test Action</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getComment <em>Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getForm <em>Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getTargetForm <em>Target Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getFormAction <em>Form Action</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getFormPanel <em>Form Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getTestData <em>Test Data</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getActionResult <em>Action Result</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getTestCase <em>Test Case</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getDelayBefore <em>Delay Before</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionImpl#getDelayAfter <em>Delay After</em>}</li>
 * </ul>
 * @generated
 */
public class GUITestActionImpl extends EObjectImpl implements GUITestAction {
	/**
	 * The default value of the '{@link #getComment() <em>Comment</em>}' attribute
	 * @see #getComment()
	 * @generated
	 * @ordered
	 */
	protected static final String COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getComment() <em>Comment</em>}' attribute
	 * @see #getComment()
	 * @generated
	 * @ordered
	 */
	protected String comment = COMMENT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getForm() <em>Form</em>}' reference
	 * @see #getForm()
	 * @generated
	 * @ordered
	 */
	protected Form form;

	/**
	 * The cached value of the '{@link #getTargetForm() <em>Target Form</em>}' reference
	 * @see #getTargetForm()
	 * @generated
	 * @ordered
	 */
	protected Form targetForm;

	/**
	 * The cached value of the '{@link #getFormAction() <em>Form Action</em>}' reference
	 * @see #getFormAction()
	 * @generated
	 * @ordered
	 */
	protected FormAction formAction;

	/**
	 * The cached value of the '{@link #getFormPanel() <em>Form Panel</em>}' reference
	 * @see #getFormPanel()
	 * @generated
	 * @ordered
	 */
	protected FormPanel formPanel;

	/**
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final GUITestActionType TYPE_EDEFAULT = GUITestActionType.EXECUTE_FORM_ACTION;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected GUITestActionType type = TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getTestData() <em>Test Data</em>}' containment reference list
	 * @see #getTestData()
	 * @generated
	 * @ordered
	 */
	protected EList<GUITestData> testData;

	/**
	 * The cached value of the '{@link #getActionResult() <em>Action Result</em>}' containment reference
	 * @see #getActionResult()
	 * @generated
	 * @ordered
	 */
	protected GUITestActionResult actionResult;

	/**
	 * The default value of the '{@link #getDelayBefore() <em>Delay Before</em>}' attribute
	 * @see #getDelayBefore()
	 * @generated
	 * @ordered
	 */
	protected static final Integer DELAY_BEFORE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDelayBefore() <em>Delay Before</em>}' attribute
	 * @see #getDelayBefore()
	 * @generated
	 * @ordered
	 */
	protected Integer delayBefore = DELAY_BEFORE_EDEFAULT;

	/**
	 * The default value of the '{@link #getDelayAfter() <em>Delay After</em>}' attribute
	 * @see #getDelayAfter()
	 * @generated
	 * @ordered
	 */
	protected static final Integer DELAY_AFTER_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDelayAfter() <em>Delay After</em>}' attribute
	 * @see #getDelayAfter()
	 * @generated
	 * @ordered
	 */
	protected Integer delayAfter = DELAY_AFTER_EDEFAULT;

	/**
	 * @generated
	 */
	protected GUITestActionImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.GUI_TEST_ACTION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getComment()
	 * @generated
	 */
	@Override
	public String getComment() {
		return comment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setComment(String newComment) {
		final String oldComment = comment;
		comment = newComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__COMMENT, oldComment, comment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getForm()
	 * @generated
	 */
	@Override
	public Form getForm() {
		if (form != null && form.eIsProxy()) {
			final var oldForm = (InternalEObject) form;
			form = (Form) eResolveProxy(oldForm);

			if (form != oldForm && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.GUI_TEST_ACTION__FORM, oldForm, form));
		}

		return form;
	}

	/**
	 * @return the form
	 * @generated
	 */
	public Form basicGetForm() {
		return form;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setForm(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setForm(Form newForm) {
		final Form oldForm = form;
		form = newForm;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__FORM, oldForm, form));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTargetForm()
	 * @generated
	 */
	@Override
	public Form getTargetForm() {
		if (targetForm != null && targetForm.eIsProxy()) {
			final var oldTargetForm = (InternalEObject) targetForm;
			targetForm = (Form) eResolveProxy(oldTargetForm);

			if (targetForm != oldTargetForm && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.GUI_TEST_ACTION__TARGET_FORM, oldTargetForm,
						targetForm));
		}

		return targetForm;
	}

	/**
	 * @return the target form
	 * @generated
	 */
	public Form basicGetTargetForm() {
		return targetForm;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setTargetForm(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setTargetForm(Form newTargetForm) {
		final Form oldTargetForm = targetForm;
		targetForm = newTargetForm;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__TARGET_FORM, oldTargetForm, targetForm));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getFormAction()
	 * @generated
	 */
	@Override
	public FormAction getFormAction() {
		if (formAction != null && formAction.eIsProxy()) {
			final var oldFormAction = (InternalEObject) formAction;
			formAction = (FormAction) eResolveProxy(oldFormAction);

			if (formAction != oldFormAction && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.GUI_TEST_ACTION__FORM_ACTION, oldFormAction,
						formAction));
		}

		return formAction;
	}

	/**
	 * @return the form action
	 * @generated
	 */
	public FormAction basicGetFormAction() {
		return formAction;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setFormAction(net.codecadenza.eclipse.model.client.FormAction)
	 * @generated
	 */
	@Override
	public void setFormAction(FormAction newFormAction) {
		final FormAction oldFormAction = formAction;
		formAction = newFormAction;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__FORM_ACTION, oldFormAction, formAction));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getFormPanel()
	 * @generated
	 */
	@Override
	public FormPanel getFormPanel() {
		if (formPanel != null && formPanel.eIsProxy()) {
			final var oldFormPanel = (InternalEObject) formPanel;
			formPanel = (FormPanel) eResolveProxy(oldFormPanel);

			if (formPanel != oldFormPanel && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.GUI_TEST_ACTION__FORM_PANEL, oldFormPanel,
						formPanel));
		}

		return formPanel;
	}

	/**
	 * @return the form panel
	 * @generated
	 */
	public FormPanel basicGetFormPanel() {
		return formPanel;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setFormPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 * @generated
	 */
	@Override
	public void setFormPanel(FormPanel newFormPanel) {
		final FormPanel oldFormPanel = formPanel;
		formPanel = newFormPanel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__FORM_PANEL, oldFormPanel, formPanel));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getType()
	 * @generated
	 */
	@Override
	public GUITestActionType getType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setType(net.codecadenza.eclipse.model.testing.GUITestActionType)
	 * @generated
	 */
	@Override
	public void setType(GUITestActionType newType) {
		final GUITestActionType oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__TYPE, oldType, type));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestData()
	 * @generated
	 */
	@Override
	public EList<GUITestData> getTestData() {
		if (testData == null)
			testData = new EObjectContainmentWithInverseEList<>(GUITestData.class, this, TestingPackage.GUI_TEST_ACTION__TEST_DATA,
					TestingPackage.GUI_TEST_DATA__TEST_ACTION);

		return testData;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getActionResult()
	 * @generated
	 */
	@Override
	public GUITestActionResult getActionResult() {
		return actionResult;
	}

	/**
	 * @param newActionResult
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetActionResult(GUITestActionResult newActionResult, NotificationChain msgs) {
		final GUITestActionResult oldActionResult = actionResult;
		actionResult = newActionResult;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__ACTION_RESULT,
					oldActionResult, newActionResult);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setActionResult(net.codecadenza.eclipse.model.testing.
	 * GUITestActionResult)
	 * @generated
	 */
	@Override
	public void setActionResult(GUITestActionResult newActionResult) {
		if (newActionResult != actionResult) {
			NotificationChain msgs = null;

			if (actionResult != null)
				msgs = ((InternalEObject) actionResult).eInverseRemove(this, TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION,
						GUITestActionResult.class, msgs);

			if (newActionResult != null)
				msgs = ((InternalEObject) newActionResult).eInverseAdd(this, TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION,
						GUITestActionResult.class, msgs);

			msgs = basicSetActionResult(newActionResult, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__ACTION_RESULT, newActionResult,
					newActionResult));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestCase()
	 * @generated
	 */
	@Override
	public GUITestCase getTestCase() {
		if (eContainerFeatureID() != TestingPackage.GUI_TEST_ACTION__TEST_CASE)
			return null;

		return (GUITestCase) eInternalContainer();
	}

	/**
	 * @param newTestCase
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetTestCase(GUITestCase newTestCase, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newTestCase, TestingPackage.GUI_TEST_ACTION__TEST_CASE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setTestCase(net.codecadenza.eclipse.model.testing.GUITestCase)
	 * @generated
	 */
	@Override
	public void setTestCase(GUITestCase newTestCase) {
		if (newTestCase != eInternalContainer()
				|| (eContainerFeatureID() != TestingPackage.GUI_TEST_ACTION__TEST_CASE && newTestCase != null)) {
			if (EcoreUtil.isAncestor(this, newTestCase))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newTestCase != null)
				msgs = ((InternalEObject) newTestCase).eInverseAdd(this, TestingPackage.GUI_TEST_CASE__TEST_ACTIONS, GUITestCase.class,
						msgs);

			msgs = basicSetTestCase(newTestCase, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__TEST_CASE, newTestCase, newTestCase));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getDelayBefore()
	 * @generated
	 */
	@Override
	public Integer getDelayBefore() {
		return delayBefore;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setDelayBefore(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setDelayBefore(Integer newDelayBefore) {
		final Integer oldDelayBefore = delayBefore;
		delayBefore = newDelayBefore;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__DELAY_BEFORE, oldDelayBefore,
					delayBefore));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getDelayAfter()
	 * @generated
	 */
	@Override
	public Integer getDelayAfter() {
		return delayAfter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#setDelayAfter(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setDelayAfter(Integer newDelayAfter) {
		final Integer oldDelayAfter = delayAfter;
		delayAfter = newDelayAfter;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION__DELAY_AFTER, oldDelayAfter, delayAfter));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	@SuppressWarnings("unchecked")
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case TestingPackage.GUI_TEST_ACTION__TEST_DATA:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getTestData()).basicAdd(otherEnd, msgs);
			case TestingPackage.GUI_TEST_ACTION__ACTION_RESULT:
				if (actionResult != null)
					msgs = ((InternalEObject) actionResult).eInverseRemove(this,
							EOPPOSITE_FEATURE_BASE - TestingPackage.GUI_TEST_ACTION__ACTION_RESULT, null, msgs);

				return basicSetActionResult((GUITestActionResult) otherEnd, msgs);
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetTestCase((GUITestCase) otherEnd, msgs);
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
			case TestingPackage.GUI_TEST_ACTION__TEST_DATA:
				return ((InternalEList<?>) getTestData()).basicRemove(otherEnd, msgs);
			case TestingPackage.GUI_TEST_ACTION__ACTION_RESULT:
				return basicSetActionResult(null, msgs);
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				return basicSetTestCase(null, msgs);
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
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				return eInternalContainer().eInverseRemove(this, TestingPackage.GUI_TEST_CASE__TEST_ACTIONS, GUITestCase.class, msgs);
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
			case TestingPackage.GUI_TEST_ACTION__COMMENT:
				return getComment();
			case TestingPackage.GUI_TEST_ACTION__FORM:
				if (resolve)
					return getForm();

				return basicGetForm();
			case TestingPackage.GUI_TEST_ACTION__TARGET_FORM:
				if (resolve)
					return getTargetForm();
				return basicGetTargetForm();
			case TestingPackage.GUI_TEST_ACTION__FORM_ACTION:
				if (resolve)
					return getFormAction();

				return basicGetFormAction();
			case TestingPackage.GUI_TEST_ACTION__FORM_PANEL:
				if (resolve)
					return getFormPanel();

				return basicGetFormPanel();
			case TestingPackage.GUI_TEST_ACTION__TYPE:
				return getType();
			case TestingPackage.GUI_TEST_ACTION__TEST_DATA:
				return getTestData();
			case TestingPackage.GUI_TEST_ACTION__ACTION_RESULT:
				return getActionResult();
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				return getTestCase();
			case TestingPackage.GUI_TEST_ACTION__DELAY_BEFORE:
				return getDelayBefore();
			case TestingPackage.GUI_TEST_ACTION__DELAY_AFTER:
				return getDelayAfter();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.GUI_TEST_ACTION__COMMENT:
				setComment((String) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__FORM:
				setForm((Form) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__TARGET_FORM:
				setTargetForm((Form) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__FORM_ACTION:
				setFormAction((FormAction) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__FORM_PANEL:
				setFormPanel((FormPanel) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__TYPE:
				setType((GUITestActionType) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__TEST_DATA:
				getTestData().clear();
				getTestData().addAll((Collection<? extends GUITestData>) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__ACTION_RESULT:
				setActionResult((GUITestActionResult) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				setTestCase((GUITestCase) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__DELAY_BEFORE:
				setDelayBefore((Integer) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION__DELAY_AFTER:
				setDelayAfter((Integer) newValue);
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
			case TestingPackage.GUI_TEST_ACTION__COMMENT:
				setComment(COMMENT_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_ACTION__FORM:
				setForm((Form) null);
				return;
			case TestingPackage.GUI_TEST_ACTION__TARGET_FORM:
				setTargetForm((Form) null);
				return;
			case TestingPackage.GUI_TEST_ACTION__FORM_ACTION:
				setFormAction((FormAction) null);
				return;
			case TestingPackage.GUI_TEST_ACTION__FORM_PANEL:
				setFormPanel((FormPanel) null);
				return;
			case TestingPackage.GUI_TEST_ACTION__TYPE:
				setType(TYPE_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_ACTION__TEST_DATA:
				getTestData().clear();
				return;
			case TestingPackage.GUI_TEST_ACTION__ACTION_RESULT:
				setActionResult((GUITestActionResult) null);
				return;
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				setTestCase((GUITestCase) null);
				return;
			case TestingPackage.GUI_TEST_ACTION__DELAY_BEFORE:
				setDelayBefore(DELAY_BEFORE_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_ACTION__DELAY_AFTER:
				setDelayAfter(DELAY_AFTER_EDEFAULT);
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
			case TestingPackage.GUI_TEST_ACTION__COMMENT:
				return comment != null;
			case TestingPackage.GUI_TEST_ACTION__FORM:
				return form != null;
			case TestingPackage.GUI_TEST_ACTION__TARGET_FORM:
				return targetForm != null;
			case TestingPackage.GUI_TEST_ACTION__FORM_ACTION:
				return formAction != null;
			case TestingPackage.GUI_TEST_ACTION__FORM_PANEL:
				return formPanel != null;
			case TestingPackage.GUI_TEST_ACTION__TYPE:
				return type != TYPE_EDEFAULT;
			case TestingPackage.GUI_TEST_ACTION__TEST_DATA:
				return testData != null && !testData.isEmpty();
			case TestingPackage.GUI_TEST_ACTION__ACTION_RESULT:
				return actionResult != null;
			case TestingPackage.GUI_TEST_ACTION__TEST_CASE:
				return getTestCase() != null;
			case TestingPackage.GUI_TEST_ACTION__DELAY_BEFORE:
				return delayBefore != null;
			case TestingPackage.GUI_TEST_ACTION__DELAY_AFTER:
				return delayAfter != null;
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
		result.append("( comment: ");
		result.append(comment);
		result.append(", type: ");
		result.append(type);
		result.append(", delayBefore: ");
		result.append(delayBefore);
		result.append(", delayAfter: ");
		result.append(delayAfter);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#needsTestData()
	 * @generated not
	 */
	@Override
	public boolean needsTestData() {
		if (getActionResult() != null)
			return true;

		if (type == GUITestActionType.OPEN_PAGE_DIRECT) {
			if (form.getFormType() == FormTypeEnumeration.UPDATE || form.getFormType() == FormTypeEnumeration.READONLY
					|| form.getFormType() == FormTypeEnumeration.ADD)
				return true;

			for (final GUITestData guiTestData : getTestData())
				if (guiTestData.getType() == GUITestDataType.PAGE_TITLE)
					return true;
		}
		else if (type == GUITestActionType.EXECUTE_FORM_ACTION && getFormAction() != null
				&& getFormAction().getType() == ActionType.UPLOAD_IMPORT) {
			final var exchangeMethod = (DataExchangeMethod) getFormAction().getBoundaryMethod().getServiceMethod();

			// A test data object is necessary when performing a data import that requires an upload file
			if (!exchangeMethod.getMethodParameters().isEmpty())
				return true;
		}
		else if (type == GUITestActionType.OPEN_PAGE_BY_NAVIGATOR || type == GUITestActionType.ENTER_FORM_DATA
				|| type == GUITestActionType.VALIDATE_FORM_DATA || type == GUITestActionType.SEARCH_ROW_ALL_PAGES
				|| type == GUITestActionType.SEARCH_ROW_CURRENT_PAGE || type == GUITestActionType.UPLOAD_FILE
				|| type == GUITestActionType.ENTER_SEARCH_DATA || type == GUITestActionType.COUNT_RECORDS
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_EQUAL || type == GUITestActionType.VALIDATE_ROW_COUNT_GREATER
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_SMALLER || type == GUITestActionType.OPEN_LOGIN_PAGE)
			return true;

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestAction#getTestDataByType(net.codecadenza.eclipse.model.testing.
	 * GUITestDataType)
	 * @generated not
	 */
	@Override
	public GUITestData getTestDataByType(GUITestDataType type) {
		final Optional<GUITestData> guiTestData = getTestData().stream().filter(e -> e.getType() == type).findFirst();

		if (guiTestData.isPresent())
			return guiTestData.get();

		return null;
	}

}
