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

import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType;
import net.codecadenza.eclipse.model.testing.GUITestActionStatus;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>GUI Test Action Result</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl#getMessageText <em>Message Text</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl#getStatus <em>Status</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl#getTestAction <em>Test Action</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestActionResultImpl#getComponentType <em>Component Type</em>}</li>
 * </ul>
 * @generated
 */
public class GUITestActionResultImpl extends EObjectImpl implements GUITestActionResult {
	/**
	 * The default value of the '{@link #getMessageText() <em>Message Text</em>}' attribute
	 * @see #getMessageText()
	 * @generated
	 * @ordered
	 */
	protected static final String MESSAGE_TEXT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMessageText() <em>Message Text</em>}' attribute
	 * @see #getMessageText()
	 * @generated
	 * @ordered
	 */
	protected String messageText = MESSAGE_TEXT_EDEFAULT;

	/**
	 * The default value of the '{@link #getStatus() <em>Status</em>}' attribute
	 * @see #getStatus()
	 * @generated
	 * @ordered
	 */
	protected static final GUITestActionStatus STATUS_EDEFAULT = GUITestActionStatus.INFO;

	/**
	 * The cached value of the '{@link #getStatus() <em>Status</em>}' attribute
	 * @see #getStatus()
	 * @generated
	 * @ordered
	 */
	protected GUITestActionStatus status = STATUS_EDEFAULT;

	/**
	 * The default value of the '{@link #getComponentType() <em>Component Type</em>}' attribute
	 * @see #getComponentType()
	 * @generated
	 * @ordered
	 */
	protected static final GUITestActionResultComponentType COMPONENT_TYPE_EDEFAULT = GUITestActionResultComponentType.DIALOG;

	/**
	 * The cached value of the '{@link #getComponentType() <em>Component Type</em>}' attribute
	 * @see #getComponentType()
	 * @generated
	 * @ordered
	 */
	protected GUITestActionResultComponentType componentType = COMPONENT_TYPE_EDEFAULT;

	/**
	 * @generated
	 */
	protected GUITestActionResultImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.GUI_TEST_ACTION_RESULT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getMessageText()
	 * @generated
	 */
	@Override
	public String getMessageText() {
		return messageText;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#setMessageText(java.lang.String)
	 * @generated
	 */
	@Override
	public void setMessageText(String newMessageText) {
		final String oldMessageText = messageText;
		messageText = newMessageText;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION_RESULT__MESSAGE_TEXT, oldMessageText,
					messageText));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getStatus()
	 * @generated
	 */
	@Override
	public GUITestActionStatus getStatus() {
		return status;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#setStatus(net.codecadenza.eclipse.model.testing.
	 * GUITestActionStatus)
	 * @generated
	 */
	@Override
	public void setStatus(GUITestActionStatus newStatus) {
		final GUITestActionStatus oldStatus = status;
		status = newStatus == null ? STATUS_EDEFAULT : newStatus;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION_RESULT__STATUS, oldStatus, status));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getTestAction()
	 * @generated
	 */
	@Override
	public GUITestAction getTestAction() {
		if (eContainerFeatureID() != TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION)
			return null;

		return (GUITestAction) eInternalContainer();
	}

	/**
	 * @param newTestAction
	 * @param msgs
	 * @return chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetTestAction(GUITestAction newTestAction, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newTestAction, TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#setTestAction(net.codecadenza.eclipse.model.testing.
	 * GUITestAction)
	 * @generated
	 */
	@Override
	public void setTestAction(GUITestAction newTestAction) {
		if (newTestAction != eInternalContainer()
				|| (eContainerFeatureID() != TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION && newTestAction != null)) {
			if (EcoreUtil.isAncestor(this, newTestAction))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newTestAction != null)
				msgs = ((InternalEObject) newTestAction).eInverseAdd(this, TestingPackage.GUI_TEST_ACTION__ACTION_RESULT,
						GUITestAction.class, msgs);

			msgs = basicSetTestAction(newTestAction, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION, newTestAction,
					newTestAction));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#getComponentType()
	 * @generated
	 */
	@Override
	public GUITestActionResultComponentType getComponentType() {
		return componentType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestActionResult#
	 * setComponentType(net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType)
	 * @generated
	 */
	@Override
	public void setComponentType(GUITestActionResultComponentType newComponentType) {
		final GUITestActionResultComponentType oldComponentType = componentType;
		componentType = newComponentType == null ? COMPONENT_TYPE_EDEFAULT : newComponentType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_ACTION_RESULT__COMPONENT_TYPE,
					oldComponentType, componentType));
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
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetTestAction((GUITestAction) otherEnd, msgs);
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
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				return basicSetTestAction(null, msgs);
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
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				return eInternalContainer().eInverseRemove(this, TestingPackage.GUI_TEST_ACTION__ACTION_RESULT, GUITestAction.class,
						msgs);
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
			case TestingPackage.GUI_TEST_ACTION_RESULT__MESSAGE_TEXT:
				return getMessageText();
			case TestingPackage.GUI_TEST_ACTION_RESULT__STATUS:
				return getStatus();
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				return getTestAction();
			case TestingPackage.GUI_TEST_ACTION_RESULT__COMPONENT_TYPE:
				return getComponentType();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.GUI_TEST_ACTION_RESULT__MESSAGE_TEXT:
				setMessageText((String) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION_RESULT__STATUS:
				setStatus((GUITestActionStatus) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				setTestAction((GUITestAction) newValue);
				return;
			case TestingPackage.GUI_TEST_ACTION_RESULT__COMPONENT_TYPE:
				setComponentType((GUITestActionResultComponentType) newValue);
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
			case TestingPackage.GUI_TEST_ACTION_RESULT__MESSAGE_TEXT:
				setMessageText(MESSAGE_TEXT_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_ACTION_RESULT__STATUS:
				setStatus(STATUS_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				setTestAction((GUITestAction) null);
				return;
			case TestingPackage.GUI_TEST_ACTION_RESULT__COMPONENT_TYPE:
				setComponentType(COMPONENT_TYPE_EDEFAULT);
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
			case TestingPackage.GUI_TEST_ACTION_RESULT__MESSAGE_TEXT:
				return messageText != null;
			case TestingPackage.GUI_TEST_ACTION_RESULT__STATUS:
				return status != STATUS_EDEFAULT;
			case TestingPackage.GUI_TEST_ACTION_RESULT__TEST_ACTION:
				return getTestAction() != null;
			case TestingPackage.GUI_TEST_ACTION_RESULT__COMPONENT_TYPE:
				return componentType != COMPONENT_TYPE_EDEFAULT;
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
		result.append(" (messageText: ");
		result.append(messageText);
		result.append(", status: ");
		result.append(status);
		result.append(", componentType: ");
		result.append(componentType);
		result.append(')');

		return result.toString();
	}

}
