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

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>GUI Test Data</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl#getFormField <em>Form Field</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl#getTableColumnField <em>Table Column Field</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl#getNewValue <em>New Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl#getExpectedValue <em>Expected Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.GUITestDataImpl#getTestAction <em>Test Action</em>}</li>
 * </ul>
 * @generated
 */
public class GUITestDataImpl extends EObjectImpl implements GUITestData {
	/**
	 * The cached value of the '{@link #getFormField() <em>Form Field</em>}' reference
	 * @see #getFormField()
	 * @generated
	 * @ordered
	 */
	protected FormField formField;

	/**
	 * The cached value of the '{@link #getTableColumnField() <em>Table Column Field</em>}' reference
	 * @see #getTableColumnField()
	 * @generated
	 * @ordered
	 */
	protected TableColumnField tableColumnField;

	/**
	 * The default value of the '{@link #getNewValue() <em>New Value</em>}' attribute
	 * @see #getNewValue()
	 * @generated
	 * @ordered
	 */
	protected static final String NEW_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getNewValue() <em>New Value</em>}' attribute
	 * @see #getNewValue()
	 * @generated
	 * @ordered
	 */
	protected String newValue = NEW_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getExpectedValue() <em>Expected Value</em>}' attribute
	 * @see #getExpectedValue()
	 * @generated
	 * @ordered
	 */
	protected static final String EXPECTED_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getExpectedValue() <em>Expected Value</em>}' attribute
	 * @see #getExpectedValue()
	 * @generated
	 * @ordered
	 */
	protected String expectedValue = EXPECTED_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final GUITestDataType TYPE_EDEFAULT = GUITestDataType.FORM_FIELD;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected GUITestDataType type = TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getFilterValue() <em>Filter Value</em>}' attribute
	 * @see #getFilterValue()
	 * @generated
	 * @ordered
	 */
	protected static final String FILTER_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFilterValue() <em>Filter Value</em>}' attribute
	 * @see #getFilterValue()
	 * @generated
	 * @ordered
	 */
	protected String filterValue = FILTER_VALUE_EDEFAULT;

	/**
	 * @generated
	 */
	protected GUITestDataImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.GUI_TEST_DATA;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getFormField()
	 * @generated
	 */
	@Override
	public FormField getFormField() {
		if (formField != null && formField.eIsProxy()) {
			final var oldFormField = (InternalEObject) formField;
			formField = (FormField) eResolveProxy(oldFormField);

			if (formField != oldFormField && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.GUI_TEST_DATA__FORM_FIELD, oldFormField, formField));
		}

		return formField;
	}

	/**
	 * @return the form field
	 */
	public FormField basicGetFormField() {
		return formField;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setFormField(net.codecadenza.eclipse.model.client.FormField)
	 * @generated
	 */
	@Override
	public void setFormField(FormField newFormField) {
		final FormField oldFormField = formField;
		formField = newFormField;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__FORM_FIELD, oldFormField, formField));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getTableColumnField()
	 * @generated
	 */
	@Override
	public TableColumnField getTableColumnField() {
		if (tableColumnField != null && tableColumnField.eIsProxy()) {
			final var oldTableColumnField = (InternalEObject) tableColumnField;
			tableColumnField = (TableColumnField) eResolveProxy(oldTableColumnField);

			if (tableColumnField != oldTableColumnField && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.GUI_TEST_DATA__TABLE_COLUMN_FIELD,
						oldTableColumnField, tableColumnField));
		}

		return tableColumnField;
	}

	/**
	 * @return the table column field
	 */
	public TableColumnField basicGetTableColumnField() {
		return tableColumnField;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setTableColumnField(net.codecadenza.eclipse.model.client.
	 * TableColumnField)
	 * @generated
	 */
	@Override
	public void setTableColumnField(TableColumnField newTableColumnField) {
		final TableColumnField oldTableColumnField = tableColumnField;
		tableColumnField = newTableColumnField;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__TABLE_COLUMN_FIELD, oldTableColumnField,
					tableColumnField));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getNewValue()
	 * @generated
	 */
	@Override
	public String getNewValue() {
		return newValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setNewValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setNewValue(String newNewValue) {
		final String oldNewValue = newValue;
		newValue = newNewValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__NEW_VALUE, oldNewValue, newValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getExpectedValue()
	 * @generated
	 */
	@Override
	public String getExpectedValue() {
		return expectedValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setExpectedValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setExpectedValue(String newExpectedValue) {
		final String oldExpectedValue = expectedValue;
		expectedValue = newExpectedValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__EXPECTED_VALUE, oldExpectedValue,
					expectedValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getType()
	 * @generated
	 */
	@Override
	public GUITestDataType getType() {
		return type;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setType(net.codecadenza.eclipse.model.testing.GUITestDataType)
	 * @generated
	 */
	@Override
	public void setType(GUITestDataType newType) {
		final GUITestDataType oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__TYPE, oldType, type));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getTestAction()
	 * @generated
	 */
	@Override
	public GUITestAction getTestAction() {
		if (eContainerFeatureID() != TestingPackage.GUI_TEST_DATA__TEST_ACTION)
			return null;

		return (GUITestAction) eInternalContainer();
	}

	/**
	 * @param newTestAction
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetTestAction(GUITestAction newTestAction, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newTestAction, TestingPackage.GUI_TEST_DATA__TEST_ACTION, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setTestAction(net.codecadenza.eclipse.model.testing.GUITestAction)
	 * @generated
	 */
	@Override
	public void setTestAction(GUITestAction newTestAction) {
		if (newTestAction != eInternalContainer()
				|| (eContainerFeatureID() != TestingPackage.GUI_TEST_DATA__TEST_ACTION && newTestAction != null)) {
			if (EcoreUtil.isAncestor(this, newTestAction))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newTestAction != null)
				msgs = ((InternalEObject) newTestAction).eInverseAdd(this, TestingPackage.GUI_TEST_ACTION__TEST_DATA, GUITestAction.class,
						msgs);

			msgs = basicSetTestAction(newTestAction, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__TEST_ACTION, newTestAction, newTestAction));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#getFilterValue()
	 * @generated
	 */
	@Override
	public String getFilterValue() {
		return filterValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.GUITestData#setFilterValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setFilterValue(String newFilterValue) {
		final String oldFilterValue = filterValue;
		filterValue = newFilterValue;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, TestingPackage.GUI_TEST_DATA__FILTER_VALUE, oldFilterValue, filterValue));
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
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
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
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
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
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
				return eInternalContainer().eInverseRemove(this, TestingPackage.GUI_TEST_ACTION__TEST_DATA, GUITestAction.class, msgs);
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
			case TestingPackage.GUI_TEST_DATA__FORM_FIELD:
				if (resolve)
					return getFormField();

				return basicGetFormField();
			case TestingPackage.GUI_TEST_DATA__TABLE_COLUMN_FIELD:
				if (resolve)
					return getTableColumnField();

				return basicGetTableColumnField();
			case TestingPackage.GUI_TEST_DATA__NEW_VALUE:
				return getNewValue();
			case TestingPackage.GUI_TEST_DATA__EXPECTED_VALUE:
				return getExpectedValue();
			case TestingPackage.GUI_TEST_DATA__TYPE:
				return getType();
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
				return getTestAction();
			case TestingPackage.GUI_TEST_DATA__FILTER_VALUE:
				return getFilterValue();
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
			case TestingPackage.GUI_TEST_DATA__FORM_FIELD:
				setFormField((FormField) newValue);
				return;
			case TestingPackage.GUI_TEST_DATA__TABLE_COLUMN_FIELD:
				setTableColumnField((TableColumnField) newValue);
				return;
			case TestingPackage.GUI_TEST_DATA__NEW_VALUE:
				setNewValue((String) newValue);
				return;
			case TestingPackage.GUI_TEST_DATA__EXPECTED_VALUE:
				setExpectedValue((String) newValue);
				return;
			case TestingPackage.GUI_TEST_DATA__TYPE:
				setType((GUITestDataType) newValue);
				return;
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
				setTestAction((GUITestAction) newValue);
				return;
			case TestingPackage.GUI_TEST_DATA__FILTER_VALUE:
				setFilterValue((String) newValue);
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
			case TestingPackage.GUI_TEST_DATA__FORM_FIELD:
				setFormField((FormField) null);
				return;
			case TestingPackage.GUI_TEST_DATA__TABLE_COLUMN_FIELD:
				setTableColumnField((TableColumnField) null);
				return;
			case TestingPackage.GUI_TEST_DATA__NEW_VALUE:
				setNewValue(NEW_VALUE_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_DATA__EXPECTED_VALUE:
				setExpectedValue(EXPECTED_VALUE_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_DATA__TYPE:
				setType(TYPE_EDEFAULT);
				return;
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
				setTestAction((GUITestAction) null);
				return;
			case TestingPackage.GUI_TEST_DATA__FILTER_VALUE:
				setFilterValue(FILTER_VALUE_EDEFAULT);
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
			case TestingPackage.GUI_TEST_DATA__FORM_FIELD:
				return formField != null;
			case TestingPackage.GUI_TEST_DATA__TABLE_COLUMN_FIELD:
				return tableColumnField != null;
			case TestingPackage.GUI_TEST_DATA__NEW_VALUE:
				return newValue != null;
			case TestingPackage.GUI_TEST_DATA__EXPECTED_VALUE:
				return expectedValue != null;
			case TestingPackage.GUI_TEST_DATA__TYPE:
				return type != TYPE_EDEFAULT;
			case TestingPackage.GUI_TEST_DATA__TEST_ACTION:
				return getTestAction() != null;
			case TestingPackage.GUI_TEST_DATA__FILTER_VALUE:
				return filterValue != null;
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
		result.append(" (newValue: ");
		result.append(newValue);
		result.append(", expectedValue: ");
		result.append(expectedValue);
		result.append(", type: ");
		result.append(type);
		result.append(", filterValue: ");
		result.append(filterValue);
		result.append(')');

		return result.toString();
	}

}
