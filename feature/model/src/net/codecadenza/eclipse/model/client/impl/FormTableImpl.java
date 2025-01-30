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
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Form Table</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getRowIndex <em>Row Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getSpanCols <em>Span Cols</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getSpanRows <em>Span Rows</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#isVerticalSpan <em>Vertical Span</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#isHorizontalSpan <em>Horizontal Span</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getFormPanel <em>Form Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getFields <em>Fields</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl#getAssociation <em>Association</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FormTableImpl extends EObjectImpl implements FormTable {
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
	 * The default value of the '{@link #getColIndex() <em>Col Index</em>}' attribute
	 * @see #getColIndex()
	 * @generated
	 * @ordered
	 */
	protected static final int COL_INDEX_EDEFAULT = 1;

	/**
	 * The cached value of the '{@link #getColIndex() <em>Col Index</em>}' attribute
	 * @see #getColIndex()
	 * @generated
	 * @ordered
	 */
	protected int colIndex = COL_INDEX_EDEFAULT;

	/**
	 * The default value of the '{@link #getRowIndex() <em>Row Index</em>}' attribute
	 * @see #getRowIndex()
	 * @generated
	 * @ordered
	 */
	protected static final int ROW_INDEX_EDEFAULT = 1;

	/**
	 * The cached value of the '{@link #getRowIndex() <em>Row Index</em>}' attribute
	 * @see #getRowIndex()
	 * @generated
	 * @ordered
	 */
	protected int rowIndex = ROW_INDEX_EDEFAULT;

	/**
	 * The default value of the '{@link #getSpanCols() <em>Span Cols</em>}' attribute
	 * @see #getSpanCols()
	 * @generated
	 * @ordered
	 */
	protected static final int SPAN_COLS_EDEFAULT = 1;

	/**
	 * The cached value of the '{@link #getSpanCols() <em>Span Cols</em>}' attribute
	 * @see #getSpanCols()
	 * @generated
	 * @ordered
	 */
	protected int spanCols = SPAN_COLS_EDEFAULT;

	/**
	 * The default value of the '{@link #getSpanRows() <em>Span Rows</em>}' attribute
	 * @see #getSpanRows()
	 * @generated
	 * @ordered
	 */
	protected static final int SPAN_ROWS_EDEFAULT = 1;

	/**
	 * The cached value of the '{@link #getSpanRows() <em>Span Rows</em>}' attribute
	 * @see #getSpanRows()
	 * @generated
	 * @ordered
	 */
	protected int spanRows = SPAN_ROWS_EDEFAULT;

	/**
	 * The default value of the '{@link #isVerticalSpan() <em>Vertical Span</em>}' attribute
	 * @see #isVerticalSpan()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VERTICAL_SPAN_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isVerticalSpan() <em>Vertical Span</em>}' attribute
	 * @see #isVerticalSpan()
	 * @generated
	 * @ordered
	 */
	protected boolean verticalSpan = VERTICAL_SPAN_EDEFAULT;

	/**
	 * The default value of the '{@link #isHorizontalSpan() <em>Horizontal Span</em>}' attribute
	 * @see #isHorizontalSpan()
	 * @generated
	 * @ordered
	 */
	protected static final boolean HORIZONTAL_SPAN_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isHorizontalSpan() <em>Horizontal Span</em>}' attribute
	 * @see #isHorizontalSpan()
	 * @generated
	 * @ordered
	 */
	protected boolean horizontalSpan = HORIZONTAL_SPAN_EDEFAULT;

	/**
	 * The cached value of the '{@link #getFormPanel() <em>Form Panel</em>}' reference
	 * @see #getFormPanel()
	 * @generated
	 * @ordered
	 */
	protected FormPanel formPanel;

	/**
	 * The cached value of the '{@link #getFields() <em>Fields</em>}' containment reference list
	 * @see #getFields()
	 * @generated
	 * @ordered
	 */
	protected EList<TableColumnField> fields;

	/**
	 * The cached value of the '{@link #getAssociation() <em>Association</em>}' reference
	 * @see #getAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation association;

	/**
	 * @generated
	 */
	protected FormTableImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.FORM_TABLE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getColIndex()
	 * @generated
	 */
	@Override
	public int getColIndex() {
		return colIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setColIndex(int)
	 * @generated
	 */
	@Override
	public void setColIndex(int newColIndex) {
		final int oldColIndex = colIndex;
		colIndex = newColIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__COL_INDEX, oldColIndex, colIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getRowIndex()
	 * @generated
	 */
	@Override
	public int getRowIndex() {
		return rowIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setRowIndex(int)
	 * @generated
	 */
	@Override
	public void setRowIndex(int newRowIndex) {
		final int oldRowIndex = rowIndex;
		rowIndex = newRowIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__ROW_INDEX, oldRowIndex, rowIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getSpanCols()
	 * @generated
	 */
	@Override
	public int getSpanCols() {
		return spanCols;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setSpanCols(int)
	 * @generated
	 */
	@Override
	public void setSpanCols(int newSpanCols) {
		final int oldSpanCols = spanCols;
		spanCols = newSpanCols;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__SPAN_COLS, oldSpanCols, spanCols));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getSpanRows()
	 * @generated
	 */
	@Override
	public int getSpanRows() {
		return spanRows;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setSpanRows(int)
	 * @generated
	 */
	@Override
	public void setSpanRows(int newSpanRows) {
		final int oldSpanRows = spanRows;
		spanRows = newSpanRows;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__SPAN_ROWS, oldSpanRows, spanRows));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#isVerticalSpan()
	 * @generated
	 */
	@Override
	public boolean isVerticalSpan() {
		return verticalSpan;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setVerticalSpan(boolean)
	 * @generated
	 */
	@Override
	public void setVerticalSpan(boolean newVerticalSpan) {
		final boolean oldVerticalSpan = verticalSpan;
		verticalSpan = newVerticalSpan;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__VERTICAL_SPAN, oldVerticalSpan, verticalSpan));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#isHorizontalSpan()
	 * @generated
	 */
	@Override
	public boolean isHorizontalSpan() {
		return horizontalSpan;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setHorizontalSpan(boolean)
	 * @generated
	 */
	@Override
	public void setHorizontalSpan(boolean newHorizontalSpan) {
		final boolean oldHorizontalSpan = horizontalSpan;
		horizontalSpan = newHorizontalSpan;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__HORIZONTAL_SPAN, oldHorizontalSpan,
					horizontalSpan));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getFormPanel()
	 * @generated
	 */
	@Override
	public FormPanel getFormPanel() {
		if (formPanel != null && formPanel.eIsProxy()) {
			final var oldFormPanel = (InternalEObject) formPanel;
			formPanel = (FormPanel) eResolveProxy(oldFormPanel);

			if (formPanel != oldFormPanel && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_TABLE__FORM_PANEL, oldFormPanel, formPanel));
		}

		return formPanel;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public FormPanel basicGetFormPanel() {
		return formPanel;
	}

	/**
	 * @param newFormPanel
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetFormPanel(FormPanel newFormPanel, NotificationChain msgs) {
		final FormPanel oldFormPanel = formPanel;
		formPanel = newFormPanel;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__FORM_PANEL, oldFormPanel,
					newFormPanel);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setFormPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 * @generated
	 */
	@Override
	public void setFormPanel(FormPanel newFormPanel) {
		if (newFormPanel != formPanel) {
			NotificationChain msgs = null;

			if (formPanel != null)
				msgs = ((InternalEObject) formPanel).eInverseRemove(this, ClientPackage.FORM_PANEL__FORM_TABLE, FormPanel.class, msgs);

			if (newFormPanel != null)
				msgs = ((InternalEObject) newFormPanel).eInverseAdd(this, ClientPackage.FORM_PANEL__FORM_TABLE, FormPanel.class, msgs);

			msgs = basicSetFormPanel(newFormPanel, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__FORM_PANEL, newFormPanel, newFormPanel));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getFields()
	 * @generated
	 */
	@Override
	public EList<TableColumnField> getFields() {
		if (fields == null)
			fields = new EObjectContainmentWithInverseEList<>(TableColumnField.class, this, ClientPackage.FORM_TABLE__FIELDS,
					ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE);

		return fields;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_TABLE__ASSOCIATION, oldAssociation,
						association));
		}

		return association;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public AbstractDomainAssociation basicGetAssociation() {
		return association;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_TABLE__ASSOCIATION, oldAssociation, association));
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
			case ClientPackage.FORM_TABLE__FORM_PANEL:
				if (formPanel != null)
					msgs = ((InternalEObject) formPanel).eInverseRemove(this, ClientPackage.FORM_PANEL__FORM_TABLE, FormPanel.class, msgs);

				return basicSetFormPanel((FormPanel) otherEnd, msgs);
			case ClientPackage.FORM_TABLE__FIELDS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getFields()).basicAdd(otherEnd, msgs);
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
			case ClientPackage.FORM_TABLE__FORM_PANEL:
				return basicSetFormPanel(null, msgs);
			case ClientPackage.FORM_TABLE__FIELDS:
				return ((InternalEList<?>) getFields()).basicRemove(otherEnd, msgs);
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
			case ClientPackage.FORM_TABLE__NAME:
				return getName();
			case ClientPackage.FORM_TABLE__COL_INDEX:
				return getColIndex();
			case ClientPackage.FORM_TABLE__ROW_INDEX:
				return getRowIndex();
			case ClientPackage.FORM_TABLE__SPAN_COLS:
				return getSpanCols();
			case ClientPackage.FORM_TABLE__SPAN_ROWS:
				return getSpanRows();
			case ClientPackage.FORM_TABLE__VERTICAL_SPAN:
				return isVerticalSpan();
			case ClientPackage.FORM_TABLE__HORIZONTAL_SPAN:
				return isHorizontalSpan();
			case ClientPackage.FORM_TABLE__FORM_PANEL:
				if (resolve)
					return getFormPanel();

				return basicGetFormPanel();
			case ClientPackage.FORM_TABLE__FIELDS:
				return getFields();
			case ClientPackage.FORM_TABLE__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
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
			case ClientPackage.FORM_TABLE__NAME:
				setName((String) newValue);
				return;
			case ClientPackage.FORM_TABLE__COL_INDEX:
				setColIndex((Integer) newValue);
				return;
			case ClientPackage.FORM_TABLE__ROW_INDEX:
				setRowIndex((Integer) newValue);
				return;
			case ClientPackage.FORM_TABLE__SPAN_COLS:
				setSpanCols((Integer) newValue);
				return;
			case ClientPackage.FORM_TABLE__SPAN_ROWS:
				setSpanRows((Integer) newValue);
				return;
			case ClientPackage.FORM_TABLE__VERTICAL_SPAN:
				setVerticalSpan((Boolean) newValue);
				return;
			case ClientPackage.FORM_TABLE__HORIZONTAL_SPAN:
				setHorizontalSpan((Boolean) newValue);
				return;
			case ClientPackage.FORM_TABLE__FORM_PANEL:
				setFormPanel((FormPanel) newValue);
				return;
			case ClientPackage.FORM_TABLE__FIELDS:
				getFields().clear();
				getFields().addAll((Collection<? extends TableColumnField>) newValue);
				return;
			case ClientPackage.FORM_TABLE__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
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
			case ClientPackage.FORM_TABLE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__COL_INDEX:
				setColIndex(COL_INDEX_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__ROW_INDEX:
				setRowIndex(ROW_INDEX_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__SPAN_COLS:
				setSpanCols(SPAN_COLS_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__SPAN_ROWS:
				setSpanRows(SPAN_ROWS_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__VERTICAL_SPAN:
				setVerticalSpan(VERTICAL_SPAN_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__HORIZONTAL_SPAN:
				setHorizontalSpan(HORIZONTAL_SPAN_EDEFAULT);
				return;
			case ClientPackage.FORM_TABLE__FORM_PANEL:
				setFormPanel((FormPanel) null);
				return;
			case ClientPackage.FORM_TABLE__FIELDS:
				getFields().clear();
				return;
			case ClientPackage.FORM_TABLE__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
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
			case ClientPackage.FORM_TABLE__NAME:
				return name != null;
			case ClientPackage.FORM_TABLE__COL_INDEX:
				return colIndex != COL_INDEX_EDEFAULT;
			case ClientPackage.FORM_TABLE__ROW_INDEX:
				return rowIndex != ROW_INDEX_EDEFAULT;
			case ClientPackage.FORM_TABLE__SPAN_COLS:
				return spanCols != SPAN_COLS_EDEFAULT;
			case ClientPackage.FORM_TABLE__SPAN_ROWS:
				return spanRows != SPAN_ROWS_EDEFAULT;
			case ClientPackage.FORM_TABLE__VERTICAL_SPAN:
				return verticalSpan != VERTICAL_SPAN_EDEFAULT;
			case ClientPackage.FORM_TABLE__HORIZONTAL_SPAN:
				return horizontalSpan != HORIZONTAL_SPAN_EDEFAULT;
			case ClientPackage.FORM_TABLE__FORM_PANEL:
				return formPanel != null;
			case ClientPackage.FORM_TABLE__FIELDS:
				return fields != null && !fields.isEmpty();
			case ClientPackage.FORM_TABLE__ASSOCIATION:
				return association != null;
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
		result.append(", colIndex: ");
		result.append(colIndex);
		result.append(", rowIndex: ");
		result.append(rowIndex);
		result.append(", spanCols: ");
		result.append(spanCols);
		result.append(", spanRows: ");
		result.append(spanRows);
		result.append(", verticalSpan: ");
		result.append(verticalSpan);
		result.append(", horizontalSpan: ");
		result.append(horizontalSpan);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormTable#hasVisibleBooleanFields()
	 * @generated not
	 */
	@Override
	public boolean hasVisibleBooleanFields() {
		return getFields().stream().filter(TableColumnField::isVisible).map(TableColumnField::getFieldType)
				.anyMatch(t -> t == TableColumnFieldTypeEnumeration.BOOLEAN);
	}

}
