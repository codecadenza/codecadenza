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

import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Form Field</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getRowIndex <em>Row Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#isVisible <em>Visible</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#isSpanCols <em>Span Cols</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#isReadonly <em>Readonly</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#isMandatory <em>Mandatory</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getFieldType <em>Field Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getPanel <em>Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getWidth <em>Width</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getDefaultValue <em>Default Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getListOfValues <em>List Of Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#getDTOAttribute <em>DTO Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl#isAddFormLinkToLabel <em>Add Form Link To Label</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FormFieldImpl extends EObjectImpl implements FormField {
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
	 * The default value of the '{@link #isVisible() <em>Visible</em>}' attribute
	 * @see #isVisible()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VISIBLE_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isVisible() <em>Visible</em>}' attribute
	 * @see #isVisible()
	 * @generated
	 * @ordered
	 */
	protected boolean visible = VISIBLE_EDEFAULT;

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
	 * The default value of the '{@link #isSpanCols() <em>Span Cols</em>}' attribute
	 * @see #isSpanCols()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SPAN_COLS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSpanCols() <em>Span Cols</em>}' attribute
	 * @see #isSpanCols()
	 * @generated
	 * @ordered
	 */
	protected boolean spanCols = SPAN_COLS_EDEFAULT;

	/**
	 * The default value of the '{@link #isReadonly() <em>Readonly</em>}' attribute
	 * @see #isReadonly()
	 * @generated
	 * @ordered
	 */
	protected static final boolean READONLY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isReadonly() <em>Readonly</em>}' attribute
	 * @see #isReadonly()
	 * @generated
	 * @ordered
	 */
	protected boolean readonly = READONLY_EDEFAULT;

	/**
	 * The default value of the '{@link #isMandatory() <em>Mandatory</em>}' attribute
	 * @see #isMandatory()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MANDATORY_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isMandatory() <em>Mandatory</em>}' attribute
	 * @see #isMandatory()
	 * @generated
	 * @ordered
	 */
	protected boolean mandatory = MANDATORY_EDEFAULT;

	/**
	 * The default value of the '{@link #getFieldType() <em>Field Type</em>}' attribute
	 * @see #getFieldType()
	 * @generated
	 * @ordered
	 */
	protected static final FormFieldTypeEnumeration FIELD_TYPE_EDEFAULT = FormFieldTypeEnumeration.SIMPLE_TEXT;

	/**
	 * The cached value of the '{@link #getFieldType() <em>Field Type</em>}' attribute
	 * @see #getFieldType()
	 * @generated
	 * @ordered
	 */
	protected FormFieldTypeEnumeration fieldType = FIELD_TYPE_EDEFAULT;

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
	 * The default value of the '{@link #getDefaultValue() <em>Default Value</em>}' attribute
	 * @see #getDefaultValue()
	 * @generated
	 * @ordered
	 */
	protected static final String DEFAULT_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDefaultValue() <em>Default Value</em>}' attribute
	 * @see #getDefaultValue()
	 * @generated
	 * @ordered
	 */
	protected String defaultValue = DEFAULT_VALUE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getListOfValues() <em>List Of Values</em>}' reference
	 * @see #getListOfValues()
	 * @generated
	 * @ordered
	 */
	protected Form listOfValues;

	/**
	 * The cached value of the '{@link #getDTOAttribute() <em>DTO Attribute</em>}' reference
	 * @see #getDTOAttribute()
	 * @generated
	 * @ordered
	 */
	protected DTOBeanAttribute dTOAttribute;

	/**
	 * The default value of the '{@link #isAddFormLinkToLabel() <em>Add Form Link To Label</em>}' attribute
	 * @see #isAddFormLinkToLabel()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADD_FORM_LINK_TO_LABEL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAddFormLinkToLabel() <em>Add Form Link To Label</em>}' attribute
	 * @see #isAddFormLinkToLabel()
	 * @generated
	 * @ordered
	 */
	protected boolean addFormLinkToLabel = ADD_FORM_LINK_TO_LABEL_EDEFAULT;

	/**
	 * @generated
	 */
	protected FormFieldImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.FORM_FIELD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getColIndex()
	 * @generated
	 */
	@Override
	public int getColIndex() {
		return colIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setColIndex(int)
	 * @generated
	 */
	@Override
	public void setColIndex(int newColIndex) {
		final int oldColIndex = colIndex;
		colIndex = newColIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__COL_INDEX, oldColIndex, colIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getRowIndex()
	 * @generated
	 */
	@Override
	public int getRowIndex() {
		return rowIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setRowIndex(int)
	 */
	@Override
	public void setRowIndex(int newRowIndex) {
		final int oldRowIndex = rowIndex;
		rowIndex = newRowIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__ROW_INDEX, oldRowIndex, rowIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#isVisible()
	 * @generated
	 */
	@Override
	public boolean isVisible() {
		return visible;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setVisible(boolean)
	 * @generated
	 */
	@Override
	public void setVisible(boolean newVisible) {
		final boolean oldVisible = visible;
		visible = newVisible;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__VISIBLE, oldVisible, visible));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__LABEL, oldLabel, label));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#isSpanCols()
	 * @generated
	 */
	@Override
	public boolean isSpanCols() {
		return spanCols;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setSpanCols(boolean)
	 * @generated
	 */
	@Override
	public void setSpanCols(boolean newSpanCols) {
		final boolean oldSpanCols = spanCols;
		spanCols = newSpanCols;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__SPAN_COLS, oldSpanCols, spanCols));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#isReadonly()
	 * @generated
	 */
	@Override
	public boolean isReadonly() {
		return readonly;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setReadonly(boolean)
	 * @generated
	 */
	@Override
	public void setReadonly(boolean newReadonly) {
		final boolean oldReadonly = readonly;
		readonly = newReadonly;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__READONLY, oldReadonly, readonly));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#isMandatory()
	 * @generated
	 */
	@Override
	public boolean isMandatory() {
		return mandatory;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setMandatory(boolean)
	 * @generated
	 */
	@Override
	public void setMandatory(boolean newMandatory) {
		final boolean oldMandatory = mandatory;
		mandatory = newMandatory;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__MANDATORY, oldMandatory, mandatory));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getFieldType()
	 * @generated
	 */
	@Override
	public FormFieldTypeEnumeration getFieldType() {
		return fieldType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setFieldType(net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setFieldType(FormFieldTypeEnumeration newFieldType) {
		final FormFieldTypeEnumeration oldFieldType = fieldType;
		fieldType = newFieldType == null ? FIELD_TYPE_EDEFAULT : newFieldType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__FIELD_TYPE, oldFieldType, fieldType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getPanel()
	 * @generated
	 */
	@Override
	public FormPanel getPanel() {
		if (eContainerFeatureID() != ClientPackage.FORM_FIELD__PANEL)
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
		msgs = eBasicSetContainer((InternalEObject) newPanel, ClientPackage.FORM_FIELD__PANEL, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setPanel(net.codecadenza.eclipse.model.client.FormPanel)
	 * @generated
	 */
	@Override
	public void setPanel(FormPanel newPanel) {
		if (newPanel != eInternalContainer() || (eContainerFeatureID() != ClientPackage.FORM_FIELD__PANEL && newPanel != null)) {
			if (EcoreUtil.isAncestor(this, newPanel))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newPanel != null)
				msgs = ((InternalEObject) newPanel).eInverseAdd(this, ClientPackage.FORM_PANEL__FIELDS, FormPanel.class, msgs);

			msgs = basicSetPanel(newPanel, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__PANEL, newPanel, newPanel));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getWidth()
	 * @generated
	 */
	@Override
	public int getWidth() {
		return width;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setWidth(int)
	 * @generated
	 */
	@Override
	public void setWidth(int newWidth) {
		final int oldWidth = width;
		width = newWidth;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__WIDTH, oldWidth, width));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getDefaultValue()
	 * @generated
	 */
	@Override
	public String getDefaultValue() {
		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setDefaultValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDefaultValue(String newDefaultValue) {
		final String oldDefaultValue = defaultValue;
		defaultValue = newDefaultValue;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__DEFAULT_VALUE, oldDefaultValue, defaultValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getListOfValues()
	 * @generated
	 */
	@Override
	public Form getListOfValues() {
		if (listOfValues != null && listOfValues.eIsProxy()) {
			final var oldListOfValues = (InternalEObject) listOfValues;
			listOfValues = (Form) eResolveProxy(oldListOfValues);

			if (listOfValues != oldListOfValues && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_FIELD__LIST_OF_VALUES, oldListOfValues,
						listOfValues));
		}

		return listOfValues;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Form basicGetListOfValues() {
		return listOfValues;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setListOfValues(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setListOfValues(Form newListOfValues) {
		final Form oldListOfValues = listOfValues;
		listOfValues = newListOfValues;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__LIST_OF_VALUES, oldListOfValues, listOfValues));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getDTOAttribute()
	 * @generated
	 */
	@Override
	public DTOBeanAttribute getDTOAttribute() {
		if (dTOAttribute != null && dTOAttribute.eIsProxy()) {
			final var oldDTOAttribute = (InternalEObject) dTOAttribute;
			dTOAttribute = (DTOBeanAttribute) eResolveProxy(oldDTOAttribute);

			if (dTOAttribute != oldDTOAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_FIELD__DTO_ATTRIBUTE, oldDTOAttribute,
						dTOAttribute));
		}

		return dTOAttribute;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DTOBeanAttribute basicGetDTOAttribute() {
		return dTOAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setDTOAttribute(net.codecadenza.eclipse.model.dto.DTOBeanAttribute)
	 * @generated
	 */
	@Override
	public void setDTOAttribute(DTOBeanAttribute newDTOAttribute) {
		final DTOBeanAttribute oldDTOAttribute = dTOAttribute;
		dTOAttribute = newDTOAttribute;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_FIELD__DTO_ATTRIBUTE, oldDTOAttribute, dTOAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#isAddFormLinkToLabel()
	 * @generated
	 */
	@Override
	public boolean isAddFormLinkToLabel() {
		return addFormLinkToLabel;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#setAddFormLinkToLabel(boolean)
	 * @generated
	 */
	@Override
	public void setAddFormLinkToLabel(boolean newAddFormLinkToLabel) {
		final boolean oldAddFormLinkToLabel = addFormLinkToLabel;
		addFormLinkToLabel = newAddFormLinkToLabel;

		if (eNotificationRequired()) {
			final int featureID = ClientPackage.FORM_FIELD__ADD_FORM_LINK_TO_LABEL;

			eNotify(new ENotificationImpl(this, Notification.SET, featureID, oldAddFormLinkToLabel, addFormLinkToLabel));
		}
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
			case ClientPackage.FORM_FIELD__PANEL:
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
			case ClientPackage.FORM_FIELD__PANEL:
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
			case ClientPackage.FORM_FIELD__PANEL:
				return eInternalContainer().eInverseRemove(this, ClientPackage.FORM_PANEL__FIELDS, FormPanel.class, msgs);
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
			case ClientPackage.FORM_FIELD__NAME:
				return getName();
			case ClientPackage.FORM_FIELD__COL_INDEX:
				return getColIndex();
			case ClientPackage.FORM_FIELD__ROW_INDEX:
				return getRowIndex();
			case ClientPackage.FORM_FIELD__VISIBLE:
				return isVisible();
			case ClientPackage.FORM_FIELD__LABEL:
				return getLabel();
			case ClientPackage.FORM_FIELD__SPAN_COLS:
				return isSpanCols();
			case ClientPackage.FORM_FIELD__READONLY:
				return isReadonly();
			case ClientPackage.FORM_FIELD__MANDATORY:
				return isMandatory();
			case ClientPackage.FORM_FIELD__FIELD_TYPE:
				return getFieldType();
			case ClientPackage.FORM_FIELD__PANEL:
				return getPanel();
			case ClientPackage.FORM_FIELD__WIDTH:
				return getWidth();
			case ClientPackage.FORM_FIELD__DEFAULT_VALUE:
				return getDefaultValue();
			case ClientPackage.FORM_FIELD__LIST_OF_VALUES:
				if (resolve)
					return getListOfValues();

				return basicGetListOfValues();
			case ClientPackage.FORM_FIELD__DTO_ATTRIBUTE:
				if (resolve)
					return getDTOAttribute();

				return basicGetDTOAttribute();
			case ClientPackage.FORM_FIELD__ADD_FORM_LINK_TO_LABEL:
				return isAddFormLinkToLabel();
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
			case ClientPackage.FORM_FIELD__NAME:
				setName((String) newValue);
				return;
			case ClientPackage.FORM_FIELD__COL_INDEX:
				setColIndex((Integer) newValue);
				return;
			case ClientPackage.FORM_FIELD__ROW_INDEX:
				setRowIndex((Integer) newValue);
				return;
			case ClientPackage.FORM_FIELD__VISIBLE:
				setVisible((Boolean) newValue);
				return;
			case ClientPackage.FORM_FIELD__LABEL:
				setLabel((String) newValue);
				return;
			case ClientPackage.FORM_FIELD__SPAN_COLS:
				setSpanCols((Boolean) newValue);
				return;
			case ClientPackage.FORM_FIELD__READONLY:
				setReadonly((Boolean) newValue);
				return;
			case ClientPackage.FORM_FIELD__MANDATORY:
				setMandatory((Boolean) newValue);
				return;
			case ClientPackage.FORM_FIELD__FIELD_TYPE:
				setFieldType((FormFieldTypeEnumeration) newValue);
				return;
			case ClientPackage.FORM_FIELD__PANEL:
				setPanel((FormPanel) newValue);
				return;
			case ClientPackage.FORM_FIELD__WIDTH:
				setWidth((Integer) newValue);
				return;
			case ClientPackage.FORM_FIELD__DEFAULT_VALUE:
				setDefaultValue((String) newValue);
				return;
			case ClientPackage.FORM_FIELD__LIST_OF_VALUES:
				setListOfValues((Form) newValue);
				return;
			case ClientPackage.FORM_FIELD__DTO_ATTRIBUTE:
				setDTOAttribute((DTOBeanAttribute) newValue);
				return;
			case ClientPackage.FORM_FIELD__ADD_FORM_LINK_TO_LABEL:
				setAddFormLinkToLabel((Boolean) newValue);
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
			case ClientPackage.FORM_FIELD__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__COL_INDEX:
				setColIndex(COL_INDEX_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__ROW_INDEX:
				setRowIndex(ROW_INDEX_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__VISIBLE:
				setVisible(VISIBLE_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__SPAN_COLS:
				setSpanCols(SPAN_COLS_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__READONLY:
				setReadonly(READONLY_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__MANDATORY:
				setMandatory(MANDATORY_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__FIELD_TYPE:
				setFieldType(FIELD_TYPE_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__PANEL:
				setPanel((FormPanel) null);
				return;
			case ClientPackage.FORM_FIELD__WIDTH:
				setWidth(WIDTH_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__DEFAULT_VALUE:
				setDefaultValue(DEFAULT_VALUE_EDEFAULT);
				return;
			case ClientPackage.FORM_FIELD__LIST_OF_VALUES:
				setListOfValues((Form) null);
				return;
			case ClientPackage.FORM_FIELD__DTO_ATTRIBUTE:
				setDTOAttribute((DTOBeanAttribute) null);
				return;
			case ClientPackage.FORM_FIELD__ADD_FORM_LINK_TO_LABEL:
				setAddFormLinkToLabel(ADD_FORM_LINK_TO_LABEL_EDEFAULT);
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
			case ClientPackage.FORM_FIELD__NAME:
				return name != null;
			case ClientPackage.FORM_FIELD__COL_INDEX:
				return colIndex != COL_INDEX_EDEFAULT;
			case ClientPackage.FORM_FIELD__ROW_INDEX:
				return rowIndex != ROW_INDEX_EDEFAULT;
			case ClientPackage.FORM_FIELD__VISIBLE:
				return visible != VISIBLE_EDEFAULT;
			case ClientPackage.FORM_FIELD__LABEL:
				return label != null;
			case ClientPackage.FORM_FIELD__SPAN_COLS:
				return spanCols != SPAN_COLS_EDEFAULT;
			case ClientPackage.FORM_FIELD__READONLY:
				return readonly != READONLY_EDEFAULT;
			case ClientPackage.FORM_FIELD__MANDATORY:
				return mandatory != MANDATORY_EDEFAULT;
			case ClientPackage.FORM_FIELD__FIELD_TYPE:
				return fieldType != FIELD_TYPE_EDEFAULT;
			case ClientPackage.FORM_FIELD__PANEL:
				return getPanel() != null;
			case ClientPackage.FORM_FIELD__WIDTH:
				return width != WIDTH_EDEFAULT;
			case ClientPackage.FORM_FIELD__DEFAULT_VALUE:
				return defaultValue != null;
			case ClientPackage.FORM_FIELD__LIST_OF_VALUES:
				return listOfValues != null;
			case ClientPackage.FORM_FIELD__DTO_ATTRIBUTE:
				return dTOAttribute != null;
			case ClientPackage.FORM_FIELD__ADD_FORM_LINK_TO_LABEL:
				return addFormLinkToLabel != ADD_FORM_LINK_TO_LABEL_EDEFAULT;
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
		result.append(", visible: ");
		result.append(visible);
		result.append(", label: ");
		result.append(label);
		result.append(", spanCols: ");
		result.append(spanCols);
		result.append(", readonly: ");
		result.append(readonly);
		result.append(", mandatory: ");
		result.append(mandatory);
		result.append(", fieldType: ");
		result.append(fieldType);
		result.append(", width: ");
		result.append(width);
		result.append(", defaultValue: ");
		result.append(defaultValue);
		result.append(", addFormLinkToLabel: ");
		result.append(addFormLinkToLabel);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#isHidden()
	 * @generated not
	 */
	@Override
	public boolean isHidden() {
		final FormFieldTypeEnumeration type = getFieldType();

		if (type == FormFieldTypeEnumeration.SELECTION_BY_CLIENT || type == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
				|| type == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
			return true;

		return !isVisible();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#findListOfValues()
	 * @generated not
	 */
	@Override
	public Form findListOfValues() {
		// The DTO attribute must reference a DTO bean!
		if (getDTOAttribute().getReferencedDTOBean() == null)
			return null;

		final Form form = getPanel().getForm();
		final DomainObject refDomainObject = getDTOAttribute().getReferencedDTOBean().getDomainObject();
		final Project project = refDomainObject.getNamespace().getProject();
		final boolean isPkTypeString = refDomainObject.getPKAttribute().getJavaType().isString();
		final boolean isUpdateForm = form.getFormType() == FormTypeEnumeration.UPDATE;
		final boolean standardValidation = project.getValidationType() == ValidationTypeEnumeration.STANDARD;
		boolean optional = false;

		if (getDTOAttribute().getAssociation() instanceof final ManyToOneAssociation mto)
			optional = mto.isOptional();

		// A list-of-values must not be used in an 'UPDATE' form of a JSF application that uses facades if the field is optional and
		// has a primary key attribute of type String! Otherwise, after using the list-of-values, a runtime exception will be thrown
		// if the user attempts to save the changes afterwards. In this case, the EntityManager validates and attaches the object that
		// is bound to the form. However, the validation fails because the primary key attribute contains an empty String!
		if (!project.isBoundaryMode() && project.hasJSFClient() && isPkTypeString && isUpdateForm && optional && standardValidation)
			return null;

		return project.getLOVFormsOfProject().stream().filter(lovForm -> lovForm.getDomainObject().equals(refDomainObject))
				.findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#fillEmptyColumn()
	 * @generated not
	 */
	@Override
	public boolean fillEmptyGridColumn() {
		FormField nextField = null;
		FormField previousField = null;

		// Skip fields that really span over two columns!
		if (isSpanCols() && getColIndex() == 1)
			return false;

		final int fieldIndex = getPanel().getFields().indexOf(this);

		if (getColIndex() == 1) {
			// If the field is in the first column we must search for the next field!
			if ((fieldIndex + 1) < getPanel().getFields().size()) {
				nextField = getPanel().getFields().get(fieldIndex + 1);

				// The column must be filled if both fields are placed on different rows
				if (nextField.getRowIndex() != getRowIndex())
					return true;
			}
			else {
				// If the current field is the last in the list we must fill up the second column!
				return true;
			}
		}
		else if (fieldIndex > 0) {
			// If the field is in the second column we must search for the previous field!
			previousField = getPanel().getFields().get(fieldIndex - 1);

			// The column must be filled if both fields are placed on different rows
			if (previousField.getRowIndex() != getRowIndex())
				return true;
		}
		else {
			// If the current field in the second column is the first in the list we must fill up the first column!
			return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormField#getConvertedDefaultValue()
	 * @generated not
	 */
	@Override
	public String getConvertedDefaultValue() {
		if (getDefaultValue() == null || getDefaultValue().isEmpty())
			return "";

		final JavaType type = getDTOAttribute().getDomainAttribute().getJavaType();

		if (type.isString())
			return "\"" + getDefaultValue() + "\"";
		else if (type.isChar())
			return "'" + getDefaultValue() + "'";
		else if (type.isType(JavaType.INT, JavaType.DOUBLE))
			return getDefaultValue();
		else if (type.isType(JavaType.LONG))
			return getDefaultValue() + "L";
		else if (type.isType(JavaType.LONG_OBJ))
			return "Long.valueOf(" + getDefaultValue() + ")";
		else if (type.isType(JavaType.INTEGER))
			return "Integer.valueOf(" + getDefaultValue() + ")";
		else if (type.isUUID())
			return "java.util.UUID.randomUUID()";
		else if (type.isType(JavaType.DOUBLE_OBJ))
			return "Double.valueOf(" + getDefaultValue() + ")";
		else if (type.isType(JavaType.FLOAT))
			return getDefaultValue() + "F";
		else if (type.isType(JavaType.FLOAT_OBJ))
			return "Float.valueOf(" + getDefaultValue() + "F)";
		else if (type.isBigDecimal())
			return "new java.math.BigDecimal(\"" + getDefaultValue() + "\")";

		throw new IllegalStateException("The type '" + type.getName() + "' is not supported!");
	}

}
