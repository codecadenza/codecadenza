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
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Table Column Field</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#isVisible <em>Visible</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#isIdentifier <em>Identifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getFormTable <em>Form Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getWidth <em>Width</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getTitle <em>Title</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getLovForm <em>Lov Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getFieldType <em>Field Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#isAssociationRef <em>Association Ref</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#isSearchable <em>Searchable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl#getDTOAttribute <em>DTO Attribute</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class TableColumnFieldImpl extends EObjectImpl implements TableColumnField {
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
	 * The default value of the '{@link #isIdentifier() <em>Identifier</em>}' attribute
	 * @see #isIdentifier()
	 * @generated
	 * @ordered
	 */
	protected static final boolean IDENTIFIER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isIdentifier() <em>Identifier</em>}' attribute
	 * @see #isIdentifier()
	 * @generated
	 * @ordered
	 */
	protected boolean identifier = IDENTIFIER_EDEFAULT;

	/**
	 * The default value of the '{@link #getWidth() <em>Width</em>}' attribute
	 * @see #getWidth()
	 * @generated
	 * @ordered
	 */
	protected static final int WIDTH_EDEFAULT = 100;

	/**
	 * The cached value of the '{@link #getWidth() <em>Width</em>}' attribute
	 * @see #getWidth()
	 * @generated
	 * @ordered
	 */
	protected int width = WIDTH_EDEFAULT;

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
	 * The cached value of the '{@link #getLovForm() <em>Lov Form</em>}' reference
	 * @see #getLovForm()
	 * @generated
	 * @ordered
	 */
	protected Form lovForm;

	/**
	 * The default value of the '{@link #getFieldType() <em>Field Type</em>}' attribute
	 * @see #getFieldType()
	 * @generated
	 * @ordered
	 */
	protected static final TableColumnFieldTypeEnumeration FIELD_TYPE_EDEFAULT = TableColumnFieldTypeEnumeration.STRING;

	/**
	 * The cached value of the '{@link #getFieldType() <em>Field Type</em>}' attribute
	 * @see #getFieldType()
	 * @generated
	 * @ordered
	 */
	protected TableColumnFieldTypeEnumeration fieldType = FIELD_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #isAssociationRef() <em>Association Ref</em>}' attribute
	 * @see #isAssociationRef()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ASSOCIATION_REF_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAssociationRef() <em>Association Ref</em>}' attribute
	 * @see #isAssociationRef()
	 * @generated
	 * @ordered
	 */
	protected boolean associationRef = ASSOCIATION_REF_EDEFAULT;

	/**
	 * The default value of the '{@link #isSearchable() <em>Searchable</em>}' attribute
	 * @see #isSearchable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SEARCHABLE_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isSearchable() <em>Searchable</em>}' attribute
	 * @see #isSearchable()
	 * @generated
	 * @ordered
	 */
	protected boolean searchable = SEARCHABLE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getDTOAttribute() <em>DTO Attribute</em>}' reference
	 * @see #getDTOAttribute()
	 * @generated
	 * @ordered
	 */
	protected DTOBeanAttribute dTOAttribute;

	/**
	 * @generated
	 */
	protected TableColumnFieldImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.TABLE_COLUMN_FIELD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getColIndex()
	 * @generated
	 */
	@Override
	public int getColIndex() {
		return colIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setColIndex(int)
	 * @generated
	 */
	@Override
	public void setColIndex(int newColIndex) {
		final int oldColIndex = colIndex;
		colIndex = newColIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__COL_INDEX, oldColIndex, colIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isVisible()
	 * @generated
	 */
	@Override
	public boolean isVisible() {
		return visible;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setVisible(boolean)
	 * @generated
	 */
	@Override
	public void setVisible(boolean newVisible) {
		final boolean oldVisible = visible;
		visible = newVisible;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__VISIBLE, oldVisible, visible));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isIdentifier()
	 * @generated
	 */
	@Override
	public boolean isIdentifier() {
		return identifier;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setIdentifier(boolean)
	 * @generated
	 */
	@Override
	public void setIdentifier(boolean newIdentifier) {
		final boolean oldIdentifier = identifier;
		identifier = newIdentifier;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__IDENTIFIER, oldIdentifier, identifier));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getFormTable()
	 * @generated
	 */
	@Override
	public FormTable getFormTable() {
		if (eContainerFeatureID() != ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE)
			return null;

		return (FormTable) eInternalContainer();
	}

	/**
	 * @param newFormTable
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetFormTable(FormTable newFormTable, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newFormTable, ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setFormTable(net.codecadenza.eclipse.model.client.FormTable)
	 * @generated
	 */
	@Override
	public void setFormTable(FormTable newFormTable) {
		if (newFormTable != eInternalContainer()
				|| (eContainerFeatureID() != ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE && newFormTable != null)) {
			if (EcoreUtil.isAncestor(this, newFormTable))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newFormTable != null)
				msgs = ((InternalEObject) newFormTable).eInverseAdd(this, ClientPackage.FORM_TABLE__FIELDS, FormTable.class, msgs);

			msgs = basicSetFormTable(newFormTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE, newFormTable,
					newFormTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getWidth()
	 * @generated
	 */
	@Override
	public int getWidth() {
		return width;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setWidth(int)
	 * @generated
	 */
	@Override
	public void setWidth(int newWidth) {
		final int oldWidth = width;
		width = newWidth;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__WIDTH, oldWidth, width));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getTitle()
	 * @generated
	 */
	@Override
	public String getTitle() {
		return title;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setTitle(java.lang.String)
	 * @generated
	 */
	@Override
	public void setTitle(String newTitle) {
		final String oldTitle = title;
		title = newTitle;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__TITLE, oldTitle, title));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getLovForm()
	 * @generated
	 */
	@Override
	public Form getLovForm() {
		if (lovForm != null && lovForm.eIsProxy()) {
			final var oldLovForm = (InternalEObject) lovForm;
			lovForm = (Form) eResolveProxy(oldLovForm);

			if (lovForm != oldLovForm && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TABLE_COLUMN_FIELD__LOV_FORM, oldLovForm, lovForm));
		}

		return lovForm;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Form basicGetLovForm() {
		return lovForm;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setLovForm(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setLovForm(Form newLovForm) {
		final Form oldLovForm = lovForm;
		lovForm = newLovForm;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__LOV_FORM, oldLovForm, lovForm));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getFieldType()
	 * @generated
	 */
	@Override
	public TableColumnFieldTypeEnumeration getFieldType() {
		return fieldType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setFieldType(net.codecadenza.eclipse.model.client.
	 * TableColumnFieldTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setFieldType(TableColumnFieldTypeEnumeration newFieldType) {
		final TableColumnFieldTypeEnumeration oldFieldType = fieldType;
		fieldType = newFieldType == null ? FIELD_TYPE_EDEFAULT : newFieldType;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__FIELD_TYPE, oldFieldType, fieldType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isAssociationRef()
	 * @generated
	 */
	@Override
	public boolean isAssociationRef() {
		return associationRef;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setAssociationRef(boolean)
	 * @generated
	 */
	@Override
	public void setAssociationRef(boolean newAssociationRef) {
		final boolean oldAssociationRef = associationRef;
		associationRef = newAssociationRef;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__ASSOCIATION_REF, oldAssociationRef,
					associationRef));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isSearchable()
	 * @generated
	 */
	@Override
	public boolean isSearchable() {
		return searchable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#setSearchable(boolean)
	 * @generated
	 */
	@Override
	public void setSearchable(boolean newSearchable) {
		final boolean oldSearchable = searchable;
		searchable = newSearchable;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__SEARCHABLE, oldSearchable, searchable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getDTOAttribute()
	 * @generated
	 */
	@Override
	public DTOBeanAttribute getDTOAttribute() {
		if (dTOAttribute != null && dTOAttribute.eIsProxy()) {
			final var oldDTOAttribute = (InternalEObject) dTOAttribute;
			dTOAttribute = (DTOBeanAttribute) eResolveProxy(oldDTOAttribute);

			if (dTOAttribute != oldDTOAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TABLE_COLUMN_FIELD__DTO_ATTRIBUTE,
						oldDTOAttribute, dTOAttribute));
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
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#
	 * setDTOAttribute(net.codecadenza.eclipse.model.dto.DTOBeanAttribute)
	 * @generated
	 */
	@Override
	public void setDTOAttribute(DTOBeanAttribute newDTOAttribute) {
		final DTOBeanAttribute oldDTOAttribute = dTOAttribute;
		dTOAttribute = newDTOAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TABLE_COLUMN_FIELD__DTO_ATTRIBUTE, oldDTOAttribute,
					dTOAttribute));
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
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetFormTable((FormTable) otherEnd, msgs);
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
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				return basicSetFormTable(null, msgs);
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
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				return eInternalContainer().eInverseRemove(this, ClientPackage.FORM_TABLE__FIELDS, FormTable.class, msgs);
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
			case ClientPackage.TABLE_COLUMN_FIELD__COL_INDEX:
				return getColIndex();
			case ClientPackage.TABLE_COLUMN_FIELD__VISIBLE:
				return isVisible();
			case ClientPackage.TABLE_COLUMN_FIELD__IDENTIFIER:
				return isIdentifier();
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				return getFormTable();
			case ClientPackage.TABLE_COLUMN_FIELD__WIDTH:
				return getWidth();
			case ClientPackage.TABLE_COLUMN_FIELD__TITLE:
				return getTitle();
			case ClientPackage.TABLE_COLUMN_FIELD__LOV_FORM:
				if (resolve)
					return getLovForm();

				return basicGetLovForm();
			case ClientPackage.TABLE_COLUMN_FIELD__FIELD_TYPE:
				return getFieldType();
			case ClientPackage.TABLE_COLUMN_FIELD__ASSOCIATION_REF:
				return isAssociationRef();
			case ClientPackage.TABLE_COLUMN_FIELD__SEARCHABLE:
				return isSearchable();
			case ClientPackage.TABLE_COLUMN_FIELD__DTO_ATTRIBUTE:
				if (resolve)
					return getDTOAttribute();

				return basicGetDTOAttribute();
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
			case ClientPackage.TABLE_COLUMN_FIELD__COL_INDEX:
				setColIndex((Integer) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__VISIBLE:
				setVisible((Boolean) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__IDENTIFIER:
				setIdentifier((Boolean) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				setFormTable((FormTable) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__WIDTH:
				setWidth((Integer) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__TITLE:
				setTitle((String) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__LOV_FORM:
				setLovForm((Form) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__FIELD_TYPE:
				setFieldType((TableColumnFieldTypeEnumeration) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__ASSOCIATION_REF:
				setAssociationRef((Boolean) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__SEARCHABLE:
				setSearchable((Boolean) newValue);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__DTO_ATTRIBUTE:
				setDTOAttribute((DTOBeanAttribute) newValue);
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
			case ClientPackage.TABLE_COLUMN_FIELD__COL_INDEX:
				setColIndex(COL_INDEX_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__VISIBLE:
				setVisible(VISIBLE_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__IDENTIFIER:
				setIdentifier(IDENTIFIER_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				setFormTable((FormTable) null);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__WIDTH:
				setWidth(WIDTH_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__TITLE:
				setTitle(TITLE_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__LOV_FORM:
				setLovForm((Form) null);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__FIELD_TYPE:
				setFieldType(FIELD_TYPE_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__ASSOCIATION_REF:
				setAssociationRef(ASSOCIATION_REF_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__SEARCHABLE:
				setSearchable(SEARCHABLE_EDEFAULT);
				return;
			case ClientPackage.TABLE_COLUMN_FIELD__DTO_ATTRIBUTE:
				setDTOAttribute((DTOBeanAttribute) null);
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
			case ClientPackage.TABLE_COLUMN_FIELD__COL_INDEX:
				return colIndex != COL_INDEX_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__VISIBLE:
				return visible != VISIBLE_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__IDENTIFIER:
				return identifier != IDENTIFIER_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__FORM_TABLE:
				return getFormTable() != null;
			case ClientPackage.TABLE_COLUMN_FIELD__WIDTH:
				return width != WIDTH_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__TITLE:
				return title != null;
			case ClientPackage.TABLE_COLUMN_FIELD__LOV_FORM:
				return lovForm != null;
			case ClientPackage.TABLE_COLUMN_FIELD__FIELD_TYPE:
				return fieldType != FIELD_TYPE_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__ASSOCIATION_REF:
				return associationRef != ASSOCIATION_REF_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__SEARCHABLE:
				return searchable != SEARCHABLE_EDEFAULT;
			case ClientPackage.TABLE_COLUMN_FIELD__DTO_ATTRIBUTE:
				return dTOAttribute != null;
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
		result.append(" (colIndex: ");
		result.append(colIndex);
		result.append(", visible: ");
		result.append(visible);
		result.append(", identifier: ");
		result.append(identifier);
		result.append(", width: ");
		result.append(width);
		result.append(", title: ");
		result.append(title);
		result.append(", fieldType: ");
		result.append(fieldType);
		result.append(", associationRef: ");
		result.append(associationRef);
		result.append(", searchable: ");
		result.append(searchable);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#hasTemporalType()
	 * @generated not
	 */
	@Override
	public boolean hasTemporalType() {
		return fieldType == TableColumnFieldTypeEnumeration.DATE || fieldType == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR
				|| fieldType == TableColumnFieldTypeEnumeration.LOCAL_DATE
				|| fieldType == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#hasDateFormat()
	 * @generated not
	 */
	@Override
	public boolean hasDateFormat() {
		if (fieldType == TableColumnFieldTypeEnumeration.LOCAL_DATE)
			return true;

		return getDTOAttribute().getDomainAttribute().getTemporalType() == TemporalTypeEnumeration.DATE;
	}

}
