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
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.PACK_PAGE_OBJECT;
import static net.codecadenza.eclipse.shared.Constants.UI_PANEL_FOLDER;

import java.util.Collection;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldComparator;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaTestFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Form Panel</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getColumnCount <em>Column Count</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getColIndex <em>Col Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getRowIndex <em>Row Index</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#isVerticalspan <em>Verticalspan</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#isHorizontalSpan <em>Horizontal Span</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getFormGroup <em>Form Group</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getForm <em>Form</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#isDrawBorder <em>Draw Border</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getBasePanel <em>Base Panel</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getFormTable <em>Form Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getFields <em>Fields</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getActions <em>Actions</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getDTO <em>DTO</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getBoundaryMethod <em>Boundary Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl#getAssociation <em>Association</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FormPanelImpl extends EObjectImpl implements FormPanel {
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
	 * The default value of the '{@link #getColumnCount() <em>Column Count</em>}' attribute
	 * @see #getColumnCount()
	 * @generated
	 * @ordered
	 */
	protected static final int COLUMN_COUNT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getColumnCount() <em>Column Count</em>}' attribute
	 * @see #getColumnCount()
	 * @generated
	 * @ordered
	 */
	protected int columnCount = COLUMN_COUNT_EDEFAULT;

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
	 * The default value of the '{@link #isVerticalspan() <em>Verticalspan</em>}' attribute
	 * @see #isVerticalspan()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VERTICALSPAN_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isVerticalspan() <em>Verticalspan</em>}' attribute
	 * @see #isVerticalspan()
	 * @generated
	 * @ordered
	 */
	protected boolean verticalspan = VERTICALSPAN_EDEFAULT;

	/**
	 * The default value of the '{@link #isHorizontalSpan() <em>Horizontal Span</em>}' attribute
	 * @see #isHorizontalSpan()
	 * @generated
	 * @ordered
	 */
	protected static final boolean HORIZONTAL_SPAN_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isHorizontalSpan() <em>Horizontal Span</em>}' attribute
	 * @see #isHorizontalSpan()
	 * @generated
	 * @ordered
	 */
	protected boolean horizontalSpan = HORIZONTAL_SPAN_EDEFAULT;

	/**
	 * The cached value of the '{@link #getFormGroup() <em>Form Group</em>}' reference
	 * @see #getFormGroup()
	 * @generated
	 * @ordered
	 */
	protected FormGroup formGroup;

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
	 * The cached value of the '{@link #getForm() <em>Form</em>}' reference
	 * @see #getForm()
	 * @generated
	 * @ordered
	 */
	protected Form form;

	/**
	 * The default value of the '{@link #isDrawBorder() <em>Draw Border</em>}' attribute
	 * @see #isDrawBorder()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DRAW_BORDER_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isDrawBorder() <em>Draw Border</em>}' attribute
	 * @see #isDrawBorder()
	 * @generated
	 * @ordered
	 */
	protected boolean drawBorder = DRAW_BORDER_EDEFAULT;

	/**
	 * The cached value of the '{@link #getBasePanel() <em>Base Panel</em>}' reference
	 * @see #getBasePanel()
	 * @generated
	 * @ordered
	 */
	protected FormPanel basePanel;

	/**
	 * The cached value of the '{@link #getFormTable() <em>Form Table</em>}' reference
	 * @see #getFormTable()
	 * @generated
	 * @ordered
	 */
	protected FormTable formTable;

	/**
	 * The cached value of the '{@link #getFields() <em>Fields</em>}' containment reference list
	 * @see #getFields()
	 * @generated
	 * @ordered
	 */
	protected EList<FormField> fields;

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
	 * The cached value of the '{@link #getBoundaryMethod() <em>Boundary Method</em>}' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod boundaryMethod;

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
	protected FormPanelImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.FORM_PANEL;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getColumnCount()
	 * @generated
	 */
	@Override
	public int getColumnCount() {
		return columnCount;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setColumnCount(int)
	 * @generated
	 */
	@Override
	public void setColumnCount(int newColumnCount) {
		final int oldColumnCount = columnCount;
		columnCount = newColumnCount;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__COLUMN_COUNT, oldColumnCount, columnCount));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getColIndex()
	 * @generated
	 */
	@Override
	public int getColIndex() {
		return colIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setColIndex(int)
	 * @generated
	 */
	@Override
	public void setColIndex(int newColIndex) {
		final int oldColIndex = colIndex;
		colIndex = newColIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__COL_INDEX, oldColIndex, colIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getRowIndex()
	 * @generated
	 */
	@Override
	public int getRowIndex() {
		return rowIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setRowIndex(int)
	 * @generated
	 */
	@Override
	public void setRowIndex(int newRowIndex) {
		final int oldRowIndex = rowIndex;
		rowIndex = newRowIndex;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__ROW_INDEX, oldRowIndex, rowIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#isVerticalspan()
	 * @generated
	 */
	@Override
	public boolean isVerticalspan() {
		return verticalspan;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setVerticalspan(boolean)
	 * @generated
	 */
	@Override
	public void setVerticalspan(boolean newVerticalspan) {
		final boolean oldVerticalspan = verticalspan;
		verticalspan = newVerticalspan;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__VERTICALSPAN, oldVerticalspan, verticalspan));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#isHorizontalSpan()
	 * @generated
	 */
	@Override
	public boolean isHorizontalSpan() {
		return horizontalSpan;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setHorizontalSpan(boolean)
	 * @generated
	 */
	@Override
	public void setHorizontalSpan(boolean newHorizontalSpan) {
		final boolean oldHorizontalSpan = horizontalSpan;
		horizontalSpan = newHorizontalSpan;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__HORIZONTAL_SPAN, oldHorizontalSpan,
					horizontalSpan));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFormGroup()
	 * @generated
	 */
	@Override
	public FormGroup getFormGroup() {
		if (formGroup != null && formGroup.eIsProxy()) {
			final var oldFormGroup = (InternalEObject) formGroup;
			formGroup = (FormGroup) eResolveProxy(oldFormGroup);

			if (formGroup != oldFormGroup && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__FORM_GROUP, oldFormGroup, formGroup));
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
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__FORM_GROUP, oldFormGroup,
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
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setFormGroup(net.codecadenza.eclipse.model.client.FormGroup)
	 * @generated
	 */
	@Override
	public void setFormGroup(FormGroup newFormGroup) {
		if (newFormGroup != formGroup) {
			NotificationChain msgs = null;

			if (formGroup != null)
				msgs = ((InternalEObject) formGroup).eInverseRemove(this, ClientPackage.FORM_GROUP__PANELS, FormGroup.class, msgs);

			if (newFormGroup != null)
				msgs = ((InternalEObject) newFormGroup).eInverseAdd(this, ClientPackage.FORM_GROUP__PANELS, FormGroup.class, msgs);

			msgs = basicSetFormGroup(newFormGroup, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__FORM_GROUP, newFormGroup, newFormGroup));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__LABEL, oldLabel, label));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getForm()
	 * @generated
	 */
	@Override
	public Form getForm() {
		if (form != null && form.eIsProxy()) {
			final var oldForm = (InternalEObject) form;
			form = (Form) eResolveProxy(oldForm);

			if (form != oldForm && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__FORM, oldForm, form));
		}

		return form;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Form basicGetForm() {
		return form;
	}

	/**
	 * @param newForm
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetForm(Form newForm, NotificationChain msgs) {
		final Form oldForm = form;
		form = newForm;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__FORM, oldForm, newForm);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setForm(net.codecadenza.eclipse.model.client.Form)
	 * @generated
	 */
	@Override
	public void setForm(Form newForm) {
		if (newForm != form) {
			NotificationChain msgs = null;

			if (form != null)
				msgs = ((InternalEObject) form).eInverseRemove(this, ClientPackage.FORM__FORM_PANELS, Form.class, msgs);

			if (newForm != null)
				msgs = ((InternalEObject) newForm).eInverseAdd(this, ClientPackage.FORM__FORM_PANELS, Form.class, msgs);

			msgs = basicSetForm(newForm, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__FORM, newForm, newForm));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#isDrawBorder()
	 * @generated
	 */
	@Override
	public boolean isDrawBorder() {
		return drawBorder;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setDrawBorder(boolean)
	 * @generated
	 */
	@Override
	public void setDrawBorder(boolean newDrawBorder) {
		final boolean oldDrawBorder = drawBorder;
		drawBorder = newDrawBorder;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__DRAW_BORDER, oldDrawBorder, drawBorder));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getBasePanel()
	 * @generated
	 */
	@Override
	public FormPanel getBasePanel() {
		if (basePanel != null && basePanel.eIsProxy()) {
			final var oldBasePanel = (InternalEObject) basePanel;
			basePanel = (FormPanel) eResolveProxy(oldBasePanel);

			if (basePanel != oldBasePanel && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__BASE_PANEL, oldBasePanel, basePanel));
		}

		return basePanel;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public FormPanel basicGetBasePanel() {
		return basePanel;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setBasePanel(net.codecadenza.eclipse.model.client.FormPanel)
	 * @generated
	 */
	@Override
	public void setBasePanel(FormPanel newBasePanel) {
		final FormPanel oldBasePanel = basePanel;
		basePanel = newBasePanel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__BASE_PANEL, oldBasePanel, basePanel));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFormTable()
	 * @generated
	 */
	@Override
	public FormTable getFormTable() {
		if (formTable != null && formTable.eIsProxy()) {
			final var oldFormTable = (InternalEObject) formTable;
			formTable = (FormTable) eResolveProxy(oldFormTable);

			if (formTable != oldFormTable && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__FORM_TABLE, oldFormTable, formTable));
		}

		return formTable;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public FormTable basicGetFormTable() {
		return formTable;
	}

	/**
	 * @param newFormTable
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetFormTable(FormTable newFormTable, NotificationChain msgs) {
		final FormTable oldFormTable = formTable;
		formTable = newFormTable;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__FORM_TABLE, oldFormTable,
					newFormTable);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setFormTable(net.codecadenza.eclipse.model.client.FormTable)
	 * @generated
	 */
	@Override
	public void setFormTable(FormTable newFormTable) {
		if (newFormTable != formTable) {
			NotificationChain msgs = null;

			if (formTable != null)
				msgs = ((InternalEObject) formTable).eInverseRemove(this, ClientPackage.FORM_TABLE__FORM_PANEL, FormTable.class, msgs);

			if (newFormTable != null)
				msgs = ((InternalEObject) newFormTable).eInverseAdd(this, ClientPackage.FORM_TABLE__FORM_PANEL, FormTable.class, msgs);

			msgs = basicSetFormTable(newFormTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__FORM_TABLE, newFormTable, newFormTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFields()
	 * @generated
	 */
	@Override
	public EList<FormField> getFields() {
		if (fields == null)
			fields = new EObjectContainmentWithInverseEList<>(FormField.class, this, ClientPackage.FORM_PANEL__FIELDS,
					ClientPackage.FORM_FIELD__PANEL);

		return fields;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getActions()
	 * @generated
	 */
	@Override
	public EList<FormAction> getActions() {
		if (actions == null)
			actions = new EObjectContainmentWithInverseEList<>(FormAction.class, this, ClientPackage.FORM_PANEL__ACTIONS,
					ClientPackage.FORM_ACTION__PANEL);

		return actions;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getDTO()
	 * @generated
	 */
	@Override
	public DTOBean getDTO() {
		if (dTO != null && dTO.eIsProxy()) {
			final var oldDTO = (InternalEObject) dTO;
			dTO = (DTOBean) eResolveProxy(oldDTO);

			if (dTO != oldDTO && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__DTO, oldDTO, dTO));
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
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setDTO(net.codecadenza.eclipse.model.dto.DTOBean)
	 * @generated
	 */
	@Override
	public void setDTO(DTOBean newDTO) {
		final DTOBean oldDTO = dTO;
		dTO = newDTO;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__DTO, oldDTO, dTO));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getBoundaryMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getBoundaryMethod() {
		if (boundaryMethod != null && boundaryMethod.eIsProxy()) {
			final var oldBoundaryMethod = (InternalEObject) boundaryMethod;
			boundaryMethod = (BoundaryMethod) eResolveProxy(oldBoundaryMethod);

			if (boundaryMethod != oldBoundaryMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__BOUNDARY_METHOD, oldBoundaryMethod,
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
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setBoundaryMethod(net.codecadenza.eclipse.model.boundary.BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setBoundaryMethod(BoundaryMethod newBoundaryMethod) {
		final BoundaryMethod oldBoundaryMethod = boundaryMethod;
		boundaryMethod = newBoundaryMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__BOUNDARY_METHOD, oldBoundaryMethod,
					boundaryMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.FORM_PANEL__ASSOCIATION, oldAssociation,
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
	 * @see net.codecadenza.eclipse.model.client.FormPanel#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.FORM_PANEL__ASSOCIATION, oldAssociation, association));
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
			case ClientPackage.FORM_PANEL__FORM_GROUP:
				if (formGroup != null)
					msgs = ((InternalEObject) formGroup).eInverseRemove(this, ClientPackage.FORM_GROUP__PANELS, FormGroup.class, msgs);

				return basicSetFormGroup((FormGroup) otherEnd, msgs);
			case ClientPackage.FORM_PANEL__FORM:
				if (form != null)
					msgs = ((InternalEObject) form).eInverseRemove(this, ClientPackage.FORM__FORM_PANELS, Form.class, msgs);

				return basicSetForm((Form) otherEnd, msgs);
			case ClientPackage.FORM_PANEL__FORM_TABLE:
				if (formTable != null)
					msgs = ((InternalEObject) formTable).eInverseRemove(this, ClientPackage.FORM_TABLE__FORM_PANEL, FormTable.class, msgs);

				return basicSetFormTable((FormTable) otherEnd, msgs);
			case ClientPackage.FORM_PANEL__FIELDS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getFields()).basicAdd(otherEnd, msgs);
			case ClientPackage.FORM_PANEL__ACTIONS:
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
			case ClientPackage.FORM_PANEL__FORM_GROUP:
				return basicSetFormGroup(null, msgs);
			case ClientPackage.FORM_PANEL__FORM:
				return basicSetForm(null, msgs);
			case ClientPackage.FORM_PANEL__FORM_TABLE:
				return basicSetFormTable(null, msgs);
			case ClientPackage.FORM_PANEL__FIELDS:
				return ((InternalEList<?>) getFields()).basicRemove(otherEnd, msgs);
			case ClientPackage.FORM_PANEL__ACTIONS:
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
			case ClientPackage.FORM_PANEL__NAME:
				return getName();
			case ClientPackage.FORM_PANEL__COLUMN_COUNT:
				return getColumnCount();
			case ClientPackage.FORM_PANEL__COL_INDEX:
				return getColIndex();
			case ClientPackage.FORM_PANEL__ROW_INDEX:
				return getRowIndex();
			case ClientPackage.FORM_PANEL__VERTICALSPAN:
				return isVerticalspan();
			case ClientPackage.FORM_PANEL__HORIZONTAL_SPAN:
				return isHorizontalSpan();
			case ClientPackage.FORM_PANEL__FORM_GROUP:
				if (resolve)
					return getFormGroup();

				return basicGetFormGroup();
			case ClientPackage.FORM_PANEL__LABEL:
				return getLabel();
			case ClientPackage.FORM_PANEL__FORM:
				if (resolve)
					return getForm();

				return basicGetForm();
			case ClientPackage.FORM_PANEL__DRAW_BORDER:
				return isDrawBorder();
			case ClientPackage.FORM_PANEL__BASE_PANEL:
				if (resolve)
					return getBasePanel();

				return basicGetBasePanel();
			case ClientPackage.FORM_PANEL__FORM_TABLE:
				if (resolve)
					return getFormTable();

				return basicGetFormTable();
			case ClientPackage.FORM_PANEL__FIELDS:
				return getFields();
			case ClientPackage.FORM_PANEL__ACTIONS:
				return getActions();
			case ClientPackage.FORM_PANEL__DTO:
				if (resolve)
					return getDTO();

				return basicGetDTO();
			case ClientPackage.FORM_PANEL__BOUNDARY_METHOD:
				if (resolve)
					return getBoundaryMethod();

				return basicGetBoundaryMethod();
			case ClientPackage.FORM_PANEL__ASSOCIATION:
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
			case ClientPackage.FORM_PANEL__NAME:
				setName((String) newValue);
				return;
			case ClientPackage.FORM_PANEL__COLUMN_COUNT:
				setColumnCount((Integer) newValue);
				return;
			case ClientPackage.FORM_PANEL__COL_INDEX:
				setColIndex((Integer) newValue);
				return;
			case ClientPackage.FORM_PANEL__ROW_INDEX:
				setRowIndex((Integer) newValue);
				return;
			case ClientPackage.FORM_PANEL__VERTICALSPAN:
				setVerticalspan((Boolean) newValue);
				return;
			case ClientPackage.FORM_PANEL__HORIZONTAL_SPAN:
				setHorizontalSpan((Boolean) newValue);
				return;
			case ClientPackage.FORM_PANEL__FORM_GROUP:
				setFormGroup((FormGroup) newValue);
				return;
			case ClientPackage.FORM_PANEL__LABEL:
				setLabel((String) newValue);
				return;
			case ClientPackage.FORM_PANEL__FORM:
				setForm((Form) newValue);
				return;
			case ClientPackage.FORM_PANEL__DRAW_BORDER:
				setDrawBorder((Boolean) newValue);
				return;
			case ClientPackage.FORM_PANEL__BASE_PANEL:
				setBasePanel((FormPanel) newValue);
				return;
			case ClientPackage.FORM_PANEL__FORM_TABLE:
				setFormTable((FormTable) newValue);
				return;
			case ClientPackage.FORM_PANEL__FIELDS:
				getFields().clear();
				getFields().addAll((Collection<? extends FormField>) newValue);
				return;
			case ClientPackage.FORM_PANEL__ACTIONS:
				getActions().clear();
				getActions().addAll((Collection<? extends FormAction>) newValue);
				return;
			case ClientPackage.FORM_PANEL__DTO:
				setDTO((DTOBean) newValue);
				return;
			case ClientPackage.FORM_PANEL__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) newValue);
				return;
			case ClientPackage.FORM_PANEL__ASSOCIATION:
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
			case ClientPackage.FORM_PANEL__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__COLUMN_COUNT:
				setColumnCount(COLUMN_COUNT_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__COL_INDEX:
				setColIndex(COL_INDEX_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__ROW_INDEX:
				setRowIndex(ROW_INDEX_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__VERTICALSPAN:
				setVerticalspan(VERTICALSPAN_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__HORIZONTAL_SPAN:
				setHorizontalSpan(HORIZONTAL_SPAN_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__FORM_GROUP:
				setFormGroup((FormGroup) null);
				return;
			case ClientPackage.FORM_PANEL__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__FORM:
				setForm((Form) null);
				return;
			case ClientPackage.FORM_PANEL__DRAW_BORDER:
				setDrawBorder(DRAW_BORDER_EDEFAULT);
				return;
			case ClientPackage.FORM_PANEL__BASE_PANEL:
				setBasePanel((FormPanel) null);
				return;
			case ClientPackage.FORM_PANEL__FORM_TABLE:
				setFormTable((FormTable) null);
				return;
			case ClientPackage.FORM_PANEL__FIELDS:
				getFields().clear();
				return;
			case ClientPackage.FORM_PANEL__ACTIONS:
				getActions().clear();
				return;
			case ClientPackage.FORM_PANEL__DTO:
				setDTO((DTOBean) null);
				return;
			case ClientPackage.FORM_PANEL__BOUNDARY_METHOD:
				setBoundaryMethod((BoundaryMethod) null);
				return;
			case ClientPackage.FORM_PANEL__ASSOCIATION:
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
			case ClientPackage.FORM_PANEL__NAME:
				return name != null;
			case ClientPackage.FORM_PANEL__COLUMN_COUNT:
				return columnCount != COLUMN_COUNT_EDEFAULT;
			case ClientPackage.FORM_PANEL__COL_INDEX:
				return colIndex != COL_INDEX_EDEFAULT;
			case ClientPackage.FORM_PANEL__ROW_INDEX:
				return rowIndex != ROW_INDEX_EDEFAULT;
			case ClientPackage.FORM_PANEL__VERTICALSPAN:
				return verticalspan != VERTICALSPAN_EDEFAULT;
			case ClientPackage.FORM_PANEL__HORIZONTAL_SPAN:
				return horizontalSpan != HORIZONTAL_SPAN_EDEFAULT;
			case ClientPackage.FORM_PANEL__FORM_GROUP:
				return formGroup != null;
			case ClientPackage.FORM_PANEL__LABEL:
				return label != null;
			case ClientPackage.FORM_PANEL__FORM:
				return form != null;
			case ClientPackage.FORM_PANEL__DRAW_BORDER:
				return drawBorder != DRAW_BORDER_EDEFAULT;
			case ClientPackage.FORM_PANEL__BASE_PANEL:
				return basePanel != null;
			case ClientPackage.FORM_PANEL__FORM_TABLE:
				return formTable != null;
			case ClientPackage.FORM_PANEL__FIELDS:
				return fields != null && !fields.isEmpty();
			case ClientPackage.FORM_PANEL__ACTIONS:
				return actions != null && !actions.isEmpty();
			case ClientPackage.FORM_PANEL__DTO:
				return dTO != null;
			case ClientPackage.FORM_PANEL__BOUNDARY_METHOD:
				return boundaryMethod != null;
			case ClientPackage.FORM_PANEL__ASSOCIATION:
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
		result.append(", columnCount: ");
		result.append(columnCount);
		result.append(", colIndex: ");
		result.append(colIndex);
		result.append(", rowIndex: ");
		result.append(rowIndex);
		result.append(", verticalspan: ");
		result.append(verticalspan);
		result.append(", horizontalSpan: ");
		result.append(horizontalSpan);
		result.append(", label: ");
		result.append(label);
		result.append(", drawBorder: ");
		result.append(drawBorder);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#rearrangeFields()
	 * @generated not
	 */
	@Override
	public void rearrangeFields() {
		FormField previousField = null;
		int lastRowIndex = 0;
		int currentRowIndex = 1;
		boolean firstField = true;

		// Sort the form fields
		ECollections.sort(getFields(), new FormFieldComparator());

		// Rearrange all visible form fields first
		for (final FormField formField : getFields()) {
			if (formField.isHidden())
				continue;

			if (firstField) {
				firstField = false;

				// It could be the case that the first row doesn't start with 1!
				lastRowIndex = formField.getRowIndex();
				previousField = formField;
				formField.setRowIndex(currentRowIndex);
				continue;
			}

			if (formField.getRowIndex() != lastRowIndex) {
				// If the field's row index and the last row index are different we start with a new row!
				lastRowIndex = formField.getRowIndex();
				++currentRowIndex;
			}
			else if (formField.getColIndex() == previousField.getColIndex()) {
				// It should not be allowed that two fields share the same grid layout cell!
				++currentRowIndex;
			}
			else if ((formField.getColIndex() == 1 && formField.isSpanCols())
					|| (previousField.getColIndex() == 1 && previousField.isSpanCols())) {
				// If one of both fields really spans over both columns we put the current field into the next row
				++currentRowIndex;
			}

			formField.setRowIndex(currentRowIndex);
			previousField = formField;
		}

		// Put all invisible fields underneath!
		for (final FormField formField : getFields()) {
			if (!formField.isHidden())
				continue;

			formField.setRowIndex(++currentRowIndex);
			formField.setColIndex(1);
			formField.setSpanCols(false);
		}

		// Sort all form fields again
		ECollections.sort(getFields(), new FormFieldComparator());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final Project project = getDTO().getNamespace().getProject();
		final String packageName = project.getClientNamespace().toString() + PACK_CLIENT_PANEL;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, name, packageName);
		javaFile.setComment(getLabel());

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getTypeScriptSourceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getTypeScriptSourceFile() {
		final DomainObject domainObject = getDTO().getDomainObject();
		final Project project = domainObject.getNamespace().getProject();

		if (!project.hasAngularClient())
			return null;

		final String domainObjectName = domainObject.getName().toLowerCase();
		final var path = ANGULAR_PAGE_FOLDER + "/" + domainObjectName + "/" + getName().toLowerCase() + ".ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getPageObjectSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getPageObjectSourceFile() {
		final Project project = getDTO().getNamespace().getProject();
		final AbstractTestModule testModule = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);
		final String packageName = testModule.getNamespace().toString() + PACK_PAGE_OBJECT;
		final String domainObjLabel = getDTO().getDomainObject().getLabel();

		final var javaFile = new JavaTestFile(project, BuildArtifactType.SELENIUM_TEST, name, packageName);
		javaFile.setComment("Page object class that represents a grid panel for " + domainObjLabel + " objects");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getUserInterfaceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getUserInterfaceFile() {
		final Project project = getDTO().getNamespace().getProject();

		if (!project.hasJSFClient())
			return null;

		final var path = project.getWebAppFolder() + UI_PANEL_FOLDER + "/" + name + ".xhtml";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, "");
	}

}
