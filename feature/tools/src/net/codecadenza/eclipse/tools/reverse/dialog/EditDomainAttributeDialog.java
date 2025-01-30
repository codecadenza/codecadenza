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
package net.codecadenza.eclipse.tools.reverse.dialog;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAttribute;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringModel;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining domain attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDomainAttributeDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit domain attribute";
	private static final Color READ_ONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);

	private final Project project;
	private final ReverseEngineeringModel revEngModel;
	private final JavaType attributeType;
	private final RevEngDomainAttribute revEngAttr;
	private Text txtRegEx;
	private Text txtMaxValue;
	private Text txtMinValue;
	private Text txtMaxLength;
	private Text txtMinLength;
	private DataComboViewer<TemporalTypeEnumeration> cboTemporalType;
	private DataComboViewer<JavaType> cboType;
	private Text txtLabelPlural;
	private Text txtLabel;
	private Text txtName;
	private Button chkInsertable;
	private Button chkUpdatable;
	private Button chkSetDateOnPersist;
	private Button chkSetDateOnUpdate;
	private Button chkFetchTypeEager;
	private Button chkFutureDate;
	private Button chkPastDate;
	private Button chkNullable;
	private Button chkVersion;
	private Button chkDisplayAttribute;
	private final DomainAttribute domainAttribute;
	private final boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param revEngAttr
	 * @param revEngModel
	 * @param project
	 */
	public EditDomainAttributeDialog(Shell parentShell, RevEngDomainAttribute revEngAttr, ReverseEngineeringModel revEngModel,
			Project project) {
		super(parentShell);

		this.project = project;
		this.revEngAttr = revEngAttr;
		this.revEngModel = revEngModel;
		this.domainAttribute = revEngAttr.getDomainAttribute();
		this.attributeType = domainAttribute.getJavaType();
		this.editMode = revEngAttr.isCreatedByReverseEngineering();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setLayout(new GridLayout(4, false));
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupBasicData.setText("Basic data");

		final var lblAttributeName = new Label(groupBasicData, SWT.NONE);
		lblAttributeName.setText("Name:");

		final var gdAttributeName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdAttributeName.widthHint = 200;

		txtName = new Text(groupBasicData, SWT.BORDER);
		txtName.setLayoutData(gdAttributeName);

		txtName.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				if (txtName.getText().equals(domainAttribute.getName()))
					return;

				txtLabel.setText(EclipseIDEService.buildDefaultLabel(txtName.getText()));
				txtLabelPlural.setText(EclipseIDEService.buildDefaultPluralLabel(txtName.getText()));
			}
		});

		final var lblAttributeJavaType = new Label(groupBasicData, SWT.NONE);
		lblAttributeJavaType.setText("Java type:");

		cboType = new DataComboViewer<>(groupBasicData, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(JavaType type) {
				return type.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(JavaType type) {
				if (type == null) {
					disableAllFields();
					return;
				}

				final var temporalTypes = new ArrayList<TemporalTypeEnumeration>();
				temporalTypes.add(TemporalTypeEnumeration.NONE);

				cboTemporalType.setData(temporalTypes);

				enableFields(type);

				if (type.isTemporalType())
					enableFieldsForDateTypes(type);
				else if (type.isString())
					enableFieldsForString();
				else if (type.isNumber())
					enableFieldsForNumericTypes(type);
				else if (type.isChar())
					enableFieldsForCharType();
				else if (type.isByteArray())
					enableFieldsForLobTypes();
				else if (type.isEnum())
					enableFieldsForEnum();

				cboTemporalType.setSelectedItem(domainAttribute.getTemporalType());
			}
		};

		final var gdType = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdType.widthHint = 200;

		cboType.setLayoutData(gdType);

		final var lblDBColumnName = new Label(groupBasicData, SWT.NONE);
		lblDBColumnName.setText("Column name:");

		final var txtDBColumnName = new Text(groupBasicData, SWT.BORDER | SWT.READ_ONLY);
		txtDBColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDBColumnName.setBackground(READ_ONLY_COLOR);

		final var lblDBColumnType = new Label(groupBasicData, SWT.NONE);
		lblDBColumnType.setText("Column type:");

		final var txtDBColumnType = new Text(groupBasicData, SWT.BORDER | SWT.READ_ONLY);
		txtDBColumnType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDBColumnType.setBackground(READ_ONLY_COLOR);

		final var lblAttributeLabel = new Label(groupBasicData, SWT.NONE);
		lblAttributeLabel.setText("Label:");

		txtLabel = new Text(groupBasicData, SWT.BORDER);
		txtLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtLabel.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				if (txtLabel.getText().equals(domainAttribute.getLabel()))
					return;

				txtLabelPlural.setText(EclipseIDEService.buildDefaultPluralForm(txtLabel.getText()));
			}
		});

		final var lblAttributeLabelPlural = new Label(groupBasicData, SWT.NONE);
		lblAttributeLabelPlural.setText("Label plural:");

		txtLabelPlural = new Text(groupBasicData, SWT.BORDER);
		txtLabelPlural.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblAttributePrimaryKey = new Label(groupBasicData, SWT.NONE);
		lblAttributePrimaryKey.setText("Primary key:");

		final var chkPrimaryKey = new Button(groupBasicData, SWT.CHECK);
		chkPrimaryKey.setEnabled(false);

		final var lblDisplayAttribute = new Label(groupBasicData, SWT.NONE);
		lblDisplayAttribute.setText("Display attribute:");

		chkDisplayAttribute = new Button(groupBasicData, SWT.CHECK);

		final var lblValidatorNullable = new Label(groupBasicData, SWT.NONE);
		lblValidatorNullable.setText("Nullable:");

		chkNullable = new Button(groupBasicData, SWT.CHECK);

		chkNullable.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				// If the field can be null it's minimum length should be 0 in order allow empty input!
				if (txtMinLength.isEnabled())
					txtMinLength.setText("0");
			}
		});

		final var lblVersion = new Label(groupBasicData, SWT.NONE);
		lblVersion.setText("Version:");

		chkVersion = new Button(groupBasicData, SWT.CHECK);

		final var lblAttributeInsertable = new Label(groupBasicData, SWT.NONE);
		lblAttributeInsertable.setText("Insertable:");

		chkInsertable = new Button(groupBasicData, SWT.CHECK);
		chkInsertable.setSelection(true);

		final var lblAttributeUpdatable = new Label(groupBasicData, SWT.NONE);
		lblAttributeUpdatable.setText("Updatable:");

		chkUpdatable = new Button(groupBasicData, SWT.CHECK);
		chkUpdatable.setSelection(true);

		final var lblAttributeSetDateOnPersist = new Label(groupBasicData, SWT.NONE);
		lblAttributeSetDateOnPersist.setText("Set date on persist:");

		chkSetDateOnPersist = new Button(groupBasicData, SWT.CHECK);

		chkSetDateOnPersist.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				setDateOnPersist(chkSetDateOnPersist.getSelection());
			}
		});

		final var lblAttributeSetDateOnUpdate = new Label(groupBasicData, SWT.NONE);
		lblAttributeSetDateOnUpdate.setText("Set date on update:");

		chkSetDateOnUpdate = new Button(groupBasicData, SWT.CHECK);

		chkSetDateOnUpdate.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				setDateOnUpdate(chkSetDateOnUpdate.getSelection());
			}
		});

		final var lblAttributeFetchTypeEager = new Label(groupBasicData, SWT.NONE);
		lblAttributeFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(groupBasicData, SWT.CHECK);
		chkFetchTypeEager.setSelection(true);

		final var lblAttributeTemporalType = new Label(groupBasicData, SWT.NONE);
		lblAttributeTemporalType.setText("Temporal type:");

		cboTemporalType = new DataComboViewer<>(groupBasicData, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(TemporalTypeEnumeration type) {
				return type.getName();
			}
		};

		cboTemporalType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var groupValidation = new Group(panDialogArea, SWT.NONE);
		groupValidation.setLayout(new GridLayout(4, false));
		groupValidation.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		groupValidation.setText("Validation");

		final var lblValidatorFutureDate = new Label(groupValidation, SWT.NONE);
		lblValidatorFutureDate.setText("Future date:");

		chkFutureDate = new Button(groupValidation, SWT.CHECK);

		final var lblValidatorPastDate = new Label(groupValidation, SWT.NONE);
		lblValidatorPastDate.setText("Past date:");

		chkPastDate = new Button(groupValidation, SWT.CHECK);

		final var lblValidatorMinLength = new Label(groupValidation, SWT.NONE);
		lblValidatorMinLength.setText("Min. length:");

		txtMinLength = new Text(groupValidation, SWT.BORDER);
		txtMinLength.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorMaxLength = new Label(groupValidation, SWT.NONE);
		lblValidatorMaxLength.setText("Max. length:");

		txtMaxLength = new Text(groupValidation, SWT.BORDER);
		txtMaxLength.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorMinValue = new Label(groupValidation, SWT.NONE);
		lblValidatorMinValue.setText("Min. value:");

		txtMinValue = new Text(groupValidation, SWT.BORDER);
		txtMinValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorMaxValue = new Label(groupValidation, SWT.NONE);
		lblValidatorMaxValue.setText("Max. value:");

		txtMaxValue = new Text(groupValidation, SWT.BORDER);
		txtMaxValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorRegularExpression = new Label(groupValidation, SWT.NONE);
		lblValidatorRegularExpression.setText("Regular expression:");

		txtRegEx = new Text(groupValidation, SWT.BORDER);
		txtRegEx.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtName.setSelection(0, txtName.getText().length());
		txtName.setFocus();

		// Add all Java types that are mapped to the attribute's database column type
		for (final DBColumnType colType : project.getDatabase().getAllSupportedColumnTypes())
			if (colType.getName().equalsIgnoreCase(domainAttribute.getColumn().getColumnType().getName())) {
				final List<JavaType> types = colType.getJavaTypes();

				// If the list of valid types contains type String we must add all existing Java enumerations
				for (final JavaType type : types)
					if (type.isString() && !domainAttribute.getDomainAttributeValidator().isNullable()) {
						revEngModel.getEnumerations().forEach(obj -> types.add(obj.getJavaEnum()));

						break;
					}

				cboType.setData(types);
				cboType.setSelectedItem(domainAttribute.getJavaType());
				break;
			}

		txtLabel.setText(domainAttribute.getLabel());
		txtLabelPlural.setText(domainAttribute.getLabelPlural());
		txtName.setText(domainAttribute.getName());
		txtMaxValue.setText(domainAttribute.getDomainAttributeValidator().getMaxValue());
		txtMinValue.setText(domainAttribute.getDomainAttributeValidator().getMinValue());
		txtRegEx.setText(domainAttribute.getDomainAttributeValidator().getRegularExpression());
		chkNullable.setSelection(domainAttribute.getDomainAttributeValidator().isNullable());
		chkFetchTypeEager.setSelection(domainAttribute.isFetchTypeEager());
		chkInsertable.setSelection(domainAttribute.isInsertable());
		chkUpdatable.setSelection(domainAttribute.isUpdatable());
		chkVersion.setSelection(domainAttribute.isTrackVersion());
		chkPrimaryKey.setSelection(domainAttribute.isPk());
		chkDisplayAttribute.setSelection(domainAttribute.isDisplayAttribute());
		chkFutureDate.setSelection(domainAttribute.getDomainAttributeValidator().isFutureDate());
		chkPastDate.setSelection(domainAttribute.getDomainAttributeValidator().isPastDate());
		chkSetDateOnPersist.setSelection(domainAttribute.isSetDateOnPersist());
		chkSetDateOnUpdate.setSelection(domainAttribute.isSetDateOnUpdate());
		txtDBColumnName.setText(domainAttribute.getColumn().getName());
		txtDBColumnType.setText(domainAttribute.getColumn().getColumnType().getName());

		final Optional<Integer> maxLength = domainAttribute.getMaxFieldLenght();
		final Optional<Integer> minLength = domainAttribute.getMinFieldLength();

		if (maxLength.isPresent())
			txtMaxLength.setText(Integer.toString(maxLength.get()));

		if (minLength.isPresent())
			txtMinLength.setText(Integer.toString(minLength.get()));

		if (!editMode) {
			txtName.setEditable(false);
			txtName.setBackground(READ_ONLY_COLOR);
			txtLabel.setEditable(false);
			txtLabel.setBackground(READ_ONLY_COLOR);
			txtLabelPlural.setEditable(false);
			txtLabelPlural.setBackground(READ_ONLY_COLOR);
			txtMaxLength.setEditable(false);
			txtMaxLength.setBackground(READ_ONLY_COLOR);
			txtMinLength.setEditable(false);
			txtMinLength.setBackground(READ_ONLY_COLOR);
			txtMaxValue.setEditable(false);
			txtMaxValue.setBackground(READ_ONLY_COLOR);
			txtMinValue.setEditable(false);
			txtMinValue.setBackground(READ_ONLY_COLOR);
			txtRegEx.setEditable(false);
			txtRegEx.setBackground(READ_ONLY_COLOR);
			chkFetchTypeEager.setEnabled(false);
			chkInsertable.setEnabled(false);
			chkUpdatable.setEnabled(false);
			chkVersion.setEnabled(false);
			chkPrimaryKey.setEnabled(false);
			chkDisplayAttribute.setEnabled(false);
			chkFutureDate.setEnabled(false);
			chkPastDate.setEnabled(false);
			chkSetDateOnPersist.setEnabled(false);
			chkSetDateOnUpdate.setEnabled(false);
			cboTemporalType.setEnabled(false);
			cboType.setEnabled(false);
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		if (editMode)
			createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);

		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(DLG_TITLE);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !validateAndSaveInput())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Disable all input fields
	 */
	private void disableAllFields() {
		chkFetchTypeEager.setEnabled(false);
		chkInsertable.setEnabled(false);
		chkSetDateOnPersist.setEnabled(false);
		chkSetDateOnUpdate.setEnabled(false);
		chkUpdatable.setEnabled(false);
		chkVersion.setEnabled(false);
		chkDisplayAttribute.setEnabled(false);
		chkFutureDate.setEnabled(false);
		chkNullable.setEnabled(false);
		chkPastDate.setEnabled(false);
		txtMaxLength.setEnabled(false);
		txtMinLength.setEnabled(false);
		txtMaxValue.setEnabled(false);
		txtMaxValue.setText("");
		txtMinValue.setEnabled(false);
		txtMinValue.setText("");
		txtRegEx.setEnabled(false);
		txtRegEx.setText("");
	}

	/**
	 * Enable fields that are commonly used by all types
	 * @param type
	 */
	private void enableFields(JavaType type) {
		disableAllFields();

		chkFetchTypeEager.setEnabled(true);
		chkFetchTypeEager.setSelection(true);
		chkInsertable.setEnabled(true);
		chkInsertable.setSelection(true);
		chkUpdatable.setEnabled(true);
		chkUpdatable.setSelection(true);

		if (!type.isPrimitive())
			chkNullable.setEnabled(true);
	}

	/**
	 * Enable fields for the type java.lang.String
	 */
	private void enableFieldsForString() {
		txtMinLength.setEnabled(true);
		txtMaxLength.setEnabled(true);
		chkDisplayAttribute.setEnabled(true);
		txtRegEx.setEnabled(true);
	}

	/**
	 * Enable fields for enumerations
	 */
	private void enableFieldsForEnum() {
		txtMinLength.setEnabled(true);
		txtMaxLength.setEnabled(true);

		// The value of an enum attribute type must not be null!
		chkNullable.setEnabled(false);
		chkNullable.setSelection(false);
	}

	/**
	 * Enable fields for all numeric types (e.g. int, Integer, double...)
	 * @param type
	 */
	private void enableFieldsForNumericTypes(JavaType type) {
		txtMaxValue.setEnabled(true);
		txtMinValue.setEnabled(true);

		if (type.isIntegerOrLong())
			chkVersion.setEnabled(true);
	}

	/**
	 * Enable fields for all date types
	 * @param type
	 */
	private void enableFieldsForDateTypes(JavaType type) {
		final var temporalTypes = new ArrayList<TemporalTypeEnumeration>();
		temporalTypes.add(TemporalTypeEnumeration.DATE);
		temporalTypes.add(TemporalTypeEnumeration.TIMESTAMP);

		chkSetDateOnPersist.setEnabled(true);
		chkSetDateOnUpdate.setEnabled(true);

		if (type.isDateOrCalendar()) {
			if (domainAttribute.getTemporalType() == TemporalTypeEnumeration.NONE)
				domainAttribute.setTemporalType(TemporalTypeEnumeration.TIMESTAMP);

			cboTemporalType.setData(temporalTypes);
		}
		else
			domainAttribute.setTemporalType(TemporalTypeEnumeration.NONE);

		setDateOnPersist(domainAttribute.isSetDateOnPersist());

		if (!domainAttribute.isSetDateOnPersist())
			setDateOnUpdate(domainAttribute.isSetDateOnUpdate());
	}

	/**
	 * Enable fields for the char type
	 */
	private void enableFieldsForCharType() {
		txtMinLength.setEnabled(true);
	}

	/**
	 * Enable fields for all LOB types
	 */
	private void enableFieldsForLobTypes() {
		chkFetchTypeEager.setSelection(false);

		txtMinLength.setEnabled(true);
		txtMaxLength.setEnabled(true);
	}

	/**
	 * @param selected
	 */
	private void setDateOnPersist(boolean selected) {
		if (selected) {
			chkSetDateOnUpdate.setSelection(false);
			chkInsertable.setSelection(true);
			chkUpdatable.setSelection(false);
			chkFutureDate.setSelection(false);
			chkPastDate.setSelection(false);
			chkInsertable.setEnabled(false);
			chkUpdatable.setEnabled(false);
			chkFutureDate.setEnabled(false);
			chkPastDate.setEnabled(false);
		}
		else {
			chkInsertable.setEnabled(true);
			chkUpdatable.setEnabled(true);
			chkFutureDate.setEnabled(true);
			chkPastDate.setEnabled(true);
		}
	}

	/**
	 * @param selected
	 */
	private void setDateOnUpdate(boolean selected) {
		if (selected) {
			chkSetDateOnPersist.setSelection(false);
			chkInsertable.setSelection(true);
			chkUpdatable.setSelection(true);
			chkFutureDate.setSelection(false);
			chkPastDate.setSelection(false);
			chkInsertable.setEnabled(false);
			chkUpdatable.setEnabled(false);
			chkFutureDate.setEnabled(false);
			chkPastDate.setEnabled(false);
			chkNullable.setSelection(true);
		}
		else {
			chkInsertable.setEnabled(true);
			chkUpdatable.setEnabled(true);
			chkFutureDate.setEnabled(true);
			chkPastDate.setEnabled(true);
			chkNullable.setSelection(false);
		}
	}

	/**
	 * Validate the input and save the data if the validation was successful
	 * @return true if the validation was successful
	 */
	private boolean validateAndSaveInput() {
		final IStatus status = EclipseIDEService.validateFieldName(txtName.getText());
		final JavaType selType = cboType.getSelectedItem();
		Integer minLength = null;
		Integer maxLength = null;

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
			return false;
		}

		if (txtLabel.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The label must not be empty!");
			return false;
		}

		if (txtLabelPlural.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The plural form of the label must not be empty!");
			return false;
		}

		if (selType == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A valid Java type must be selected!");
			cboType.setFocus();
			return false;
		}

		try {
			if (txtMinLength.isEnabled() && !txtMinLength.getText().isEmpty()) {
				minLength = Integer.parseInt(txtMinLength.getText());

				if (minLength < 0)
					throw new IllegalArgumentException();
			}
		}
		catch (final RuntimeException e) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The min. length requires a positive integer value!");
			return false;
		}

		if (selType.isDateOrCalendar() && domainAttribute.getTemporalType() == TemporalTypeEnumeration.NONE) {
			final var msg = "Attributes of type java.util.Date or java.util.GregorianCalendar must be supplied with a valid temporal type!";

			MessageDialog.openInformation(getShell(), DLG_TITLE, msg);
			return false;
		}

		try {
			final boolean requiresLength = !domainAttribute.getColumn().getColumnType().isOmitSizeInformation();

			if (txtMaxLength.isEnabled() && (requiresLength || !txtMaxLength.getText().isEmpty())) {
				maxLength = Integer.parseInt(txtMaxLength.getText());

				if (maxLength < 1)
					throw new IllegalArgumentException();
			}
		}
		catch (final RuntimeException e) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. length requires a positive integer value!");
			return false;
		}

		// Check if the maximum length is greater than the minimum length
		if (maxLength != null && minLength != null && minLength > maxLength) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. length must be greater than the min. length!");
			return false;
		}

		domainAttribute.setName(txtName.getText());
		domainAttribute.setLabelPlural(txtLabelPlural.getText());
		domainAttribute.setLabel(txtLabel.getText());
		domainAttribute.setJavaType(selType);
		domainAttribute.setFetchTypeEager(chkFetchTypeEager.getSelection());
		domainAttribute.setInsertable(chkInsertable.getSelection());
		domainAttribute.setSetDateOnPersist(chkSetDateOnPersist.getSelection());
		domainAttribute.setSetDateOnUpdate(chkSetDateOnUpdate.getSelection());
		domainAttribute.setTemporalType(cboTemporalType.getSelectedItem());
		domainAttribute.setUpdatable(chkUpdatable.getSelection());
		domainAttribute.setTrackVersion(chkVersion.getSelection());
		domainAttribute.setDisplayAttribute(chkDisplayAttribute.getSelection());
		domainAttribute.getDomainAttributeValidator().setFutureDate(chkFutureDate.getSelection());
		domainAttribute.getDomainAttributeValidator().setNullable(chkNullable.getSelection());
		domainAttribute.getDomainAttributeValidator().setPastDate(chkPastDate.getSelection());
		domainAttribute.getDomainAttributeValidator().setMaxValue(txtMaxValue.getText());
		domainAttribute.getDomainAttributeValidator().setMinValue(txtMinValue.getText());
		domainAttribute.getDomainAttributeValidator().setMinLength(minLength);
		domainAttribute.getDomainAttributeValidator().setMaxLength(maxLength);
		domainAttribute.getDomainAttributeValidator().setRegularExpression(txtRegEx.getText());
		domainAttribute.setLob(selType.isByteArray());

		// If the selected type represents an enum a respective association must be created!
		if (selType.isEnum()) {
			final DomainObject domainObject = revEngAttr.getParentObject().getDomainObject();

			final EnumAssociation enumAssoc = DomainFactory.eINSTANCE.createEnumAssociation();
			enumAssoc.setSource(domainObject);
			enumAssoc.setTarget((JavaEnum) selType);

			domainObject.getEnumAssociations().add(enumAssoc);
		}

		// Remove the enum association if the type has changed!
		if (!(selType.equals(attributeType)) && attributeType.isEnum()) {
			final DomainObject domainObject = revEngAttr.getParentObject().getDomainObject();
			final EnumAssociation enumAssocToDelete = domainObject.getEnumAssociations().stream()
					.filter(enumAssoc -> enumAssoc.getTarget().equals(attributeType)).findFirst().orElse(null);

			if (enumAssocToDelete != null)
				domainObject.getEnumAssociations().remove(enumAssocToDelete);
		}

		return true;
	}

}
