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
package net.codecadenza.eclipse.ui.dialog.client;

import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_VALUE_FLAG;
import static net.codecadenza.eclipse.shared.Constants.TEXT_PREFIX;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.util.validation.FormFieldCheck;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining form fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditFormFieldDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit form field";

	private Text txtRowIndex;
	private Text txtWidth;
	private Combo cboType;
	private Text txtLabel;
	private Text txtName;
	private final FormField field;
	private Button chkVisible;
	private Button chkReadonly;
	private final FormTypeEnumeration formType;
	private Text txtDefault;
	private Button chkDefault;
	private Text txtAttrName;
	private Combo cboEnumValues;
	private Button chkSpan;
	private Button chkAddDetailLink;
	private final Project project;
	private Button chkUseSecondCol;
	private Button chkMandatory;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param field
	 * @param formType
	 */
	public EditFormFieldDialog(Shell parentShell, Project project, FormField field, FormTypeEnumeration formType) {
		super(parentShell);

		this.field = field;
		this.formType = formType;
		this.project = project;
	}

	/**
	 * @return the form field
	 */
	public FormField getField() {
		return field;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		final var gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 150;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(gdName);

		final var lblType = new Label(panDialogArea, SWT.NONE);
		lblType.setText("Type:");

		cboType = new Combo(panDialogArea, SWT.READ_ONLY);
		cboType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cboType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				String fieldName = field.getName().substring(COMBO_PREFIX.length());
				final FormFieldTypeEnumeration selType = FormFieldTypeEnumeration.valueOf(cboType.getItem(cboType.getSelectionIndex()));

				if (selType == FormFieldTypeEnumeration.COMBOBOX || selType == FormFieldTypeEnumeration.PROPOSAL_TEXT
						|| selType == FormFieldTypeEnumeration.LOV)
					chkAddDetailLink.setEnabled(true);
				else {
					chkAddDetailLink.setEnabled(false);
					chkAddDetailLink.setSelection(false);
				}

				field.setListOfValues(null);

				if (selType == FormFieldTypeEnumeration.LOV) {
					final Form lovForm = field.findListOfValues();

					// In order to avoid generator problems, no invalid value may be entered in the list-of-values field!
					if (lovForm != null)
						field.setListOfValues(lovForm);
				}
				else if (selType == FormFieldTypeEnumeration.LIST || selType == FormFieldTypeEnumeration.SEARCHABLE_LIST)
					return;

				// Change the default field name due to the selected field type!
				if (selType == FormFieldTypeEnumeration.COMBOBOX || selType == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
						|| selType == FormFieldTypeEnumeration.ENUM_COMBOBOX || selType == FormFieldTypeEnumeration.SELECTION_BY_CLIENT
						|| selType == FormFieldTypeEnumeration.FORM_LINK)
					fieldName = COMBO_PREFIX + fieldName;
				else
					fieldName = TEXT_PREFIX + fieldName;

				chkVisible.setEnabled(true);
				chkVisible.setSelection(true);

				if (selType == FormFieldTypeEnumeration.SELECTION_BY_CLIENT
						|| selType == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
						|| selType == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO) {
					chkVisible.setEnabled(false);
					chkVisible.setSelection(false);
				}

				txtName.setText(fieldName);
			}
		});

		if (field.getFieldType() == FormFieldTypeEnumeration.DATE || field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME) {
			cboType.add(FormFieldTypeEnumeration.DATE.name());
			cboType.add(FormFieldTypeEnumeration.DATE_TIME.name());

			if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
				cboType.add(FormFieldTypeEnumeration.LABEL.name());
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.CHECKBOX)
			cboType.add(FormFieldTypeEnumeration.CHECKBOX.name());
		else if (field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT
				|| field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
			cboType.add(FormFieldTypeEnumeration.SIMPLE_TEXT.name());
			cboType.add(FormFieldTypeEnumeration.MULTI_LINE_TEXT.name());

			if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
				cboType.add(FormFieldTypeEnumeration.LABEL.name());
				cboType.add(FormFieldTypeEnumeration.MULTI_LINE_LABEL.name());

				if (field.getDTOAttribute().getDomainAttribute().getJavaType().isString()) {
					cboType.add(FormFieldTypeEnumeration.MAIL_LINK.name());
					cboType.add(FormFieldTypeEnumeration.WEB_LINK.name());
				}
			}
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.WEB_LINK
				|| field.getFieldType() == FormFieldTypeEnumeration.MAIL_LINK) {
			cboType.add(FormFieldTypeEnumeration.MAIL_LINK.name());
			cboType.add(FormFieldTypeEnumeration.WEB_LINK.name());
			cboType.add(FormFieldTypeEnumeration.SIMPLE_TEXT.name());
			cboType.add(FormFieldTypeEnumeration.MULTI_LINE_TEXT.name());
			cboType.add(FormFieldTypeEnumeration.LABEL.name());
			cboType.add(FormFieldTypeEnumeration.MULTI_LINE_LABEL.name());
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			cboType.add(FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD.name());
		else if (field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK
				|| field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX
				|| field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT
				|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
				|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO
				|| field.getFieldType() == FormFieldTypeEnumeration.LOV
				|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_CLIENT) {
			cboType.add(FormFieldTypeEnumeration.COMBOBOX.name());

			if (field.getDTOAttribute().getReferencedDTOBean() != null) {
				if (field.getFieldType() == FormFieldTypeEnumeration.LOV || field.findListOfValues() != null)
					cboType.add(FormFieldTypeEnumeration.LOV.name());

				cboType.add(FormFieldTypeEnumeration.PROPOSAL_TEXT.name());

				if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM || formType == FormTypeEnumeration.ADD)
					cboType.add(FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM.name());

				if (project.getApplicationLogOnDTO() != null)
					cboType.add(FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO.name());

				if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_CLIENT || project.isMandatingSupported())
					cboType.add(FormFieldTypeEnumeration.SELECTION_BY_CLIENT.name());

				if (field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK || formType == FormTypeEnumeration.READONLY
						|| formType == FormTypeEnumeration.UPDATE)
					cboType.add(FormFieldTypeEnumeration.FORM_LINK.name());
			}
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.LIST
				|| field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
			cboType.add(FormFieldTypeEnumeration.LIST.name());
			cboType.add(FormFieldTypeEnumeration.SEARCHABLE_LIST.name());
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			cboType.add(FormFieldTypeEnumeration.ENUM_COMBOBOX.name());
		else if (field.getFieldType() == FormFieldTypeEnumeration.LABEL
				|| field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL) {
			cboType.add(FormFieldTypeEnumeration.LABEL.name());

			if (field.getDTOAttribute().getDomainAttribute().getJavaType().isTemporalType()) {
				cboType.add(FormFieldTypeEnumeration.DATE.name());
				cboType.add(FormFieldTypeEnumeration.DATE_TIME.name());
			}
			else {
				cboType.add(FormFieldTypeEnumeration.SIMPLE_TEXT.name());
				cboType.add(FormFieldTypeEnumeration.MULTI_LINE_TEXT.name());
				cboType.add(FormFieldTypeEnumeration.MULTI_LINE_LABEL.name());

				if (field.getDTOAttribute().getDomainAttribute().getJavaType().isString()) {
					cboType.add(FormFieldTypeEnumeration.MAIL_LINK.name());
					cboType.add(FormFieldTypeEnumeration.WEB_LINK.name());
				}
			}
		}

		final var lblLabel = new Label(panDialogArea, SWT.NONE);
		lblLabel.setText("Label:");

		txtLabel = new Text(panDialogArea, SWT.BORDER);
		txtLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblWidth = new Label(panDialogArea, SWT.NONE);
		lblWidth.setText("Width:");

		txtWidth = new Text(panDialogArea, SWT.BORDER);
		txtWidth.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtWidth.setText(String.valueOf(field.getWidth()));

		final var lblAttrName = new Label(panDialogArea, SWT.NONE);
		lblAttrName.setText("DTO attribute name:");

		txtAttrName = new Text(panDialogArea, SWT.BORDER);
		txtAttrName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtAttrName.setText(field.getDTOAttribute().getName());

		final var lblDefault = new Label(panDialogArea, SWT.NONE);
		lblDefault.setText("Default value:");

		if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) {
			if (field.getFieldType() == FormFieldTypeEnumeration.CHECKBOX || field.getFieldType() == FormFieldTypeEnumeration.DATE
					|| field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME
					|| (field.getDTOAttribute().getDomainAttribute() != null
							&& field.getDTOAttribute().getDomainAttribute().getJavaType().isUUID()
							&& field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT)) {
				chkDefault = new Button(panDialogArea, SWT.CHECK);
				chkDefault.setSelection(field.getDefaultValue() != null && !field.getDefaultValue().isEmpty());
			}
			else if (field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT
					|| field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
				txtDefault = new Text(panDialogArea, SWT.BORDER);
				txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

				if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
					txtDefault.setText(field.getDefaultValue());
			}
			else if (field.getFieldType() == FormFieldTypeEnumeration.ENUM_COMBOBOX) {
				final var en = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();

				cboEnumValues = new Combo(panDialogArea, SWT.READ_ONLY);
				cboEnumValues.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				cboEnumValues.add("");

				en.getEnumerationValues().forEach(value -> cboEnumValues.add(value.getName()));

				int selectionIndex = 0;

				if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
					for (final String value : cboEnumValues.getItems())
						if (!value.equals(field.getDefaultValue()))
							selectionIndex++;
						else
							break;

				cboEnumValues.select(selectionIndex);
			}
			else {
				txtDefault = new Text(panDialogArea, SWT.BORDER);
				txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				txtDefault.setEnabled(false);
			}
		}
		else {
			txtDefault = new Text(panDialogArea, SWT.BORDER);
			txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtDefault.setEnabled(false);
		}

		final var lblVisible = new Label(panDialogArea, SWT.NONE);
		lblVisible.setText("Visible:");

		chkVisible = new Button(panDialogArea, SWT.CHECK);
		chkVisible.setSelection(field.isVisible());

		// Some field types must be always invisible!
		if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_CLIENT
				|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
				|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO) {
			chkVisible.setEnabled(false);
			chkVisible.setSelection(false);
		}

		final var lblSpan = new Label(panDialogArea, SWT.NONE);
		lblSpan.setText("Span:");

		chkSpan = new Button(panDialogArea, SWT.CHECK);
		chkSpan.setSelection(field.isSpanCols());

		final var lblManadatory = new Label(panDialogArea, SWT.NONE);
		lblManadatory.setText("Mandatory:");

		chkMandatory = new Button(panDialogArea, SWT.CHECK);
		chkMandatory.setSelection(field.isMandatory());

		final var lblReadonly = new Label(panDialogArea, SWT.NONE);
		lblReadonly.setText("Readonly:");

		chkReadonly = new Button(panDialogArea, SWT.CHECK);
		chkReadonly.setEnabled(formType == FormTypeEnumeration.UPDATE);
		chkReadonly.setSelection(field.isReadonly());

		txtName.setText(field.getName());

		if (field.getLabel() != null)
			txtLabel.setText(field.getLabel());

		final var lblDetail = new Label(panDialogArea, SWT.NONE);
		lblDetail.setText("Add detail link:");

		chkAddDetailLink = new Button(panDialogArea, SWT.CHECK);
		chkAddDetailLink.setEnabled(false);
		chkAddDetailLink.setSelection(field.isAddFormLinkToLabel());

		if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX
				|| field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT || field.getFieldType() == FormFieldTypeEnumeration.LOV)
			chkAddDetailLink.setEnabled(true);

		new Label(panDialogArea, SWT.NONE);
		new Label(panDialogArea, SWT.NONE);

		final var lblRowIndex = new Label(panDialogArea, SWT.NONE);
		lblRowIndex.setText("Row index:");

		txtRowIndex = new Text(panDialogArea, SWT.BORDER);
		txtRowIndex.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRowIndex.setText(Integer.toString(field.getRowIndex()));

		final var lblColIndex = new Label(panDialogArea, SWT.NONE);
		lblColIndex.setText("Second column:");

		chkUseSecondCol = new Button(panDialogArea, SWT.CHECK);
		chkUseSecondCol.setSelection(field.getColIndex() != 1);

		int i = 0;

		for (final String type : cboType.getItems()) {
			if (FormFieldTypeEnumeration.valueOf(type) == field.getFieldType()) {
				cboType.select(i);
				break;
			}

			i++;
		}

		return panDialogArea;
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
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the user input
			IStatus status = EclipseIDEService.validateFieldName(txtName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				txtName.setFocus();
				return;
			}

			status = EclipseIDEService.validateFieldName(txtAttrName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				txtAttrName.setFocus();
				return;
			}

			int width;

			try {
				width = Integer.parseInt(txtWidth.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The width requires an integer value!");
				txtWidth.setFocus();
				return;
			}

			int rowIndex;

			try {
				rowIndex = Integer.parseInt(txtRowIndex.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The row index requires an integer value!");
				txtRowIndex.setFocus();
				return;
			}

			// Save existing values
			final String fieldName = field.getName();
			final String label = field.getLabel();
			final boolean visible = field.isVisible();
			final boolean readonly = field.isReadonly();
			final boolean mandatory = field.isMandatory();
			final int fieldWidth = field.getWidth();
			final int fieldRowIndex = field.getRowIndex();
			final FormFieldTypeEnumeration fieldType = field.getFieldType();
			final boolean span = field.isSpanCols();
			final String attributeName = field.getDTOAttribute().getName();
			final int colIndex = field.getColIndex();
			final String defValue = field.getDefaultValue();
			final boolean addDetail = field.isAddFormLinkToLabel();

			field.setName(txtName.getText());
			field.setLabel(txtLabel.getText());
			field.setVisible(chkVisible.getSelection());
			field.setReadonly(chkReadonly.getSelection());
			field.setMandatory(chkMandatory.getSelection());
			field.setWidth(width);
			field.setRowIndex(rowIndex);
			field.setFieldType(FormFieldTypeEnumeration.valueOf(cboType.getItem(cboType.getSelectionIndex())));
			field.setSpanCols(chkSpan.getSelection());
			field.setAddFormLinkToLabel(chkAddDetailLink.getSelection());
			field.getDTOAttribute().setName(txtAttrName.getText());

			if (chkUseSecondCol.getSelection())
				field.setColIndex(2);
			else
				field.setColIndex(1);

			field.setDefaultValue(null);

			if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) {
				if (field.getFieldType() == FormFieldTypeEnumeration.CHECKBOX || field.getFieldType() == FormFieldTypeEnumeration.DATE
						|| field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME
						|| (field.getDTOAttribute().getDomainAttribute() != null
								&& field.getDTOAttribute().getDomainAttribute().getJavaType().isUUID()
								&& field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT)) {
					if (chkDefault.getSelection())
						field.setDefaultValue(DEFAULT_VALUE_FLAG);
				}
				else if (field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT
						|| field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
					if (!txtDefault.getText().isEmpty())
						field.setDefaultValue(txtDefault.getText());
				}
				else if (field.getFieldType() == FormFieldTypeEnumeration.ENUM_COMBOBOX) {
					final String selItem = cboEnumValues.getItem(cboEnumValues.getSelectionIndex());

					if (!selItem.isEmpty())
						field.setDefaultValue(selItem);
				}
			}

			// Check the complete form
			if (!FormFieldCheck.checkFormFields(getShell(), field.getPanel().getForm(), false)) {
				// Revert the user input
				field.setName(fieldName);
				field.setLabel(label);
				field.setVisible(visible);
				field.setReadonly(readonly);
				field.setMandatory(mandatory);
				field.setWidth(fieldWidth);
				field.setRowIndex(fieldRowIndex);
				field.setFieldType(fieldType);
				field.setSpanCols(span);
				field.getDTOAttribute().setName(attributeName);
				field.setColIndex(colIndex);
				field.setDefaultValue(defValue);
				field.setAddFormLinkToLabel(addDetail);

				return;
			}
		}

		super.buttonPressed(buttonId);
	}

}
