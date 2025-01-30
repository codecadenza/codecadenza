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

import java.util.Collection;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.util.validation.TableColumnCheck;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining table column fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditTableColumnFieldDialog extends CodeCadenzaDialog {
	public static final int DELETE = 100;
	private static final String DLG_TITLE = "Edit table column";

	private final TableColumnField field;
	private final FormTypeEnumeration formType;
	private final Project project;
	private Text txtWidth;
	private Text txtTitle;
	private Text txtAttrName;
	private Text txtSelectToken;
	private Button chkVisible;
	private Button chkId;
	private Button chkRefIdentifier;
	private DataComboViewer<Form> cboLOV;
	private Button chkSearchable;
	private boolean enableDelete = true;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param field
	 * @param formType
	 */
	public EditTableColumnFieldDialog(Shell parentShell, Project project, TableColumnField field, FormTypeEnumeration formType) {
		super(parentShell);

		this.project = project;
		this.field = field;
		this.formType = formType;

		final DTOBeanAttribute dtoAttribute = field.getDTOAttribute();

		// The field that is mapped to the primary key must not be deleted!
		if (dtoAttribute.getDTOBean().getPKAttribute().equals(dtoAttribute))
			enableDelete = false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblSelectToken = new Label(panDialogArea, SWT.NONE);
		lblSelectToken.setText("Select token:");

		txtSelectToken = new Text(panDialogArea, SWT.BORDER);
		txtSelectToken.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblTitle = new Label(panDialogArea, SWT.NONE);
		lblTitle.setText("Title:");

		txtTitle = new Text(panDialogArea, SWT.BORDER);
		txtTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblAttrName = new Label(panDialogArea, SWT.NONE);
		lblAttrName.setText("DTO attribute name:");

		txtAttrName = new Text(panDialogArea, SWT.BORDER);
		txtAttrName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblWidth = new Label(panDialogArea, SWT.NONE);
		lblWidth.setText("Width:");

		txtWidth = new Text(panDialogArea, SWT.BORDER);
		txtWidth.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));

		final var lblVisible = new Label(panDialogArea, SWT.NONE);
		lblVisible.setText("Visible:");

		chkVisible = new Button(panDialogArea, SWT.CHECK);

		final var lblId = new Label(panDialogArea, SWT.NONE);
		lblId.setText("Primary key attribute:");

		chkId = new Button(panDialogArea, SWT.CHECK);
		chkId.setEnabled(false);

		final var lblRefIdentifier = new Label(panDialogArea, SWT.NONE);
		lblRefIdentifier.setText("Ref. identifier:");

		chkRefIdentifier = new Button(panDialogArea, SWT.CHECK);
		chkRefIdentifier.setEnabled(false);

		if (formType == FormTypeEnumeration.LOV) {
			final var lblLOVReturn = new Label(panDialogArea, SWT.NONE);
			lblLOVReturn.setText("LOV return:");

			final var chkLOVReturn = new Button(panDialogArea, SWT.CHECK);
			chkLOVReturn.setEnabled(false);
			chkLOVReturn.setSelection(field.getDTOAttribute().isLovReturn());
		}

		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW) {
			final var lblLOV = new Label(panDialogArea, SWT.NONE);
			lblLOV.setText("List of values:");

			cboLOV = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
				 */
				@Override
				public String getItemText(Form element) {
					return element.getName();
				}
			};

			// Get all list-of-values forms of this project
			final Collection<Form> lovForms = project.getLOVFormsOfProject();

			// Add an empty form
			final Form emptyLOV = ClientFactory.eINSTANCE.createForm();
			emptyLOV.setName("");

			lovForms.add(emptyLOV);

			cboLOV.setData(lovForms);

			if (field.getLovForm() == null)
				cboLOV.setSelectedItem(emptyLOV);
			else
				cboLOV.setSelectedItem(field.getLovForm());

			cboLOV.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			// The searchable flag should not be maintained by the user if the attribute is not persistent
			if (field.getDTOAttribute().getDomainAttribute().isPersistent()) {
				final var lblSearchable = new Label(panDialogArea, SWT.NONE);
				lblSearchable.setText("Searchable:");

				chkSearchable = new Button(panDialogArea, SWT.CHECK);
				chkSearchable.setSelection(field.isSearchable());
			}
		}

		txtSelectToken.setText(field.getDTOAttribute().getSelectToken());
		txtTitle.setText(field.getTitle());
		txtAttrName.setText(field.getDTOAttribute().getName());
		txtWidth.setText(String.valueOf(field.getWidth()));
		chkRefIdentifier.setSelection(field.isAssociationRef());
		chkVisible.setSelection(field.isVisible());
		chkId.setSelection(field.isIdentifier());

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);

		if (enableDelete)
			createButton(parent, IDialogConstants.INTERNAL_ID, "Delete", false);
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
			if (txtSelectToken.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The 'select' keyword is missing!");
				txtSelectToken.setFocus();
				return;
			}

			// A full-featured test of the given input cannot be provided. Finally, the user is responsible for entering correct data!
			if (txtSelectToken.getText().trim().split("\\.").length < 2) {
				// The select token must contain one '.' character at least!
				var message = "The given 'select' token is incorrect as it must at least contain ";
				message += "an alias and one or more attributes delimited by '.' characters!";

				MessageDialog.openInformation(getShell(), DLG_TITLE, message);
				txtSelectToken.setFocus();
				return;
			}

			if (txtTitle.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The title must not be empty!");
				txtTitle.setFocus();
				return;
			}

			if (txtWidth.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The column width is required!");
				txtWidth.setFocus();
				return;
			}

			final IStatus status = EclipseIDEService.validateFieldName(txtAttrName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				txtAttrName.setFocus();
				return;
			}

			int width = 0;

			try {
				width = Integer.parseInt(txtWidth.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The column width requires an integer value!");
				txtWidth.setFocus();
				return;
			}

			// Save existing values
			final String selectToken = field.getDTOAttribute().getSelectToken();
			final String title = field.getTitle();
			final int fieldWidth = field.getWidth();
			final boolean visible = field.isVisible();
			final boolean identifier = field.isIdentifier();
			final boolean assocRef = field.isAssociationRef();
			final String attrName = field.getDTOAttribute().getName();
			final boolean lovRet = field.getDTOAttribute().isLovReturn();
			final Form lovForm = field.getLovForm();

			field.getDTOAttribute().setSelectToken(txtSelectToken.getText().trim());
			field.setTitle(txtTitle.getText());
			field.setWidth(width);
			field.setVisible(chkVisible.getSelection());
			field.setIdentifier(chkId.getSelection());
			field.setAssociationRef(chkRefIdentifier.getSelection());
			field.getDTOAttribute().setName(txtAttrName.getText());

			if (formType == FormTypeEnumeration.SEARCHABLE_VIEW) {
				final Form selForm = cboLOV.getSelectedItem();

				if (selForm != null) {
					if (!selForm.getName().isEmpty())
						field.setLovForm(selForm);
					else
						field.setLovForm(null);
				}

				if (field.getDTOAttribute().getDomainAttribute().isPersistent())
					field.setSearchable(chkSearchable.getSelection());
			}

			// Check all table column fields and exit if the validation has failed!
			if (!TableColumnCheck.checkTableColumnFields(getShell(), formType, field.getFormTable().getFields())) {
				// Overwrite with existing values
				field.getDTOAttribute().setSelectToken(selectToken);
				field.setTitle(title);
				field.setWidth(fieldWidth);
				field.setVisible(visible);
				field.setIdentifier(identifier);
				field.setAssociationRef(assocRef);
				field.getDTOAttribute().setName(attrName);
				field.getDTOAttribute().setLovReturn(lovRet);
				field.setLovForm(lovForm);
				return;
			}
		}
		else if (buttonId == IDialogConstants.INTERNAL_ID) {
			final int colIndex = field.getColIndex();
			final FormTable formTable = field.getFormTable();

			final boolean doIt = MessageDialog.openQuestion(getShell(), DLG_TITLE, "Do you really want to delete this column?");

			if (!doIt)
				return;

			try {
				// Synchronize GUI tests
				new GUITestCaseService(project).syncOnDeleteColumn(field);
			}
			catch (final Exception ex) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
			}

			// Remove the column from the table
			formTable.getFields().remove(field);

			// Delete the respective DTO attribute
			final DTOBean dto = field.getDTOAttribute().getDTOBean();
			dto.getAttributes().remove(field.getDTOAttribute());

			// Change the row indexes of the following fields
			for (final TableColumnField f : formTable.getFields())
				if (f.getColIndex() > colIndex)
					f.setColIndex(f.getColIndex() - 1);

			setReturnCode(DELETE);
			close();
			return;
		}

		super.buttonPressed(buttonId);
	}

}
