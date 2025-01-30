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

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAssociation;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining many-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditManyToManyAssociationDialog extends CodeCadenzaDialog {
	private static final Color READ_ONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);
	private static final String DLG_TITLE = "Edit many-to-many association";

	private final ManyToManyAssociation association;
	private Text txtAssociationName;
	private Button chkFetchTypeEager;
	private final boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param revEngAssoc
	 */
	public EditManyToManyAssociationDialog(Shell parentShell, RevEngDomainAssociation revEngAssoc) {
		super(parentShell);

		this.association = (ManyToManyAssociation) revEngAssoc.getAssociation();
		this.editMode = revEngAssoc.isCreatedByReverseEngineering();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblAssociationName = new Label(panDialogArea, SWT.NONE);
		lblAssociationName.setText("Name:");

		final var gdAssociationName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdAssociationName.widthHint = 300;

		txtAssociationName = new Text(panDialogArea, SWT.BORDER);
		txtAssociationName.setLayoutData(gdAssociationName);

		final var lblFetchTypeEager = new Label(panDialogArea, SWT.NONE);
		lblFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(panDialogArea, SWT.CHECK);

		if (association.isOwner()) {
			final var lblJoinTable = new Label(panDialogArea, SWT.NONE);
			lblJoinTable.setText("Join table:");

			final var txtJoinTableName = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
			txtJoinTableName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtJoinTableName.setText(association.getTable().getName());
			txtJoinTableName.setBackground(READ_ONLY_COLOR);

			final var lblJoinCol1 = new Label(panDialogArea, SWT.NONE);
			lblJoinCol1.setText("Join column 1:");

			final var txtJoinColumnName1 = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
			txtJoinColumnName1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtJoinColumnName1.setBackground(READ_ONLY_COLOR);

			final var lblJoinCol2 = new Label(panDialogArea, SWT.NONE);
			lblJoinCol2.setText("Join column 2:");

			final var txtJoinColumnName2 = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
			txtJoinColumnName2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtJoinColumnName2.setBackground(READ_ONLY_COLOR);

			boolean firstColumn = true;

			for (final DBColumn col : association.getTable().getColumns()) {
				if (firstColumn) {
					firstColumn = false;
					txtJoinColumnName1.setText(col.getName());
				}
				else
					txtJoinColumnName2.setText(col.getName());
			}

			if (association.isBidirectional()) {
				final var lblBidirectional = new Label(panDialogArea, SWT.NONE);
				lblBidirectional.setText("Bidirectional:");

				final var chkBidirectional = new Button(panDialogArea, SWT.CHECK);
				chkBidirectional.setEnabled(false);
				chkBidirectional.setSelection(true);

				final var lblReverseAssocName = new Label(panDialogArea, SWT.NONE);
				lblReverseAssocName.setText("Reverse name:");

				final var txtReverseAssocNameName = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
				txtReverseAssocNameName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
				txtReverseAssocNameName.setText(association.getReverseAssociation().getName());
				txtReverseAssocNameName.setBackground(READ_ONLY_COLOR);
			}
		}

		txtAssociationName.setText(association.getName());
		chkFetchTypeEager.setSelection(association.isFetchTypeEager());

		if (!editMode) {
			txtAssociationName.setEditable(false);
			txtAssociationName.setBackground(READ_ONLY_COLOR);
			chkFetchTypeEager.setEnabled(false);
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
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !validateAndSaveInput())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Validate the input and save the data if the validation was successful
	 * @return true if the validation was successful
	 */
	private boolean validateAndSaveInput() {
		final IStatus status = EclipseIDEService.validateFieldName(txtAssociationName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
			txtAssociationName.setFocus();
			return false;
		}

		association.setName(txtAssociationName.getText());
		association.setFetchTypeEager(chkFetchTypeEager.getSelection());

		return true;
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

}
