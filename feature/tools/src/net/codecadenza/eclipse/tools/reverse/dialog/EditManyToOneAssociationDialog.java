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

import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAssociation;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
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
 * Dialog for maintaining many-to-one associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditManyToOneAssociationDialog extends CodeCadenzaDialog {
	private static final Color READ_ONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);
	private static final String DLG_TITLE = "Edit many-to-one association";

	private final ManyToOneAssociation association;
	private Text txtAssociationName;
	private Button chkCascadePersist;
	private Button chkCascadeMerge;
	private Button chkCascadeRefresh;
	private Button chkCascadeRemove;
	private Button chkFetchTypeEager;
	private Button chkOptional;
	private Button chkInsertable;
	private Button chkUpdatable;
	private final boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param revEngAssoc
	 */
	public EditManyToOneAssociationDialog(Shell parentShell, RevEngDomainAssociation revEngAssoc) {
		super(parentShell);

		this.association = (ManyToOneAssociation) revEngAssoc.getAssociation();
		this.editMode = revEngAssoc.isCreatedByReverseEngineering();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblAssociationName = new Label(panDialogArea, SWT.NONE);
		lblAssociationName.setText("Name:");

		txtAssociationName = new Text(panDialogArea, SWT.BORDER);
		txtAssociationName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDBColumnName = new Label(panDialogArea, SWT.NONE);
		lblDBColumnName.setText("Join column name:");

		final var txtDBColumnName = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
		txtDBColumnName.setBackground(READ_ONLY_COLOR);
		txtDBColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblOptional = new Label(panDialogArea, SWT.NONE);
		lblOptional.setText("Optional:");

		chkOptional = new Button(panDialogArea, SWT.CHECK);

		final var lblFetchTypeEager = new Label(panDialogArea, SWT.NONE);
		lblFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadePersist = new Label(panDialogArea, SWT.NONE);
		lblCascadePersist.setText("Cascade persist:");

		chkCascadePersist = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeMerge = new Label(panDialogArea, SWT.NONE);
		lblCascadeMerge.setText("Cascade merge:");

		chkCascadeMerge = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeRefresh = new Label(panDialogArea, SWT.NONE);
		lblCascadeRefresh.setText("Cascade refresh:");

		chkCascadeRefresh = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeRemove = new Label(panDialogArea, SWT.NONE);
		lblCascadeRemove.setText("Cascade remove:");

		chkCascadeRemove = new Button(panDialogArea, SWT.CHECK);

		final var lblInsertable = new Label(panDialogArea, SWT.NONE);
		lblInsertable.setText("Insertable:");

		chkInsertable = new Button(panDialogArea, SWT.CHECK);

		final var lblUpdatable = new Label(panDialogArea, SWT.NONE);
		lblUpdatable.setText("Updatable:");

		chkUpdatable = new Button(panDialogArea, SWT.CHECK);

		txtDBColumnName.setText(association.getColumn().getName());
		txtAssociationName.setText(association.getName());
		chkUpdatable.setSelection(association.isUpdatable());
		chkInsertable.setSelection(association.isInsertable());
		chkCascadeMerge.setSelection(association.isCascadeMerge());
		chkCascadePersist.setSelection(association.isCascadePersist());
		chkCascadeRefresh.setSelection(association.isCascadeRefresh());
		chkCascadeRemove.setSelection(association.isCascadeRemove());
		chkFetchTypeEager.setSelection(association.isFetchTypeEager());
		chkOptional.setSelection(association.isOptional());

		if (!association.isOwner()) {
			chkCascadeMerge.setEnabled(false);
			chkCascadePersist.setEnabled(false);
			chkCascadeRefresh.setEnabled(false);
			chkCascadeRemove.setEnabled(false);
		}

		if (!editMode) {
			txtAssociationName.setEditable(false);
			txtAssociationName.setBackground(READ_ONLY_COLOR);
			chkUpdatable.setEnabled(false);
			chkInsertable.setEnabled(false);
			chkCascadeMerge.setEnabled(false);
			chkCascadePersist.setEnabled(false);
			chkCascadeRefresh.setEnabled(false);
			chkCascadeRemove.setEnabled(false);
			chkFetchTypeEager.setEnabled(false);
			chkOptional.setEnabled(false);
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
	 * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(Math.max(600, super.getInitialSize().x), super.getInitialSize().y);
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

		if (!chkInsertable.getSelection() && !chkOptional.getSelection()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A non-optional assocation must be insertable!");
			return false;
		}

		if (association.isOwner()) {
			association.setCascadeMerge(chkCascadeMerge.getSelection());
			association.setCascadePersist(chkCascadePersist.getSelection());
			association.setCascadeRefresh(chkCascadeRefresh.getSelection());
			association.setCascadeRemove(chkCascadeRemove.getSelection());
		}

		association.setFetchTypeEager(chkFetchTypeEager.getSelection());
		association.setName(txtAssociationName.getText());
		association.setInsertable(chkInsertable.getSelection());
		association.setUpdatable(chkUpdatable.getSelection());
		association.setOptional(chkOptional.getSelection());

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
