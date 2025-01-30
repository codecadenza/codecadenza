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

import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
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
 * Dialog for maintaining one-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditOneToManyAssociationDialog extends CodeCadenzaDialog {
	private static final Color READ_ONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);
	private static final String DLG_TITLE = "Edit one-to-many association";

	private final OneToManyAssociation association;
	private Text txtAssociationName;
	private Button chkCascadePersist;
	private Button chkCascadeMerge;
	private Button chkCascadeRefresh;
	private Button chkCascadeRemove;
	private Button chkFetchTypeEager;
	private final boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param revEngAssoc
	 */
	public EditOneToManyAssociationDialog(Shell parentShell, RevEngDomainAssociation revEngAssoc) {
		super(parentShell);

		this.association = (OneToManyAssociation) revEngAssoc.getAssociation();
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

		if (association.isBidirectional()) {
			final var lblBidirectional = new Label(panDialogArea, SWT.NONE);
			lblBidirectional.setText("Bidirectional:");

			final var chkBidirectional = new Button(panDialogArea, SWT.CHECK);
			chkBidirectional.setSelection(true);
			chkBidirectional.setEnabled(false);

			final var lblReverseAssocName = new Label(panDialogArea, SWT.NONE);
			lblReverseAssocName.setText("Reverse name:");

			final var txtReverseAssocName = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
			txtReverseAssocName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtReverseAssocName.setText(association.getReverseAssociation().getName());
			txtReverseAssocName.setBackground(READ_ONLY_COLOR);
		}

		chkCascadeMerge.setSelection(association.isCascadeMerge());
		chkCascadePersist.setSelection(association.isCascadePersist());
		chkCascadeRefresh.setSelection(association.isCascadeRefresh());
		chkCascadeRemove.setSelection(association.isCascadeRemove());
		txtAssociationName.setText(association.getName());
		chkFetchTypeEager.setSelection(association.isFetchTypeEager());

		if (!editMode) {
			txtAssociationName.setEditable(false);
			txtAssociationName.setBackground(READ_ONLY_COLOR);
			chkCascadeMerge.setEnabled(false);
			chkCascadePersist.setEnabled(false);
			chkCascadeRefresh.setEnabled(false);
			chkCascadeRemove.setEnabled(false);
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

		association.setCascadeMerge(chkCascadeMerge.getSelection());
		association.setCascadePersist(chkCascadePersist.getSelection());
		association.setCascadeRefresh(chkCascadeRefresh.getSelection());
		association.setCascadeRemove(chkCascadeRemove.getSelection());
		association.setFetchTypeEager(chkFetchTypeEager.getSelection());
		association.setName(txtAssociationName.getText());

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
