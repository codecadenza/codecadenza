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

import static net.codecadenza.eclipse.shared.Constants.MIN_ATTRIBUTE_NAME_LENGTH;

import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
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
 * Dialog for creating and maintaining form panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditFormPanelDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit form panel";
	private static final String DLG_TITLE_NEW = "Create new form panel";

	private Combo cboRow;
	private Text txtColIndex;
	private Text txtLabel;
	private Text txtName;
	private Button chkBorder;
	private final FormPanel panel;
	private final boolean doEdit;
	private String title = DLG_TITLE_NEW;

	/**
	 * Create the dialog for maintaining existing form panels
	 * @param parentShell
	 * @param panel
	 */
	public EditFormPanelDialog(Shell parentShell, FormPanel panel) {
		super(parentShell);

		this.panel = panel;
		this.doEdit = true;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Create the dialog for creating new form panels
	 * @param parentShell
	 */
	public EditFormPanelDialog(Shell parentShell) {
		super(parentShell);

		this.doEdit = false;
		this.panel = ClientFactory.eINSTANCE.createFormPanel();
	}

	/**
	 * @return the form panel
	 */
	public FormPanel getPanel() {
		return panel;
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

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblLabel = new Label(panDialogArea, SWT.NONE);
		lblLabel.setText("Label:");

		txtLabel = new Text(panDialogArea, SWT.BORDER);
		txtLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblRow = new Label(panDialogArea, SWT.NONE);
		lblRow.setText("Row:");

		cboRow = new Combo(panDialogArea, SWT.READ_ONLY);
		cboRow.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboRow.add("1");
		cboRow.add("2");

		final var lblColIndex = new Label(panDialogArea, SWT.NONE);
		lblColIndex.setText("Column index:");

		txtColIndex = new Text(panDialogArea, SWT.BORDER);
		txtColIndex.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDrawBorder = new Label(panDialogArea, SWT.NONE);
		lblDrawBorder.setText("Draw border:");

		chkBorder = new Button(panDialogArea, SWT.CHECK);

		if (doEdit) {
			txtName.setText(panel.getName());
			txtLabel.setText(panel.getLabel());
			cboRow.select(panel.getRowIndex() - 1);
			chkBorder.setSelection(panel.isDrawBorder());
			txtColIndex.setText(String.valueOf(panel.getColIndex()));
		}

		return panDialogArea;
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
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(title);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the user input
			if (txtName.getText().length() < MIN_ATTRIBUTE_NAME_LENGTH) {
				MessageDialog.openInformation(getShell(), title,
						"The name is too short! The min. length is " + MIN_ATTRIBUTE_NAME_LENGTH + " characters!");
				txtName.setFocus();
				return;
			}

			if (txtLabel.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The label must not be empty!");
				txtLabel.setFocus();
				return;
			}

			if (txtColIndex.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "A column index is required!");
				txtColIndex.setFocus();
				return;
			}

			int colIndex = 0;

			try {
				colIndex = Integer.parseInt(txtColIndex.getText());
			}
			catch (final NumberFormatException _) {
				MessageDialog.openInformation(getShell(), title, "The column index requires an integer value!");
				txtColIndex.setFocus();
				return;
			}

			panel.setName(txtName.getText());
			panel.setLabel(txtLabel.getText());
			panel.setDrawBorder(chkBorder.getSelection());
			panel.setColIndex(colIndex);
			panel.setRowIndex(cboRow.getSelectionIndex() + 1);
		}

		super.buttonPressed(buttonId);
	}

}
