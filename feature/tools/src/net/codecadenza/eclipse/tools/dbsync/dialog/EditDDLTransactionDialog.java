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
package net.codecadenza.eclipse.tools.dbsync.dialog;

import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for editing a DDL statement
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDDLTransactionDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit DDL statement";

	private Text txtStatement;
	private String statement;
	private final boolean readonly;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param statement the DDL statement
	 * @param readonly
	 */
	public EditDDLTransactionDialog(Shell parentShell, String statement, boolean readonly) {
		super(parentShell);

		this.statement = statement;
		this.readonly = readonly;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var gdStatement = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdStatement.widthHint = 500;
		gdStatement.heightHint = 300;

		txtStatement = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.H_SCROLL);
		txtStatement.setLayoutData(gdStatement);
		txtStatement.setText(statement);
		txtStatement.setEditable(!readonly);

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
			if (readonly) {
				this.setReturnCode(Dialog.CANCEL);
				close();
				return;
			}

			if (txtStatement.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The statement must not be empty!");
				return;
			}

			statement = txtStatement.getText();
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * @return the DDL statement
	 */
	public String getStatement() {
		return statement;
	}

}
