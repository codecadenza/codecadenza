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
package net.codecadenza.runtime.richclient.eclipse.dialog;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_OK;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.MSG_ERR_MIN_FIELD_LENGTH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.MSG_ERR_PASSWORDS_NOT_EQUAL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.MSG_STATUS_INIT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Abstract dialog for changing the password of the logged on user
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractChangePasswordDialog extends TitleAreaDialog {
	protected static final int MIN_PASSWORD_LENGTH = 4;

	protected Shell parentShell;
	protected Text txtOldPassword;
	protected Text txtNewPassword;
	protected Text txtNewPasswordConfirm;

	/**
	 * Create the dialog
	 * @param parentShell
	 */
	protected AbstractChangePasswordDialog(Shell parentShell) {
		super(parentShell);

		this.parentShell = parentShell;
	}

	/**
	 * Validate the input
	 * @return true if the validation was successful
	 */
	private boolean validateFormData() {
		var fieldLabel = "";
		var inputToCheck = "";

		// Clear the error message
		setErrorMessage(null);

		inputToCheck = txtNewPassword.getText();
		fieldLabel = getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD);

		if (inputToCheck.length() < MIN_PASSWORD_LENGTH) {
			this.setErrorMessage(getTranslation(MSG_ERR_MIN_FIELD_LENGTH, fieldLabel, MIN_PASSWORD_LENGTH));
			txtNewPassword.setFocus();
			return false;
		}

		fieldLabel = getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM);

		if (!txtNewPassword.getText().equals(txtNewPasswordConfirm.getText())) {
			this.setErrorMessage(getTranslation(MSG_ERR_PASSWORDS_NOT_EQUAL, fieldLabel));
			txtNewPasswordConfirm.setFocus();
			return false;
		}

		return true;
	}

	/**
	 * @param oldPassword
	 * @param newPassword
	 * @param newPasswordConfirm
	 * @return true if the save operation was finished successfully
	 */
	public abstract boolean saveNewPassword(String oldPassword, String newPassword, String newPasswordConfirm);

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final long start = System.currentTimeMillis();

		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var panMain = new Composite(panDialogArea, SWT.NONE);
		panMain.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panMain.setLayout(new GridLayout(2, false));

		final var lblOldPassword = new Label(panMain, SWT.NONE);
		lblOldPassword.setText(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD));
		lblOldPassword.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

		txtOldPassword = new Text(panMain, SWT.PASSWORD | SWT.BORDER);
		txtOldPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblNewPassword = new Label(panMain, SWT.NONE);
		lblNewPassword.setText(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD));
		lblNewPassword.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

		txtNewPassword = new Text(panMain, SWT.PASSWORD | SWT.BORDER);
		txtNewPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblNewPasswordConfirm = new Label(panMain, SWT.NONE);
		lblNewPasswordConfirm.setText(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM));
		lblNewPasswordConfirm.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

		txtNewPasswordConfirm = new Text(panMain, SWT.PASSWORD | SWT.BORDER);
		txtNewPasswordConfirm.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		setTitle(getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE));

		final long end = System.currentTimeMillis();
		setMessage(getTranslation(MSG_STATUS_INIT, String.format("%.2f", (double) (end - start) / 1000)));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(CMD_OK), false);
		createButton(parent, IDialogConstants.CANCEL_ID, getTranslation(CMD_CANCEL), false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the input
			if (!validateFormData())
				return;

			final boolean operationState = saveNewPassword(txtOldPassword.getText(), txtNewPassword.getText(),
					txtNewPasswordConfirm.getText());

			if (!operationState)
				return;
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE));
	}

}
