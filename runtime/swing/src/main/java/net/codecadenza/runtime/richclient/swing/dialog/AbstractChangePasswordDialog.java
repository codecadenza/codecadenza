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
package net.codecadenza.runtime.richclient.swing.dialog;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.MSG_ERR_MIN_FIELD_LENGTH;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.MSG_ERR_PASSWORDS_NOT_EQUAL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.border.EmptyBorder;

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
public abstract class AbstractChangePasswordDialog extends JTitleAreaDialog {
	private static final long serialVersionUID = -1177577526497576287L;
	protected static final int MIN_PASSWORD_LENGTH = 4;

	protected JPasswordField txtOldPassword;
	protected JPasswordField txtNewPassword;
	protected JPasswordField txtNewPasswordConfirm;

	/**
	 * @param oldPassword
	 * @param newPassword
	 * @param newPasswordConfirm
	 * @return true if the save operation was finished successfully
	 */
	public abstract boolean saveNewPassword(String oldPassword, String newPassword, String newPasswordConfirm);

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createContents(javax.swing.JPanel)
	 */
	@Override
	public void createContents(JPanel contentPanel) {
		setTitle(getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE));
		setTitleMessage(getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE));
		setModal(true);
		setSize(350, 240);
		setLocationRelativeTo(null);

		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

		final var gbl_contentPanel = new GridBagLayout();
		gbl_contentPanel.columnWidths = new int[] { 0, 0, 0 };
		gbl_contentPanel.rowHeights = new int[] { 0, 0, 0, 0, 0 };
		gbl_contentPanel.columnWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };
		gbl_contentPanel.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };

		contentPanel.setLayout(gbl_contentPanel);

		final var lblOldPassword = new JLabel(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD));

		final var gbc_lblOldPassword = new GridBagConstraints();
		gbc_lblOldPassword.insets = new Insets(0, 0, 5, 5);
		gbc_lblOldPassword.anchor = GridBagConstraints.WEST;
		gbc_lblOldPassword.gridx = 0;
		gbc_lblOldPassword.gridy = 0;

		contentPanel.add(lblOldPassword, gbc_lblOldPassword);

		txtOldPassword = new JPasswordField();

		final var gbc_txtOldPassword = new GridBagConstraints();
		gbc_txtOldPassword.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtOldPassword.insets = new Insets(0, 0, 5, 0);
		gbc_txtOldPassword.gridx = 1;
		gbc_txtOldPassword.gridy = 0;

		contentPanel.add(txtOldPassword, gbc_txtOldPassword);

		txtOldPassword.setColumns(10);

		final var lblNewPassword = new JLabel(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD));

		final var gbc_lblNewPassword = new GridBagConstraints();
		gbc_lblNewPassword.insets = new Insets(0, 0, 5, 5);
		gbc_lblNewPassword.anchor = GridBagConstraints.WEST;
		gbc_lblNewPassword.gridx = 0;
		gbc_lblNewPassword.gridy = 1;

		contentPanel.add(lblNewPassword, gbc_lblNewPassword);

		txtNewPassword = new JPasswordField();

		final var gbc_txtNewPassword = new GridBagConstraints();
		gbc_txtNewPassword.insets = new Insets(0, 0, 5, 0);
		gbc_txtNewPassword.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtNewPassword.gridx = 1;
		gbc_txtNewPassword.gridy = 1;

		contentPanel.add(txtNewPassword, gbc_txtNewPassword);

		txtNewPassword.setColumns(10);

		final var lblNewPasswordConfirm = new JLabel(
				getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM));

		final var gbc_lblNewPasswordConfirm = new GridBagConstraints();
		gbc_lblNewPasswordConfirm.anchor = GridBagConstraints.WEST;
		gbc_lblNewPasswordConfirm.insets = new Insets(0, 0, 5, 5);
		gbc_lblNewPasswordConfirm.gridx = 0;
		gbc_lblNewPasswordConfirm.gridy = 2;

		contentPanel.add(lblNewPasswordConfirm, gbc_lblNewPasswordConfirm);

		txtNewPasswordConfirm = new JPasswordField();

		final var gbc_txtNewPasswordConfirm = new GridBagConstraints();
		gbc_txtNewPasswordConfirm.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtNewPasswordConfirm.insets = new Insets(0, 0, 5, 0);
		gbc_txtNewPasswordConfirm.gridx = 1;
		gbc_txtNewPasswordConfirm.gridy = 2;

		contentPanel.add(txtNewPasswordConfirm, gbc_txtNewPasswordConfirm);

		txtNewPasswordConfirm.setColumns(10);
	}

	/**
	 * Validate the user input
	 * @return true if the validation was finished successfully
	 */
	private boolean validateFormData() {
		var fieldLabel = "";
		var inputToCheck = "";

		// Clear the error message
		setErrorMessage("");

		inputToCheck = new String(txtNewPassword.getPassword());
		fieldLabel = getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD);

		if (inputToCheck.length() < MIN_PASSWORD_LENGTH) {
			this.setErrorMessage(getTranslation(MSG_ERR_MIN_FIELD_LENGTH, fieldLabel, MIN_PASSWORD_LENGTH));
			txtNewPassword.requestFocusInWindow();
			return false;
		}

		fieldLabel = getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM);

		if (!new String(txtNewPassword.getPassword()).equals(new String(txtNewPasswordConfirm.getPassword()))) {
			this.setErrorMessage(getTranslation(MSG_ERR_PASSWORDS_NOT_EQUAL, fieldLabel));
			txtNewPasswordConfirm.requestFocusInWindow();
			return false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onOKClicked()
	 */
	@Override
	public void onOKClicked() {
		if (!validateFormData())
			return;

		if (!saveNewPassword(new String(txtOldPassword.getPassword()), new String(txtNewPassword.getPassword()),
				new String(txtNewPasswordConfirm.getPassword())))
			return;

		this.dispose();
	}

}
