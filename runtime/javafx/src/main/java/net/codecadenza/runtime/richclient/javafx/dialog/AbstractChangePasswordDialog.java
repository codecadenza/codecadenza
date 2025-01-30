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
package net.codecadenza.runtime.richclient.javafx.dialog;

import static javafx.scene.layout.Region.USE_COMPUTED_SIZE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.MSG_ERR_MIN_FIELD_LENGTH;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.MSG_ERR_PASSWORDS_NOT_EQUAL;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;

import javafx.geometry.HPos;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.StageStyle;
import javafx.stage.Window;
import net.codecadenza.runtime.richclient.javafx.image.ImageLoader;

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

	protected PasswordField txtOldPassword;
	protected PasswordField txtNewPassword;
	protected PasswordField txtNewPasswordConfirm;

	/**
	 * Constructor
	 * @param owner
	 */
	protected AbstractChangePasswordDialog(Window owner) {
		super(owner, getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE));

		setSize(350, 250);
		setResizable(false);

		initStyle(StageStyle.UTILITY);
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
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		setTitleMessage(getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE));
		setTitleImage(ImageLoader.getImage(ImageLoader.IMG_DEFAULT_TITLE_IMAGE));

		final var panContent = new GridPane();
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, HPos.LEFT, false));
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panContent.setHgap(5);
		panContent.setVgap(5);

		panContent.add(new Label(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD)), 0, 0);
		panContent.add(txtOldPassword = new PasswordField(), 1, 0);
		panContent.add(new Label(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD)), 0, 1);
		panContent.add(txtNewPassword = new PasswordField(), 1, 1);
		panContent.add(new Label(getTranslationForFieldLabel(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM)), 0, 2);
		panContent.add(txtNewPasswordConfirm = new PasswordField(), 1, 2);

		return panContent;
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateFormData() {
		var fieldLabel = "";
		var inputToCheck = "";

		// Clear the error message
		setErrorMessage("");

		inputToCheck = txtNewPassword.getText();
		fieldLabel = getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD);

		if (inputToCheck.length() < MIN_PASSWORD_LENGTH) {
			setErrorMessage(getTranslation(MSG_ERR_MIN_FIELD_LENGTH, fieldLabel, MIN_PASSWORD_LENGTH));
			txtNewPassword.requestFocus();
			return false;
		}

		fieldLabel = getTranslation(ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM);

		if (!txtNewPassword.getText().equals(txtNewPasswordConfirm.getText())) {
			setErrorMessage(getTranslation(MSG_ERR_PASSWORDS_NOT_EQUAL, fieldLabel));
			txtNewPasswordConfirm.requestFocus();
			return false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOKPressed()
	 */
	@Override
	protected void onOKPressed() {
		if (!validateFormData())
			return;

		if (!saveNewPassword(txtOldPassword.getText(), txtNewPassword.getText(), txtNewPasswordConfirm.getText()))
			return;

		returnCode = DialogButtonType.OK;

		close();
	}

}
