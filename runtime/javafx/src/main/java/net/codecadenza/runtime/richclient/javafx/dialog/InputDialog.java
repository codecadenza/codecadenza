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
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.INPUT_DIALOG_LBL_INPUT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.INPUT_DIALOG_MSG_INPUT_REQUIRED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.INPUT_DIALOG_MSG_INPUT_REQUIRED_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;

import javafx.geometry.HPos;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.Modality;
import javafx.stage.Window;

/**
 * <p>
 * Simple dialog for requesting a text input
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class InputDialog extends AbstractBaseDialog {
	private TextField txtInput;
	private String inputValue;
	private final String defaultValue;

	/**
	 * Constructor
	 * @param owner
	 * @param title
	 * @param defaultValue
	 */
	public InputDialog(Window owner, String title, String defaultValue) {
		super(owner, title);

		this.defaultValue = defaultValue;

		setSize(300, 150);
		initModality(Modality.APPLICATION_MODAL);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		final var panContent = new GridPane();
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(50.0, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panContent.add(new Label(getTranslationForFieldLabel(INPUT_DIALOG_LBL_INPUT)), 0, 0);
		panContent.setHgap(5);
		panContent.setVgap(5);

		txtInput = new TextField();

		if (defaultValue != null)
			txtInput.setText(defaultValue);

		panContent.add(txtInput, 1, 0);

		return panContent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOKPressed()
	 */
	@Override
	protected void onOKPressed() {
		returnCode = DialogButtonType.CANCEL;

		if (txtInput.getText().isEmpty()) {
			final String message = getTranslation(INPUT_DIALOG_MSG_INPUT_REQUIRED);
			final String title = getTranslation(INPUT_DIALOG_MSG_INPUT_REQUIRED_TITLE);

			DialogUtil.openWarningDialog(this, title, message);

			txtInput.requestFocus();

			return;
		}

		inputValue = txtInput.getText();
		returnCode = DialogButtonType.OK;

		close();
	}

	/**
	 * @return the input value
	 */
	public String getInputValue() {
		return inputValue;
	}

}
