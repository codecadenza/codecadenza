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
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FORMAT_PREFERENCES_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import javafx.geometry.HPos;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.Modality;
import javafx.stage.StageStyle;
import javafx.stage.Window;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;

/**
 * <p>
 * Dialog for changing the globally used date and number format
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormatPreferencesDialog extends AbstractBaseDialog {
	private final FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	private TextField txtDateFormat;
	private TextField txtDateTimeFormat;
	private TextField txtDecimalFormat;

	/**
	 * Constructor
	 * @param owner
	 */
	public FormatPreferencesDialog(Window owner) {
		super(owner, getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE));

		setSize(350, 200);

		initModality(Modality.APPLICATION_MODAL);
		initStyle(StageStyle.UTILITY);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		final var panContent = new GridPane();
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, HPos.LEFT, false));
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panContent.setHgap(5);
		panContent.setVgap(5);
		panContent.add(new Label(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT)), 0, 0);
		panContent.add(txtDateFormat = new TextField(userFormat.getDateFormat()), 1, 0);
		panContent.add(new Label(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT)), 0, 1);
		panContent.add(txtDateTimeFormat = new TextField(userFormat.getDateTimeFormat()), 1, 1);
		panContent.add(new Label(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT)), 0, 2);
		panContent.add(txtDecimalFormat = new TextField(userFormat.getDecimalFormat()), 1, 2);

		return panContent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOKPressed()
	 */
	@Override
	protected void onOKPressed() {
		final String title = getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE);
		final var dateFormat = new SimpleDateFormat();
		final var decimalFormat = new DecimalFormat();

		// Validate the user input before saving the new formatting data
		if (txtDateFormat.getText().isEmpty()) {
			DialogUtil.openInformationDialog(this, title, getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE));

			txtDateFormat.requestFocus();
			return;
		}

		try {
			dateFormat.applyPattern(txtDateFormat.getText());
		}
		catch (final Exception _) {
			DialogUtil.openInformationDialog(this, title, getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE));

			txtDateFormat.requestFocus();
			return;
		}

		if (txtDateTimeFormat.getText().isEmpty()) {
			DialogUtil.openInformationDialog(this, title, getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME));

			txtDateTimeFormat.requestFocus();
			return;
		}

		try {
			dateFormat.applyPattern(txtDateTimeFormat.getText());
		}
		catch (final Exception _) {
			DialogUtil.openInformationDialog(this, title, getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME));

			txtDateTimeFormat.requestFocus();
			return;
		}

		if (txtDecimalFormat.getText().isEmpty()) {
			DialogUtil.openInformationDialog(this, title, getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER));

			txtDecimalFormat.requestFocus();
			return;
		}

		try {
			decimalFormat.applyPattern(txtDecimalFormat.getText());
			decimalFormat.format(1.00);
		}
		catch (final Exception _) {
			DialogUtil.openInformationDialog(this, title, getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER));

			txtDecimalFormat.requestFocus();
			return;
		}

		final var dtoFormat = new FormatDTO(txtDateFormat.getText(), txtDateTimeFormat.getText(), txtDecimalFormat.getText(), ",");
		FormatPreferencesManager.saveFormat(dtoFormat);

		close();
	}

}
