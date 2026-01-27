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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_OK;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_RESET;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FORMAT_PREFERENCES_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for entering the format preferences
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFormatPreferencesDialog extends TitleAreaDialog {
	private Text txtNumberFormat;
	private Text txtDateTimeFormat;
	private Text txtDateFormat;

	/**
	 * Create the dialog
	 * @param parentShell
	 */
	protected AbstractFormatPreferencesDialog(Shell parentShell) {
		super(parentShell);
	}

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

	/**
	 * Save the format preferences
	 * @param format
	 */
	public abstract void saveFormatPreferences(FormatDTO format);

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		// Get the data transfer object that contains the format preferences
		final FormatDTO dtoFormat = getFormatPreferences();

		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var panFormat = new Composite(panDialogArea, SWT.NONE);
		panFormat.setLayout(new GridLayout(2, false));
		panFormat.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var lblDateFormat = new Label(panFormat, SWT.NONE);
		lblDateFormat.setText(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT));

		txtDateFormat = new Text(panFormat, SWT.BORDER);
		txtDateFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDateFormat.setText(dtoFormat.getDateFormat());

		final var lblDateTimeFormat = new Label(panFormat, SWT.NONE);
		lblDateTimeFormat.setText(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT));

		txtDateTimeFormat = new Text(panFormat, SWT.BORDER);
		txtDateTimeFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDateTimeFormat.setText(dtoFormat.getDateTimeFormat());

		final var lblNumberFormat = new Label(panFormat, SWT.NONE);
		lblNumberFormat.setText(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT));

		txtNumberFormat = new Text(panFormat, SWT.BORDER);
		txtNumberFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtNumberFormat.setText(dtoFormat.getDecimalFormat());

		setTitle(getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE));
		setMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE_MESSAGE));
		setTitleImage(ImageCache.getImage("format_preferences.png"));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(CMD_OK), true);
		createButton(parent, IDialogConstants.SKIP_ID, getTranslation(CMD_RESET), true);
		createButton(parent, IDialogConstants.CANCEL_ID, getTranslation(CMD_CANCEL), false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.SKIP_ID) {
			txtDateFormat.setText(FormatPreferencesManager.DATE_FORMAT);
			txtDateTimeFormat.setText(FormatPreferencesManager.DATE_TIME_FORMAT);
			txtNumberFormat.setText(FormatPreferencesManager.DECIMAL_FORMAT);
			return;
		}
		else if (buttonId == IDialogConstants.OK_ID) {
			final var dateFormat = new SimpleDateFormat();
			final var decimalFormat = new DecimalFormat();

			// Validate the user input before saving the new format preferences
			if (txtDateFormat.getText().isEmpty()) {
				this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE));
				txtDateFormat.setFocus();
				return;
			}

			try {
				dateFormat.applyPattern(txtDateFormat.getText());
			}
			catch (final Exception _) {
				this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE));
				txtDateFormat.setFocus();
				return;
			}

			if (txtDateTimeFormat.getText().isEmpty()) {
				this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME));
				txtDateTimeFormat.setFocus();
				return;
			}

			try {
				dateFormat.applyPattern(txtDateTimeFormat.getText());
			}
			catch (final Exception _) {
				this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME));
				txtDateTimeFormat.setFocus();
				return;
			}

			if (txtNumberFormat.getText().isEmpty()) {
				this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER));
				txtNumberFormat.setFocus();
				return;
			}

			try {
				decimalFormat.applyPattern(txtNumberFormat.getText());
				decimalFormat.format(1.00);
			}
			catch (final Exception _) {
				this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER));
				txtNumberFormat.setFocus();
				return;
			}

			final var dtoFormat = new FormatDTO(txtDateFormat.getText(), txtDateTimeFormat.getText(), txtNumberFormat.getText(), ",");
			saveFormatPreferences(dtoFormat);
		}

		super.buttonPressed(buttonId);
	}

}
