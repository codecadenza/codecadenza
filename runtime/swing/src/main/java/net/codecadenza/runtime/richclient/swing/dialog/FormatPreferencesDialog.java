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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_INFO_MESSAGE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FORMAT_PREFERENCES_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;

/**
 * <p>
 * Dialog for changing the date and the number format
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormatPreferencesDialog extends JTitleAreaDialog {
	private static final long serialVersionUID = -3711785200264253066L;

	private JTextField txtNumberFormat;
	private JTextField txtDateFormat;
	private JTextField txtDateTimeFormat;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createContents(javax.swing.JPanel)
	 */
	@Override
	public void createContents(JPanel contentPanel) {
		setInformationMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_INFO_MESSAGE));
		setTitle(getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE));
		setTitleMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_TITLE_MESSAGE));
		setTitleImage(ImageLoader.getImage(ImageLoader.FORMAT_TITLE));
		setModal(true);
		setSize(350, 240);
		setLocationRelativeTo(null);

		final FormatDTO format = FormatPreferencesManager.getFormatDTO();

		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

		final var gbl_contentPanel = new GridBagLayout();
		gbl_contentPanel.columnWidths = new int[] { 0, 0, 0 };
		gbl_contentPanel.rowHeights = new int[] { 0, 0, 0, 0, 0 };
		gbl_contentPanel.columnWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };
		gbl_contentPanel.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };

		contentPanel.setLayout(gbl_contentPanel);

		final var lblNumberFormat = new JLabel(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT));

		final var gbc_lblNumberFormat = new GridBagConstraints();
		gbc_lblNumberFormat.insets = new Insets(0, 0, 5, 5);
		gbc_lblNumberFormat.anchor = GridBagConstraints.WEST;
		gbc_lblNumberFormat.gridx = 0;
		gbc_lblNumberFormat.gridy = 0;

		contentPanel.add(lblNumberFormat, gbc_lblNumberFormat);

		txtNumberFormat = new JTextField();

		final var gbc_txtNumberFormat = new GridBagConstraints();
		gbc_txtNumberFormat.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtNumberFormat.insets = new Insets(0, 0, 5, 0);
		gbc_txtNumberFormat.gridx = 1;
		gbc_txtNumberFormat.gridy = 0;

		contentPanel.add(txtNumberFormat, gbc_txtNumberFormat);

		txtNumberFormat.setColumns(10);
		txtNumberFormat.setText(format.getDecimalFormat());

		final var lblDateFormat = new JLabel(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT));

		final var gbc_lblDateFormat = new GridBagConstraints();
		gbc_lblDateFormat.insets = new Insets(0, 0, 5, 5);
		gbc_lblDateFormat.anchor = GridBagConstraints.WEST;
		gbc_lblDateFormat.gridx = 0;
		gbc_lblDateFormat.gridy = 1;

		contentPanel.add(lblDateFormat, gbc_lblDateFormat);

		txtDateFormat = new JTextField();

		final var gbc_txtDateFormat = new GridBagConstraints();
		gbc_txtDateFormat.insets = new Insets(0, 0, 5, 0);
		gbc_txtDateFormat.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtDateFormat.gridx = 1;
		gbc_txtDateFormat.gridy = 1;

		contentPanel.add(txtDateFormat, gbc_txtDateFormat);

		txtDateFormat.setColumns(10);
		txtDateFormat.setText(format.getDateFormat());

		final var lblDateTimeFormat = new JLabel(getTranslationForFieldLabel(FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT));

		final var gbc_lblDateTimeFormat = new GridBagConstraints();
		gbc_lblDateTimeFormat.anchor = GridBagConstraints.WEST;
		gbc_lblDateTimeFormat.insets = new Insets(0, 0, 5, 5);
		gbc_lblDateTimeFormat.gridx = 0;
		gbc_lblDateTimeFormat.gridy = 2;

		contentPanel.add(lblDateTimeFormat, gbc_lblDateTimeFormat);

		txtDateTimeFormat = new JTextField();

		final var gbc_txtDateTimeFormat = new GridBagConstraints();
		gbc_txtDateTimeFormat.fill = GridBagConstraints.HORIZONTAL;
		gbc_txtDateTimeFormat.insets = new Insets(0, 0, 5, 0);
		gbc_txtDateTimeFormat.gridx = 1;
		gbc_txtDateTimeFormat.gridy = 2;

		contentPanel.add(txtDateTimeFormat, gbc_txtDateTimeFormat);

		txtDateTimeFormat.setColumns(10);
		txtDateTimeFormat.setText(format.getDateTimeFormat());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onOKClicked()
	 */
	@Override
	public void onOKClicked() {
		final var dateFormat = new SimpleDateFormat();
		final var decimalFormat = new DecimalFormat();

		// Validate the user input before saving the new formatting data
		if (txtDateFormat.getText().isEmpty()) {
			this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE));
			txtDateFormat.requestFocusInWindow();
			return;
		}

		try {
			dateFormat.applyPattern(txtDateFormat.getText());
		}
		catch (final Exception e) {
			this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE));
			txtDateFormat.requestFocusInWindow();
			return;
		}

		if (txtDateTimeFormat.getText().isEmpty()) {
			this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME));
			txtDateTimeFormat.requestFocusInWindow();
			return;
		}

		try {
			dateFormat.applyPattern(txtDateTimeFormat.getText());
		}
		catch (final Exception e) {
			this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME));
			txtDateTimeFormat.requestFocusInWindow();
			return;
		}

		if (txtNumberFormat.getText().isEmpty()) {
			this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER));
			txtNumberFormat.requestFocusInWindow();
			return;
		}

		try {
			decimalFormat.applyPattern(txtNumberFormat.getText());
			decimalFormat.format(1.00);
		}
		catch (final Exception e) {
			this.setErrorMessage(getTranslation(FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER));
			txtNumberFormat.requestFocusInWindow();
			return;
		}

		final var dtoFormat = new FormatDTO(txtDateFormat.getText(), txtDateTimeFormat.getText(), txtNumberFormat.getText(), ",");
		FormatPreferencesManager.saveFormat(dtoFormat);

		this.dispose();
	}

}
