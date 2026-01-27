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
package net.codecadenza.eclipse.tools.util.editor;

import static net.codecadenza.eclipse.shared.Constants.PREF_DATE_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_MAX_ROW_COUNT;
import static net.codecadenza.eclipse.shared.Constants.PREF_NUMBER_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_TIME_FORMAT;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * <p>
 * Preference page for query editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class QueryPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {
	public static final String ID = "net.codecadenza.eclipse.tools.util.editor.QueryPreferencePage";
	private static final String DLG_TITLE = "Preferences";

	private Text txtTimeFormat;
	private Text txtDateFormat;
	private Text txtMaxRowCount;
	private Text txtNumberFormat;
	private IPreferenceStore store;
	private Shell shell;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#doGetPreferenceStore()
	 */
	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		store = CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();
		return store;
	}

	/**
	 * Initialize the page with the values from preferences store
	 */
	void initializeValues() {
		txtMaxRowCount.setText(Integer.toString(store.getInt(PREF_MAX_ROW_COUNT)));
		txtDateFormat.setText(store.getString(PREF_DATE_FORMAT));
		txtTimeFormat.setText(store.getString(PREF_TIME_FORMAT));
		txtNumberFormat.setText(store.getString(PREF_NUMBER_FORMAT));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performApply()
	 */
	@Override
	protected void performApply() {
		int maxRowCount = 0;

		// Test if the user has inserted a valid integer value
		try {
			maxRowCount = Integer.parseInt(txtMaxRowCount.getText());
		}
		catch (final NumberFormatException _) {
			MessageDialog.openInformation(shell, DLG_TITLE, "The max. row count requires an integer value!");
			txtMaxRowCount.setFocus();
			return;
		}

		// Test if the user has inserted a valid date format
		try {
			new SimpleDateFormat(txtDateFormat.getText());
		}
		catch (final IllegalArgumentException _) {
			MessageDialog.openInformation(shell, DLG_TITLE, "The date format is not valid!");
			txtDateFormat.setFocus();
			return;
		}

		// Test if the user has inserted a valid time format
		try {
			new SimpleDateFormat(txtTimeFormat.getText());
		}
		catch (final IllegalArgumentException _) {
			MessageDialog.openInformation(shell, DLG_TITLE, "The time format is not valid!");
			txtTimeFormat.setFocus();
			return;
		}

		// Test if the user has inserted a valid number format
		try {
			new DecimalFormat(txtNumberFormat.getText());
		}
		catch (final IllegalArgumentException _) {
			MessageDialog.openInformation(shell, DLG_TITLE, "The number format is not valid!");
			txtTimeFormat.setFocus();
			return;
		}

		store.setValue(PREF_MAX_ROW_COUNT, maxRowCount);
		store.setValue(PREF_DATE_FORMAT, txtDateFormat.getText());
		store.setValue(PREF_TIME_FORMAT, txtTimeFormat.getText());
		store.setValue(PREF_NUMBER_FORMAT, txtNumberFormat.getText());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
	 */
	@Override
	protected void performDefaults() {
		txtMaxRowCount.setText(Integer.toString(store.getDefaultInt(PREF_MAX_ROW_COUNT)));
		txtDateFormat.setText(store.getDefaultString(PREF_DATE_FORMAT));
		txtTimeFormat.setText(store.getDefaultString(PREF_TIME_FORMAT));
		txtNumberFormat.setText(store.getDefaultString(PREF_NUMBER_FORMAT));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public Control createContents(Composite parent) {
		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayout(new GridLayout(2, false));

		final var lblMaxRowCount = new Label(panPageArea, SWT.NONE);
		lblMaxRowCount.setText("Max. row count:");

		txtMaxRowCount = new Text(panPageArea, SWT.BORDER);
		txtMaxRowCount.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDateFormat = new Label(panPageArea, SWT.NONE);
		lblDateFormat.setText("Date format:");

		txtDateFormat = new Text(panPageArea, SWT.BORDER);
		txtDateFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblTimeFormat = new Label(panPageArea, SWT.NONE);
		lblTimeFormat.setText("Time format:");

		txtTimeFormat = new Text(panPageArea, SWT.BORDER);
		txtTimeFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblNumberFormat = new Label(panPageArea, SWT.NONE);
		lblNumberFormat.setText("Number format:");

		txtNumberFormat = new Text(panPageArea, SWT.BORDER);
		txtNumberFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Initialize values
		initializeValues();

		return panPageArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
		// Initialize the preference page
		doGetPreferenceStore();
		shell = workbench.getActiveWorkbenchWindow().getShell();
	}

}
