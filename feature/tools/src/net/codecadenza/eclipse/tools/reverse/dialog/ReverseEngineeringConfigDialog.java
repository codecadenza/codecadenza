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
package net.codecadenza.eclipse.tools.reverse.dialog;

import static net.codecadenza.eclipse.shared.Constants.PREF_ATTR_NAME_DATE_ON_PERSIST;
import static net.codecadenza.eclipse.shared.Constants.PREF_ATTR_NAME_DATE_ON_UPDATE;
import static net.codecadenza.eclipse.shared.Constants.PREF_ATTR_NAME_TRACK_VERSION;
import static net.codecadenza.eclipse.shared.Constants.PREF_DEEP_SEARCH;
import static net.codecadenza.eclipse.shared.Constants.PREF_END_WITH;
import static net.codecadenza.eclipse.shared.Constants.PREF_NOT_END_WITH;
import static net.codecadenza.eclipse.shared.Constants.PREF_NOT_START_WITH;
import static net.codecadenza.eclipse.shared.Constants.PREF_START_WITH;

import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringConfig;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining database table filter settings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEngineeringConfigDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit reverse engineering configuration";
	private static final String ATTR_TOOLTIP_PREFIX = "If the reverse engineering process finds attributes with the entered name it will automatically ";

	private final ReverseEngineeringConfig config;
	private Text txtVersionName;
	private Text txtDateOnUpdateName;
	private Text txtDateOnPersistName;
	private Text txtStartWith;
	private Text txtEndWith;
	private Text txtNotStartWith;
	private Text txtNotEndWith;
	private Button chkDeepSearch;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param config
	 */
	public ReverseEngineeringConfigDialog(Shell parentShell, ReverseEngineeringConfig config) {
		super(parentShell);

		this.config = config;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblAttrDescription = new Label(panDialogArea, SWT.NONE);
		lblAttrDescription.setLayoutData(new GridData(SWT.FILL, SWT.LEFT, true, false, 2, 1));
		lblAttrDescription.setText("Reverse engineering attribute name mapping:");

		final var lblVersion = new Label(panDialogArea, SWT.NONE);
		lblVersion.setText("Track version:");

		txtVersionName = new Text(panDialogArea, SWT.BORDER);
		txtVersionName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtVersionName.setToolTipText(ATTR_TOOLTIP_PREFIX + "add a @Version annotation to this integer field.");

		final var lblDateOnUpdate = new Label(panDialogArea, SWT.NONE);
		lblDateOnUpdate.setText("Set date on update:");

		txtDateOnUpdateName = new Text(panDialogArea, SWT.BORDER);
		txtDateOnUpdateName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDateOnUpdateName
				.setToolTipText(ATTR_TOOLTIP_PREFIX + "add a listener to set the date value when performing an update operation.");

		final var lblDateOnPersist = new Label(panDialogArea, SWT.NONE);
		lblDateOnPersist.setText("Set date on persist:");

		txtDateOnPersistName = new Text(panDialogArea, SWT.BORDER);
		txtDateOnPersistName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDateOnPersistName
				.setToolTipText(ATTR_TOOLTIP_PREFIX + "add a listener to set the date value when performing a persist operation.");

		final var gdSeparator = new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1);
		gdSeparator.verticalIndent = 5;

		final var lblSeparator = new Label(panDialogArea, SWT.SEPARATOR | SWT.HORIZONTAL);
		lblSeparator.setLayoutData(gdSeparator);

		final var lblFilterDescription = new Label(panDialogArea, SWT.NONE);
		lblFilterDescription.setLayoutData(new GridData(SWT.FILL, SWT.LEFT, true, false, 2, 1));
		lblFilterDescription.setText("The reverse engineering process should import all tables that...");

		final var lblStartWith = new Label(panDialogArea, SWT.NONE);
		lblStartWith.setText("start with");

		final var gdStartWith = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdStartWith.heightHint = 50;

		txtStartWith = new Text(panDialogArea, SWT.BORDER | SWT.MULTI);
		txtStartWith.setLayoutData(gdStartWith);

		final var lblEndWith = new Label(panDialogArea, SWT.NONE);
		lblEndWith.setText("end with");

		final var gdEndWith = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdEndWith.heightHint = 50;

		txtEndWith = new Text(panDialogArea, SWT.BORDER | SWT.MULTI);
		txtEndWith.setLayoutData(gdEndWith);

		final var lblNotStartWith = new Label(panDialogArea, SWT.NONE);
		lblNotStartWith.setText("don't start with");

		final var gdNotStartWith = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdNotStartWith.heightHint = 50;

		txtNotStartWith = new Text(panDialogArea, SWT.BORDER | SWT.MULTI);
		txtNotStartWith.setLayoutData(gdNotStartWith);

		final var lblEndNotWith = new Label(panDialogArea, SWT.NONE);
		lblEndNotWith.setText("don't end with");

		final var gdNotEndWith = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdNotEndWith.heightHint = 50;

		txtNotEndWith = new Text(panDialogArea, SWT.BORDER | SWT.MULTI);
		txtNotEndWith.setLayoutData(gdNotEndWith);

		chkDeepSearch = new Button(panDialogArea, SWT.CHECK);
		chkDeepSearch.setLayoutData(new GridData(SWT.FILL, SWT.LEFT, true, false, 2, 1));
		chkDeepSearch.setText("include tables that are referenced via foreign keys");
		chkDeepSearch.setSelection(config.isDeepSearchEnabled());

		if (config.getVersionAttrName() != null)
			txtVersionName.setText(config.getVersionAttrName());

		if (config.getDateOnPersistAttrName() != null)
			txtDateOnPersistName.setText(config.getDateOnPersistAttrName());

		if (config.getDateOnUpdateAttrName() != null)
			txtDateOnUpdateName.setText(config.getDateOnUpdateAttrName());

		if (config.getStartWith() != null)
			txtStartWith.setText(config.getStartWith());

		if (config.getEndWith() != null)
			txtEndWith.setText(config.getEndWith());

		if (config.getNotStartWith() != null)
			txtNotStartWith.setText(config.getNotStartWith());

		if (config.getNotEndWith() != null)
			txtNotEndWith.setText(config.getNotEndWith());

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			config.setDateOnPersistAttrName(txtDateOnPersistName.getText());
			config.setDateOnUpdateAttrName(txtDateOnUpdateName.getText());
			config.setVersionAttrName(txtVersionName.getText());
			config.setStartWith(txtStartWith.getText());
			config.setEndWith(txtEndWith.getText());
			config.setNotStartWith(txtNotStartWith.getText());
			config.setNotEndWith(txtNotEndWith.getText());
			config.setDeepSearchEnabled(chkDeepSearch.getSelection());

			// Save the configuration values in the preference store of the plug-in
			final IPreferenceStore store = CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();
			store.setValue(PREF_ATTR_NAME_TRACK_VERSION, config.getVersionAttrName());
			store.setValue(PREF_ATTR_NAME_DATE_ON_PERSIST, config.getDateOnPersistAttrName());
			store.setValue(PREF_ATTR_NAME_DATE_ON_UPDATE, config.getDateOnUpdateAttrName());
			store.setValue(PREF_DEEP_SEARCH, config.isDeepSearchEnabled());
			store.setValue(PREF_START_WITH, config.getStartWith());
			store.setValue(PREF_END_WITH, config.getEndWith());
			store.setValue(PREF_NOT_START_WITH, config.getNotStartWith());
			store.setValue(PREF_NOT_END_WITH, config.getNotEndWith());
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

		newShell.setText(DLG_TITLE);
	}

}
