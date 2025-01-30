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
package net.codecadenza.eclipse.ui.dialog.db;

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
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
 * Dialog for maintaining database column types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDBColumnTypeDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit database column type";
	private static final String DLG_TITLE_NEW = "Create new database column type";

	private Text txtDatabaseTypeName;
	private Button chkOmitSizeInformation;
	private CheckboxDataGridComposite<JavaType> listJavaTypes;
	private DBColumnType columnType;
	private final Database database;
	private boolean editMode;
	private String title = DLG_TITLE_NEW;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param columnType
	 * @param database
	 */
	public EditDBColumnTypeDialog(Shell parentShell, DBColumnType columnType, Database database) {
		this(parentShell, database);

		this.columnType = columnType;
		this.editMode = true;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param database
	 */
	public EditDBColumnTypeDialog(Shell parentShell, Database database) {
		super(parentShell);

		this.database = database;
	}

	/**
	 * Initialize form data
	 */
	private void initFormData() {
		final var javaTypes = new BasicEList<JavaType>();

		for (final JavaType type : database.getProject().getAllSupportedTypes())
			if (type.isMappable())
				javaTypes.add(type);

		javaTypes.sort((javaType1, javaType2) -> javaType1.getName().toLowerCase().compareTo(javaType2.getName().toLowerCase()));

		listJavaTypes.setData(javaTypes);

		if (editMode) {
			txtDatabaseTypeName.setText(columnType.getName());
			chkOmitSizeInformation.setSelection(columnType.isOmitSizeInformation());

			listJavaTypes.setCheckedElements(columnType.getJavaTypes());
		}
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateFormData() {
		var inputToCheck = "";

		if (!editMode) {
			inputToCheck = txtDatabaseTypeName.getText();

			if (inputToCheck.isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The database column type name must not be empty!");
				txtDatabaseTypeName.setFocus();
				return false;
			}
		}

		return true;
	}

	/**
	 * Save form data
	 * @return true if the data was saved successfully
	 */
	private boolean saveData() {
		if (!editMode) {
			columnType = DbFactory.eINSTANCE.createDBColumnType();
			columnType.setName(txtDatabaseTypeName.getText());

			database.getAllSupportedColumnTypes().add(columnType);
		}

		columnType.setOmitSizeInformation(chkOmitSizeInformation.getSelection());
		columnType.getJavaTypes().clear();
		columnType.getJavaTypes().addAll(listJavaTypes.getCheckedElements());

		try {
			EclipseIDEService.saveProjectMetaData(database.getProject());

			return true;
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblDBColumnTypeName = new Label(panDialogArea, SWT.NONE);
		lblDBColumnTypeName.setText("DB column type name:");

		txtDatabaseTypeName = new Text(panDialogArea, SWT.BORDER);
		txtDatabaseTypeName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (editMode)
			txtDatabaseTypeName.setEnabled(false);

		final var lblOmitSizeInformation = new Label(panDialogArea, SWT.NONE);
		lblOmitSizeInformation.setText("Omit size information:");

		chkOmitSizeInformation = new Button(panDialogArea, SWT.CHECK);

		final var lblDatabaseColumnTypes = new Label(panDialogArea, SWT.NONE);
		lblDatabaseColumnTypes.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 4, 1));
		lblDatabaseColumnTypes.setText("Available Java types that can be mapped to this database column type:");

		listJavaTypes = new CheckboxDataGridComposite<>(panDialogArea, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(JavaType element, int columnIndex) {
				return element.getName();
			}
		};

		listJavaTypes.addColumn("Java type name", 300);
		listJavaTypes.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 4, 1));

		initFormData();

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!validateFormData())
				return;

			if (!saveData())
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

		newShell.setText(title);
	}

}
