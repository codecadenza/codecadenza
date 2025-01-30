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
package net.codecadenza.eclipse.diagram.domain.dialog;

import java.util.HashMap;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining database column meta-data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDBColumnDialog extends CodeCadenzaDialog {
	private final HashMap<String, DBColumnType> columnTypeMap = new HashMap<>();
	private Text txtName;
	private Text txtLength;
	private Text txtScale;
	private Text txtPrecision;
	private Button chkNullable;
	private final DBColumn column;
	private Combo cboDBType;
	private final String title;

	/**
	 * Constructor
	 * @param parentShell
	 * @param column
	 */
	public EditDBColumnDialog(Shell parentShell, DBColumn column) {
		super(parentShell);

		this.column = column;
		this.title = "Edit database column '" + column.getName() + "'";
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDBType = new Label(panDialogArea, SWT.NONE);
		lblDBType.setText("Database type:");

		cboDBType = new Combo(panDialogArea, SWT.READ_ONLY);
		cboDBType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cboDBType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DBColumnType colType = columnTypeMap.get(cboDBType.getItem(cboDBType.getSelectionIndex()));
				txtLength.setEnabled(!colType.isOmitSizeInformation());
			}
		});

		final var lblNullable = new Label(panDialogArea, SWT.NONE);
		lblNullable.setText("Nullable:");

		chkNullable = new Button(panDialogArea, SWT.CHECK);

		final var lblLenth = new Label(panDialogArea, SWT.NONE);
		lblLenth.setText("Length:");

		txtLength = new Text(panDialogArea, SWT.BORDER);
		txtLength.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblPrecision = new Label(panDialogArea, SWT.NONE);
		lblPrecision.setText("Precision:");

		txtPrecision = new Text(panDialogArea, SWT.BORDER);
		txtPrecision.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var lblScale = new Label(panDialogArea, SWT.NONE);
		lblScale.setText("Scale:");

		txtScale = new Text(panDialogArea, SWT.BORDER);
		txtScale.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtName.setText(column.getName());

		final var columnTypes = new BasicEList<DBColumnType>();
		columnTypes.addAll(column.getDatabaseTable().getDatabase().getAllSupportedColumnTypes());
		columnTypes.sort((columnType1, columnType2) -> columnType1.getName().compareTo(columnType2.getName()));

		columnTypes.forEach(colType -> {
			columnTypeMap.put(colType.getName(), colType);
			cboDBType.add(colType.getName());
		});

		int i = 0;

		for (final String itemName : cboDBType.getItems()) {
			if (itemName.equals(column.getColumnType().getName()))
				break;

			i++;
		}

		cboDBType.select(i);

		if (column.getColumnType().isOmitSizeInformation())
			txtLength.setEnabled(false);

		if (txtLength.isEnabled() && column.getLength() > 0)
			txtLength.setText(Integer.toString(column.getLength()));

		chkNullable.setSelection(column.isNullable());

		if (column.getPrecision() != 0)
			txtPrecision.setText(Integer.toString(column.getPrecision()));

		if (column.getScale() != 0)
			txtScale.setText(Integer.toString(column.getScale()));

		getShell().setText(title);

		return panDialogArea;
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateFormData() {
		try {
			// Check if the column name is valid!
			new DBSynchService(column.getDatabaseTable().getDatabase()).validateIdentifier(txtName.getText());
		}
		catch (final DBObjectValidationException e) {
			MessageDialog.openInformation(getShell(), title, e.getMessage());
			txtName.setFocus();
			return false;
		}

		try {
			if (txtLength.isEnabled() && !txtLength.getText().isEmpty() && Integer.parseInt(txtLength.getText()) < 0)
				throw new IllegalArgumentException();
		}
		catch (final RuntimeException e) {
			MessageDialog.openInformation(getShell(), title, "The column length requires a positive integer value!");
			txtLength.setFocus();
			return false;
		}

		if (txtPrecision.getText().isEmpty() && !txtScale.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), title, "A scale without a precision is not allowed!");
			txtPrecision.setFocus();
			return false;
		}

		if (!txtPrecision.getText().isEmpty()) {
			int scale = 0;
			int precision = 0;

			try {
				precision = Integer.parseInt(txtPrecision.getText());

				// Only positive values are allowed!
				if (precision < 1)
					throw new IllegalArgumentException();
			}
			catch (final RuntimeException e) {
				MessageDialog.openInformation(getShell(), title, "The precision requires a positive integer value!");
				txtPrecision.setFocus();
				return false;
			}

			if (!txtScale.getText().isEmpty()) {
				try {
					scale = Integer.parseInt(txtScale.getText());

					// Only positive values are allowed!
					if (scale < 1)
						throw new IllegalArgumentException();

					column.setScale(scale);
				}
				catch (final RuntimeException e) {
					MessageDialog.openInformation(getShell(), title, "The scale requires a positive integer value!");
					txtScale.setFocus();
					return false;
				}
			}

			// Definition of precision and scale: e.g. 123.34: Precision = 5, Scale = 2!
			if (scale != 0 && scale > precision) {
				MessageDialog.openInformation(getShell(), title, "The scale must be less or equal than the precision!");
				txtScale.setFocus();
				return false;
			}
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the user input
			if (!validateFormData())
				return;

			column.setName(txtName.getText());
			column.setNullable(chkNullable.getSelection());
			column.setColumnType(columnTypeMap.get(cboDBType.getItem(cboDBType.getSelectionIndex())));
			column.setLength(0);

			if (txtLength.isEnabled() && !txtLength.getText().isEmpty())
				column.setLength(Integer.parseInt(txtLength.getText()));

			if (!txtPrecision.getText().isEmpty())
				column.setPrecision(Integer.parseInt(txtPrecision.getText()));
			else
				column.setPrecision(0);

			if (!txtScale.getText().isEmpty())
				column.setScale(Integer.parseInt(txtScale.getText()));
			else
				column.setScale(0);
		}

		super.buttonPressed(buttonId);
	}

}
