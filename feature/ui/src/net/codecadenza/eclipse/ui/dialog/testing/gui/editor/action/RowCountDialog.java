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
package net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.util.GUITestActionUtil;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining test actions that validate the number of rows in a view or a grid panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RowCountDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Validate the number of displayed rows";
	private static final String OPERATOR_EQUAL = "=";
	private static final String OPERATOR_GREATER = ">=";
	private static final String OPERATOR_SMALLER = "<=";

	private Combo cboOperator;
	private Text txtRowCount;
	private GUITestAction rowCountAction;
	private final Form form;
	private final FormPanel formPanel;
	private boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param form
	 * @param formPanel
	 */
	public RowCountDialog(Shell parentShell, Form form, FormPanel formPanel) {
		super(parentShell);

		this.form = form;
		this.formPanel = formPanel;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param rowCountAction
	 */
	public RowCountDialog(Shell parentShell, GUITestAction rowCountAction) {
		super(parentShell);

		this.rowCountAction = rowCountAction;
		this.editMode = true;
		this.form = rowCountAction.getForm();
		this.formPanel = rowCountAction.getFormPanel();
	}

	/**
	 * @return the test action or null if no test action has been created
	 */
	public GUITestAction getRowCountValidationAction() {
		return rowCountAction;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 3, false);

		final var lblRows = new Label(panDialogArea, SWT.NONE);
		lblRows.setText("The expected number of rows is");

		cboOperator = new Combo(panDialogArea, SWT.READ_ONLY);
		cboOperator.setItems(OPERATOR_EQUAL, OPERATOR_GREATER, OPERATOR_SMALLER);
		cboOperator.select(0);

		final var gdRowCount = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdRowCount.minimumWidth = 100;

		txtRowCount = new Text(panDialogArea, SWT.BORDER);
		txtRowCount.setLayoutData(gdRowCount);

		if (editMode) {
			final GUITestData rowCountTestData = rowCountAction.getTestDataByType(GUITestDataType.ROW_COUNT);

			if (rowCountTestData != null && rowCountTestData.getExpectedValue() != null)
				txtRowCount.setText(rowCountTestData.getExpectedValue());

			if (rowCountAction.getType() == GUITestActionType.VALIDATE_ROW_COUNT_EQUAL)
				cboOperator.select(cboOperator.indexOf(OPERATOR_EQUAL));
			else if (rowCountAction.getType() == GUITestActionType.VALIDATE_ROW_COUNT_GREATER)
				cboOperator.select(cboOperator.indexOf(OPERATOR_SMALLER));
			else
				cboOperator.select(cboOperator.indexOf(OPERATOR_GREATER));
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// The expected number of displayed rows must be an integer!
			try {
				Integer.parseInt(txtRowCount.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The expected row number requires an integer value!");
				txtRowCount.setFocus();
				return;
			}

			if (!editMode) {
				rowCountAction = TestingFactory.eINSTANCE.createGUITestAction();
				rowCountAction.setForm(form);
				rowCountAction.setFormPanel(formPanel);
			}

			if (cboOperator.getItem(cboOperator.getSelectionIndex()).equals(OPERATOR_EQUAL))
				rowCountAction.setType(GUITestActionType.VALIDATE_ROW_COUNT_EQUAL);
			else if (cboOperator.getItem(cboOperator.getSelectionIndex()).equals(OPERATOR_GREATER))
				rowCountAction.setType(GUITestActionType.VALIDATE_ROW_COUNT_SMALLER);
			else
				rowCountAction.setType(GUITestActionType.VALIDATE_ROW_COUNT_GREATER);

			// Create a comment
			GUITestActionUtil.initTestActionComment(rowCountAction);

			GUITestData rowCountTestData = rowCountAction.getTestDataByType(GUITestDataType.ROW_COUNT);

			if (rowCountTestData == null) {
				rowCountTestData = TestingFactory.eINSTANCE.createGUITestData();
				rowCountTestData.setType(GUITestDataType.ROW_COUNT);
				rowCountTestData.setTestAction(rowCountAction);
			}

			rowCountTestData.setExpectedValue(txtRowCount.getText());
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
