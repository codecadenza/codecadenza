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

import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.util.GUITestActionUtil;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining login test actions
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoginDialog extends CodeCadenzaDialog {
	private static final String TITLE_NEW = "Create new login test action";
	private static final String TITLE_EDIT = "Edit login test action";
	private static final int LOGIN_TEST_DATA_COUNT = 2;

	private final Project project;
	private GUITestAction loginAction;
	private Text txtPassword;
	private Text txtUserName;
	private String title = TITLE_NEW;
	private boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public LoginDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param loginAction
	 */
	public LoginDialog(Shell parentShell, Project project, GUITestAction loginAction) {
		super(parentShell);

		this.editMode = true;
		this.title = TITLE_EDIT;
		this.project = project;
		this.loginAction = loginAction;
	}

	/**
	 * @return the generated test action or null if no action has been created
	 */
	public GUITestAction getLoginAction() {
		return loginAction;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblUserName = new Label(panDialogArea, SWT.NONE);
		lblUserName.setText("User name:");

		final var gdUserName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdUserName.widthHint = 300;

		txtUserName = new Text(panDialogArea, SWT.BORDER);
		txtUserName.setLayoutData(gdUserName);

		final var lblPassword = new Label(panDialogArea, SWT.NONE);
		lblPassword.setText("Password:");

		txtPassword = new Text(panDialogArea, SWT.BORDER);
		txtPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		initFields();

		txtUserName.setFocus();

		return panDialogArea;
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

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the user input
			if (txtUserName.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The user name must not be empty!");
				txtUserName.setFocus();
				return;
			}

			if (txtPassword.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The password must not be empty!");
				txtPassword.setFocus();
				return;
			}

			if (!editMode) {
				loginAction = TestingFactory.eINSTANCE.createGUITestAction();
				loginAction.setType(GUITestActionType.OPEN_LOGIN_PAGE);

				if (project.hasVaadinClient())
					loginAction.setDelayAfter(1000);

				// Create a comment
				GUITestActionUtil.initTestActionComment(loginAction);

				final GUITestData testDataUserName = TestingFactory.eINSTANCE.createGUITestData();
				testDataUserName.setType(GUITestDataType.FORM_FIELD);
				testDataUserName.setTestAction(loginAction);

				final GUITestData testDataPassword = TestingFactory.eINSTANCE.createGUITestData();
				testDataPassword.setType(GUITestDataType.FORM_FIELD);
				testDataPassword.setTestAction(loginAction);
			}

			// The login action must contain exactly two test data objects!
			if (loginAction.getTestData().size() != LOGIN_TEST_DATA_COUNT) {
				MessageDialog.openInformation(getShell(), title, "The test data for the login action is either missing or incomplete!");
				return;
			}

			int testDataIndex = 0;

			// The first test data object holds the user name
			final GUITestData testDataUserName = loginAction.getTestData().get(testDataIndex++);
			testDataUserName.setNewValue(txtUserName.getText());

			// The second test data object holds the password
			final GUITestData testDataPassword = loginAction.getTestData().get(testDataIndex);
			testDataPassword.setNewValue(txtPassword.getText());
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * Initialize the user name and the password field
	 */
	private void initFields() {
		GUITestAction existingLoginAction = null;

		if (!editMode) {
			// Search for an existing login action in order to use its test data for the initialization
			for (final GUITestCase testCase : project.getAllGUITestCases()) {
				for (final GUITestAction testAction : testCase.getTestActions())
					if (testAction.getType() == GUITestActionType.OPEN_LOGIN_PAGE) {
						existingLoginAction = testAction;
						break;
					}

				if (existingLoginAction != null)
					break;
			}
		}
		else
			existingLoginAction = loginAction;

		if (existingLoginAction != null && existingLoginAction.getTestData().size() == LOGIN_TEST_DATA_COUNT) {
			int testDataIndex = 0;

			final GUITestData testDataUserName = existingLoginAction.getTestData().get(testDataIndex++);
			final GUITestData testDataPassword = existingLoginAction.getTestData().get(testDataIndex);

			if (testDataUserName.getNewValue() != null)
				txtUserName.setText(testDataUserName.getNewValue());

			if (testDataPassword.getNewValue() != null)
				txtPassword.setText(testDataPassword.getNewValue());
		}
	}

}
