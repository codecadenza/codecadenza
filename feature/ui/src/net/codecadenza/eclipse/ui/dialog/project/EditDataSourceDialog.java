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
package net.codecadenza.eclipse.ui.dialog.project;

import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.IProjectBuildService;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.util.db.DBManager;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.panel.DriverSelectionPanel;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining data source settings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDataSourceDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE = "Edit data source";

	private Text txtDriverName;
	private Text txtDataSource;
	private Text txtURL;
	private Text txtUserName;
	private Text txtPassword;
	private final Datasource ds;
	private final Project project;
	private DriverSelectionPanel driverSelectionPanel;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public EditDataSourceDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.ds = project.getDataSource();
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		setTitle("Edit data source of project");
		setMessage("Enter data");

		final var lblDriver = new Label(panDialogArea, SWT.NONE);
		lblDriver.setText("Driver name:");

		txtDriverName = new Text(panDialogArea, SWT.BORDER);
		txtDriverName.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		txtDriverName.setEditable(false);
		txtDriverName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDriverName.setText(ds.getDriverName());

		final var lblURL = new Label(panDialogArea, SWT.NONE);
		lblURL.setText("Connection URL:");

		txtURL = new Text(panDialogArea, SWT.BORDER);
		txtURL.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtURL.setText(ds.getConnectionURL());

		final var lblDataSource = new Label(panDialogArea, SWT.NONE);
		lblDataSource.setText("Data source name:");

		txtDataSource = new Text(panDialogArea, SWT.BORDER);
		txtDataSource.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDataSource.setText(ds.getName());

		final var lblUsername = new Label(panDialogArea, SWT.LEFT);
		lblUsername.setText("User name:");

		txtUserName = new Text(panDialogArea, SWT.SINGLE | SWT.BORDER);
		txtUserName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtUserName.setText(ds.getUserName());

		final var lblPassword = new Label(panDialogArea, SWT.LEFT);
		lblPassword.setText("Password:");

		txtPassword = new Text(panDialogArea, SWT.SINGLE | SWT.BORDER | SWT.PASSWORD);
		txtPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtPassword.setText(ds.getPassword());

		final var lblDrivers = new Label(panDialogArea, SWT.NONE);
		lblDrivers.setText("JDBC drivers:");
		lblDrivers.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

		driverSelectionPanel = new DriverSelectionPanel(panDialogArea, SWT.NONE, ds.getDriverList());
		driverSelectionPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.PROCEED_ID, "Test", true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/**
	 * Save the changes
	 * @return true if the operation finished successfully
	 */
	private boolean applyChanges() {
		try {
			ds.setConnectionURL(txtURL.getText());
			ds.setName(txtDataSource.getText());
			ds.setPassword(txtPassword.getText());
			ds.setUserName(txtUserName.getText());
			ds.getDriverList().clear();
			ds.getDriverList().addAll(driverSelectionPanel.getSelectedLibraries());

			EclipseIDEService.saveProjectMetaData(project);

			final IProjectBuildService buildService = ProjectBuildFactory.getBuildService(project);

			if (!project.isSpringBootApplication()) {
				buildService.rebuildPersistenceUnit();
				buildService.rebuildDataSourceFile();
			}
			else
				buildService.rebuildSpringBootConfigurationFiles();

			return true;
		}
		catch (final Exception e) {
			setErrorMessage(e.getMessage());
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
			return false;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.PROCEED_ID) {
			final Shell shell = getShell();
			shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

			try (var dbManager = new DBManager(txtDriverName.getText(), txtURL.getText(), txtUserName.getText(), txtPassword.getText(),
					driverSelectionPanel.getSelectedLibraries())) {
				final String message = dbManager.testConnection();

				shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_ARROW));

				if (message.isEmpty()) {
					setErrorMessage(null);
					setMessage("Successfully connected!");
				}
				else
					setErrorMessage(message);
			}
			catch (final Exception ex) {
				CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
			}

			return;
		}
		else if (buttonId == IDialogConstants.OK_ID) {
			if (txtURL.getText().isEmpty()) {
				setErrorMessage("The connection URL must not be empty!");
				return;
			}

			if (txtDataSource.getText().isEmpty()) {
				setErrorMessage("The data source name must not be empty!");
				return;
			}

			if (txtUserName.getText().isEmpty()) {
				setErrorMessage("The user name must not be empty!");
				return;
			}

			if (txtPassword.getText().isEmpty()) {
				setErrorMessage("The password must not be empty!");
				return;
			}

			if (!applyChanges())
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

		newShell.setText(DLG_TITLE);
	}

}
