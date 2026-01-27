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
package net.codecadenza.eclipse.ui.wizard;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_DS;

import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.resource.db.DatabaseTemplate;
import net.codecadenza.eclipse.tools.util.db.DBManager;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.panel.DriverSelectionPanel;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Wizard page for database connection settings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBConnectionWizardPage extends WizardPage {
	private static final String DLG_TITLE_CONNECTION_TEST = "Database connection test";

	private Text txtDriverName;
	private Combo cboVendor;
	private Text txtURL;
	private IStatus currentStatus;
	private boolean pageVisible;
	private String userName = "";
	private String password;
	private String driverName = "";
	private String connectionURL = "";
	private String catalogName = "";
	private String schemaName = "";
	private Text txtUserName;
	private Text txtPassword;
	private Text txtSchemaName;
	private String dsName;
	private DriverSelectionPanel driverSelectionPanel;
	private EList<DatabaseTemplate> databaseTemplateList;
	private DatabaseTemplate databaseTemplate;

	/**
	 * Constructor
	 * @param pageNumber
	 */
	public DBConnectionWizardPage(int pageNumber) {
		super("page" + pageNumber);

		this.currentStatus = createStatus(IStatus.OK, "");

		setTitle("Database connection");
		setDescription("Enter database connection settings");
	}

	/**
	 * Initialize fields with default values of the respective database template
	 * @param dbVendor
	 */
	private void initDialogFields(String dbVendor) {
		for (final DatabaseTemplate template : databaseTemplateList)
			if (template.getVendorGroup() == DBVendorGroupEnumeration.valueOf(dbVendor)) {
				txtURL.setText(template.getSampleURL());
				connectionURL = txtURL.getText();

				txtDriverName.setText(template.getDriverName());
				driverName = txtDriverName.getText();

				txtSchemaName.setText(template.getDefaultSchemaName());
				schemaName = txtSchemaName.getText();

				databaseTemplate = template;
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayout(new GridLayout(2, false));

		var errorMessage = "";

		try {
			databaseTemplateList = CodeCadenzaResourcePlugin.getDatabaseTemplates();
		}
		catch (final Exception e) {
			databaseTemplateList = new BasicEList<>();
			errorMessage = "Error while reading XML file containing database templates!";
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}

		final var lblVendor = new Label(panPageArea, SWT.NONE);
		lblVendor.setText("Database vendor:");

		cboVendor = new Combo(panPageArea, SWT.NONE | SWT.READ_ONLY);
		cboVendor.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cboVendor.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				initDialogFields(cboVendor.getItem(cboVendor.getSelectionIndex()));
			}
		});

		// Add all database vendors to the respective combobox
		for (final DBVendorGroupEnumeration vendor : DBVendorGroupEnumeration.values())
			cboVendor.add(vendor.getName());

		// Preselect a database vendor
		cboVendor.select(0);

		final var lblDriverName = new Label(panPageArea, SWT.NONE);
		lblDriverName.setText("Driver name:");

		txtDriverName = new Text(panPageArea, SWT.BORDER);
		txtDriverName.setEditable(false);
		txtDriverName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblURL = new Label(panPageArea, SWT.NONE);
		lblURL.setText("Connection URL:");

		txtURL = new Text(panPageArea, SWT.BORDER);
		txtURL.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtURL.addModifyListener(_ -> validateConnectionURL(txtURL.getText()));

		final var lblDataSource = new Label(panPageArea, SWT.NONE);
		lblDataSource.setText("Data source name:");

		final var txtDataSource = new Text(panPageArea, SWT.BORDER);
		txtDataSource.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDataSource.setText(DEFAULT_DS);
		txtDataSource.addModifyListener(_ -> validateDSName(txtDataSource.getText()));

		final var lblUsername = new Label(panPageArea, SWT.LEFT);
		lblUsername.setText("User name:");

		txtUserName = new Text(panPageArea, SWT.SINGLE | SWT.BORDER);
		txtUserName.setText("sa");
		txtUserName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtUserName.addModifyListener(_ -> validateUserName(txtUserName.getText()));

		final var lblPassword = new Label(panPageArea, SWT.LEFT);
		lblPassword.setText("Password:");

		txtPassword = new Text(panPageArea, SWT.SINGLE | SWT.BORDER | SWT.PASSWORD);
		txtPassword.setText("sa");
		txtPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtPassword.addModifyListener(_ -> validatePassword(txtPassword.getText()));

		final var lblCatalog = new Label(panPageArea, SWT.LEFT);
		lblCatalog.setText("Database catalog:");

		final var txtCatalogName = new Text(panPageArea, SWT.BORDER);
		txtCatalogName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtCatalogName.addModifyListener(_ -> catalogName = txtCatalogName.getText());

		final var lblSchemaName = new Label(panPageArea, SWT.LEFT);
		lblSchemaName.setText("Database schema:");

		txtSchemaName = new Text(panPageArea, SWT.BORDER);
		txtSchemaName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtSchemaName.addModifyListener(_ -> schemaName = txtSchemaName.getText());

		final var lblDrivers = new Label(panPageArea, SWT.NONE);
		lblDrivers.setText("JDBC drivers:");
		lblDrivers.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

		final var gdDriverSelectionPanel = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdDriverSelectionPanel.heightHint = 120;

		driverSelectionPanel = new DriverSelectionPanel(panPageArea, SWT.NONE, null);
		driverSelectionPanel.setLayoutData(gdDriverSelectionPanel);

		setControl(panPageArea);

		dsName = txtDataSource.getText();
		userName = txtUserName.getText();
		password = txtPassword.getText();

		new Label(panPageArea, SWT.NONE);
		new Label(panPageArea, SWT.NONE);
		new Label(panPageArea, SWT.NONE);

		final var cmdTest = new Button(panPageArea, SWT.NONE);
		cmdTest.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
		cmdTest.setText("Test connection");

		cmdTest.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				getShell().setCursor(getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

				try (var dbManager = new DBManager(txtDriverName.getText(), txtURL.getText(), txtUserName.getText(),
						txtPassword.getText(), driverSelectionPanel.getSelectedLibraries())) {
					final String message = dbManager.testConnection();

					getShell().setCursor(getShell().getDisplay().getSystemCursor(SWT.CURSOR_ARROW));

					if (message.isEmpty())
						MessageDialog.openInformation(getShell(), DLG_TITLE_CONNECTION_TEST, "Successfully connected!");
					else
						MessageDialog.openWarning(getShell(), DLG_TITLE_CONNECTION_TEST, message);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
				}
			}
		});

		initDialogFields(cboVendor.getItem(cboVendor.getSelectionIndex()));

		validateUserName(userName);
		validateDSName(dsName);
		validateConnectionURL(connectionURL);
		validateDriverName(driverName);

		if (!errorMessage.isEmpty()) {
			final IStatus status = createStatus(IStatus.ERROR, errorMessage);
			updateStatus(status);
		}
	}

	/**
	 * Validate the user name
	 * @param text
	 */
	private void validateUserName(String text) {
		IStatus status = createStatus(IStatus.INFO, "User name entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "The user name must be entered!");

		updateStatus(status);

		userName = text;
	}

	/**
	 * Validate the password
	 * @param text
	 */
	private void validatePassword(String text) {
		IStatus status = createStatus(IStatus.INFO, "Password entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "The password must be entered!");

		updateStatus(status);

		password = text;
	}

	/**
	 * Validate the data source name
	 * @param text
	 */
	private void validateDSName(String text) {
		IStatus status = createStatus(IStatus.INFO, "Data source name entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "The data source name must be entered!");

		updateStatus(status);

		dsName = text;
	}

	/**
	 * Validate the connection URL
	 * @param text
	 */
	private void validateConnectionURL(String text) {
		IStatus status = createStatus(IStatus.INFO, "Connection URL entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "The connection URL must be entered!");

		updateStatus(status);

		connectionURL = text;
	}

	/**
	 * Validate the driver name
	 * @param text
	 */
	private void validateDriverName(String text) {
		IStatus status = createStatus(IStatus.INFO, "Driver name entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "The driver name must be entered!");

		updateStatus(status);

		driverName = text;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		pageVisible = visible;

		updateStatus(currentStatus);
	}

	/**
	 * Update the status line and the 'Finish' button depending on the status
	 * @param status
	 */
	private void updateStatus(IStatus status) {
		currentStatus = status;
		setPageComplete(!status.matches(IStatus.ERROR));

		if (pageVisible)
			applyToStatusLine(this, status);
	}

	/**
	 * @param page
	 * @param status
	 */
	private static void applyToStatusLine(DialogPage page, IStatus status) {
		String errorMessage = null;
		String warningMessage = null;
		final String statusMessage = status.getMessage();

		if (!statusMessage.isEmpty()) {
			if (status.matches(IStatus.ERROR))
				errorMessage = statusMessage;
			else if (!status.isOK())
				warningMessage = statusMessage;
		}

		page.setErrorMessage(errorMessage);
		page.setMessage(warningMessage);
	}

	/**
	 * @param severity
	 * @param message
	 * @return the status
	 */
	private static IStatus createStatus(int severity, String message) {
		return new Status(severity, CodeCadenzaUserInterfacePlugin.PLUGIN_ID, severity, message, null);
	}

	/**
	 * @return the connection URL
	 */
	public String getConnectionURL() {
		return connectionURL;
	}

	/**
	 * @return the name of the data source
	 */
	public String getDSName() {
		return dsName;
	}

	/**
	 * @return the user name
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @return the name of the JDBC driver
	 */
	public String getDriverName() {
		return driverName;
	}

	/**
	 * @return the database catalog name
	 */
	public String getCatalogName() {
		return catalogName;
	}

	/**
	 * @return the database schema name
	 */
	public String getSchemaName() {
		return schemaName;
	}

	/**
	 * @return the selected database template
	 */
	public DatabaseTemplate getDatabaseTemplate() {
		return databaseTemplate;
	}

	/**
	 * @return a list containing the fully qualified paths of all selected libraries
	 */
	public java.util.List<String> getSelectedLibraries() {
		return driverSelectionPanel.getSelectedLibraries();
	}

}
