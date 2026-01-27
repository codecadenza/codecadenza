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

import static net.codecadenza.eclipse.shared.Constants.PREF_TEST_CASE_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.PREF_TEST_CONTEXT_SELENIUM;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.SeleniumDriver;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.panel.IntegrationTestModulePanel;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Wizard page for the test module configuration
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestingWizardPage extends WizardPage {
	private static final int DEFAULT_IMPLICIT_WAIT_TIME = 1;
	private static final int DEFAULT_PAGE_LOAD_TIMEOUT = 5;

	private final ApplicationWizardPage pageApp;
	private final IntegrationWizardPage pageIntegration;
	private Button chkSelenium;
	private Text txtSeleniumNamespace;
	private Combo cboDriver;
	private Button cmdBrowse;
	private Text txtDriverPath;
	private Text txtImplicitWaitTime;
	private Button chkMaximizeWindow;
	private Text txtPageLoadTimeout;
	private Text txtTestCaseSuffix;
	private String seleniumNamespaceName;
	private SeleniumDriver driver;
	private String driverPath;
	private int pageLoadTimeout;
	private int implicitWaitTime;
	private boolean maximizeWindow;
	private String testCaseSuffix;
	private IStatus currentStatus;
	private boolean pageVisible;
	private IntegrationTestModulePanel panSOAP;
	private IntegrationTestModulePanel panREST;
	private IntegrationTestModulePanel panRMI;
	private IntegrationTestModulePanel panKafka;
	private IntegrationTestModulePanel panJMS;
	private Button chkSOAP;
	private Button chkREST;
	private Button chkRMI;
	private Button chkKafka;
	private Button chkJMS;

	/**
	 * Constructor
	 * @param pageNumber
	 * @param pageApp
	 * @param pageIntegration
	 */
	public TestingWizardPage(int pageNumber, ApplicationWizardPage pageApp, IntegrationWizardPage pageIntegration) {
		super("page" + pageNumber);

		this.currentStatus = createStatus(IStatus.OK, "");
		this.pageApp = pageApp;
		this.pageIntegration = pageIntegration;

		setTitle("Test modules");
		setDescription("Definition of test modules");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayout(new GridLayout());

		setControl(panPageArea);

		chkSelenium = new Button(panPageArea, SWT.CHECK);
		chkSelenium.setText("Add Selenium test module");

		chkSelenium.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				initSeleniumFields(chkSelenium.getSelection());
			}
		});

		final var tabFolder = new TabFolder(panPageArea, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var tabItemSelenium = new TabItem(tabFolder, SWT.NONE);
		tabItemSelenium.setText("Selenium");

		final var panSelenium = new Composite(tabFolder, SWT.NONE);
		panSelenium.setLayout(new GridLayout(2, false));

		final var lblSeleniumNamespace = new Label(panSelenium, SWT.NONE);
		lblSeleniumNamespace.setText("Package:");

		txtSeleniumNamespace = new Text(panSelenium, SWT.BORDER);
		txtSeleniumNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtSeleniumNamespace.addModifyListener(_ -> applySeleniumNamespace(txtSeleniumNamespace.getText()));

		final var lblDriver = new Label(panSelenium, SWT.NONE);
		lblDriver.setText("Driver:");

		cboDriver = new Combo(panSelenium, SWT.READ_ONLY);
		cboDriver.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final SeleniumDriver seleniumDriver : SeleniumDriver.values())
			cboDriver.add(seleniumDriver.getName());

		cboDriver.select(0);

		cboDriver.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				driver = SeleniumDriver.valueOf(cboDriver.getItem(cboDriver.getSelectionIndex()));
			}
		});

		driver = SeleniumDriver.valueOf(cboDriver.getItem(cboDriver.getSelectionIndex()));

		final var lblDriverPath = new Label(panSelenium, SWT.NONE);
		lblDriverPath.setText("Driver path:");

		final var glPanBrowse = new GridLayout(2, false);
		glPanBrowse.marginHeight = 0;
		glPanBrowse.marginWidth = 0;
		glPanBrowse.horizontalSpacing = 0;

		final var panBrowse = new Composite(panSelenium, SWT.NONE);
		panBrowse.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panBrowse.setLayout(glPanBrowse);

		txtDriverPath = new Text(panBrowse, SWT.BORDER);
		txtDriverPath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDriverPath.setEditable(false);
		txtDriverPath.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));

		cmdBrowse = new Button(panBrowse, SWT.NONE);
		cmdBrowse.setText("Browse...");

		cmdBrowse.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlgFile = new FileDialog(getShell(), SWT.OPEN);
				dlgFile.setText("Open");

				driverPath = dlgFile.open();

				if (driverPath == null)
					txtDriverPath.setText("");
				else
					txtDriverPath.setText(driverPath);
			}
		});

		final var lblImplicitWaitTime = new Label(panSelenium, SWT.NONE);
		lblImplicitWaitTime.setText("Implicit wait time (in seconds):");

		txtImplicitWaitTime = new Text(panSelenium, SWT.BORDER);
		txtImplicitWaitTime.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtImplicitWaitTime.addModifyListener(_ -> applyImplicitWaitTime(txtImplicitWaitTime.getText()));

		final var lblPageLoadTimeout = new Label(panSelenium, SWT.NONE);
		lblPageLoadTimeout.setText("Page load timeout (in seconds):");

		txtPageLoadTimeout = new Text(panSelenium, SWT.BORDER);
		txtPageLoadTimeout.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtPageLoadTimeout.addModifyListener(_ -> applyPageLoadTimeout(txtPageLoadTimeout.getText()));

		final var lblMaximizeWindow = new Label(panSelenium, SWT.NONE);
		lblMaximizeWindow.setText("Maximize window:");

		chkMaximizeWindow = new Button(panSelenium, SWT.CHECK);

		chkMaximizeWindow.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				maximizeWindow = chkMaximizeWindow.getSelection();
			}
		});

		final var lblTestCaseSuffix = new Label(panSelenium, SWT.NONE);
		lblTestCaseSuffix.setText("Test case suffix:");

		txtTestCaseSuffix = new Text(panSelenium, SWT.BORDER);
		txtTestCaseSuffix.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtTestCaseSuffix.addModifyListener(_ -> applyTestCaseSuffix(txtTestCaseSuffix.getText()));

		tabItemSelenium.setControl(panSelenium);

		initIntegrationTestModuleGroup(panPageArea);
	}

	/**
	 * Initialize the panel for the integration test modules
	 * @param panParent
	 */
	private void initIntegrationTestModuleGroup(Composite panParent) {
		final var groupIntegrationTests = new Group(panParent, SWT.NONE);
		groupIntegrationTests.setText("Integration test modules");
		groupIntegrationTests.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupIntegrationTests.setLayout(new GridLayout(1, false));

		final var panSelection = new Composite(groupIntegrationTests, SWT.NONE);
		panSelection.setLayout(new GridLayout(5, false));
		panSelection.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		chkSOAP = new Button(panSelection, SWT.CHECK);
		chkSOAP.setText("Add SOAP module");

		chkSOAP.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkSOAP.getSelection())
					panSOAP.setIntegrationModule(pageIntegration.getIntegrationModule(IntegrationTechnology.SOAP));
				else
					panSOAP.setIntegrationModule(null);
			}
		});

		chkREST = new Button(panSelection, SWT.CHECK);
		chkREST.setText("Add REST module");

		chkREST.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkREST.getSelection())
					panREST.setIntegrationModule(pageIntegration.getIntegrationModule(IntegrationTechnology.REST));
				else
					panREST.setIntegrationModule(null);
			}
		});

		chkRMI = new Button(panSelection, SWT.CHECK);
		chkRMI.setText("Add RMI module");

		chkRMI.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkRMI.getSelection())
					panRMI.setIntegrationModule(pageIntegration.getIntegrationModule(IntegrationTechnology.RMI));
				else
					panRMI.setIntegrationModule(null);
			}
		});

		chkKafka = new Button(panSelection, SWT.CHECK);
		chkKafka.setText("Add Kafka module");

		chkKafka.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkKafka.getSelection())
					panKafka.setIntegrationModule(pageIntegration.getIntegrationModule(IntegrationTechnology.KAFKA));
				else
					panKafka.setIntegrationModule(null);
			}
		});

		chkJMS = new Button(panSelection, SWT.CHECK);
		chkJMS.setText("Add JMS module");

		chkJMS.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkJMS.getSelection())
					panJMS.setIntegrationModule(pageIntegration.getIntegrationModule(IntegrationTechnology.JMS));
				else
					panJMS.setIntegrationModule(null);
			}
		});

		final var tabFolder = new TabFolder(groupIntegrationTests, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		final var tabItemSOAP = new TabItem(tabFolder, SWT.NONE);
		tabItemSOAP.setText("SOAP");

		panSOAP = new IntegrationTestModulePanel(tabFolder, SWT.NONE, this);
		panSOAP.setLayout(new GridLayout(2, false));

		tabItemSOAP.setControl(panSOAP);

		final var tabItemREST = new TabItem(tabFolder, SWT.NONE);
		tabItemREST.setText("REST");

		panREST = new IntegrationTestModulePanel(tabFolder, SWT.NONE, this);
		panREST.setLayout(new GridLayout(2, false));

		tabItemREST.setControl(panREST);

		final var tabItemRMI = new TabItem(tabFolder, SWT.NONE);
		tabItemRMI.setText("RMI");

		panRMI = new IntegrationTestModulePanel(tabFolder, SWT.NONE, this);
		panRMI.setLayout(new GridLayout(2, false));

		tabItemRMI.setControl(panRMI);

		final var tabItemKafka = new TabItem(tabFolder, SWT.NONE);
		tabItemKafka.setText("Kafka");

		panKafka = new IntegrationTestModulePanel(tabFolder, SWT.NONE, this);
		panKafka.setLayout(new GridLayout(2, false));

		tabItemKafka.setControl(panKafka);

		final var tabItemJMS = new TabItem(tabFolder, SWT.NONE);
		tabItemJMS.setText("JMS");

		panJMS = new IntegrationTestModulePanel(tabFolder, SWT.NONE, this);
		panJMS.setLayout(new GridLayout(2, false));

		tabItemJMS.setControl(panJMS);
	}

	/**
	 * Initialize form fields that belong to the Selenium test module
	 * @param addSeleniumTestModule controls if the respective fields should be either enabled or disabled
	 */
	private void initSeleniumFields(boolean addSeleniumTestModule) {
		// Reset all text fields
		txtSeleniumNamespace.setText("");
		txtDriverPath.setText("");
		txtPageLoadTimeout.setText("");
		txtImplicitWaitTime.setText("");
		txtTestCaseSuffix.setText("");

		// Either enable or disable all Selenium-related fields depending on the selected state
		txtSeleniumNamespace.setEnabled(addSeleniumTestModule);
		cboDriver.setEnabled(addSeleniumTestModule);
		txtDriverPath.setEnabled(addSeleniumTestModule);
		cmdBrowse.setEnabled(addSeleniumTestModule);
		txtImplicitWaitTime.setEnabled(addSeleniumTestModule);
		txtPageLoadTimeout.setEnabled(addSeleniumTestModule);
		chkMaximizeWindow.setEnabled(addSeleniumTestModule);
		txtTestCaseSuffix.setEnabled(addSeleniumTestModule);
		chkMaximizeWindow.setSelection(addSeleniumTestModule);

		if (addSeleniumTestModule) {
			maximizeWindow = true;

			// Read default values from preference store
			final IPreferenceStore store = CodeCadenzaUserInterfacePlugin.getInstance().getPreferenceStore();

			txtSeleniumNamespace.setText(store.getString(PREF_TEST_CONTEXT_SELENIUM));
			txtTestCaseSuffix.setText(store.getString(PREF_TEST_CASE_SUFFIX));

			// Set reasonable default values for implicit wait time and page load timeout
			txtImplicitWaitTime.setText(Integer.toString(DEFAULT_IMPLICIT_WAIT_TIME));
			txtPageLoadTimeout.setText(Integer.toString(DEFAULT_PAGE_LOAD_TIMEOUT));
		}
	}

	/**
	 * Initialize the test modules
	 * @return a list containing all test modules
	 */
	private List<AbstractTestModule> initTestModules() {
		final var testModules = new ArrayList<AbstractTestModule>();

		if (chkSelenium.getSelection()) {
			final Namespace testNamespace = JavaFactory.eINSTANCE.createNamespace();
			testNamespace.setName(seleniumNamespaceName);

			final SeleniumTestModule testModule = TestingFactory.eINSTANCE.createSeleniumTestModule();
			testModule.setTestCaseSuffix(testCaseSuffix);
			testModule.setDriver(driver);
			testModule.setDriverPath(driverPath);
			testModule.setPageLoadTime(pageLoadTimeout);
			testModule.setImplicitWaitTime(implicitWaitTime);
			testModule.setMaximizeWindow(maximizeWindow);
			testModule.setNamespace(testNamespace);

			testModules.add(testModule);
		}

		if (panSOAP.getIntegrationTestModule() != null)
			testModules.add(panSOAP.getIntegrationTestModule());

		if (panREST.getIntegrationTestModule() != null)
			testModules.add(panREST.getIntegrationTestModule());

		if (panRMI.getIntegrationTestModule() != null)
			testModules.add(panRMI.getIntegrationTestModule());

		if (panKafka.getIntegrationTestModule() != null)
			testModules.add(panKafka.getIntegrationTestModule());

		if (panJMS.getIntegrationTestModule() != null)
			testModules.add(panJMS.getIntegrationTestModule());

		return testModules;
	}

	/**
	 * @param text
	 */
	private void applySeleniumNamespace(String text) {
		if (!chkSelenium.getSelection())
			return;

		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		seleniumNamespaceName = text;
	}

	/**
	 * @param text
	 */
	private void applyTestCaseSuffix(String text) {
		if (!chkSelenium.getSelection())
			return;

		final IStatus status = EclipseIDEService.validateJavaTypeName(text);
		updateStatus(status);

		testCaseSuffix = text;
	}

	/**
	 * @param text
	 */
	private void applyImplicitWaitTime(String text) {
		if (!chkSelenium.getSelection())
			return;

		IStatus status = createStatus(IStatus.INFO, "Implicit wait time entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "An implicit wait time must be entered!");

		if (!text.isEmpty())
			try {
				implicitWaitTime = Integer.parseInt(text);

				if (implicitWaitTime < 0)
					status = createStatus(IStatus.ERROR, "The implicit wait time requires a positive integer value!");
			}
			catch (final NumberFormatException _) {
				status = createStatus(IStatus.ERROR, "The implicit wait time requires an integer value!");
			}

		updateStatus(status);
	}

	/**
	 * @param text
	 */
	private void applyPageLoadTimeout(String text) {
		if (!chkSelenium.getSelection())
			return;

		IStatus status = createStatus(IStatus.INFO, "Page load timeout entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "The page load timeout must be entered!");

		if (!text.isEmpty())
			try {
				pageLoadTimeout = Integer.parseInt(text);
			}
			catch (final NumberFormatException _) {
				status = createStatus(IStatus.ERROR, "The page load timeout requires an integer value!");
			}

		updateStatus(status);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		pageVisible = visible;

		if (visible && currentStatus.matches(IStatus.ERROR))
			currentStatus = createStatus(IStatus.ERROR, "");

		if (visible) {
			boolean enableSelenium = false;

			if (pageApp.getClientPlatform() == ClientPlatformEnumeration.ANGULAR
					|| pageApp.getClientPlatform() == ClientPlatformEnumeration.JSF_PRIMEFACES
					|| pageApp.getClientPlatform() == ClientPlatformEnumeration.VAADIN)
				enableSelenium = true;

			chkSelenium.setSelection(enableSelenium);
			chkSelenium.setEnabled(enableSelenium);

			initSeleniumFields(enableSelenium);

			if (pageIntegration.isAddSOAPModule() && pageIntegration.isAddSOAPClient()) {
				final IntegrationModule integrationModule = pageIntegration.getIntegrationModule(IntegrationTechnology.SOAP);

				chkSOAP.setEnabled(true);
				chkSOAP.setSelection(true);
				panSOAP.setIntegrationModule(integrationModule);
			}
			else {
				chkSOAP.setEnabled(false);
				chkSOAP.setSelection(false);
				panSOAP.setIntegrationModule(null);
			}

			if (pageIntegration.isAddRESTModule() && pageIntegration.isAddRESTClient()) {
				final IntegrationModule integrationModule = pageIntegration.getIntegrationModule(IntegrationTechnology.REST);

				chkREST.setEnabled(true);
				chkREST.setSelection(true);
				panREST.setIntegrationModule(integrationModule);
			}
			else {
				chkREST.setEnabled(false);
				chkREST.setSelection(false);
				panREST.setIntegrationModule(null);
			}

			if (pageIntegration.isAddRMIModule() && pageIntegration.isAddRMIClient()) {
				final IntegrationModule integrationModule = pageIntegration.getIntegrationModule(IntegrationTechnology.RMI);

				chkRMI.setEnabled(true);
				chkRMI.setSelection(true);
				panRMI.setIntegrationModule(integrationModule);
			}
			else {
				chkRMI.setEnabled(false);
				chkRMI.setSelection(false);
				panRMI.setIntegrationModule(null);
			}

			if (pageIntegration.isAddKafkaModule() && pageIntegration.isAddKafkaClient()) {
				final IntegrationModule integrationModule = pageIntegration.getIntegrationModule(IntegrationTechnology.KAFKA);

				chkKafka.setEnabled(true);
				chkKafka.setSelection(true);
				panKafka.setIntegrationModule(integrationModule);
			}
			else {
				chkKafka.setEnabled(false);
				chkKafka.setSelection(false);
				panKafka.setIntegrationModule(null);
			}

			if (pageIntegration.isAddJMSModule() && pageIntegration.isAddJMSClient()) {
				final IntegrationModule integrationModule = pageIntegration.getIntegrationModule(IntegrationTechnology.JMS);

				chkJMS.setEnabled(true);
				chkJMS.setSelection(true);
				panJMS.setIntegrationModule(integrationModule);
			}
			else {
				chkJMS.setEnabled(false);
				chkJMS.setSelection(false);
				panJMS.setIntegrationModule(null);
			}

			// Reset the status as we don't want to see the last status after initializing the fields
			updateStatus(createStatus(IStatus.OK, ""));
		}

		updateStatus(currentStatus);
	}

	/**
	 * Update the status line and the 'Finish' button depending on the status
	 * @param status
	 */
	public void updateStatus(IStatus status) {
		currentStatus = status;
		setPageComplete(!status.matches(IStatus.ERROR));

		if (pageVisible)
			applyToStatusLine(this, status);
	}

	/**
	 * @param page
	 * @param status
	 */
	public static void applyToStatusLine(DialogPage page, IStatus status) {
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
	 * @return a list containing all test modules that has been defined by the user
	 */
	public List<AbstractTestModule> getTestModules() {
		return initTestModules();
	}

	/**
	 * @return true if a Selenium test module should be added
	 */
	public boolean isAddSeleniumTestModule() {
		return chkSelenium.getSelection();
	}

	/**
	 * @return true if a REST integration test module should be added
	 */
	public boolean isAddRESTTestModule() {
		return chkREST.getSelection();
	}

	/**
	 * @return true if a SOAP integration test module should be added
	 */
	public boolean isAddSOAPTestModule() {
		return chkSOAP.getSelection();
	}

	/**
	 * @return true if an RMI integration test module should be added
	 */
	public boolean isAddRMITestModule() {
		return chkRMI.getSelection();
	}

	/**
	 * @return true if a Kafka integration test module should be added
	 */
	public boolean isAddKafkaTestModule() {
		return chkKafka.getSelection();
	}

	/**
	 * @return true if a JMS integration test module should be added
	 */
	public boolean isAddJMSTestModule() {
		return chkJMS.getSelection();
	}

}
