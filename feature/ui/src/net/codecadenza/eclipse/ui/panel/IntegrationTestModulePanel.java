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
package net.codecadenza.eclipse.ui.panel;

import static net.codecadenza.eclipse.shared.Constants.PREF_DATE_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_NUMBER_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_TIME_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Random;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.wizard.TestingWizardPage;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for the initialization of an integration test module
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestModulePanel extends Composite {
	private final TestingWizardPage wizardPage;
	private final Text txtDateFormat;
	private final Random random = new Random();
	private final Text txtDateTimeFormat;
	private final Text txtDecimalFormat;
	private final Text txtDecimalSeparator;
	private final Text txtGroupingSeparator;
	private final Text txtTimeout;
	private final Text txtTestSuffix;
	private IntegrationModule integrationModule;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 * @param wizardPage
	 */
	public IntegrationTestModulePanel(Composite parent, int style, TestingWizardPage wizardPage) {
		super(parent, style);

		this.wizardPage = wizardPage;

		final var glMain = new GridLayout(4, false);
		glMain.horizontalSpacing = 2;
		glMain.verticalSpacing = 2;
		glMain.horizontalSpacing = 4;

		final var panMain = new Composite(this, SWT.NONE);
		panMain.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panMain.setLayout(glMain);

		final IPreferenceStore store = CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();
		final DecimalFormat format = (DecimalFormat) DecimalFormat.getInstance();
		final DecimalFormatSymbols symbols = format.getDecimalFormatSymbols();

		final var lblTimeout = new Label(panMain, SWT.NONE);
		lblTimeout.setText("Default timeout (ms):");

		txtTimeout = new Text(panMain, SWT.BORDER);
		txtTimeout.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtTimeout.setEnabled(false);
		txtTimeout.setText("1000");
		txtTimeout.addModifyListener(_ -> validateTimeout(txtTimeout.getText()));

		final var lblTestSuffix = new Label(panMain, SWT.NONE);
		lblTestSuffix.setText("Test case suffix:");

		txtTestSuffix = new Text(panMain, SWT.BORDER);
		txtTestSuffix.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtTestSuffix.setEnabled(false);
		txtTestSuffix.setText("Test");
		txtTestSuffix.addModifyListener(_ -> validateTestCaseSuffix(txtTestSuffix.getText()));

		final var lblDateTimeFormat = new Label(panMain, SWT.NONE);
		lblDateTimeFormat.setText("Date time format:");

		txtDateTimeFormat = new Text(panMain, SWT.BORDER);
		txtDateTimeFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDateTimeFormat.setEnabled(false);
		txtDateTimeFormat.setText(store.getString(PREF_DATE_FORMAT) + " " + store.getString(PREF_TIME_FORMAT));
		txtDateTimeFormat.addModifyListener(_ -> validateDateFormat(txtDateTimeFormat.getText()));

		final var lblDateFormat = new Label(panMain, SWT.NONE);
		lblDateFormat.setText("Date format:");

		txtDateFormat = new Text(panMain, SWT.BORDER);
		txtDateFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDateFormat.setEnabled(false);
		txtDateFormat.setText(store.getString(PREF_DATE_FORMAT));
		txtDateFormat.addModifyListener(_ -> validateDateFormat(txtDateFormat.getText()));

		final var lblDecimalFormat = new Label(panMain, SWT.NONE);
		lblDecimalFormat.setText("Decimal format:");

		txtDecimalFormat = new Text(panMain, SWT.BORDER);
		txtDecimalFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDecimalFormat.setEnabled(false);
		txtDecimalFormat.setText(store.getString(PREF_NUMBER_FORMAT));
		txtDecimalFormat.addModifyListener(_ -> validateDecimalFormat(txtDecimalFormat.getText()));

		new Label(panMain, SWT.NONE);
		new Label(panMain, SWT.NONE);

		final var lblDecimalSeparator = new Label(panMain, SWT.NONE);
		lblDecimalSeparator.setText("Decimal separator:");

		txtDecimalSeparator = new Text(panMain, SWT.BORDER);
		txtDecimalSeparator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDecimalSeparator.setEnabled(false);
		txtDecimalSeparator.setText(Character.toString(symbols.getDecimalSeparator()));
		txtDecimalSeparator.addModifyListener(_ -> validateDecimalFormat(txtDecimalFormat.getText()));

		final var lblGroupingSeparator = new Label(panMain, SWT.NONE);
		lblGroupingSeparator.setText("Grouping separator:");

		txtGroupingSeparator = new Text(panMain, SWT.BORDER);
		txtGroupingSeparator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtGroupingSeparator.setEnabled(false);
		txtGroupingSeparator.setText(Character.toString(symbols.getGroupingSeparator()));
		txtGroupingSeparator.addModifyListener(_ -> validateDecimalFormat(txtDecimalFormat.getText()));
	}

	/**
	 * Set the integration module and initialize all input fields
	 * @param integrationModule
	 */
	public void setIntegrationModule(IntegrationModule integrationModule) {
		this.integrationModule = integrationModule;

		txtTimeout.setEnabled(integrationModule != null);
		txtTestSuffix.setEnabled(integrationModule != null);
		txtDateFormat.setEnabled(integrationModule != null);
		txtDateTimeFormat.setEnabled(integrationModule != null);
		txtDecimalFormat.setEnabled(integrationModule != null);
		txtDecimalSeparator.setEnabled(integrationModule != null);
		txtGroupingSeparator.setEnabled(integrationModule != null);
	}

	/**
	 * @return a new fully initialized integration test module or null if the respective integration module is not available
	 */
	public IntegrationTestModule getIntegrationTestModule() {
		if (integrationModule == null)
			return null;

		final Namespace namespace = JavaFactory.eINSTANCE.createNamespace();
		namespace.setName(integrationModule.getNamespace().getName() + SUB_PACKAGE_INT_CLIENT);

		final IntegrationTestModule integrationTestModule = TestingFactory.eINSTANCE.createIntegrationTestModule();
		integrationTestModule.setNamespace(namespace);
		integrationTestModule.setIntegrationModule(integrationModule);
		integrationTestModule.setDefaultTimeout(Integer.parseInt(txtTimeout.getText()));
		integrationTestModule.setTestCaseSuffix(txtTestSuffix.getText());
		integrationTestModule.setDateTimeFormat(txtDateTimeFormat.getText());
		integrationTestModule.setDateFormat(txtDateFormat.getText());
		integrationTestModule.setDecimalFormat(txtDecimalFormat.getText());
		integrationTestModule.setGroupingSeparator(txtGroupingSeparator.getText().charAt(0));
		integrationTestModule.setDecimalSeparator(txtDecimalSeparator.getText().charAt(0));

		return integrationTestModule;
	}

	/**
	 * Validate the date format
	 * @param text
	 */
	private void validateDateFormat(String text) {
		IStatus status = createStatus(IStatus.OK, "");

		try {
			final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(text).withZone(ZoneId.systemDefault());
			dateFormat.format(Instant.now());
		}
		catch (final Exception e) {
			status = createStatus(IStatus.ERROR, e.getMessage());
			wizardPage.updateStatus(status);
		}

		wizardPage.updateStatus(status);
	}

	/**
	 * Validate the decimal format
	 * @param text
	 */
	private void validateDecimalFormat(String text) {
		IStatus status = createStatus(IStatus.OK, "");

		try {
			final String decimalSeparator = txtDecimalSeparator.getText();
			final String groupingSeparator = txtGroupingSeparator.getText();

			if (decimalSeparator.length() != 1 || groupingSeparator.length() != 1)
				throw new IllegalStateException("Exactly one separator character is expected!");

			final DecimalFormat decimalFormat = new DecimalFormat(text);
			decimalFormat.getDecimalFormatSymbols().setDecimalSeparator(decimalSeparator.charAt(0));
			decimalFormat.getDecimalFormatSymbols().setGroupingSeparator(groupingSeparator.charAt(0));
			decimalFormat.format(random.nextDouble());
		}
		catch (final Exception e) {
			status = createStatus(IStatus.ERROR, e.getMessage());
			wizardPage.updateStatus(status);
		}

		wizardPage.updateStatus(status);
	}

	/**
	 * Validate the timeout
	 * @param text
	 */
	private void validateTimeout(String text) {
		IStatus status = createStatus(IStatus.OK, "");

		try {
			final int timeout = Integer.parseInt(text);

			if (timeout < 1)
				throw new IllegalStateException("The timeout must be greater than 0!");
		}
		catch (final Exception e) {
			status = createStatus(IStatus.ERROR, e.getMessage());
			wizardPage.updateStatus(status);
		}

		wizardPage.updateStatus(status);
	}

	/**
	 * Validate the test case suffix
	 * @param text
	 */
	private void validateTestCaseSuffix(String text) {
		final IStatus status = EclipseIDEService.validateJavaTypeName(text);
		wizardPage.updateStatus(status);
	}

	/**
	 * Create a new status
	 * @param severity
	 * @param message
	 * @return a new status
	 */
	private static IStatus createStatus(int severity, String message) {
		return new Status(severity, CodeCadenzaUserInterfacePlugin.PLUGIN_ID, severity, message, null);
	}

}
