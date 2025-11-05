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
package net.codecadenza.eclipse.ui.dialog.testing.integration.panel;

import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for maintaining integration method test invocations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestInvocationPanel extends TestInvocationPanel {
	private final IntegrationTestCase testCase;
	private final IntegrationTestModule testModule;
	private final IntegrationMethodTestInvocation methodInvocation;
	private final BoundaryMethodTypeEnumeration methodType;
	private TabFolder tabFolderMethod;
	private TabItem tabItemReturnValue;
	private ReturnValuePanel panReturnValue;
	private ParametersPanel panParameters;
	private boolean editMode;
	private Text txtName;
	private Text txtCommand;
	private Button chkExpectToFail;
	private Button chkExpectReturnNull;
	private Text txtTimeout;

	/**
	 * Create the panel for creating a new integration test case
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param integrationMethod
	 */
	public IntegrationTestInvocationPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			AbstractIntegrationMethod integrationMethod) {
		super(parent);

		this.testCase = testCase;
		this.testModule = testModule;
		this.methodInvocation = new IntegrationTestCaseService(testModule).initMethodInvocation(testCase, integrationMethod, null);
		this.methodInvocation.setTimeout(testModule.getDefaultTimeout());
		this.methodType = integrationMethod.getBoundaryMethod().getMethodType();

		initPanel();
	}

	/**
	 * Create the panel for updating an existing integration test case
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param testInvocation
	 */
	public IntegrationTestInvocationPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation testInvocation) {
		super(parent);

		this.testCase = testCase;
		this.testModule = testModule;
		this.methodInvocation = testInvocation;
		this.methodType = testInvocation.getIntegrationMethod().getBoundaryMethod().getMethodType();
		this.editMode = true;

		initPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.TestInvocationPanel#generateRandomValues()
	 */
	@Override
	public void generateRandomValues() {
		panParameters.generateRandomValues();
	}

	/**
	 * Initialize the panel
	 */
	void initPanel() {
		setLayout(new GridLayout());

		final var tabFolder = new TabFolder(this, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var tabItemBasicData = new TabItem(tabFolder, SWT.NONE);
		tabItemBasicData.setText("Basic data");

		final var panBasic = new Composite(tabFolder, SWT.NONE);
		panBasic.setLayout(new GridLayout(4, false));
		panBasic.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var lblName = new Label(panBasic, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panBasic, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtName.setText(methodInvocation.getTestMethodName());

		final var lblTimeout = new Label(panBasic, SWT.NONE);
		lblTimeout.setText("Timeout:");

		txtTimeout = new Text(panBasic, SWT.BORDER);
		txtTimeout.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (methodInvocation.getTimeout() != null)
			txtTimeout.setText(methodInvocation.getTimeout().toString());

		final var lblExpectToFail = new Label(panBasic, SWT.NONE);
		lblExpectToFail.setText("Expect to fail:");

		chkExpectToFail = new Button(panBasic, SWT.CHECK);
		chkExpectToFail.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		chkExpectToFail.setSelection(methodInvocation.isExpectToFail());

		chkExpectToFail.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (methodInvocation.canReturnNull()) {
					chkExpectReturnNull.setEnabled(!chkExpectToFail.getSelection());
					chkExpectReturnNull.setSelection(false);
				}

				toggleReturnValuePanel();
			}
		});

		if (methodInvocation.canReturnNull()) {
			final var lblExpectReturnNull = new Label(panBasic, SWT.NONE);
			lblExpectReturnNull.setText("Expect return null:");

			chkExpectReturnNull = new Button(panBasic, SWT.CHECK);
			chkExpectReturnNull.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
			chkExpectReturnNull.setSelection(methodInvocation.isExpectedReturnNull());
			chkExpectReturnNull.setEnabled(!chkExpectToFail.getSelection());

			chkExpectReturnNull.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					toggleReturnValuePanel();
				}
			});
		}

		tabItemBasicData.setControl(panBasic);

		if (enablePostProcessingStatement()) {
			final var tabItemPostProcessing = new TabItem(tabFolder, SWT.NONE);
			tabItemPostProcessing.setText("Post processing");

			final var panPostProcessing = new Composite(tabFolder, SWT.NONE);
			panPostProcessing.setLayout(new GridLayout(2, false));
			panPostProcessing.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

			final var lblCommand = new Label(panPostProcessing, SWT.NONE);
			lblCommand.setText("Command:");

			final var gdCommand = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdCommand.heightHint = 50;

			txtCommand = new Text(panPostProcessing, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
			txtCommand.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
			txtCommand.setLayoutData(gdCommand);

			if (methodInvocation.getPostProcessingStatement() != null)
				txtCommand.setText(methodInvocation.getPostProcessingStatement());

			tabItemPostProcessing.setControl(panPostProcessing);
		}

		final boolean addReturnValuePanel = !methodInvocation.isDownloadFile() && !methodInvocation.isReturnVoid()
				&& !methodInvocation.isExpectToFail() && !methodInvocation.isExpectedReturnNull();

		if (!methodInvocation.getParameters().isEmpty() || addReturnValuePanel) {
			tabFolderMethod = new TabFolder(this, SWT.NONE);
			tabFolderMethod.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

			if (!methodInvocation.getParameters().isEmpty()) {
				final var tabItemParameters = new TabItem(tabFolderMethod, SWT.NONE);
				tabItemParameters.setText("Parameters");

				panParameters = new ParametersPanel(tabFolderMethod, testModule, testCase, methodInvocation);
				tabItemParameters.setControl(panParameters);
			}

			if (addReturnValuePanel)
				addReturnValuePanel();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.TestInvocationPanel#validateAndApplyInput()
	 */
	@Override
	public String validateAndApplyInput() {
		final IStatus status = EclipseIDEService.validateMethodName(txtName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			txtName.setFocus();
			return status.getMessage();
		}

		for (final IntegrationMethodTestInvocation existingInvocation : testCase.getMethodInvocations()) {
			if (editMode && existingInvocation.equals(methodInvocation))
				continue;

			if (existingInvocation.getTestMethodName().equals(txtName.getText())) {
				txtName.setFocus();
				return "Another test method with the same name already exists!";
			}
		}

		// If a method returns void and should track generated attributes it is necessary to define a post-processing command!
		if (!chkExpectToFail.getSelection() && methodInvocation.isReturnVoid() && txtCommand != null
				&& txtCommand.getText().isEmpty()) {
			// Preserve the original state of the 'isExpectToFail' attribute as it must be set properly before calling
			// getTrackedAttributes()!
			final boolean expectToFail = methodInvocation.isExpectToFail();
			String validationMessage = null;

			if (expectToFail)
				methodInvocation.setExpectToFail(false);

			if (methodInvocation.getTrackedAttribute() != null) {
				txtCommand.setFocus();
				validationMessage = "Tracking of generated values requires a post-processing command!";
			}

			if (expectToFail)
				methodInvocation.setExpectToFail(true);

			if (validationMessage != null)
				return validationMessage;
		}

		if (!txtTimeout.getText().isEmpty())
			try {
				Integer.parseInt(txtTimeout.getText());
			}
			catch (final Exception e) {
				txtTimeout.setFocus();
				return e.getMessage();
			}

		try {
			if (txtCommand != null)
				validatePostProcessingStatement(txtCommand.getText());

			if (panParameters != null)
				panParameters.validateAndApplyInput();

			if (panReturnValue != null)
				panReturnValue.validateAndApplyInput();

			methodInvocation.setTestMethodName(txtName.getText());
			methodInvocation.setExpectToFail(chkExpectToFail.getSelection());
			methodInvocation.setExpectedReturnNull(chkExpectReturnNull != null && chkExpectReturnNull.getSelection());

			if (txtCommand != null)
				methodInvocation.setPostProcessingStatement(txtCommand.getText());

			if (!txtTimeout.getText().isEmpty())
				methodInvocation.setTimeout(Integer.parseInt(txtTimeout.getText()));
			else
				methodInvocation.setTimeout(null);

			if (!editMode)
				testCase.getMethodInvocations().add(methodInvocation);
		}
		catch (final Exception e) {
			return e.getMessage();
		}

		return null;
	}

	/**
	 * Check if the given statement is basically correct
	 * @param statement the statement to be checked
	 * @throws IllegalStateException if the validation has failed
	 */
	private void validatePostProcessingStatement(String statement) {
		final String postProcessingStatement = statement.trim();

		if (postProcessingStatement.isEmpty())
			return;

		if (!postProcessingStatement.toLowerCase().startsWith("select"))
			throw new IllegalStateException("The post processing statement must start with 'select'!");

		methodInvocation.getPostProcessingAttributes().clear();

		for (final String attributeName : methodInvocation.extractAttributeNamesFromStatement()) {
			boolean found = false;

			for (final MethodInvocationParameter param : methodInvocation.getParameters()) {
				if (param.isRepresentsList())
					continue;

				if (param.getType() instanceof MappingObject) {
					final TestDataObject testDataObject = param.getParameterValues().getFirst();

					for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
						if (attribute.getMappingAttribute() == null || attribute.getMappingAttribute().getDomainAttribute() == null)
							continue;

						if (attribute.getMappingAttribute().getDomainAttribute().getName().equals(attributeName)) {
							found = true;
							methodInvocation.getPostProcessingAttributes().add(attribute);
							break;
						}
					}
				}
				else if (param.getName().equals(attributeName))
					found = true;

				if (found)
					break;
			}

			if (!found)
				throw new IllegalStateException("An attribute with the name '" + attributeName + "' could not be found!");
		}
	}

	/**
	 * Add the return value panel and the respective tab item, or dispose these controls if the method either is expected to fail or
	 * if it is expected to return null!
	 */
	private void toggleReturnValuePanel() {
		if (chkExpectToFail.getSelection() || (chkExpectReturnNull != null && chkExpectReturnNull.getSelection())) {
			if (tabItemReturnValue != null)
				tabItemReturnValue.dispose();
		}
		else
			addReturnValuePanel();
	}

	/**
	 * Add the panel for the return value of the {@link IntegrationMethodTestInvocation}. If the method downloads a file or if it is
	 * returning void the panel won't be added!
	 */
	private void addReturnValuePanel() {
		if (methodInvocation.isDownloadFile() || methodInvocation.isReturnVoid())
			return;

		panReturnValue = new ReturnValuePanel(tabFolderMethod, testModule, testCase, methodInvocation, true);

		tabItemReturnValue = new TabItem(tabFolderMethod, SWT.NONE);
		tabItemReturnValue.setText("Return value");
		tabItemReturnValue.setControl(panReturnValue);
	}

	/**
	 * @return true if it makes sense to execute a post-processing statement as the respective invocation probably manipulates data
	 */
	private boolean enablePostProcessingStatement() {
		return methodType == BoundaryMethodTypeEnumeration.UPDATE || methodType == BoundaryMethodTypeEnumeration.CREATE
				|| methodType == BoundaryMethodTypeEnumeration.CHANGE_PARENT || methodType == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT
				|| methodType == BoundaryMethodTypeEnumeration.CHANGE_ASSOCIATION
				|| methodType == BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION
				|| methodType == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION
				|| methodType == BoundaryMethodTypeEnumeration.DELETE || methodType == BoundaryMethodTypeEnumeration.DELETE_ALL
				|| methodType == BoundaryMethodTypeEnumeration.UPLOAD || methodType == BoundaryMethodTypeEnumeration.CHANGE_PASSWORD
				|| methodType == BoundaryMethodTypeEnumeration.COPY || methodType == BoundaryMethodTypeEnumeration.SAVE;
	}

}
