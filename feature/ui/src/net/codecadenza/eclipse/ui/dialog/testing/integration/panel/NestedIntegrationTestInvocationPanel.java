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
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for maintaining nested test invocations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class NestedIntegrationTestInvocationPanel extends TestInvocationPanel {
	private final IntegrationTestModule testModule;
	private final IntegrationTestCase testCase;
	private final IntegrationMethodTestInvocation parentInvocation;
	private final IntegrationMethodTestInvocation nestedInvocation;
	private final BoundaryMethodTypeEnumeration methodType;
	private ReturnValuePanel panReturnValue;
	private ParametersPanel panParameters;
	private ExpectedSizePanel panExpectedSize;
	private boolean editMode;

	/**
	 * Create the dialog for creating a new nested integration test invocation
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param parentInvocation
	 */
	public NestedIntegrationTestInvocationPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation parentInvocation) {
		super(parent);

		this.testModule = testModule;
		this.testCase = testCase;
		this.parentInvocation = parentInvocation;
		this.nestedInvocation = new IntegrationTestCaseService(testModule).initMethodInvocation(testCase,
				parentInvocation.getIntegrationMethod(), parentInvocation);
		this.methodType = parentInvocation.getIntegrationMethod().getBoundaryMethod().getMethodType();

		initPanel();
	}

	/**
	 * Create the dialog for updating an existing nested integration test invocation
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param parentInvocation
	 * @param nestedInvocation
	 */
	public NestedIntegrationTestInvocationPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation parentInvocation, IntegrationMethodTestInvocation nestedInvocation) {
		super(parent);

		this.testModule = testModule;
		this.testCase = testCase;
		this.parentInvocation = parentInvocation;
		this.nestedInvocation = nestedInvocation;
		this.methodType = parentInvocation.getIntegrationMethod().getBoundaryMethod().getMethodType();
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

		final var panParentGroup = new Group(this, SWT.BORDER);
		panParentGroup.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		panParentGroup.setLayout(new GridLayout(2, false));
		panParentGroup.setText("Basic data of parent invocation");

		final var lblName = new Label(panParentGroup, SWT.NONE);
		lblName.setText("Name:");

		final var txtName = new Text(panParentGroup, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtName.setText(parentInvocation.getTestMethodName());
		txtName.setEditable(false);

		final var lblExpectToFail = new Label(panParentGroup, SWT.NONE);
		lblExpectToFail.setText("Expect to fail:");

		final var chkExpectToFail = new Button(panParentGroup, SWT.CHECK);
		chkExpectToFail.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		chkExpectToFail.setSelection(parentInvocation.isExpectToFail());
		chkExpectToFail.setEnabled(false);

		final var lblTimeout = new Label(panParentGroup, SWT.NONE);
		lblTimeout.setText("Timeout:");

		final var txtTimeout = new Text(panParentGroup, SWT.BORDER);
		txtTimeout.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtTimeout.setEditable(false);

		if (parentInvocation.getTimeout() != null)
			txtTimeout.setText(parentInvocation.getTimeout().toString());

		if (!parentInvocation.isExpectToFail() && (methodType == BoundaryMethodTypeEnumeration.DOWNLOAD
				|| methodType == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT)) {
			panExpectedSize = new ExpectedSizePanel(panParentGroup, nestedInvocation, "Expected file size:", false);
			panExpectedSize.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		}

		final boolean addReturnValuePanel = !parentInvocation.isDownloadFile() && !parentInvocation.isReturnVoid()
				&& !parentInvocation.isExpectToFail() && !parentInvocation.isExpectedReturnNull();

		if (!parentInvocation.getParameters().isEmpty() || addReturnValuePanel) {
			final var tabFolderMethod = new TabFolder(this, SWT.NONE);
			tabFolderMethod.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

			if (!parentInvocation.getParameters().isEmpty()) {
				final var tabItemParameters = new TabItem(tabFolderMethod, SWT.NONE);
				tabItemParameters.setText("Parameters");

				panParameters = new ParametersPanel(tabFolderMethod, testModule, testCase, nestedInvocation);
				tabItemParameters.setControl(panParameters);
			}

			if (addReturnValuePanel) {
				panReturnValue = new ReturnValuePanel(tabFolderMethod, testModule, testCase, nestedInvocation, false);

				final var tabItemReturnValue = new TabItem(tabFolderMethod, SWT.NONE);
				tabItemReturnValue.setText("Return value");
				tabItemReturnValue.setControl(panReturnValue);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.TestInvocationPanel#validateAndApplyInput()
	 */
	@Override
	public String validateAndApplyInput() {
		try {
			if (panExpectedSize != null)
				panExpectedSize.validateAndApplyInput();

			if (panParameters != null)
				panParameters.validateAndApplyInput();

			if (panReturnValue != null)
				panReturnValue.validateAndApplyInput();

			if (!editMode)
				parentInvocation.getNestedInvocations().add(nestedInvocation);
		}
		catch (final Exception e) {
			return e.getMessage();
		}

		return null;
	}

}
