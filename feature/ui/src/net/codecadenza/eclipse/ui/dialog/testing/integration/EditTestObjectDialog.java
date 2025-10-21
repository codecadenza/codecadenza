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
package net.codecadenza.eclipse.ui.dialog.testing.integration;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.TestDataAttributePanelData;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.TestDataAttributePanelFactory;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.AbstractMethodPanel;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

/**
 * <p>
 * Dialog for maintaining a {@link TestDataObject}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditTestObjectDialog extends CodeCadenzaDialog {
	private final IntegrationTestModule testModule;
	private final IntegrationTestCase testCase;
	private final IntegrationMethodTestInvocation methodInvocation;
	private final List<AbstractTestDataAttributePanel> testDataAttributePanels = new ArrayList<>();
	private final TestDataObject testDataObject;
	private final boolean validationMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testModule
	 * @param testCase
	 * @param methodInvocation
	 * @param testDataObject
	 * @param validationMode
	 */
	public EditTestObjectDialog(Shell parentShell, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation, TestDataObject testDataObject, boolean validationMode) {
		super(parentShell);

		this.testModule = testModule;
		this.testCase = testCase;
		this.methodInvocation = methodInvocation;
		this.testDataObject = testDataObject;
		this.validationMode = validationMode;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite panDialogArea = (Composite) super.createDialogArea(parent);
		panDialogArea.setLayout(new FillLayout());

		final ScrolledComposite scrolledComposite = AbstractMethodPanel.createScrolledComposite(panDialogArea);
		final boolean addTabFolder = AbstractMethodPanel.isAttributeTabFolderRequired(testDataObject);

		final var panAttributes = new Composite(scrolledComposite, SWT.NONE);
		panAttributes.setLayout(new GridLayout(2, false));

		for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
			if (addTabFolder && AbstractMethodPanel.isAddAttributeToTabFolder(attribute))
				continue;

			final var lblAttribute = new Label(panAttributes, SWT.NONE);
			lblAttribute.setText(attribute.getLabel());

			if (attribute.isMandatory())
				lblAttribute.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

			final TestDataAttributePanelData initializationData = new TestDataAttributePanelData(panAttributes, testModule, testCase,
					methodInvocation, attribute, validationMode, false);

			final var gdAttribute = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdAttribute.minimumWidth = 500;

			final AbstractTestDataAttributePanel panAttribute = TestDataAttributePanelFactory.initPanel(initializationData);
			panAttribute.setLayoutData(gdAttribute);

			testDataAttributePanels.add(panAttribute);
		}

		if (addTabFolder) {
			final var tabFolder = new TabFolder(panAttributes, SWT.NONE);
			tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));

			// Add all attributes that need a dedicated tab
			for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
				if (!AbstractMethodPanel.isAddAttributeToTabFolder(attribute))
					continue;

				final var tabItemAttribute = new TabItem(tabFolder, SWT.NONE);
				tabItemAttribute.setText(attribute.getLabel());

				final TestDataAttributePanelData initializationData = new TestDataAttributePanelData(tabFolder, testModule, testCase,
						methodInvocation, attribute, validationMode, validationMode);

				final AbstractTestDataAttributePanel panAttribute = TestDataAttributePanelFactory.initPanel(initializationData);
				panAttribute.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

				testDataAttributePanels.add(panAttribute);
				tabItemAttribute.setControl(panAttribute);
			}
		}

		panAttributes.pack();

		scrolledComposite.setContent(panAttributes);
		scrolledComposite.setMinSize(panAttributes.computeSize(SWT.DEFAULT, SWT.DEFAULT));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText("Edit test data object of type " + testDataObject.getMappingObject().getName());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !validateAndApplyInput())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateAndApplyInput() {
		try {
			for (final AbstractTestDataAttributePanel panDataAttribute : testDataAttributePanels)
				panDataAttribute.validateAndApplyInput();
		}
		catch (final Exception ex) {
			MessageDialog.openInformation(getParentShell(), "Edit test data object", ex.getMessage());
			return false;
		}

		return true;
	}

}
