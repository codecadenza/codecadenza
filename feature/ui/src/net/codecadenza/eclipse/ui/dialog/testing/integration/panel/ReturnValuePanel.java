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

import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.TestDataAttributePanelData;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.TestDataAttributePanelFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

/**
 * <p>
 * Panel for entering the test data for the method return value
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReturnValuePanel extends AbstractMethodPanel {
	/**
	 * Create the panel
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param testInvocation
	 * @param enableAssertionOperators
	 */
	public ReturnValuePanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation testInvocation, boolean enableAssertionOperators) {
		super(parent, testModule, testCase, testInvocation, enableAssertionOperators);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.AbstractMethodPanel#initPanel()
	 */
	@Override
	protected void initPanel() {
		setLayout(new FillLayout());

		final ScrolledComposite scrolledComposite = createScrolledComposite(this);

		final var panReturnValue = new Composite(scrolledComposite, SWT.NONE);
		panReturnValue.setLayout(new GridLayout(2, false));

		final JavaType returnType = integrationMethod.getReturnType();

		if (returnType instanceof final MappingObject mappingObject) {
			final var label = new StringBuilder("Return type ");

			if (methodInvocation.isReturnList())
				label.append("List<");

			label.append(mappingObject.getName());

			if (methodInvocation.isReturnList())
				label.append(">");

			final var lblMappingObject = new Label(panReturnValue, SWT.NONE);
			lblMappingObject.setText(label.toString());
			lblMappingObject.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 2, 1));
		}

		if (methodInvocation.isReturnList()) {
			final var gridTestObjects = new TestDataObjectGridPanel(panReturnValue, testModule, testCase, (MappingObject) returnType,
					methodInvocation, true, true);

			testDataObjectGridPanels.add(gridTestObjects);

			gridTestObjects.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
			gridTestObjects.setData(methodInvocation.getReturnValues());
		}
		else {
			final TestDataObject testDataObject = methodInvocation.getReturnValues().getFirst();
			final boolean addTabFolder = isAttributeTabFolderRequired(testDataObject);

			for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
				// Skip attributes that are not relevant for the return value. If an attribute is mapped to a file a remote interface
				// won't return the respective content. Rather, it contains the path of the file on the remote system and usually
				// it doesn't make sense to evaluate this value.
				if (attribute.isMappedToFile() || attribute.isTrackValue() || (addTabFolder && isAddAttributeToTabFolder(attribute)))
					continue;

				final var label = new StringBuilder();

				if (attribute.getMappingAttribute() == null) {
					label.append("Return value ");
					label.append(attribute.getJavaType().getName());
				}
				else
					label.append(attribute.getLabel());

				final var lblAttribute = new Label(panReturnValue, SWT.NONE);
				lblAttribute.setText(label.toString());

				if (attribute.isMandatory())
					lblAttribute.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

				final TestDataAttributePanelData initializationData = new TestDataAttributePanelData(panReturnValue, testModule, testCase,
						methodInvocation, attribute, enableAssertionOperators, true);

				final AbstractTestDataAttributePanel panAttribute = TestDataAttributePanelFactory.initPanel(initializationData);
				panAttribute.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

				testDataAttributePanels.add(panAttribute);
			}

			if (addTabFolder) {
				final var tabFolder = new TabFolder(panReturnValue, SWT.NONE);
				tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));

				// Add all attributes that need a dedicated tab
				for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
					if (!isAddAttributeToTabFolder(attribute))
						continue;

					final var tabItemAttribute = new TabItem(tabFolder, SWT.NONE);
					tabItemAttribute.setText(attribute.getLabel());

					final TestDataAttributePanelData initializationData = new TestDataAttributePanelData(tabFolder, testModule, testCase,
							methodInvocation, attribute, enableAssertionOperators, true);

					final AbstractTestDataAttributePanel panAttribute = TestDataAttributePanelFactory.initPanel(initializationData);
					panAttribute.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

					testDataAttributePanels.add(panAttribute);
					tabItemAttribute.setControl(panAttribute);
				}
			}
		}

		panReturnValue.pack();

		scrolledComposite.setContent(panReturnValue);
		scrolledComposite.setMinSize(panReturnValue.computeSize(SWT.DEFAULT, SWT.DEFAULT));
	}

}
