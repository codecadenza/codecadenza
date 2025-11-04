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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.shared.Constants;
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
 * Panel for entering test data for the method parameters
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ParametersPanel extends AbstractMethodPanel {
	/**
	 * Create the panel
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param methodInvocation
	 */
	public ParametersPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation) {
		super(parent, testModule, testCase, methodInvocation, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.AbstractMethodPanel#initPanel()
	 */
	@Override
	protected void initPanel() {
		setLayout(new FillLayout());

		final ScrolledComposite scrolledComposite = createScrolledComposite(this);

		final var panParameters = new Composite(scrolledComposite, SWT.NONE);
		panParameters.setLayout(new GridLayout(2, false));

		final List<MethodInvocationParameter> sortedParameters = new ArrayList<>(methodInvocation.getParameters());
		sortedParameters.sort(Comparator.comparingInt(param -> param.getType() == null ? 1 : 0));

		for (final MethodInvocationParameter param : sortedParameters) {
			final JavaType type = param.getType();

			// Use the SearchInput type for parameters that have no type!
			if (type == null) {
				final var lblSearchInputParam = new Label(panParameters, SWT.NONE);
				lblSearchInputParam.setText("Fields of parameter " + Constants.INTEGRATION_SEARCH_PARAM_TYPE + " " + param.getName());
				lblSearchInputParam.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 2, 1));
				lblSearchInputParam.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

				final MethodInvocationParameter parameter = methodInvocation.getParameters().getFirst();
				final TestDataObject searchInputObject = parameter.getParameterValues().getFirst();

				final var searchInputPanel = new SearchInputPanel(panParameters, testModule, testCase, methodInvocation,
						searchInputObject);
				searchInputPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));

				continue;
			}

			if (type instanceof final MappingObject mappingObject) {
				final var label = new StringBuilder();

				if (param.isRepresentsList())
					label.append("Objects of parameter ");
				else
					label.append("Fields of parameter ");

				label.append(mappingObject.getName());
				label.append(" ");
				label.append(param.getName());

				final var lblMappingObject = new Label(panParameters, SWT.NONE);
				lblMappingObject.setText(label.toString());
				lblMappingObject.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 2, 1));
			}

			if (param.isRepresentsList()) {
				final var gridTestObjects = new TestDataObjectGridPanel(panParameters, testModule, testCase, (MappingObject) type,
						methodInvocation, false, false);

				testDataObjectGridPanels.add(gridTestObjects);

				gridTestObjects.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
				gridTestObjects.setData(methodInvocation.getReturnValues());
			}
			else {
				final TestDataObject testDataObject = param.getParameterValues().getFirst();
				final boolean addTabFolder = isAttributeTabFolderRequired(testDataObject);

				for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
					if (attribute.isTrackValue() || (addTabFolder && isAddAttributeToTabFolder(attribute)))
						continue;

					final var label = new StringBuilder();

					if (attribute.getMappingAttribute() == null) {
						label.append("Parameter ");
						label.append(param.getType().getName());
						label.append(" ");
						label.append(param.getName());
					}
					else
						label.append(attribute.getLabel());

					final var lblAttribute = new Label(panParameters, SWT.NONE);
					lblAttribute.setText(label.toString());

					if (attribute.isMandatory())
						lblAttribute.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

					final TestDataAttributePanelData initializationData = new TestDataAttributePanelData(panParameters, testModule,
							testCase, methodInvocation, attribute, false, false);

					final AbstractTestDataAttributePanel panAttribute = TestDataAttributePanelFactory.initPanel(initializationData);
					panAttribute.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

					testDataAttributePanels.add(panAttribute);
				}

				if (addTabFolder) {
					final var tabFolder = new TabFolder(panParameters, SWT.NONE);
					tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));

					// Add all attributes that need a dedicated tab
					for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
						if (!isAddAttributeToTabFolder(attribute))
							continue;

						final var tabItemAttribute = new TabItem(tabFolder, SWT.NONE);
						tabItemAttribute.setText(attribute.getLabel());

						final TestDataAttributePanelData initializationData = new TestDataAttributePanelData(tabFolder, testModule, testCase,
								methodInvocation, attribute, false, false);

						final AbstractTestDataAttributePanel panAttribute = TestDataAttributePanelFactory.initPanel(initializationData);
						panAttribute.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

						testDataAttributePanels.add(panAttribute);
						tabItemAttribute.setControl(panAttribute);
					}
				}
			}
		}

		panParameters.pack();

		scrolledComposite.setContent(panParameters);
		scrolledComposite.setMinSize(panParameters.computeSize(SWT.DEFAULT, SWT.DEFAULT));
	}

	/**
	 * Generate random values
	 */
	public void generateRandomValues() {
		testDataAttributePanels.forEach(AbstractTestDataAttributePanel::onRequestRandomValue);
	}

}
