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
import java.util.List;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Abstract base class for entering test data of a given integration method
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractMethodPanel extends Composite {
	protected final IntegrationTestModule testModule;
	protected final IntegrationTestCase testCase;
	protected final IntegrationMethodTestInvocation methodInvocation;
	protected final AbstractIntegrationMethod integrationMethod;
	protected final List<AbstractTestDataAttributePanel> testDataAttributePanels = new ArrayList<>();
	protected final List<TestDataObjectGridPanel> testDataObjectGridPanels = new ArrayList<>();
	protected final boolean enableAssertionOperators;

	/**
	 * Create the panel
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param methodInvocation
	 * @param enableAssertionOperators
	 */
	protected AbstractMethodPanel(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation, boolean enableAssertionOperators) {
		super(parent, SWT.NONE);

		this.testModule = testModule;
		this.testCase = testCase;
		this.methodInvocation = methodInvocation;
		this.integrationMethod = methodInvocation.getIntegrationMethod();
		this.enableAssertionOperators = enableAssertionOperators;

		initPanel();
	}

	/**
	 * Initialize the panel
	 */
	protected abstract void initPanel();

	/**
	 * Create a {@link ScrolledComposite}
	 * @param parent
	 * @return a new {@link ScrolledComposite}
	 */
	public static ScrolledComposite createScrolledComposite(Composite parent) {
		final var scrolledComposite = new ScrolledComposite(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		scrolledComposite.setLayout(new FillLayout());
		scrolledComposite.setExpandHorizontal(true);
		scrolledComposite.setExpandVertical(true);

		return scrolledComposite;
	}

	/**
	 * Validate the user input
	 */
	public void validateAndApplyInput() {
		for (final AbstractTestDataAttributePanel panDataAttribute : testDataAttributePanels)
			panDataAttribute.validateAndApplyInput();

		for (final TestDataObjectGridPanel testDataObjectGridPanel : testDataObjectGridPanels)
			testDataObjectGridPanel.validateAndApplyInput();
	}

	/**
	 * @param testDataObject
	 * @return true if a tab folder should be added for specific attributes that require more space
	 */
	public static boolean isAttributeTabFolderRequired(TestDataObject testDataObject) {
		return testDataObject.getAttributes().stream().anyMatch(AbstractMethodPanel::isAddAttributeToTabFolder);
	}

	/**
	 * @param testDataAttribute
	 * @return true if the given test data attribute should be added to a dedicated tab folder item
	 */
	public static boolean isAddAttributeToTabFolder(TestDataAttribute testDataAttribute) {
		return testDataAttribute.isMappedToList() || testDataAttribute.isMappedToElementCollection();
	}

}
