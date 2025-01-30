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
package net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium;

import static net.codecadenza.eclipse.shared.Constants.LOGIN_PAGE_NAME;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.generator.testing.gui.action.IGUITestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.imp.SeleniumGeneratorUtil;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;

/**
 * <p>
 * Abstract base class for Selenium test action generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSeleniumTestActionGenerator implements IGUITestActionGenerator {
	protected Set<String> declarations = new HashSet<>();
	protected GUITestAction testAction;
	protected Form form;
	protected String pageObjectDeclaration;
	protected Project project;

	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	protected AbstractSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
		this.declarations = declarations;
		this.testAction = testAction;
		this.project = project;
		this.form = testAction.getForm();

		if (form != null)
			this.pageObjectDeclaration = form.getName() + " " + form.getLowerCaseName();
	}

	/**
	 * @return a comment for this test action
	 */
	protected String addComment() {
		if (testAction.getComment() != null && !testAction.getComment().isEmpty())
			return "// " + testAction.getComment() + "\n";

		return "";
	}

	/**
	 * @return the initialization of the test data object
	 */
	protected String addTestDataInit() {
		final var testDataDeclaration = "PageActionTestData testData";
		final var b = new StringBuilder();

		if (form == null && testAction.getType() != GUITestActionType.OPEN_LOGIN_PAGE)
			return b.toString();

		if (!declarations.contains(testDataDeclaration)) {
			b.append(testDataDeclaration);

			declarations.add(testDataDeclaration);
		}
		else
			b.append("testData");

		final String formName = form != null ? form.getName() : LOGIN_PAGE_NAME;

		b.append(" = testContext.getDataProvider().getNextTestDataForPage(" + formName + ".class);\n");

		return b.toString();
	}

	/**
	 * @return the page object declaration
	 */
	protected String addPageObjectDeclaration() {
		final var b = new StringBuilder();

		if (form == null)
			return b.toString();

		if (!declarations.contains(pageObjectDeclaration)) {
			b.append("var ");

			declarations.add(pageObjectDeclaration);
		}

		b.append(form.getLowerCaseName());

		return b.toString();
	}

	/**
	 * Create the code fragment that is responsible for the validation of a page title
	 * @param form
	 * @return the generated content or an empty string if no page title validation should be performed
	 */
	protected String addPageTitleValidation(Form form) {
		final var b = new StringBuilder();

		if (form == null)
			return b.toString();

		final GUITestData testData = testAction.getTestDataByType(GUITestDataType.PAGE_TITLE);

		if (testData == null)
			return b.toString();

		b.append("\n// Validate if the page with the expected title has been opened\n");
		b.append(form.getLowerCaseName() + ".validatePageTitle(testData.getElementTestDataById(");
		b.append("PageElementTestData.PAGE_TITLE_ID));\n");

		return b.toString();
	}

	/**
	 * Create the code fragment for opening a tab page before using a grid panel
	 * @return the generated content or an empty string if a tab page doesn't have to be opened
	 * @throws IllegalStateException if the test action's form panel could not be found
	 */
	protected String openTabPageOfGridPanel() {
		if (testAction.getFormPanel() == null || testAction.getForm() == null)
			return "";

		final List<GUITestAction> testActions = testAction.getTestCase().getTestActions();

		// Collect all panels of this form
		final List<FormPanel> formPanels = testAction.getForm().getFormPanels().stream().toList();

		// Search for the form panel that uses the given grid panel
		final FormPanel formPanel = formPanels.stream().filter(e -> testAction.getFormPanel().equals(e.getBasePanel())).findFirst()
				.orElse(null);

		if (formPanel == null)
			throw new IllegalStateException("The form panel for the test action '" + testAction.getComment() + "' could not be found!");

		// Determine if the grid panel is placed in a tab page
		final long columnCount = formPanels.stream().filter(e -> e.getRowIndex() == formPanel.getRowIndex()).count();

		if (columnCount < 2)
			return "";

		final int currentActionIndex = testActions.indexOf(testAction);

		// Test if the tab page has been already opened in a previous action
		for (final GUITestAction action : testActions) {
			final int actionIndex = testActions.indexOf(action);

			if (actionIndex == (currentActionIndex - 1) && testAction.getFormPanel().equals(action.getFormPanel()))
				return "";
		}

		final String tabPageName = SeleniumGeneratorUtil.getTabPageName(formPanel);

		final var b = new StringBuilder();
		b.append("// Open tab page before using grid panel\n");
		b.append(form.getLowerCaseName() + ".openTabPage(" + form.getName() + "." + tabPageName + ");\n\n");

		return b.toString();
	}

	/**
	 * Create the code fragment that waits for a message dialog or a notification message
	 * @return the generated content
	 */
	protected String addWaitForFeedback() {
		final var b = new StringBuilder();

		if (testAction.getActionResult() == null)
			return b.toString();

		final GUITestActionResult actionResult = testAction.getActionResult();

		if (project.hasVaadinClient() || actionResult.getComponentType() == GUITestActionResultComponentType.DIALOG) {
			var componentDeclaration = "";

			b.append("\n// Wait for message dialog to be opened and validate its status and message text!\n");

			if (project.hasVaadinClient())
				componentDeclaration = "PopUpDialog dlg";
			else
				componentDeclaration = "MessageDialog dlg";

			if (!declarations.contains(componentDeclaration)) {
				b.append(componentDeclaration);

				declarations.add(componentDeclaration);
			}
			else
				b.append("dlg");

			b.append(" = ");
		}
		else
			b.append("\n// Wait for notification message and validate its status and message text!\n");

		if (form != null)
			b.append(form.getLowerCaseName());
		else
			b.append("loginPage");

		if (project.hasVaadinClient() || actionResult.getComponentType() == GUITestActionResultComponentType.DIALOG) {
			b.append(".waitForMessageDialog(testData.getResult());\n");
			b.append("dlg.closeDialog();\n");
		}
		else
			b.append(".waitForNotificationMessage(testData.getResult());\n");

		return b.toString();
	}

}
