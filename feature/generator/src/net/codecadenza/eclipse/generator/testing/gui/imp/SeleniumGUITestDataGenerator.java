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
package net.codecadenza.eclipse.generator.testing.gui.imp;

import static net.codecadenza.eclipse.shared.Constants.LOGIN_PAGE_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_PAGE_OBJECT;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.util.regex.Pattern;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for Selenium test data files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumGUITestDataGenerator {
	private static final String NEW_LINE_TOKEN = "!NEW_LINE!";
	private static final Pattern NEW_LINE_PATTERN = Pattern.compile("\r\n|\r|\n");

	private final GUITestCase testCase;
	private final Project project;

	/**
	 * Constructor
	 * @param testCase
	 */
	public SeleniumGUITestDataGenerator(GUITestCase testCase) {
		this.testCase = testCase;
		this.project = testCase.getNamespace().getProject();
	}

	/**
	 * Create or update the test data XML file for a given test case
	 * @throws Exception if the generation of the test data file has failed
	 */
	protected void createFile() throws Exception {
		final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";
		final BuildArtifactType artifactType = testCase.getTestModule().getArtifactType();
		final var dataFile = new WorkspaceFile(project, artifactType, path, createContent());

		EclipseIDEService.createOrUpdateFile(dataFile);
	}

	/**
	 * Create the test data XML file
	 * @return the generated content
	 * @throws IllegalStateException if a test action's form panel could not be found
	 */
	private String createContent() {
		final var b = new StringBuilder();
		final AbstractTestModule testModule = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);
		boolean isFirstAction = true;

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<test_case>\n");

		for (final GUITestAction testAction : testCase.getTestActions()) {
			if (!testAction.needsTestData())
				continue;

			if (testAction.getType() == GUITestActionType.OPEN_LOGIN_PAGE) {
				b.append(addLoginTestData(testAction));

				if (isFirstAction)
					isFirstAction = false;
				else
					b.append("\n");

				continue;
			}

			if (testAction.getForm() == null)
				continue;

			final var pageClassName = testModule.getNamespace().toString() + PACK_PAGE_OBJECT + "." + testAction.getForm().getName();
			final var objectId = testAction.getTestData().stream().filter(testData -> testData.getType() == GUITestDataType.OBJECT_ID)
					.findFirst().map(GUITestData::getFilterValue).orElse("");

			if (isFirstAction)
				isFirstAction = false;
			else
				b.append("\n");

			if (testAction.getComment() != null && !testAction.getComment().isEmpty())
				b.append("\t<!-- " + testAction.getComment() + " -->\n");

			// Create an entry for every test action
			b.append("\t<test_action pageClassName=\"" + pageClassName + "\"");

			if (!objectId.isEmpty())
				b.append(" objectId=\"" + escapeXMLCharacters(objectId) + "\"");

			if (testAction.getType() == GUITestActionType.OPEN_PAGE_BY_NAVIGATOR) {
				final String navigationTarget;

				if (project.hasAngularClient())
					navigationTarget = "/" + testAction.getForm().getDomainObject().getName().toLowerCase() + "/"
							+ testAction.getForm().getName().toLowerCase();
				else if (project.hasJSFClient())
					navigationTarget = "/" + project.getCode() + testAction.getForm().getResourcePath();
				else
					navigationTarget = testAction.getForm().getTitle();

				b.append(" navigationTarget=\"" + escapeXMLCharacters(navigationTarget) + "\"");
			}

			b.append(">\n");

			// Create entries for test data
			for (final GUITestData testData : testAction.getTestData()) {
				if (testData.getType() == GUITestDataType.OBJECT_ID)
					continue;

				final String newValue = testData.getNewValue();
				final String expectedValue = testData.getExpectedValue();
				final String filterValue = testData.getFilterValue();

				// Skip the entry if the test data object provides no value
				if (newValue == null && expectedValue == null && filterValue == null)
					continue;

				b.append("\t\t<element id=\"");

				if (testData.getType() == GUITestDataType.FORM_FIELD && testAction.getForm() != null) {
					if (testAction.getFormAction() != null) {
						if (testAction.getFormAction().getType() == ActionType.DIRECT_UPLOAD
								|| testAction.getFormAction().getType() == ActionType.INDIRECT_UPLOAD)
							b.append(escapeXMLCharacters(SeleniumGeneratorUtil.getUploadButtonId(testAction.getFormAction())));
						else if (testAction.getFormAction().getType() == ActionType.DOWNLOAD)
							b.append(escapeXMLCharacters(SeleniumGeneratorUtil.getDownloadButtonId(testAction.getFormAction())));
						else if (testAction.getFormAction().getType() == ActionType.UPLOAD_IMPORT) {
							if (testAction.getFormPanel() != null) {
								// Determine the form panel
								final FormPanel formPanel = testAction.getForm().getFormPanels().stream()
										.filter(e -> testAction.getFormPanel().equals(e.getBasePanel())).findFirst().orElse(null);

								if (formPanel == null)
									throw new IllegalStateException(
											"The form panel for the test action '" + testAction.getComment() + "' could not be found!");

								b.append(escapeXMLCharacters(SeleniumGeneratorUtil.getImportDialogId(formPanel, testAction.getFormAction())));
							}
							else
								b.append(escapeXMLCharacters(SeleniumGeneratorUtil.getImportDialogId(testAction.getFormAction())));
						}
					}

					if (testData.getFormField() != null)
						b.append(escapeXMLCharacters(SeleniumGeneratorUtil.getFieldId(testAction.getForm(), testData.getFormField())));
				}
				else if (testData.getTableColumnField() != null) {
					if (testData.getType() == GUITestDataType.SEARCH_FILTER)
						b.append(escapeXMLCharacters(
								SeleniumGeneratorUtil.getSearchFilterId(testAction.getForm(), testData.getTableColumnField())));
					else if (testData.getType() == GUITestDataType.SEARCH_OPERATOR)
						b.append(escapeXMLCharacters(
								SeleniumGeneratorUtil.getSearchOperatorId(testAction.getForm(), testData.getTableColumnField())));
					else if (testData.getType() == GUITestDataType.SEARCH_SORT_ORDER)
						b.append(escapeXMLCharacters(
								SeleniumGeneratorUtil.getSearchSortOrderId(testAction.getForm(), testData.getTableColumnField())));
				}
				else if (testData.getType() == GUITestDataType.PAGE_TITLE)
					b.append(GUITestDataType.PAGE_TITLE.name());
				else if (testData.getType() == GUITestDataType.ROW_COUNT)
					b.append(GUITestDataType.ROW_COUNT.name());
				else if (testData.getType() == GUITestDataType.CELL_VALUE)
					b.append(GUITestDataType.CELL_VALUE.name());
				else if (testData.getType() == GUITestDataType.ROW_INDEX)
					b.append(GUITestDataType.ROW_INDEX.name());

				b.append("\"");

				if (filterValue != null)
					b.append(" filterValue=\"" + escapeXMLCharacters(filterValue) + "\"");

				if (newValue != null)
					b.append(" newValue=\"" + escapeXMLCharacters(newValue) + "\"");

				if (expectedValue != null)
					b.append(" expectedValue=\"" + escapeXMLCharacters(expectedValue) + "\"");

				b.append("/>\n");
			}

			b.append(addActionResult(testAction));
			b.append("\t</test_action>\n");
		}

		b.append("</test_case>\n");

		return b.toString();
	}

	/**
	 * Create the XML test data fragment for a login action
	 * @param testAction
	 * @return the XML test data fragment. An empty string is returned if the test action provides inappropriate data!
	 */
	private String addLoginTestData(GUITestAction testAction) {
		final var b = new StringBuilder();

		if (testAction.getTestData().size() != 2)
			return b.toString();

		final var pageClassName = testAction.getTestCase().getNamespace().toString() + PACK_PAGE_OBJECT + "." + LOGIN_PAGE_NAME;
		final GUITestData userNameTestData = testAction.getTestData().get(0);
		final GUITestData passwordTestData = testAction.getTestData().get(1);
		final var formPrefix = project.hasJSFClient() ? SeleniumGeneratorUtil.FORM_PREFIX : "";

		b.append("\t<test_action pageClassName=\"" + pageClassName + "\">\n");
		b.append("\t\t<element id=\"" + formPrefix + "txtUserName\" newValue=\"");
		b.append(escapeXMLCharacters(userNameTestData.getNewValue()) + "\"/>\n");
		b.append("\t\t<element id=\"" + formPrefix + "txtPassword\" newValue=\"");
		b.append(escapeXMLCharacters(passwordTestData.getNewValue()) + "\"/>\n");
		b.append(addActionResult(testAction));
		b.append("\t</test_action>\n");

		return b.toString();
	}

	/**
	 * Add a XML test data fragment for a GUI test action result
	 * @param testAction
	 * @return the generated XML test data fragment. An empty string is returned if the given test action doesn't provide a result!
	 */
	private String addActionResult(GUITestAction testAction) {
		final var b = new StringBuilder();

		if (testAction.getActionResult() != null) {
			final String status = testAction.getActionResult().getStatus().name();
			final String message = testAction.getActionResult().getMessageText();

			b.append("\t\t<result status=\"" + status + "\" message=\"" + escapeXMLCharacters(message) + "\"></result>\n");
		}

		return b.toString();
	}

	/**
	 * Escape XML characters
	 * @param input
	 * @return the converted string
	 */
	private String escapeXMLCharacters(String input) {
		final var b = new StringBuilder();

		if (input == null)
			return b.toString();

		for (int i = 0; i < input.length(); i++) {
			final char c = input.charAt(i);

			switch (c) {
				case '<':
					b.append("&lt;");
					break;
				case '>':
					b.append("&gt;");
					break;
				case '\"':
					b.append("&quot;");
					break;
				case '&':
					b.append("&amp;");
					break;
				case '\'':
					b.append("&apos;");
					break;
				default:
					if (c > 0x7e)
						b.append("&#" + ((int) c) + ";");
					else
						b.append(c);
			}
		}

		// Replace all new-line characters by predefined tokens and return the converted result!
		return NEW_LINE_PATTERN.matcher(b.toString()).replaceAll(NEW_LINE_TOKEN);
	}

}
