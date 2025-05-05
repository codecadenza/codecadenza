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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import net.codecadenza.eclipse.generator.testing.gui.imp.SeleniumGeneratorUtil;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;

/**
 * <p>
 * Generator for test actions that either enter or validate form field data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnterDataSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	private static final int ROW_INDEX_TOP = 1;
	private static final int ROW_INDEX_BOTTOM = 2;
	private String currentTabPageId = "";

	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public EnterDataSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
		super(testAction, project, declarations);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.testing.gui.action.IGUITestActionGenerator#createTestAction()
	 */
	@Override
	public String createTestAction() {
		final var b = new StringBuilder();
		b.append(addComment());
		b.append(addTestDataInit());

		// Search for the initially visible panel of the first row after opening the page
		Optional<FormPanel> initialPanel = form.getFormPanels().stream().filter(e -> e.getRowIndex() == ROW_INDEX_TOP)
				.min((x, y) -> x.getRowIndex() - y.getRowIndex());

		if (initialPanel.isPresent()) {
			currentTabPageId = SeleniumGeneratorUtil.getTabPageName(initialPanel.get());

			b.append(setTestDataForFieldsOfRow(ROW_INDEX_TOP));
		}

		// Search for the initially visible panel of the second row after opening the page
		initialPanel = form.getFormPanels().stream().filter(e -> e.getRowIndex() == ROW_INDEX_BOTTOM)
				.min((x, y) -> x.getRowIndex() - y.getRowIndex());

		if (initialPanel.isPresent()) {
			currentTabPageId = SeleniumGeneratorUtil.getTabPageName(initialPanel.get());

			b.append(setTestDataForFieldsOfRow(ROW_INDEX_BOTTOM));
		}

		return b.toString();
	}

	/**
	 * Either enter or validate data for all visible form fields of a given row
	 * @param rowIndex
	 * @return the generated content
	 */
	private String setTestDataForFieldsOfRow(int rowIndex) {
		final var b = new StringBuilder();
		final Map<String, List<FormField>> fieldMap = getFormFieldsByTab(rowIndex);

		// Always validate fields before entering new data!
		for (final GUITestData testData : testAction.getTestData()) {
			final FormField formField = testData.getFormField();

			if (formField == null || formField.isHidden() || formField.getPanel().getRowIndex() != rowIndex)
				continue;

			final var fieldGetter = "get" + formField.getName().substring(0, 1).toUpperCase() + formField.getName().substring(1);
			final FormFieldTypeEnumeration fieldType = formField.getFieldType();

			if (testData.getExpectedValue() != null) {
				// Check if the field is placed on a different tab page
				b.append(openTabPage(formField, fieldMap));

				b.append(form.getLowerCaseName() + ".");

				if (fieldType == FormFieldTypeEnumeration.SIMPLE_TEXT || fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
					b.append("validateInputFieldValue");
				else if (fieldType == FormFieldTypeEnumeration.DATE || fieldType == FormFieldTypeEnumeration.DATE_TIME) {
					if (project.hasAngularClient() || (project.hasVaadinClient() && !formField.isReadonly()))
						b.append("validateDateFieldValue");
					else
						b.append("validateInputFieldValue");
				}
				else if (fieldType == FormFieldTypeEnumeration.ENUM_COMBOBOX) {
					if (formField.isReadonly() && !project.hasAngularClient())
						b.append("validateInputFieldValue");
					else
						b.append("validateComboboxItem");
				}
				else if (fieldType == FormFieldTypeEnumeration.COMBOBOX)
					b.append("validateComboboxItem");
				else if (fieldType == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
					b.append("validateInputFieldValue");
				else if (fieldType == FormFieldTypeEnumeration.PROPOSAL_TEXT)
					b.append("validateAutoCompleteItem");
				else if (fieldType == FormFieldTypeEnumeration.LOV)
					b.append("validateLoVItem");
				else if (fieldType == FormFieldTypeEnumeration.CHECKBOX)
					b.append("validateCheckBoxValue");
				else if (fieldType == FormFieldTypeEnumeration.LIST || fieldType == FormFieldTypeEnumeration.SEARCHABLE_LIST)
					b.append(fieldGetter + "().validateSelection");
				else if (fieldType == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
					b.append(fieldGetter + "().validateElements");
				else if (fieldType == FormFieldTypeEnumeration.FORM_LINK && !project.hasJSFClient())
					b.append("validateInternalLinkText");
				else if (fieldType == FormFieldTypeEnumeration.MAIL_LINK && project.hasAngularClient())
					b.append("validateMailLinkText");
				else if (fieldType == FormFieldTypeEnumeration.WEB_LINK && project.hasAngularClient())
					b.append("validateWebLinkText");
				else if (project.hasAngularClient())
					b.append("validateInputFieldValue");
				else
					b.append("validateLabelText");

				b.append("(testData.getElementTestDataById(");
				b.append(form.getName() + ".FIELD_ID_" + testData.getFormField().getName().toUpperCase() + "));\n");
			}
		}

		for (final GUITestData testData : testAction.getTestData()) {
			final FormField formField = testData.getFormField();

			if (formField == null || formField.isHidden() || formField.getPanel().getRowIndex() != rowIndex)
				continue;

			final boolean enterData = testAction.getType() == GUITestActionType.ENTER_FORM_DATA && testData.getNewValue() != null;
			final FormFieldTypeEnumeration fieldType = formField.getFieldType();
			final var fieldGetter = "get" + formField.getName().substring(0, 1).toUpperCase() + formField.getName().substring(1) + "()";

			if (enterData) {
				// Check if the field is placed on a different tab page
				b.append(openTabPage(formField, fieldMap));

				if (fieldType == FormFieldTypeEnumeration.LIST || fieldType == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
					if (fieldType == FormFieldTypeEnumeration.SEARCHABLE_LIST)
						b.append(performLookup(testData));

					if (form.getFormType() == FormTypeEnumeration.UPDATE) {
						// Remove all selected items from a list component before adding new ones
						b.append(form.getLowerCaseName() + "." + fieldGetter + ".removeAllSelectedItems();\n");
					}
				}

				b.append(form.getLowerCaseName() + ".");

				if (fieldType == FormFieldTypeEnumeration.SIMPLE_TEXT)
					b.append("setInputFieldValue");
				else if (fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
					if (project.hasVaadinClient())
						b.append("setMultiLineFieldValue");
					else
						b.append("setInputFieldValue");
				}
				else if (fieldType == FormFieldTypeEnumeration.DATE || fieldType == FormFieldTypeEnumeration.DATE_TIME)
					b.append("setDateFieldValue");
				else if (fieldType == FormFieldTypeEnumeration.COMBOBOX || fieldType == FormFieldTypeEnumeration.ENUM_COMBOBOX)
					b.append("selectComboboxItem");
				else if (fieldType == FormFieldTypeEnumeration.PROPOSAL_TEXT)
					b.append("selectAutoCompleteItem");
				else if (fieldType == FormFieldTypeEnumeration.LOV)
					b.append("selectLoVItem");
				else if (fieldType == FormFieldTypeEnumeration.CHECKBOX)
					b.append("setCheckBoxValue");
				else if (fieldType == FormFieldTypeEnumeration.LIST || fieldType == FormFieldTypeEnumeration.SEARCHABLE_LIST)
					b.append(fieldGetter + ".selectItems");
				else if (fieldType == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
					b.append(fieldGetter + ".addElements");

				b.append("(testData.getElementTestDataById(");
				b.append(form.getName() + ".FIELD_ID_" + testData.getFormField().getName().toUpperCase() + ")");

				if (fieldType == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR) {
					if (form.getFormType() == FormTypeEnumeration.UPDATE)
						b.append(", true");
					else
						b.append(", false");
				}

				b.append(");\n");
			}
		}

		return b.toString();
	}

	/**
	 * Check if it's necessary that another tab page must be opened before accessing the next field
	 * @param formField
	 * @param fieldMap
	 * @return the generated content
	 */
	private String openTabPage(FormField formField, Map<String, List<FormField>> fieldMap) {
		final var b = new StringBuilder();
		var tabPageName = "";

		for (final Map.Entry<String, List<FormField>> entry : fieldMap.entrySet()) {
			final long count = entry.getValue().stream().filter(e -> e.equals(formField)).count();

			if (count > 0) {
				tabPageName = entry.getKey();
				break;
			}
		}

		// Check if the next tab page must be opened
		if (!currentTabPageId.equals(tabPageName)) {
			b.append(form.getLowerCaseName() + ".openTabPage(" + form.getName() + "." + tabPageName + ");\n");
			currentTabPageId = tabPageName;
		}

		return b.toString();
	}

	/**
	 * @param rowIndex
	 * @return a map containing all tab page IDs as the key and a list with respective form fields as the value
	 */
	private Map<String, List<FormField>> getFormFieldsByTab(int rowIndex) {
		final var fieldMap = new HashMap<String, List<FormField>>();

		for (final GUITestData testData : testAction.getTestData()) {
			final FormField formField = testData.getFormField();

			if (formField == null || formField.isHidden())
				continue;

			for (final FormPanel panel : form.getFormPanels())
				if (panel.getRowIndex() == rowIndex && formField.getPanel().equals(panel)) {
					final String tabPageName = SeleniumGeneratorUtil.getTabPageName(panel);

					if (!fieldMap.containsKey(tabPageName)) {
						final var fieldList = new ArrayList<FormField>();
						fieldList.add(formField);

						fieldMap.put(tabPageName, fieldList);
					}
					else
						fieldMap.get(tabPageName).add(formField);

					break;
				}
		}

		return fieldMap;
	}

	/**
	 * Create an item lookup operation for a searchable field
	 * @param testData
	 * @return the generated content
	 */
	private String performLookup(GUITestData testData) {
		final var b = new StringBuilder();
		final FormField formField = testData.getFormField();

		if (testData.getFilterValue() == null || testData.getFilterValue().isEmpty())
			return b.toString();

		final var fieldGetter = "get" + formField.getName().substring(0, 1).toUpperCase() + formField.getName().substring(1) + "()";

		b.append(form.getLowerCaseName() + "." + fieldGetter + ".searchItems");
		b.append("(testData.getElementTestDataById(");
		b.append(form.getName() + ".FIELD_ID_" + testData.getFormField().getName().toUpperCase() + "));\n");

		return b.toString();
	}

}
