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

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.BasicEList;

/**
 * <p>
 * Generator for Selenium page objects that represent forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumFormPageObjectGenerator extends AbstractJavaSourceGenerator {
	private static final String FIELD_ID_PREFIX = "FIELD_ID_";
	private static final String GRID_PANEL_PREFIX = "gridPanel";
	private static final String RESOURCE_PATH = "RESOURCE_PATH";
	private static final String LIST_TYPE = "DualDataListComponent";
	private static final String ELEMENT_COLLECTION_EDITOR_TYPE = "ElementCollectionEditorComponent";

	private final Form form;
	private final Project project;
	private final FormTypeEnumeration formType;

	/**
	 * Constructor
	 * @param form
	 */
	public SeleniumFormPageObjectGenerator(Form form) {
		super(form.getPageObjectSourceFile());

		this.form = form;
		this.project = form.getDomainObject().getNamespace().getProject();
		this.formType = form.getFormType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("net.codecadenza.runtime.selenium.junit");

		if (project.hasAngularClient())
			importPackage("net.codecadenza.runtime.selenium.page.imp.angular");
		else if (project.hasJSFClient())
			importPackage("net.codecadenza.runtime.selenium.page.imp.primefaces");
		else
			importPackage("net.codecadenza.runtime.selenium.page.imp.vaadin");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + form.getName() + " extends ");

		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW)
			b.append("AbstractSearchPageObject");
		else if (formType == FormTypeEnumeration.SIMPLE_VIEW)
			b.append("AbstractViewPageObject");
		else
			b.append("AbstractPageObject");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final var resourcePath = "\"" + form.getResourcePath() + "\"";

		// Create the constant that contains the resource path
		addPublicConstant(JavaType.STRING, RESOURCE_PATH, resourcePath).create();

		if (formType != FormTypeEnumeration.SEARCHABLE_VIEW && formType != FormTypeEnumeration.SIMPLE_VIEW) {
			// Create the field ID constants for all visible fields of a single-record form
			for (final FormField field : form.getAllFormFields()) {
				if (field.isHidden())
					continue;

				final String fieldName = FIELD_ID_PREFIX + field.getName().toUpperCase();
				final var fieldId = "\"" + SeleniumGeneratorUtil.getFieldId(form, field) + "\"";

				addPublicConstant(JavaType.STRING, fieldName, fieldId).create();
			}

			// Add ID constants for all tab pages
			final Map<String, String> tabPageIds = getTabPageIds(true);
			tabPageIds.putAll(getTabPageIds(false));

			tabPageIds.keySet().forEach(tabPageId -> {
				final String fieldName = tabPageIds.get(tabPageId);
				final var fieldValue = "\"" + tabPageId + "\"";

				addPublicConstant(JavaType.STRING, fieldName, fieldValue).create();
			});

			// Create fields for all list and element collection editor fields of a single-record form
			for (final FormField field : form.getAllFormFields()) {
				if (field.isHidden())
					continue;

				if (field.getFieldType() == FormFieldTypeEnumeration.LIST
						|| field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST)
					addPrivateField(LIST_TYPE, field.getName()).withFinalModifier().create();
				else if (field.getFieldType() == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
					addPrivateField(ELEMENT_COLLECTION_EDITOR_TYPE, field.getName()).withFinalModifier().create();
			}

			// Create fields for all grid panels
			for (final FormPanel panel : form.getFormPanels()) {
				if (panel.getBasePanel() == null)
					continue;

				final String typeName = panel.getBasePanel().getName();
				final String fieldName = GRID_PANEL_PREFIX + panel.getBasePanel().getAssociation().getUpperCaseName();

				addPrivateField(typeName, fieldName).withFinalModifier().create();

				for (final FormAction action : panel.getBasePanel().getActions())
					if (action.getType() == ActionType.UPLOAD_IMPORT) {
						final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

						if (!exchangeMethod.getMethodParameters().isEmpty()) {
							// Create a constant for the ID of the file selection dialog if the import operation expects a file
							final String dialogName = SeleniumGeneratorUtil.getImportDialogName(action);
							final var dialogId = "\"" + SeleniumGeneratorUtil.getImportDialogId(panel, action) + "\"";

							addPublicConstant(JavaType.STRING, dialogName, dialogId).create();
						}
					}
			}

			// Create constants for all file handling operations
			form.getActions().forEach(action -> {
				if (action.getType() == ActionType.DIRECT_UPLOAD || action.getType() == ActionType.INDIRECT_UPLOAD) {
					final String buttonName = SeleniumGeneratorUtil.getUploadButtonName(action);
					final var buttonId = "\"" + SeleniumGeneratorUtil.getUploadButtonId(action) + "\"";

					addPublicConstant(JavaType.STRING, buttonName, buttonId).create();

					if (project.hasJSFClient()) {
						final String dialogName = SeleniumGeneratorUtil.getUploadDialogName(action);
						final var dialogId = "\"" + SeleniumGeneratorUtil.getUploadDialogId(action) + "\"";

						addPublicConstant(JavaType.STRING, dialogName, dialogId).create();
					}
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					final String buttonName = SeleniumGeneratorUtil.getDownloadButtonName(action);
					final var buttonId = "\"" + SeleniumGeneratorUtil.getDownloadButtonId(action) + "\"";

					addPublicConstant(JavaType.STRING, buttonName, buttonId).create();
				}
			});
		}
		else {
			// Create constants for all data import operations
			form.getActions().stream().filter(action -> action.getType() == ActionType.UPLOAD_IMPORT).forEach(action -> {
				final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

				if (!exchangeMethod.getMethodParameters().isEmpty()) {
					// Create a constant for the ID of the file selection dialog if the import operation expects a file
					final String dialogName = SeleniumGeneratorUtil.getImportDialogName(action);
					final var dialogId = "\"" + SeleniumGeneratorUtil.getImportDialogId(action) + "\"";

					addPublicConstant(JavaType.STRING, dialogName, dialogId).create();
				}
			});
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final var methodSignature = form.getName() + "(SeleniumTestContext testContext)";
		boolean firstField = true;

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param testContext\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super(testContext);\n");

		// Initialize list and element collection editor fields of a single-record form
		for (final FormField field : form.getAllFormFields()) {
			if (field.isHidden() || (field.getFieldType() != FormFieldTypeEnumeration.LIST
					&& field.getFieldType() != FormFieldTypeEnumeration.SEARCHABLE_LIST
					&& field.getFieldType() != FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR))
				continue;

			final String fieldId = FIELD_ID_PREFIX + field.getName().toUpperCase();

			if (firstField) {
				b.append("\n");

				firstField = false;
			}

			if (field.getFieldType() == FormFieldTypeEnumeration.LIST
					|| field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
				b.append(field.getName() + " = new " + LIST_TYPE + "(this, " + fieldId + ", ");

				if (field.getFieldType() == FormFieldTypeEnumeration.LIST)
					b.append("false");
				else
					b.append("true");
			}
			else if (field.getFieldType() == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
				b.append(field.getName() + " = new " + ELEMENT_COLLECTION_EDITOR_TYPE + "(this, " + fieldId);

			b.append(");\n");
		}

		// Initialize grid panels
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() == null)
				continue;

			final String typeName = panel.getBasePanel().getName();
			final String fieldName = GRID_PANEL_PREFIX + panel.getBasePanel().getAssociation().getUpperCaseName();
			final String gridPanelId = SeleniumGeneratorUtil.getGridPanelId(panel);

			if (firstField) {
				b.append("\n");

				firstField = false;
			}

			b.append(fieldName + " = new " + typeName + "(testContext, \"" + gridPanelId + "\");\n");
		}

		b.append("}\n\n");

		addConstructor(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (formType != FormTypeEnumeration.SEARCHABLE_VIEW && formType != FormTypeEnumeration.SIMPLE_VIEW) {
			// Add getters for all list and element collection editor fields
			for (final FormField field : form.getAllFormFields()) {
				if (field.isHidden())
					continue;

				if (field.getFieldType() == FormFieldTypeEnumeration.LIST
						|| field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
					final var comment = "the " + field.getDTOAttribute().getReferencedDTOBean().getDomainObject().getLabel()
							+ " selection list field";

					addGetter(LIST_TYPE, field.getName(), comment);
				}
				else if (field.getFieldType() == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR) {
					final var comment = "the element collection editor for the " + field.getDTOAttribute().getDomainAttribute().getLabel();

					addGetter(ELEMENT_COLLECTION_EDITOR_TYPE, field.getName(), comment);
				}
			}

			// Add getters for all grid panels
			for (final FormPanel panel : form.getFormPanels()) {
				if (panel.getBasePanel() == null)
					continue;

				final String typeName = panel.getBasePanel().getName();
				final AbstractDomainAssociation assoc = panel.getBasePanel().getAssociation();
				final String fieldName = GRID_PANEL_PREFIX + assoc.getUpperCaseName();
				final var comment = "the grid panel that contains all " + assoc.getTarget().getLabelPlural() + " of this "
						+ form.getDomainObject().getLabel();

				addGetter(typeName, fieldName, comment);
			}
		}
	}

	/**
	 * @param firstTab
	 * @return a map containing the ID of a tab page as the key and the name of the corresponding constant as value
	 */
	private Map<String, String> getTabPageIds(boolean firstTab) {
		final var panelsOfRow = new BasicEList<FormPanel>();
		final var tabPageIds = new HashMap<String, String>();
		final int rowIndex = firstTab ? 1 : 2;

		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == rowIndex)
				panelsOfRow.add(panel);

		if (panelsOfRow.size() > 1)
			panelsOfRow.forEach(panel -> {
				final String tabPageId = SeleniumGeneratorUtil.getTabPageId(panel);
				final String tabPageName = SeleniumGeneratorUtil.getTabPageName(panel);

				tabPageIds.put(tabPageId, tabPageName);
			});

		return tabPageIds;
	}

}
