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

import static net.codecadenza.eclipse.shared.Constants.ACTION_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.BUTTON_PREFIX;

import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.BasicEList;

/**
 * <p>
 * Utility class for Selenium generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumGeneratorUtil {
	public static final String FORM_PREFIX = "form:";
	public static final String TAB_VIEW_PREFIX_1 = "tabview1:";
	public static final String TAB_VIEW_PREFIX_2 = "tabview2:";
	public static final String TAB_NAME_PREFIX = "TAB_PAGE_";
	public static final String TAB_ID_PREFIX = "tab";
	public static final String UPLOAD_BUTTON_PREFIX = "UPLOAD_BUTTON_";
	public static final String DOWNLOAD_BUTTON_PREFIX = "DOWNLOAD_BUTTON_";
	public static final String UPLOAD_SUFFIX = "Upload";
	public static final String DOWNLOAD_SUFFIX = "Download";
	public static final String DIALOG_NAME_PREFIX = "DIALOG_";
	public static final String DIALOG_PREFIX = "dlg";
	public static final String PROPOSAL_SUFFIX = "_input";
	public static final String DATE_SUFFIX = "_input";
	public static final String LOV_SUFFIX = "Disp";
	public static final String FILTER_INPUT_PREFIX = "fi_";
	public static final String FILTER_COMBO_PREFIX = "fc_";
	public static final String FILTER_DATE_PREFIX = "fd_";
	public static final String SORT_INPUT_PREFIX = "s_";
	public static final String OPERATOR_INPUT_PREFIX = "o_";
	public static final String JSF_SEARCH_TAB_ID = "tabViewSearchInput:";
	private static final Pattern ACTION_PREFIX_PATTERN = Pattern.compile(ACTION_PREFIX);

	/**
	 * Prevent instantiation
	 */
	private SeleniumGeneratorUtil() {

	}

	/**
	 * Create the ID of a form field in order to find it in the respective HTML form
	 * @param form
	 * @param field
	 * @return the ID of the given form field
	 */
	public static final String getFieldId(Form form, FormField field) {
		final Project project = form.getDomainObject().getNamespace().getProject();
		final var panelsOfFirstRow = new BasicEList<FormPanel>();
		final var panelsOfSecondRow = new BasicEList<FormPanel>();

		if (!project.hasJSFClient())
			return field.getName();

		String fieldId = FORM_PREFIX;

		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == 1)
				panelsOfFirstRow.add(panel);
			else
				panelsOfSecondRow.add(panel);

		long fieldCount = panelsOfFirstRow.stream().flatMap(e -> e.getFields().stream()).filter(e -> e.equals(field)).count();

		if (fieldCount > 0 && panelsOfFirstRow.size() > 1)
			fieldId += TAB_VIEW_PREFIX_1;

		fieldCount = panelsOfSecondRow.stream().flatMap(e -> e.getFields().stream()).filter(e -> e.equals(field)).count();

		if (fieldCount > 0 && panelsOfSecondRow.size() > 1)
			fieldId += TAB_VIEW_PREFIX_2;

		// Primefaces proposal text, date and list-of-values fields need a special suffix in order to handle them properly!
		if (field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT && !field.isReadonly())
			fieldId += field.getName() + PROPOSAL_SUFFIX;
		else if (field.getFieldType() == FormFieldTypeEnumeration.LOV) {
			final DTOBean listDTO = field.getDTOAttribute().getReferencedDTOBean();

			fieldId += field.getName();

			if (listDTO.getDisplayAttribute() != null)
				fieldId += LOV_SUFFIX;
		}
		else if ((field.getFieldType() == FormFieldTypeEnumeration.DATE || field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME)
				&& !field.isReadonly())
			fieldId += field.getName() + DATE_SUFFIX;
		else
			fieldId += field.getName();

		return fieldId;
	}

	/**
	 * Create the ID of a grid panel in order to find it in the respective HTML form
	 * @param gridPanel
	 * @return the ID of a grid panel
	 */
	public static final String getGridPanelId(FormPanel gridPanel) {
		return getPanelIdPrefix(gridPanel) + "grid" + gridPanel.getBasePanel().getName();
	}

	/**
	 * Create the ID prefix of a panel in order to find it in the respective HTML form
	 * @param panel
	 * @return the ID prefix of a panel
	 */
	public static final String getPanelIdPrefix(FormPanel panel) {
		final Form form = panel.getForm();
		final Project project = form.getDomainObject().getNamespace().getProject();
		var panelIdPrefix = "";

		if (project.hasJSFClient()) {
			final long colCount1 = form.getFormPanels().stream().filter(e -> e.getRowIndex() == 1).count();
			final long colCount2 = form.getFormPanels().stream().filter(e -> e.getRowIndex() == 2).count();

			panelIdPrefix += FORM_PREFIX;

			if (panel.getRowIndex() == 1 && colCount1 > 1)
				panelIdPrefix += TAB_VIEW_PREFIX_1;
			else if (panel.getRowIndex() == 2 && colCount2 > 1)
				panelIdPrefix += TAB_VIEW_PREFIX_2;
		}

		return panelIdPrefix;
	}

	/**
	 * @param formPanel
	 * @return the name of the constant for a given tab panel
	 */
	public static final String getTabPageName(FormPanel formPanel) {
		return TAB_NAME_PREFIX + formPanel.getName().toUpperCase();
	}

	/**
	 * @param formPanel
	 * @return the ID of a tab panel
	 */
	public static final String getTabPageId(FormPanel formPanel) {
		final String panelName = formPanel.getName().substring(0, 1).toUpperCase() + formPanel.getName().substring(1);

		return getPanelIdPrefix(formPanel) + TAB_ID_PREFIX + panelName;
	}

	/**
	 * @param formAction
	 * @return the name of the constant for a file upload button that is bound to the given form action
	 */
	public static final String getUploadButtonName(FormAction formAction) {
		final DomainAttribute uploadAttr = formAction.getBoundaryMethod().getDomainAttribute();

		return UPLOAD_BUTTON_PREFIX + uploadAttr.getName().toUpperCase();
	}

	/**
	 * @param formAction
	 * @return the ID of a file upload button that is bound to a given form action
	 */
	public static final String getUploadButtonId(FormAction formAction) {
		final DomainAttribute uploadAttr = formAction.getBoundaryMethod().getDomainAttribute();
		final Project project = uploadAttr.getDomainObject().getNamespace().getProject();
		final var idPrefix = project.hasJSFClient() ? FORM_PREFIX : "";

		return idPrefix + BUTTON_PREFIX + uploadAttr.getUpperCaseName() + UPLOAD_SUFFIX;
	}

	/**
	 * @param formAction
	 * @return the name of the constant for an upload dialog that is bound to the given form action
	 */
	public static final String getUploadDialogName(FormAction formAction) {
		final DomainAttribute uploadAttr = formAction.getBoundaryMethod().getDomainAttribute();

		return DIALOG_NAME_PREFIX + uploadAttr.getName().toUpperCase();
	}

	/**
	 * @param formAction
	 * @return the ID of an upload dialog that is bound to the given form action
	 */
	public static final String getUploadDialogId(FormAction formAction) {
		final DomainAttribute uploadAttr = formAction.getBoundaryMethod().getDomainAttribute();
		final Project project = uploadAttr.getDomainObject().getNamespace().getProject();
		final var idPrefix = project.hasJSFClient() ? FORM_PREFIX : "";

		return idPrefix + DIALOG_PREFIX + uploadAttr.getUpperCaseName() + UPLOAD_SUFFIX;
	}

	/**
	 * @param formAction
	 * @return the name of the constant for a file download button that is bound to the given form action
	 */
	public static final String getDownloadButtonName(FormAction formAction) {
		final DomainAttribute downloadAttr = formAction.getBoundaryMethod().getDomainAttribute();

		return DOWNLOAD_BUTTON_PREFIX + downloadAttr.getName().toUpperCase();
	}

	/**
	 * @param formAction
	 * @return the ID of a file download button that is bound to a given form action
	 */
	public static final String getDownloadButtonId(FormAction formAction) {
		final DomainAttribute downloadAttr = formAction.getBoundaryMethod().getDomainAttribute();
		final Project project = downloadAttr.getDomainObject().getNamespace().getProject();
		final var idPrefix = project.hasJSFClient() ? FORM_PREFIX : "";

		return idPrefix + BUTTON_PREFIX + downloadAttr.getUpperCaseName() + DOWNLOAD_SUFFIX;
	}

	/**
	 * @param formAction
	 * @return the name of the constant for a file import dialog that is bound to the given form action
	 */
	public static final String getImportDialogName(FormAction formAction) {
		return DIALOG_NAME_PREFIX + formAction.getName().toUpperCase();
	}

	/**
	 * @param formAction
	 * @return the ID of a file import dialog that is bound to the given form action
	 */
	public static final String getImportDialogId(FormAction formAction) {
		final Project project = formAction.getBoundaryMethod().getBoundaryBean().getDomainObject().getNamespace().getProject();
		var idPrefix = "";
		String actionName = formAction.getName();

		if (project.hasJSFClient())
			idPrefix = FORM_PREFIX;

		if (actionName.startsWith(ACTION_PREFIX))
			actionName = ACTION_PREFIX_PATTERN.matcher(actionName).replaceFirst("");

		actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);

		return idPrefix + DIALOG_PREFIX + actionName + UPLOAD_SUFFIX;
	}

	/**
	 * @param panel
	 * @param formAction
	 * @return the ID of a file import dialog that is bound to the given form action
	 */
	public static final String getImportDialogId(FormPanel panel, FormAction formAction) {
		final String idPrefix = getPanelIdPrefix(panel);
		String actionName = formAction.getName();

		if (actionName.startsWith(ACTION_PREFIX))
			actionName = ACTION_PREFIX_PATTERN.matcher(actionName).replaceFirst("");

		actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);

		return idPrefix + DIALOG_PREFIX + actionName + UPLOAD_SUFFIX;
	}

	/**
	 * @param form
	 * @param field
	 * @return the ID for a search filter field
	 */
	public static final String getSearchFilterId(Form form, TableColumnField field) {
		final Project project = form.getDomainObject().getNamespace().getProject();
		String fieldPrefix = FILTER_INPUT_PREFIX;
		var fieldSuffix = "";

		if (field.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN
				|| field.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
			fieldPrefix = FILTER_COMBO_PREFIX;
		else if ((field.getFieldType() == TableColumnFieldTypeEnumeration.DATE
				|| field.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE
				|| field.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME
				|| field.getFieldType() == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR) && !project.hasJSFClient())
			fieldPrefix = FILTER_DATE_PREFIX;

		if (!project.hasJSFClient())
			return fieldPrefix + getSearchFieldIndex(form, field);

		final BoundaryMethod autoCompleteMethod = JSFGeneratorUtil
				.getAutoCompleteMethod(field.getDTOAttribute().getDomainAttribute());

		// If the field provides a proposal lookup component an additional suffix is necessary!
		if (autoCompleteMethod != null)
			fieldSuffix = PROPOSAL_SUFFIX;

		return FORM_PREFIX + JSF_SEARCH_TAB_ID + fieldPrefix + getSearchFieldIndex(form, field) + fieldSuffix;
	}

	/**
	 * @param form
	 * @param field
	 * @return the ID for a search operator field
	 */
	public static final String getSearchOperatorId(Form form, TableColumnField field) {
		final Project project = form.getDomainObject().getNamespace().getProject();

		if (!project.hasJSFClient())
			return OPERATOR_INPUT_PREFIX + getSearchFieldIndex(form, field);

		return FORM_PREFIX + JSF_SEARCH_TAB_ID + OPERATOR_INPUT_PREFIX + getSearchFieldIndex(form, field);
	}

	/**
	 * @param form
	 * @param field
	 * @return the ID for a sort field in a search dialog
	 */
	public static final String getSearchSortOrderId(Form form, TableColumnField field) {
		final Project project = form.getDomainObject().getNamespace().getProject();

		if (!project.hasJSFClient())
			return SORT_INPUT_PREFIX + getSearchFieldIndex(form, field);

		return FORM_PREFIX + JSF_SEARCH_TAB_ID + SORT_INPUT_PREFIX + getSearchFieldIndex(form, field);
	}

	/**
	 * Determine the search field index
	 * @param form
	 * @param field
	 * @return the index of a field in a search dialog used for generating the ID
	 */
	private static int getSearchFieldIndex(Form form, TableColumnField field) {
		final FormTable table = form.getFormPanels().get(0).getFormTable();
		final Project project = form.getDomainObject().getNamespace().getProject();
		final boolean nonSearchableFieldsSupported = !project.hasVaadinClient();

		int colIndex = 0;

		for (final TableColumnField col : table.getFields()) {
			if (!col.isVisible())
				continue;

			if (nonSearchableFieldsSupported && !col.isSearchable()) {
				colIndex++;
				continue;
			}

			if (col.equals(field))
				break;

			colIndex++;
		}

		return colIndex;
	}

}
