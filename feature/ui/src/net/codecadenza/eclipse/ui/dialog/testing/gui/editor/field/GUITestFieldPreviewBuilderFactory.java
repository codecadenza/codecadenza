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
package net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field;

import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.CheckboxPreviewBuilder;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.EnumComboboxPreviewBuilder;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.ItemSelectionPreviewBuilder;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.ListPreviewBuilder;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.ReadonlyFieldPreviewBuilder;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.TextPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.FieldPreviewBuilder;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Factory to get a field preview builder depending on the given field type
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestFieldPreviewBuilderFactory {
	/**
	 * Prevent instantiation
	 */
	private GUITestFieldPreviewBuilderFactory() {

	}

	/**
	 * Get a preview builder due to the given field type
	 * @param formPanel
	 * @param testData
	 * @param maintainTestData
	 * @param enableDatabaseLookup
	 * @return the corresponding field preview builder
	 * @throws IllegalStateException if a suitable field preview builder hasn't been found
	 */
	public static FieldPreviewBuilder getFieldBuilder(Composite formPanel, GUITestData testData, boolean maintainTestData,
			boolean enableDatabaseLookup) {
		final FormFieldTypeEnumeration type = testData.getFormField().getFieldType();
		final FieldPreviewBuilder builder;

		if (type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			builder = new EnumComboboxPreviewBuilder(testData, formPanel, maintainTestData);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT || type == FormFieldTypeEnumeration.LOV
				|| type == FormFieldTypeEnumeration.COMBOBOX)
			builder = new ItemSelectionPreviewBuilder(testData, formPanel, maintainTestData, enableDatabaseLookup);
		else if (type == FormFieldTypeEnumeration.CHECKBOX)
			builder = new CheckboxPreviewBuilder(testData, formPanel, maintainTestData);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			builder = new TextPreviewBuilder(testData, formPanel, maintainTestData, enableDatabaseLookup);
		else if (type == FormFieldTypeEnumeration.MULTI_LINE_TEXT || type == FormFieldTypeEnumeration.SIMPLE_TEXT
				|| type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			builder = new TextPreviewBuilder(testData, formPanel, maintainTestData, enableDatabaseLookup);
		else if (type == FormFieldTypeEnumeration.LABEL || type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			builder = new ReadonlyFieldPreviewBuilder(testData, formPanel, maintainTestData, enableDatabaseLookup);
		else if (type == FormFieldTypeEnumeration.FORM_LINK || type == FormFieldTypeEnumeration.WEB_LINK
				|| type == FormFieldTypeEnumeration.MAIL_LINK)
			builder = new ReadonlyFieldPreviewBuilder(testData, formPanel, maintainTestData, enableDatabaseLookup);
		else if (type == FormFieldTypeEnumeration.LIST || type == FormFieldTypeEnumeration.SEARCHABLE_LIST)
			builder = new ListPreviewBuilder(testData, formPanel, maintainTestData, enableDatabaseLookup);
		else
			throw new IllegalStateException("A preview builder for the form field type '" + type + "' is not available!");

		return builder;
	}

}
