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
package net.codecadenza.eclipse.ui.preview.field;

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.CheckboxPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.ComboboxPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.DatePreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.ElementCollectionEditorPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.LOVPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.LabelPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.LinkPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.ListPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.ProposalTextPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.field.imp.TextPreviewBuilder;
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
public class FieldPreviewBuilderFactory {
	/**
	 * Prevent instantiation
	 */
	private FieldPreviewBuilderFactory() {

	}

	/**
	 * Get a field preview builder due to the given field type
	 * @param formPreviewBuilder
	 * @param field
	 * @param formPanel
	 * @return the corresponding field preview builder
	 * @throws IllegalStateException if a field preview builder could not be found
	 */
	public static FieldPreviewBuilder getFieldBuilder(VisualFormEditorPreviewBuilder formPreviewBuilder, FormField field,
			Composite formPanel) {
		final FormFieldTypeEnumeration type = field.getFieldType();
		final FieldPreviewBuilder builder;

		if (type == FormFieldTypeEnumeration.COMBOBOX || type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			builder = new ComboboxPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.CHECKBOX)
			builder = new CheckboxPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			builder = new DatePreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.MULTI_LINE_TEXT || type == FormFieldTypeEnumeration.SIMPLE_TEXT
				|| type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			builder = new TextPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.LABEL || type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			builder = new LabelPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.FORM_LINK || type == FormFieldTypeEnumeration.WEB_LINK
				|| type == FormFieldTypeEnumeration.MAIL_LINK)
			builder = new LinkPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			builder = new ProposalTextPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.LOV)
			builder = new LOVPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.LIST || type == FormFieldTypeEnumeration.SEARCHABLE_LIST)
			builder = new ListPreviewBuilder(formPreviewBuilder, field, formPanel);
		else if (type == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
			builder = new ElementCollectionEditorPreviewBuilder(formPreviewBuilder, field, formPanel);
		else
			throw new IllegalStateException("A preview builder for the form field type '" + type + "' is not available!");

		return builder;
	}

}
