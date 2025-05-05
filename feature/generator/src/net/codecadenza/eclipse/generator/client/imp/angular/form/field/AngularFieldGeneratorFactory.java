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
package net.codecadenza.eclipse.generator.client.imp.angular.form.field;

import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;

/**
 * <p>
 * Factory that provides a field generator depending on the field's type
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularFieldGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private AngularFieldGeneratorFactory() {

	}

	/**
	 * Get a form field generator depending on the form field type
	 * @param formatter
	 * @param field
	 * @return a form field generator
	 * @throws IllegalStateException if a form field generator for the given form field type doesn't exist
	 */
	public static AbstractAngularFieldGenerator getFieldGenerator(AngularContentFormatter formatter, FormField field) {
		final FormFieldTypeEnumeration type = field.getFieldType();

		if (type == FormFieldTypeEnumeration.SIMPLE_TEXT || type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			return new AngularTextFieldGenerator(field, formatter, false);
		else if (type == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
			return new AngularTextFieldGenerator(field, formatter, true);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			return new AngularProposalTextFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.COMBOBOX)
			return new AngularComboboxFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			return new AngularDateFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.LIST || type == FormFieldTypeEnumeration.SEARCHABLE_LIST)
			return new AngularListFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.CHECKBOX)
			return new AngularCheckBoxFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			return new AngularEnumComboFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.LOV)
			return new AngularListOfValuesFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_CLIENT)
			return new AngularSelectionByClientFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
			return new AngularSelectionByParentFormFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
			return new AngularLogOnFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.LABEL)
			return new AngularTextFieldGenerator(field, formatter, false, true);
		else if (type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			return new AngularTextFieldGenerator(field, formatter, true, true);
		else if (type == FormFieldTypeEnumeration.FORM_LINK)
			return new AngularInternalLinkFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.WEB_LINK)
			return new AngularWebLinkFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.MAIL_LINK)
			return new AngularMailLinkFieldGenerator(field, formatter);
		else if (type == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
			return new AngularElementCollectionEditorGenerator(field, formatter);

		throw new IllegalStateException("A generator for the form field type '" + type + "' is not available!");
	}

}
