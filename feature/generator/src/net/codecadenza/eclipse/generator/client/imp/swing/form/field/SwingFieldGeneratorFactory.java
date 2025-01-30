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
package net.codecadenza.eclipse.generator.client.imp.swing.form.field;

import net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
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
public class SwingFieldGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private SwingFieldGeneratorFactory() {

	}

	/**
	 * Get a form field generator depending on the form field type
	 * @param formGenerator
	 * @param field
	 * @param i18n
	 * @return a form field generator
	 * @throws IllegalStateException if a form field generator for the given form field type doesn't exist
	 */
	public static IFieldGenerator getFieldGenerator(AbstractJavaSourceGenerator formGenerator, FormField field,
			RichClientI18NGenerator i18n) {
		final FormFieldTypeEnumeration type = field.getFieldType();
		AbstractRichClientFieldGenerator fieldGenerator = null;

		if (type == FormFieldTypeEnumeration.CHECKBOX)
			fieldGenerator = new SwingCheckBoxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.COMBOBOX)
			fieldGenerator = new SwingComboboxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			fieldGenerator = new SwingDateFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			fieldGenerator = new SwingEnumComboFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LIST)
			fieldGenerator = new SwingSearchableListFieldGenerator(field, formGenerator, false);
		else if (type == FormFieldTypeEnumeration.LOV)
			fieldGenerator = new SwingListOfValuesFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SIMPLE_TEXT || type == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
			fieldGenerator = new SwingTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			fieldGenerator = new SwingProposalTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SEARCHABLE_LIST)
			fieldGenerator = new SwingSearchableListFieldGenerator(field, formGenerator, true);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
			fieldGenerator = new SwingSelectionByParentFormFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
			fieldGenerator = new SwingLogOnFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			fieldGenerator = new SwingDocumentSizeFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LABEL || type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			fieldGenerator = new SwingLabelFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.WEB_LINK || type == FormFieldTypeEnumeration.MAIL_LINK)
			fieldGenerator = new SwingExtLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.FORM_LINK)
			fieldGenerator = new SwingIntLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_CLIENT)
			fieldGenerator = new SwingSelectionByClientFieldGenerator(field, formGenerator);
		else
			throw new IllegalStateException("A generator for the form field type '" + type + "' is not available!");

		fieldGenerator.setI18n(i18n);
		fieldGenerator.setModelObjectName(field.getPanel().getForm().getDTO().getDomainObject().getLowerCaseName());

		return fieldGenerator;
	}

}
