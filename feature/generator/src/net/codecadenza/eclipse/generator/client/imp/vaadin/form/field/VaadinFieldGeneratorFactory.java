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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form.field;

import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
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
public class VaadinFieldGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private VaadinFieldGeneratorFactory() {

	}

	/**
	 * Get a form field generator depending on the form field type
	 * @param formGenerator
	 * @param field
	 * @param i18n
	 * @return a form field generator
	 * @throws IllegalStateException if a form field generator for the given form field type doesn't exist
	 */
	public static AbstractVaadinFieldGenerator getFieldGenerator(AbstractJavaSourceGenerator formGenerator, FormField field,
			VaadinI18NGenerator i18n) {
		final FormFieldTypeEnumeration type = field.getFieldType();
		AbstractVaadinFieldGenerator fieldGenerator = null;

		if (type == FormFieldTypeEnumeration.CHECKBOX)
			fieldGenerator = new VaadinCheckBoxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SIMPLE_TEXT || type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			fieldGenerator = new VaadinTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
			fieldGenerator = new VaadinMultiLineTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			fieldGenerator = new VaadinComboboxFieldGenerator(field, formGenerator, true);
		else if (type == FormFieldTypeEnumeration.COMBOBOX)
			fieldGenerator = new VaadinComboboxFieldGenerator(field, formGenerator, false);
		else if (type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			fieldGenerator = new VaadinEnumComboFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LABEL || type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			fieldGenerator = new VaadinLabelFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LIST)
			fieldGenerator = new VaadinListFieldGenerator(field, formGenerator, false);
		else if (type == FormFieldTypeEnumeration.LOV)
			fieldGenerator = new VaadinListOfValuesFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.FORM_LINK)
			fieldGenerator = new VaadinInternalLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			fieldGenerator = new VaadinDateFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.MAIL_LINK || type == FormFieldTypeEnumeration.WEB_LINK)
			fieldGenerator = new VaadinExtLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
			fieldGenerator = new VaadinSelectionByParentFormFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_CLIENT)
			fieldGenerator = new VaadinSelectionByClientFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
			fieldGenerator = new VaadinLogOnFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SEARCHABLE_LIST)
			fieldGenerator = new VaadinListFieldGenerator(field, formGenerator, !field.isReadonly());
		else
			throw new IllegalStateException("A generator for the form field type '" + type + "' is not available!");

		fieldGenerator.setI18n(i18n);
		fieldGenerator.setModelObjectName(field.getPanel().getForm().getDTO().getDomainObject().getLowerCaseName());

		return fieldGenerator;
	}

}
