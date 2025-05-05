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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field;

import static net.codecadenza.eclipse.generator.client.common.form.AbstractSingleRecordFormGenerator.INIT_MODEL_OBJ_NAME_PREFIX;

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;

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
public class JSFFieldGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private JSFFieldGeneratorFactory() {

	}

	/**
	 * Get a form field generator depending on the form field type
	 * @param formGenerator
	 * @param field
	 * @param i18n
	 * @return a form field generator
	 * @throws IllegalStateException if a form field generator for the given form field type doesn't exist
	 */
	public static AbstractJSFFieldGenerator getFieldGenerator(AbstractJavaSourceGenerator formGenerator, FormField field,
			JSFI18NGenerator i18n) {
		final FormFieldTypeEnumeration type = field.getFieldType();
		AbstractJSFFieldGenerator fieldGenerator = null;

		if (type == FormFieldTypeEnumeration.SIMPLE_TEXT || type == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
			fieldGenerator = new JSFTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.CHECKBOX)
			fieldGenerator = new JSFCheckBoxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			fieldGenerator = new JSFDateFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LABEL || type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			fieldGenerator = new JSFLabelFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.COMBOBOX)
			fieldGenerator = new JSFComboboxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SEARCHABLE_LIST)
			fieldGenerator = new JSFSearchableListFieldGenerator(field, formGenerator, true);
		else if (type == FormFieldTypeEnumeration.LIST)
			fieldGenerator = new JSFSearchableListFieldGenerator(field, formGenerator, false);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			fieldGenerator = new JSFProposalTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			fieldGenerator = new JSFEnumComboFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
			fieldGenerator = new JSFSelectionByParentFormFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
			fieldGenerator = new JSFLogOnFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			fieldGenerator = new JSFDocumentSizeFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LOV)
			fieldGenerator = new JSFListOfValuesFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.MAIL_LINK || type == FormFieldTypeEnumeration.WEB_LINK)
			fieldGenerator = new JSFExtLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.FORM_LINK)
			fieldGenerator = new JSFIntLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_CLIENT)
			fieldGenerator = new JSFSelectionByClientFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
			fieldGenerator = new JSFElementCollectionEditorFieldGenerator(field, formGenerator);
		else
			throw new IllegalStateException("A generator for the form field type '" + type + "' is not available!");

		final DTOBean formDTO = field.getPanel().getForm().getDTO();
		String objectName = formDTO.getDomainObject().getLowerCaseName();

		if (!field.getDTOAttribute().getDTOBean().equals(formDTO))
			objectName = INIT_MODEL_OBJ_NAME_PREFIX + field.getDTOAttribute().getDTOBean().getDomainObject().getName();

		fieldGenerator.setModelObjectName(objectName);
		fieldGenerator.setI18n(i18n);
		return fieldGenerator;
	}

}
