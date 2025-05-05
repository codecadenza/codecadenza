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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field;

import net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.rap.RAPExtLinkFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.rcp.RCPExtLinkFieldGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.project.Project;

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
public class EclipseFieldGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private EclipseFieldGeneratorFactory() {

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
		final Project project = field.getDTOAttribute().getDTOBean().getNamespace().getProject();
		AbstractRichClientFieldGenerator fieldGenerator = null;

		if (type == FormFieldTypeEnumeration.CHECKBOX)
			fieldGenerator = new EclipseCheckBoxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.COMBOBOX)
			fieldGenerator = new EclipseComboboxFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DATE || type == FormFieldTypeEnumeration.DATE_TIME)
			fieldGenerator = new EclipseDateFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.ENUM_COMBOBOX)
			fieldGenerator = new EclipseEnumComboFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LIST)
			fieldGenerator = new EclipseListFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LOV)
			fieldGenerator = new EclipseListOfValuesFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SIMPLE_TEXT || type == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
			fieldGenerator = new EclipseTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			fieldGenerator = new EclipseProposalTextFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
			fieldGenerator = new EclipseSelectionByParentFormFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
			fieldGenerator = new EclipseLogOnFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD)
			fieldGenerator = new EclipseDocumentSizeFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.LABEL || type == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			fieldGenerator = new EclipseLabelFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.FORM_LINK)
			fieldGenerator = new EclipseIntLinkFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SELECTION_BY_CLIENT)
			fieldGenerator = new EclipseSelectionByClientFieldGenerator(field, formGenerator);
		else if (type == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
			if (!field.isReadonly())
				fieldGenerator = new EclipseSearchableListFieldGenerator(field, formGenerator);
			else
				fieldGenerator = new EclipseListFieldGenerator(field, formGenerator);
		}
		else if (type == FormFieldTypeEnumeration.WEB_LINK || type == FormFieldTypeEnumeration.MAIL_LINK) {
			if (project.hasRCPClient())
				fieldGenerator = new RCPExtLinkFieldGenerator(field, formGenerator);
			else
				fieldGenerator = new RAPExtLinkFieldGenerator(field, formGenerator);
		}
		else if (type == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
			fieldGenerator = new EclipseElementCollectionEditorGenerator(field, formGenerator);
		else
			throw new IllegalStateException("A generator for the form field type '" + type + "' is not available!");

		fieldGenerator.setI18n(i18n);
		fieldGenerator.setModelObjectName(field.getPanel().getForm().getDTO().getDomainObject().getLowerCaseName());

		return fieldGenerator;
	}

}
