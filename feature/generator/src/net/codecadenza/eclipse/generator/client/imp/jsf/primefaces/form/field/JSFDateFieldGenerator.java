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

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;

/**
 * <p>
 * Generator for date fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFDateFieldGenerator extends AbstractJSFFieldGenerator {
	private final JavaType type;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFDateFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.type = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		if (field.getDefaultValue() == null || field.getDefaultValue().isEmpty())
			return "";

		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (type.isDateOrCalendar())
			b.append(modelObjectName + "." + setter + "(new " + type.getNamespace().toString() + "." + type.getName() + "());\n");
		else
			b.append(modelObjectName + "." + setter + "(" + type.getNamespace().toString() + "." + type.getName() + ".now());\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		boolean addValidationTag = false;
		var fieldBinding = managedBeanName + "." + modelObjectName + "." + field.getDTOAttribute().getModelFieldName();

		if (type.isCalendar())
			fieldBinding += ".time";

		if (!field.isVisible())
			return "";

		if (field.isMandatory() && (project.isBoundaryMode() || project.getValidationType() == ValidationTypeEnumeration.INTERNAL))
			addValidationTag = true;

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));

		if (field.isReadonly()) {
			b.append("\t\t<p:inputText ");
			b.append("id=\"" + field.getName() + "\" value=\"#{" + fieldBinding + "}\" readonly=\"true\">\n");

			if (type.isDateOrCalendar()) {
				if (field.getFieldType() == FormFieldTypeEnumeration.DATE)
					b.append("\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateFormat}\" ");
				else
					b.append("\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\" ");

				b.append("timeZone=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");
			}
			else {
				if (type.isLocalDate()) {
					b.append("\t\t\t<f:converter converterId=\"");
					b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateConverter\"/>\n");
					b.append("\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateFormat}\"/>\n");
				}
				else {
					b.append("\t\t\t<f:converter converterId=\"");
					b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateTimeConverter\"/>\n");
					b.append("\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\"/>\n");
				}

				b.append("\t\t\t<f:attribute name=\"timeZone\" value=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");
			}

			b.append("\t\t</p:inputText>\n");
		}
		else {
			b.append("\t\t<p:datePicker id=\"" + field.getName() + "\" value=\"#{" + fieldBinding + "}\" ");
			b.append("pattern=\"#{" + USER_SESSION_BEAN + ".");

			if (field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME)
				b.append("dateTimeFormat");
			else
				b.append("dateFormat");

			b.append("}\" showButtonBar=\"true\" showIcon=\"true\" showOnFocus=\"false\"");

			if (!type.isLocalDateTime() && field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME)
				b.append(" showTime=\"true\"");

			b.append(">\n");

			if (addValidationTag)
				b.append("\t\t\t<f:validateRequired/>\n");

			b.append("\t\t</p:datePicker>\n");
		}

		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
