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
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for label fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFLabelFieldGenerator extends AbstractJSFFieldGenerator {
	private final JavaType type;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFLabelFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.type = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		var fieldBinding = managedBeanName + "." + modelObjectName + "." + field.getDTOAttribute().getModelFieldName();

		if (type.isCalendar())
			fieldBinding += ".time";

		if (!field.isVisible())
			return "";

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));

		if (field.getFieldType() == FormFieldTypeEnumeration.LABEL)
			b.append("\t\t<h:outputText styleClass=\"label-field-value\" ");
		else
			b.append("\t\t<p:inputTextarea style=\"width:200px;height:80px;\" readonly=\"true\" ");

		b.append("id=\"" + field.getName() + "\" value=\"#{" + fieldBinding + "}\">\n");

		if (type.isDecimalNumber())
			b.append("\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
		else if (type.isDateOrCalendar()) {
			if (field.getDTOAttribute().getDomainAttribute().getTemporalType() == TemporalTypeEnumeration.DATE)
				b.append("\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateFormat}\" ");
			else
				b.append("\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\" ");

			b.append("timeZone=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");
		}
		else if (type.isLocalDate()) {
			b.append("\t\t\t<f:converter converterId=\"");
			b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateConverter\"/>\n");
			b.append("\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateFormat}\"/>\n");
		}
		else if (type.isLocalDateTime()) {
			b.append("\t\t\t<f:converter converterId=\"");
			b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateTimeConverter\"/>\n");
			b.append("\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\"/>\n");
		}
		else if (type.isUUID()) {
			b.append("\t\t\t<f:converter converterId=\"");
			b.append("net.codecadenza.runtime.webclient.primefaces.converter.UUIDConverter\"/>\n");
		}

		if (type.isLocalDate() || type.isLocalDateTime())
			b.append("\t\t\t<f:attribute name=\"timeZone\" value=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");

		if (field.getFieldType() == FormFieldTypeEnumeration.LABEL)
			b.append("\t\t</h:outputText>\n");
		else
			b.append("\t\t</p:inputTextarea>\n");

		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
