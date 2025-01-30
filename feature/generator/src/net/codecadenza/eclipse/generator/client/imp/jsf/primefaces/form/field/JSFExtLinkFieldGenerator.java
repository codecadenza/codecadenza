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

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;

/**
 * <p>
 * Generator for web and email link fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFExtLinkFieldGenerator extends AbstractJSFFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFExtLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));
		b.append("\t\t<h:outputLink ");
		b.append("id=\"" + field.getName() + "\" ");

		// Currently using a style class doesn't work as expected!
		b.append("style=\"font-weight: bold;color: #0063ac\" ");
		b.append("value=\"");

		if (field.getFieldType() == FormFieldTypeEnumeration.MAIL_LINK)
			b.append("mailto:");
		else
			b.append("http://");

		b.append("#{" + managedBeanName + "." + modelObjectName + ".");
		b.append(field.getDTOAttribute().getModelFieldName() + "}\" target=\"_blank\" ");
		b.append(">\n");
		b.append("\t\t\t<h:outputText ");
		b.append("value=\"#{" + managedBeanName + "." + modelObjectName + ".");
		b.append(field.getDTOAttribute().getModelFieldName() + "}\" ");
		b.append("/>\n");
		b.append("\t\t</h:outputLink>\n");
		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
