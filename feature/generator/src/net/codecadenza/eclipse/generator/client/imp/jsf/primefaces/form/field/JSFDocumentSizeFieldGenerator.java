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

/**
 * <p>
 * Generator for fields that display the size of a document
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFDocumentSizeFieldGenerator extends AbstractJSFFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFDocumentSizeFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
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
		b.append("\t\t<p:inputText ");
		b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName);
		b.append("." + field.getDTOAttribute().getModelFieldName() + "}\" ");
		b.append("readonly=\"true\" converter=\"");

		if (field.getDTOAttribute().getDomainAttribute().getJavaType().isInteger())
			b.append("net.codecadenza.runtime.webclient.primefaces.converter.FileSizeIntConverter");
		else
			b.append("net.codecadenza.runtime.webclient.primefaces.converter.FileSizeLongConverter");

		b.append("\"/>\n");
		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
