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

import net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Abstract base class for all JSF form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AbstractJSFFieldGenerator extends AbstractClientFieldGenerator {
	protected JSFI18NGenerator i18n;
	protected String managedBeanName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public AbstractJSFFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.managedBeanName = JSFGeneratorUtil.createManagedBeanName(field.getPanel().getForm().getName());
	}

	/**
	 * Add further field-related methods
	 */
	public void addFieldMethods() {
		// This method doesn't provide an implementation as some subclasses don't need to create further methods!
	}

	/**
	 * @return the I18N generator
	 */
	public JSFI18NGenerator getI18n() {
		return i18n;
	}

	/**
	 * @param i18n
	 */
	public void setI18n(JSFI18NGenerator i18n) {
		this.i18n = i18n;
	}

	/**
	 * @param listDTO
	 * @param defaultItemName
	 * @return the generated content
	 */
	public String addDefaultListItem(DTOBean listDTO, String defaultItemName) {
		final var b = new StringBuilder();
		final DTOBeanAttribute displayAttribute = listDTO.getDisplayAttribute();

		b.append("final var " + defaultItemName + " = new " + listDTO.getModelClassName() + "(");
		b.append(listDTO.getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue());

		if (displayAttribute != null)
			b.append(", \"\"");

		b.append(");\n");

		return b.toString();
	}

	/**
	 * @param field
	 * @param hasOneColumn
	 * @param firstCol
	 * @return the content to fill empty columns
	 */
	public String fillGridColumn(FormField field, boolean hasOneColumn, boolean firstCol) {
		if (hasOneColumn)
			return "";

		final boolean fieldFound = field.getPanel().getFields().stream()
				.anyMatch(f -> f.isVisible() && f.getRowIndex() == field.getRowIndex() && !f.equals(field));

		if (!fieldFound && ((firstCol && field.getColIndex() == 2) || (!firstCol && field.getColIndex() == 1))) {
			final var b = new StringBuilder();
			b.append("\t\t<h:outputLabel/>\n");
			b.append("\t\t<h:outputLabel/>\n");
			b.append("\t\t<h:outputLabel/>\n");

			return b.toString();
		}

		return "";
	}

	/**
	 * @param fieldName
	 * @return the generated content
	 */
	public String addToolTipFragment(String fieldName) {
		final AbstractDomainAssociation assoc = field.getDTOAttribute().getAssociation();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (attr != null && attr.getUserComment() != null && !attr.getUserComment().isEmpty())
			return "\t\t<p:tooltip for=\"" + fieldName + "\" value=\"" + i18n.getI18N(attr) + "\"/>\n";

		if (attr == null && assoc != null && assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			return "\t\t<p:tooltip for=\"" + fieldName + "\" value=\"" + i18n.getI18N(assoc) + "\"/>\n";

		return "\t\t<p:tooltip for=\"" + fieldName + "\" value=\"\"/>\n";
	}

}
