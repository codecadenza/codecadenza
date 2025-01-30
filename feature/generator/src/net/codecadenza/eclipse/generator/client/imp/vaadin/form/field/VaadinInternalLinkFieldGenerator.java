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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for fields that open a form by using a link
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinInternalLinkFieldGenerator extends AbstractVaadinFieldGenerator {
	private final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinInternalLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		return "InternalDialogLinkField<" + listDTO.getModelClassName() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.component");
		formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.util");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute() == null ? listDTO.getPKAttribute()
				: listDTO.getDisplayAttribute();
		final AbstractDomainAssociation assoc = field.getDTOAttribute().getAssociation();
		Form readonlyForm = null;

		for (final Form f : project.getAllFormsOfProject()) {
			if (!f.getDomainObject().equals(listDTO.getDomainObject()))
				continue;

			if (f.getFormType() == FormTypeEnumeration.READONLY) {
				readonlyForm = f;
				break;
			}
		}

		b.append(field.getName() + " = new InternalDialogLinkField<>(event ->\n");
		b.append("{\n");
		b.append("final " + listDTO.getModelClassName() + " item = " + field.getName() + ".getValue();\n\n");
		b.append("if(item == null)\n");
		b.append("return;\n\n");

		if (readonlyForm != null) {
			b.append("navigator.navigateTo(" + readonlyForm.getName() + ".class, ");
			b.append("item." + listDTO.getPKAttribute().getModelGetterName() + ");\n");
		}
		else
			b.append("// No appropriate form found!\n");

		b.append("}, " + i18n.getLocaleFragment() + ");\n\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");
		b.append(field.getName() + ".setLabel(" + i18n.getI18N(field) + ");\n");
		b.append(addItemLabelGenerator(displayAttr));

		if (assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			b.append(field.getName() + ".setTitle(" + i18n.getI18N(assoc) + ");\n");

		b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

}
