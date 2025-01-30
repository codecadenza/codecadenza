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

import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for list-of-values fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinListOfValuesFieldGenerator extends AbstractVaadinFieldGenerator {
	private final DTOBean listDTO;
	private final Form lovForm;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinListOfValuesFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.lovForm = field.getListOfValues();
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		return "ListOfValuesField<" + listDTO.getModelClassName() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.component");

		if (!field.isReadonly()) {
			if (project.isBoundaryMode())
				formGenerator.importPackage(listDTO.getNamespace().toString());
			else
				formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

			formGenerator.importPackage(project.getClientNamespace().toString() + PACK_CLIENT_LOV);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		final var b = new StringBuilder();
		final var validationMessage = "Field must not be empty!";

		// Do not check fields that are either invisible, read-only or optional!
		if (!field.isVisible() || field.isReadonly() || !field.isMandatory())
			return "";

		b.append(".asRequired(" + i18n.getI18NMessage("msg_err_empty_field", validationMessage) + ")");

		return b.toString();
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
		final var locale = i18n.getLocaleFragment();

		b.append(field.getName() + " = new ListOfValuesField<>");

		if (!field.isReadonly()) {
			b.append("(event ->\n");
			b.append("{\n");
			b.append(lovForm.getLowerCaseName() + ".addDetachListener(detachEvent ->\n");
			b.append("{\n");
			b.append("if(" + lovForm.getLowerCaseName() + ".isApplyValue())\n");
			b.append(field.getName() + ".setValue(null);\n\n");
			b.append("if(" + lovForm.getLowerCaseName() + ".getSelectedId() != null)\n");
			b.append("{\n");
			b.append("final var selectedItem = new " + listDTO.getModelClassName() + "(");
			b.append(lovForm.getLowerCaseName() + ".getSelectedId());\n");

			if (listDTO.getDisplayAttribute() != null) {
				b.append("selectedItem." + listDTO.getDisplayAttribute().getModelSetterName() + "(");
				b.append(lovForm.getLowerCaseName() + ".getSelectedDisplayValue());\n");
			}

			b.append("\n");
			b.append(field.getName() + ".setValue(selectedItem);\n");
			b.append("}\n");
			b.append("});\n\n");
			b.append(lovForm.getLowerCaseName() + ".open();\n");
			b.append("}, " + locale + ");\n\n");
		}
		else
			b.append("(" + locale + ");\n");

		b.append(addItemLabelGenerator(displayAttr));
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");
		b.append(field.getName() + ".setLabel(" + i18n.getI18N(field) + ");\n");

		if (assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			b.append(field.getName() + ".setTitle(" + i18n.getI18N(assoc) + ");\n");

		b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

}
