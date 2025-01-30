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

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for combobox fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinComboboxFieldGenerator extends AbstractVaadinFieldGenerator {
	private final DTOBean listDTO;
	private final boolean withFilter;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 * @param withFilter
	 */
	public VaadinComboboxFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator, boolean withFilter) {
		super(field, formGenerator);

		this.withFilter = withFilter;
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		return "ComboBox<" + listDTO.getModelClassName() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("com.vaadin.flow.component.combobox");

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		if (field.isReadonly() || withFilter)
			formGenerator.importClass("java.util.Collections");

		if (!field.isReadonly())
			formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.provider.data");
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
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(listDTO,
				BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		final var invocationGenerator = new ServiceInvocationGenerator(method, listDTO, b);

		b.append(field.getName() + " = new ComboBox<>();\n");
		b.append(addItemLabelGenerator(displayAttr));
		b.append(field.getName() + ".setLabel(" + i18n.getI18N(field) + ");\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");

		if (field.isReadonly())
			b.append(field.getName() + ".setItems(Collections.emptyList());\n");
		else if (withFilter) {
			b.append(field.getName() + ".setItems(BackEndFilteringDataProvider.fromCallback(filter ->\n");
			b.append("{\n");
			b.append("if(filter == null || filter.isEmpty())\n");
			b.append("return Collections.emptyList();\n\n");
			b.append("return ");

			invocationGenerator.addInvocation("filter");

			b.append("}));\n\n");
		}
		else {
			b.append(field.getName() + ".setAllowCustomValue(false);\n");
			b.append(field.getName() + ".setItems(BackEndDataProvider.fromCallback(() ->");

			invocationGenerator.addInvocation(true, "null");

			b.append("));\n");
		}

		b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

}
