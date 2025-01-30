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

import static net.codecadenza.eclipse.shared.Constants.CONVERSION_SUFFIX;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;

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
public class JSFComboboxFieldGenerator extends AbstractJSFFieldGenerator {
	private final String getter;
	private final String setter;
	private final DTOBean listDTO;
	private final String listName;
	private final DTOBeanAttribute pkAttribute;
	private final FormTypeEnumeration formType;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFComboboxFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.getter = field.getDTOAttribute().getModelGetterName();
		this.setter = field.getDTOAttribute().getModelSetterName();
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.listName = field.getDTOAttribute().getName() + "List";
		this.pkAttribute = listDTO.getPKAttribute();
		this.formType = field.getPanel().getForm().getFormType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("java.util");

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("Collection<" + listDTO.getModelClassName() + ">", listName).withTransientModifier().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(listDTO,
				BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);

		if (!field.isReadonly()) {
			// Load the items
			b.append(listName + " = ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

			if (formType == FormTypeEnumeration.UPDATE) {
				// It might be the case that the current element isn't contained in the list that was fetched from the server!
				b.append("\nif(" + modelObjectName + "." + getter + " != null && !" + listName);
				b.append(".contains(" + modelObjectName + "." + getter + "))\n");
				b.append(listName + ".add(" + modelObjectName + "." + getter + ");\n\n");
			}

			if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && field.isMandatory()) {
				// Preselect the field with a random list item
				b.append("\nif(!" + listName + ".isEmpty())\n");
				b.append(modelObjectName + "." + setter + "(" + listName + ".iterator().next());\n\n");
			}
		}
		else {
			b.append(listName + " = new ArrayList<>();\n");

			if (!field.isMandatory())
				b.append("\nif(" + modelObjectName + "." + getter + " != null)\n");

			b.append(listName + ".add(" + modelObjectName + "." + getter + ");\n");
			b.append("\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return getDefaultInitializationFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.AbstractJSFFieldGenerator#addFieldMethods()
	 */
	@Override
	public void addFieldMethods() {
		if (!field.isVisible())
			return;

		formGenerator.addGetter("Collection<" + listDTO.getModelClassName() + ">", listName,
				"the " + listDTO.getDomainObject().getLabel() + " list");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String converterName = JSFGeneratorUtil.createManagedBeanName(listDTO.getModelClassName() + CONVERSION_SUFFIX);
		final var varName = field.getName() + "Item";
		DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();

		if (displayAttr == null)
			displayAttr = pkAttribute;

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));
		b.append("\t\t<p:selectOneMenu ");

		if (field.isReadonly())
			b.append("readonly=\"true\" ");

		b.append("converter=\"#{" + converterName + "}\" id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + ".");
		b.append(modelObjectName + "." + field.getDTOAttribute().getModelFieldName() + "}\">\n");

		if (!field.isMandatory())
			b.append("\t\t\t<f:selectItem noSelectionOption=\"true\" itemLabel=\"\"/>\n");

		b.append("\t\t\t<f:selectItems value=\"#{" + managedBeanName + "." + listName + "}\" var=\"" + varName);
		b.append("\" itemLabel=\"#{" + varName + "." + displayAttr.getModelFieldName() + "}");

		if (listDTO.getDisplayAttribute() == null && !listDTO.getPKAttribute().getDomainAttribute().getJavaType().isString())
			b.append(" #{" + managedBeanName + ".emptyString}");

		b.append("\" itemValue=\"#{" + varName + "}\"/>\n");

		if (field.isMandatory() && !field.isReadonly()
				&& (project.isBoundaryMode() || project.getValidationType() == ValidationTypeEnumeration.INTERNAL))
			b.append("\t\t\t<f:validateRequired/>\n");

		b.append("\t\t</p:selectOneMenu>\n");
		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
