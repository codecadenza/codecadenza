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

import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

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
public class EclipseComboboxFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean listDTO;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;
	private final String listName;
	private final String emptyItemName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseComboboxFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		this.listName = field.getDTOAttribute().getName() + "Items";
		this.emptyItemName = "empty" + field.getDTOAttribute().getUpperCaseName() + "Item";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		if (field.isAddFormLinkToLabel()) {
			formGenerator.importPackage("org.eclipse.swt.events");
			formGenerator.addImports(new EclipseSecurityHelper(project).getSecurityImports());
		}

		if (field.isReadonly())
			return;

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		formGenerator.importPackage("java.util");
		formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.widget");

		if (listDTO.getDomainObject().isMandated()) {
			final var securityHelper = new EclipseSecurityHelper(project);

			formGenerator.addImports(securityHelper.getSecurityManagerImports());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		if (field.isReadonly())
			formGenerator.addPrivateField("Text", field.getName()).create();
		else
			formGenerator.addPrivateField("DataComboViewer<" + listDTO.getModelClassName() + ">", field.getName()).create();
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
		final String getter = modelObjectName + "." + field.getDTOAttribute().getModelGetterName();

		if (field.isReadonly()) {
			DTOBeanAttribute attr = listDTO.getDisplayAttribute();

			if (attr == null)
				attr = listDTO.getPKAttribute();

			final DomainAttribute domainAttribute = attr.getDomainAttribute();
			final String attributeGetter = attr.getModelGetterName();
			final String checkStatement = getCheckFragment();

			if (!checkStatement.isEmpty())
				b.append("\n");

			// The check-fragment to avoid NPEs is only necessary if the field is read-only!
			b.append(checkStatement);
			b.append(field.getName() + ".setText(" + domainAttribute.convertToString(getter + "." + attributeGetter) + ");\n");

			if (!checkStatement.isEmpty())
				b.append("\n");
		}
		else {
			final DTOBeanAttribute pkAttribute = listDTO.getPKAttribute();

			if (!field.isMandatory()) {
				b.append("final var " + emptyItemName + " = new " + listDTO.getModelClassName() + "(");
				b.append(pkAttribute.getDomainAttribute().getEmptyItemDefaultValue());
				b.append(");\n");

				final DTOBeanAttribute displayAttribute = listDTO.getDisplayAttribute();

				if (displayAttribute != null)
					b.append(emptyItemName + "." + displayAttribute.getModelSetterName() + "(\"\");\n\n");
			}

			b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

			if (!field.isMandatory())
				b.append("\n" + listName + ".add(" + emptyItemName + ");\n");

			b.append("\n");
			b.append("// It might be the case that the current element isn't contained ");
			b.append("in the list that was fetched from the server!\n");

			if (field.isMandatory())
				b.append("if(!" + listName + ".contains(" + getter + "))\n");
			else
				b.append("if(" + getter + " != null && !" + listName + ".contains(" + getter + "))\n");

			b.append(listName + ".add(" + getter + ");\n\n");
			b.append(field.getName() + ".setData(" + listName + ");\n");

			if (field.isMandatory())
				b.append(field.getName() + ".setSelectedItem(" + getter + ");\n");
			else {
				b.append("\n");
				b.append("if(" + getter + " != null)\n");
				b.append(field.getName() + ".setSelectedItem(" + getter + ");\n");
				b.append("else\n");
				b.append(field.getName() + ".setSelectedItem(" + emptyItemName + ");\n");
			}

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
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final DTOBeanAttribute pkAttribute = listDTO.getPKAttribute();

		if (!field.isMandatory()) {
			b.append("final var " + emptyItemName + " = new " + listDTO.getModelClassName() + "(");
			b.append(pkAttribute.getDomainAttribute().getEmptyItemDefaultValue());
			b.append(");\n");

			final DTOBeanAttribute displayAttribute = listDTO.getDisplayAttribute();

			if (displayAttribute != null)
				b.append(emptyItemName + "." + displayAttribute.getModelSetterName() + "(\"\");\n\n");
		}

		b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

		if (!field.isMandatory())
			b.append("\n" + listName + ".add(" + emptyItemName + ");\n");

		b.append("\n");
		b.append(field.getName() + ".setData(" + listName + ");\n");

		if (field.isMandatory()) {
			b.append("\n");
			b.append("// Make a random selection!\n");
			b.append("if(!" + listName + ".isEmpty())\n");
			b.append(field.getName() + ".setSelectedItem(" + listName + ".stream().findFirst().orElse(null));\n");
		}
		else
			b.append(field.getName() + ".setSelectedItem(" + emptyItemName + ");\n");

		b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		final var b = new StringBuilder();
		final JavaType type = listDTO.getPKAttribute().getDomainAttribute().getJavaType();

		if (field.isReadonly() || !field.isVisible())
			return "";

		final String setter = field.getDTOAttribute().getModelSetterName();

		if (!project.isBoundaryMode() && !field.isMandatory()) {
			// If the form uses a facade and the respective field is optional we must handle the default item differently!
			final String listIdGetter = listDTO.getPKAttribute().getModelGetterName();

			b.append("\nif(" + field.getName() + ".getSelectedItem()." + listIdGetter);

			if (type.isString())
				b.append(".isEmpty()");
			else
				b.append(" == " + listDTO.getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue());

			b.append(")\n");
			b.append(objectName + "." + setter + "(null);\n");
			b.append("else\n");
		}

		b.append(objectName + "." + setter + "(" + field.getName() + ".getSelectedItem());\n");

		if (!project.isBoundaryMode() && !field.isMandatory())
			b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();
		DTOBeanAttribute dtoAttribute = listDTO.getDisplayAttribute();

		if (dtoAttribute == null)
			dtoAttribute = listDTO.getPKAttribute();

		if (field.isReadonly()) {
			b.append(field.getName() + " = new Text(" + panel.getName() + ", SWT.BORDER);\n");
			b.append(field.getName() + ".setEditable(false);\n");
			b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));\n");
		}
		else {
			final String getter = dtoAttribute.getModelGetterName();
			final DomainAttribute domainAttribute = dtoAttribute.getDomainAttribute();

			b.append("\n");
			b.append(field.getName() + " = new DataComboViewer<>(" + panel.getName() + ", SWT.READ_ONLY)\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.widget.");
			b.append("DataComboViewer#getItemText(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public String getItemText(" + listDTO.getModelClassName() + " element)\n");
			b.append("{\n");
			b.append("return " + domainAttribute.convertToString("element." + getter) + ";\n");
			b.append("}\n");
			b.append("};\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		final var b = new StringBuilder();
		final String validationMessage = i18n.getI18NMessage("msg_err_no_selection", "Field \"{0}\" requires selection!",
				FIELD_LABEL_VALIDATION);

		// Do not check fields that are either invisible, read-only or optional!
		if (!field.isVisible() || field.isReadonly() || !field.isMandatory())
			return "";

		b.append("// Check combobox '" + field.getName() + "'\n");
		b.append("if(" + field.getName() + ".getSelectedItem() == null)\n");
		b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
		b.append("\n");

		return b.toString();
	}

}
