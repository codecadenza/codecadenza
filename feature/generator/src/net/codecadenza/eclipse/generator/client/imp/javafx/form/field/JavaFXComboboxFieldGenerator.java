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
package net.codecadenza.eclipse.generator.client.imp.javafx.form.field;

import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
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
public class JavaFXComboboxFieldGenerator extends AbstractJavaFXFieldGenerator {
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
	public JavaFXComboboxFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		this.listName = field.getDTOAttribute().getName() + "Items";
		this.emptyItemName = "empty" + field.getDTOAttribute().getUpperCaseName() + "Item";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		super.addImports();

		if (field.isReadonly())
			return;

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		formGenerator.importPackage("java.util");
		formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.control");

		if (listDTO.getDomainObject().isMandated())
			formGenerator.addImports(securityHelper.getSecurityManagerImports());
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
			formGenerator.addPrivateField("TextField", field.getName()).create();
		else
			formGenerator.addPrivateField("DataComboBox<" + listDTO.getModelClassName() + ">", field.getName()).create();
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
		final String getter = field.getDTOAttribute().getModelGetterName();

		if (field.isReadonly()) {
			DTOBeanAttribute attr = listDTO.getDisplayAttribute();

			if (attr == null)
				attr = listDTO.getPKAttribute();

			final String attributeGetter = modelObjectName + "." + getter + "." + attr.getModelGetterName();
			final String checkStatement = getCheckFragment();

			if (!checkStatement.isEmpty())
				b.append("\n");

			// The check-fragment to avoid NPEs is only necessary if the field is read-only!
			b.append(checkStatement);
			b.append(field.getName() + ".setText(" + attr.getDomainAttribute().convertToString(attributeGetter) + ");\n");

			if (!checkStatement.isEmpty())
				b.append("\n");
		}
		else {
			final DTOBeanAttribute pkAttribute = listDTO.getPKAttribute();

			if (!field.isMandatory()) {
				final DTOBeanAttribute displayAttribute = listDTO.getDisplayAttribute();

				b.append("final var " + emptyItemName + " = new " + listDTO.getModelClassName() + "(");
				b.append(pkAttribute.getDomainAttribute().getEmptyItemDefaultValue());
				b.append(");\n");

				if (displayAttribute != null)
					b.append(emptyItemName + "." + displayAttribute.getModelSetterName() + "(\"\");\n");

				b.append("\n");
			}

			b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

			if (!field.isMandatory())
				b.append("\n" + listName + ".add(" + emptyItemName + ");\n");

			b.append("\n");
			b.append("// It might be the case that the current element isn't ");
			b.append("contained in the list that was fetched from the server!\n");

			if (field.isMandatory())
				b.append("if(");
			else
				b.append("if(" + modelObjectName + "." + getter + " != null && ");

			b.append("!" + listName + ".contains(" + modelObjectName + "." + getter + "))\n");
			b.append(listName + ".add(" + modelObjectName + "." + getter + ");\n\n");
			b.append(field.getName() + ".getItems().addAll(" + listName + ");\n");

			if (field.isMandatory())
				b.append(field.getName() + ".setValue(" + modelObjectName + "." + getter + ");\n");
			else {
				b.append("\nif(" + modelObjectName + "." + getter + " != null)\n");
				b.append(field.getName() + ".setValue(" + modelObjectName + "." + getter + ");\n");
				b.append("else\n");
				b.append(field.getName() + ".setValue(" + emptyItemName + ");\n");
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
				b.append(emptyItemName + "." + displayAttribute.getModelSetterName() + "(\"\");\n");

			b.append("\n");
		}

		b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

		if (!field.isMandatory())
			b.append("\n" + listName + ".add(" + emptyItemName + ");\n");

		b.append("\n");
		b.append(field.getName() + ".getItems().addAll(" + listName + ");\n");

		if (field.isMandatory()) {
			b.append("\n");
			b.append("// Make a random selection!\n");
			b.append("if(!" + listName + ".isEmpty())\n");
			b.append(field.getName() + ".setValue(" + listName + ".stream().findFirst().orElse(null));\n");
		}
		else
			b.append(field.getName() + ".setValue(" + emptyItemName + ");\n");

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

			b.append("\nif(" + field.getName() + ".getValue()." + listIdGetter);

			if (type.isString())
				b.append(".isEmpty()");
			else
				b.append(" == " + listDTO.getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue());

			b.append(")\n");
			b.append(objectName + "." + setter + "(null);\n");
			b.append("else\n");
		}

		b.append(objectName + "." + setter + "(" + field.getName() + ".getValue());\n");

		if (!project.isBoundaryMode() && !field.isMandatory())
			b.append("\n");

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
		b.append("if(" + field.getName() + ".getValue() == null)\n");
		b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
		b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();

		if (field.isReadonly()) {
			b.append(field.getName() + " = new TextField();\n");
			b.append(field.getName() + ".setEditable(false);\n");
			b.append(super.getFieldDefinitionFragment(hasOneColumn));

			return b.toString();
		}

		DTOBeanAttribute dtoAttribute = listDTO.getDisplayAttribute();

		if (dtoAttribute == null)
			dtoAttribute = listDTO.getPKAttribute();

		b.append("\n");
		b.append(field.getName() + " = new DataComboBox<>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.DataComboBox#getItemText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getItemText(" + listDTO.getModelClassName() + " element)\n");
		b.append("{\n");

		final String getter = dtoAttribute.getModelGetterName();
		final JavaType type = dtoAttribute.getDomainAttribute().getJavaType();

		if (!field.isMandatory() && !type.isString()) {
			// Don't display auxiliary numbers that represent ID values of default list items!
			b.append("if(element." + getter + " == ");
			b.append(dtoAttribute.getDomainAttribute().getEmptyItemDefaultValue());
			b.append(")\nreturn\"\";\n\n");
		}

		b.append("return " + dtoAttribute.getDomainAttribute().convertToString("element." + getter) + ";\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
