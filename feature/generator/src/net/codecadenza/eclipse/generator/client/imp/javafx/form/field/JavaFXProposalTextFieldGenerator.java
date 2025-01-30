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

/**
 * <p>
 * Generator for fields that display proposal items after entering a filter text
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXProposalTextFieldGenerator extends AbstractJavaFXFieldGenerator {
	private final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JavaFXProposalTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());

		if (!field.isVisible())
			return;

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		super.addImports();

		formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.control");

		if (field.isReadonly())
			return;

		if (boundaryBean == null)
			throw new IllegalStateException("The boundary bean could not be found!");

		formGenerator.importPackage("java.util");

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

		formGenerator.addPrivateField("AbstractProposalTextField<" + listDTO.getModelClassName() + ">", field.getName()).create();
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

		b.append("// Check proposal text field '" + field.getName() + "'\n");
		b.append("if(" + field.getName() + ".getSelectedItem() == null)\n");
		b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
		b.append("\n");

		return b.toString();
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

		b.append(field.getName() + ".setSelectedItem(" + modelObjectName + "." + getter + ");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (field.isReadonly() || !field.isVisible())
			return "";

		b.append(objectName + "." + setter + "(" + field.getName() + ".getSelectedItem());\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		final String modelClassName = listDTO.getModelClassName();
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(listDTO,
				BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		DTOBeanAttribute attr = listDTO.getDisplayAttribute();

		if (!field.isVisible())
			return "";

		if (attr == null)
			attr = listDTO.getPKAttribute();

		b.append("\n");
		b.append(field.getName() + " = new AbstractProposalTextField<>()\n");
		b.append("{\n");

		if (!field.isReadonly()) {
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
			b.append("AbstractProposalTextField#getProposalItems(java.lang.String)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public List<" + modelClassName + "> getProposalItems(String filter) throws Exception\n");
			b.append("{\n");
			b.append("return ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation("filter");

			b.append("}\n\n");
		}

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
		b.append("AbstractProposalTextField#getProposalText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getProposalText(" + modelClassName + " selectedItem)\n");
		b.append("{\n");

		final String getter = attr.getModelGetterName();

		if (!field.isMandatory()) {
			b.append("if(selectedItem == null)\n");
			b.append("return \"\";\n\n");
		}

		b.append("return " + attr.getDomainAttribute().convertToString("selectedItem." + getter) + ";\n");
		b.append("}\n");
		b.append("};\n\n");

		if (field.isReadonly())
			b.append(field.getName() + ".setEditable(false);\n");

		b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
