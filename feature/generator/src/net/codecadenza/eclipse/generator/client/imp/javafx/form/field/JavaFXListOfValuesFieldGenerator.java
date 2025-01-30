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
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

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
public class JavaFXListOfValuesFieldGenerator extends AbstractJavaFXFieldGenerator {
	private final BoundaryBean boundaryBean;
	private final DTOBean listDTO;
	private final DTOBean lovDTO;
	private final Form lovForm;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JavaFXListOfValuesFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.lovForm = field.getListOfValues();
		this.lovDTO = lovForm.getDTO();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
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

		formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.control");
		formGenerator.importPackage(project.getClientNamespace().toString() + PACK_CLIENT_LOV);
		formGenerator.importPackage(lovDTO.getNamespace().toString());

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

		formGenerator
				.addPrivateField("AbstractLOVField<" + listDTO.getModelClassName() + ", " + lovDTO.getName() + ">", field.getName())
				.create();
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

		b.append("// Check list-of-values field '" + field.getName() + "'\n");
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

		b.append(field.getName() + ".setSelectedItem(" + modelObjectName + "." + getter + ");\n\n");

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
		if (!field.isVisible())
			return "";

		final DTOBeanAttribute displayAttr = lovDTO.getDisplayAttribute();
		final var b = new StringBuilder();
		final DTOBeanAttribute dtoPkAttr = lovDTO.getPKAttribute();

		b.append(field.getName() + " = new AbstractLOVField<>");
		b.append("(new " + lovForm.getName() + "(this");

		if (!field.isMandatory())
			b.append(", true");

		b.append("))\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
		b.append("AbstractLOVField#getItemText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getItemText(" + listDTO.getModelClassName() + " selectedItem)\n");
		b.append("{\n");

		if (displayAttr == null)
			b.append("return " + dtoPkAttr.getDomainAttribute().convertToString("selectedItem." + dtoPkAttr.getModelGetterName()));
		else
			b.append("return selectedItem." + displayAttr.getModelGetterName());

		b.append(";\n");
		b.append("}\n");

		if (!field.isReadonly()) {
			b.append("\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
			b.append("AbstractLOVField#convertSelection(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public " + listDTO.getModelClassName() + " convertSelection(" + lovDTO.getName() + " selectedItem)\n");
			b.append("{\n");

			if (project.isBoundaryMode()) {
				b.append("return new " + listDTO.getModelClassName() + "(selectedItem." + dtoPkAttr.getModelGetterName());

				if (displayAttr != null)
					b.append(", selectedItem." + displayAttr.getModelGetterName());

				b.append(");\n");
			}
			else {
				final RepositoryMethod method = boundaryBean.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

				// Find and attach the selected domain object
				b.append("return ");

				new ServiceInvocationGenerator(method, b).addInvocation("selectedItem." + dtoPkAttr.getModelGetterName());
			}

			b.append("}\n");
		}

		b.append("};\n\n");

		if (field.isReadonly())
			b.append(field.getName() + ".setEditable(false);\n");

		b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
