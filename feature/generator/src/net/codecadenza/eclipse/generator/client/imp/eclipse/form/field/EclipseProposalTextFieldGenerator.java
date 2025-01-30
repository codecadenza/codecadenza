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
public class EclipseProposalTextFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean listDTO;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseProposalTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
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
		DTOBeanAttribute attr = listDTO.getDisplayAttribute();

		if (attr == null)
			attr = listDTO.getPKAttribute();

		if (field.isReadonly()) {
			final String attrGetter = attr.getModelGetterName();
			final String checkStatement = getCheckFragment();
			final String attributeGetter = modelObjectName + "." + getter + "." + attrGetter;

			if (!checkStatement.isEmpty())
				b.append("\n");

			b.append(checkStatement);
			b.append(field.getName() + ".setText(" + attr.getDomainAttribute().convertToString(attributeGetter) + ");\n");

			if (!checkStatement.isEmpty())
				b.append("\n");
		}
		else
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
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();

		if (field.isReadonly()) {
			b.append(field.getName() + " = new Text(" + panel.getName() + ", SWT.BORDER);\n");
			b.append(field.getName() + ".setEditable(false);\n");
			b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));\n");
		}
		else {
			final String modelClassName = listDTO.getModelClassName();
			DTOBeanAttribute attr = listDTO.getDisplayAttribute();

			if (attr == null)
				attr = listDTO.getPKAttribute();

			b.append("\n");
			b.append(field.getName() + " = new AbstractProposalTextField<>(" + panel.getName() + ", SWT.BORDER");

			// If the proposal text field must perform lookups upon numeric data the proposals should come up as soon as user enters one
			// character!
			if (attr.getDomainAttribute().getJavaType().isIntegerOrLong())
				b.append(", 1");

			b.append(")\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.widget.");
			b.append("AbstractProposalTextField#getProposalData(java.lang.String)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public Collection<" + modelClassName + "> getProposalData(String filter)\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");
			b.append("Display.getDefault().getActiveShell().setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT));\n\n");
			b.append("return ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation("filter");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while fetching data for proposal text field '" + field.getName() + "'!", "e");

			b.append("\n");
			b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_data_fetch", "Data fetch") + ", ");
			b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: ") + " + e.getMessage());\n");
			b.append("return new ArrayList<>();\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("Display.getDefault().getActiveShell().setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_ARROW));\n");
			b.append("}\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.widget.");
			b.append("AbstractProposalTextField#getProposalLabel(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public String getProposalLabel(" + modelClassName + " element)\n");
			b.append("{\n");
			b.append("return " + attr.getDomainAttribute().convertToString("element." + attr.getModelGetterName()) + ";\n");
			b.append("}\n");
			b.append("};\n\n");
		}

		return b.toString();
	}

}
