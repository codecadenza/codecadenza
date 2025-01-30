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
package net.codecadenza.eclipse.generator.client.imp.swing.form.field;

import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;

import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
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
public class SwingProposalTextFieldGenerator extends AbstractSwingFieldGenerator {
	private final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public SwingProposalTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible() || field.isReadonly())
			return;

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		formGenerator.importPackage("java.util");
		formGenerator.importPackage("net.codecadenza.runtime.richclient.swing.widget");

		if (listDTO.getDomainObject().isMandated()) {
			final var securityHelper = new SwingSecurityHelper(project);

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
			formGenerator.addPrivateField("JTextField", field.getName()).create();
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
		b.append("if(" + field.getName() + ".getSelectedElement() == null)\n");
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
		final String getter = modelObjectName + "." + field.getDTOAttribute().getModelGetterName();
		DTOBeanAttribute attr = listDTO.getDisplayAttribute();

		if (attr == null)
			attr = listDTO.getPKAttribute();

		if (field.isReadonly()) {
			final String attrGetter = getter + "." + attr.getModelGetterName();
			final String checkStatement = getCheckFragment();

			if (!checkStatement.isEmpty())
				b.append("\n");

			b.append(checkStatement);
			b.append(field.getName() + ".setText(" + attr.getDomainAttribute().convertToString(attrGetter) + ");\n");

			if (!checkStatement.isEmpty())
				b.append("\n");
		}
		else
			b.append(field.getName() + ".setSelectedElement(" + getter + ");\n");

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

		b.append(objectName + "." + setter + "(" + field.getName() + ".getSelectedElement());\n");

		return b.toString();
	}

}
