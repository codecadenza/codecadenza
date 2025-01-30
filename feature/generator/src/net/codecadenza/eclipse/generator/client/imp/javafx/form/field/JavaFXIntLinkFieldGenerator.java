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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
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
public class JavaFXIntLinkFieldGenerator extends AbstractJavaFXFieldGenerator {
	private final String getter;
	private Form readonlyForm;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JavaFXIntLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.getter = field.getDTOAttribute().getModelGetterName();

		// Search for a suitable target form
		for (final Form f : project.getAllFormsOfProject()) {
			if (!f.getDomainObject().equals(field.getDTOAttribute().getReferencedDTOBean().getDomainObject()))
				continue;

			if (f.getFormType() == FormTypeEnumeration.READONLY) {
				this.readonlyForm = f;
				break;
			}
		}
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

		if (readonlyForm != null)
			formGenerator.addImports(securityHelper.getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("Hyperlink", field.getName()).create();
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
		final DTOBean listDTO = field.getDTOAttribute().getReferencedDTOBean();
		DTOBeanAttribute attr = listDTO.getDisplayAttribute();

		if (attr == null)
			attr = listDTO.getPKAttribute();

		final String attributeGetter = modelObjectName + "." + getter + "." + attr.getModelGetterName();
		final String checkStatement = getCheckFragment();

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append(field.getName() + ".setText(" + attr.getDomainAttribute().convertToString(attributeGetter) + ");\n");

		if (!checkStatement.isEmpty())
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
		final String checkFragment = getCheckFragment();
		final DTOBeanAttribute pkAttr = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute();
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		b.append(field.getName() + " = new Hyperlink();\n");
		b.append(field.getName() + ".setOnAction(event ->\n");
		b.append("{\n");

		if (readonlyForm != null) {
			final var openForm = new StringBuilder(checkFragment);
			openForm.append("new " + readonlyForm.getName());
			openForm.append("(this, " + modelObjectName + "." + getter + "." + pkAttr.getModelGetterName() + ").open();\n");

			if (!checkFragment.isEmpty()) {
				final var messageText = "Cannot open respective detail form because of missing selection value for this field!";
				final String title = i18n.getI18NMessage("msg_title_form_link", "Open detail form");

				openForm.append("else\n");
				openForm.append("DialogUtil.openWarningDialog(this, " + title + ", ");
				openForm.append(i18n.getI18NMessage("msg_info_form_link_no_sel", messageText) + ");\n");
			}

			b.append(securityHelper.wrapSecurityCode(readonlyForm.getRoles(), openForm.toString()));
		}

		b.append("});\n\n");
		b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
