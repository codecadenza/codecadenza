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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;

/**
 * <p>
 * Generator for web and email link fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinExtLinkFieldGenerator extends AbstractVaadinFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinExtLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("com.vaadin.flow.component.html");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		return "Anchor";
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
		b.append(field.getName() + " = new " + getFieldTypeName() + "();\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");
		b.append(field.getName() + ".setTitle(" + i18n.getI18N(field) + ");\n");
		b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		final String checkStatement = getCheckFragment();

		if (!checkStatement.isEmpty()) {
			b.append(checkStatement);
			b.append("{\n");
		}

		if (field.getFieldType() == FormFieldTypeEnumeration.WEB_LINK) {
			final var url = "urlFor" + field.getName().substring(0, 1).toUpperCase() + field.getName().substring(1);

			b.append("String " + url + " = " + modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + ";\n\n");
			b.append("if(!" + url + ".contains(\"https://\") && !" + url + ".contains(\"http://\"))\n");
			b.append(url + " = \"http://\" + " + url + ";\n\n");
			b.append(field.getName() + ".setHref(" + url + ");\n");
		}
		else {
			b.append(field.getName() + ".setHref(\"mailto:\" + " + modelObjectName + ".");
			b.append(field.getDTOAttribute().getModelGetterName() + ");\n");
		}

		b.append(field.getName() + ".setTarget(\"_blank\");\n");
		b.append(field.getName() + ".setText(" + modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + ");\n");

		if (!checkStatement.isEmpty())
			b.append("}\n");

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
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getBinding(java.lang.String)
	 */
	@Override
	public String getBinding(String binderName) {
		return "";
	}

}
