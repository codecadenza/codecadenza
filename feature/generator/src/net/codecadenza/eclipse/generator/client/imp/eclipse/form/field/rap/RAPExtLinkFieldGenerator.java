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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.rap;

import net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;

/**
 * <p>
 * RAP implementation of a generator for web and email link fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RAPExtLinkFieldGenerator extends AbstractEclipseFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public RAPExtLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		formGenerator.importPackage("org.eclipse.swt.events");
		formGenerator.importClass("org.eclipse.rap.rwt.RWT");
		formGenerator.importClass("org.eclipse.rap.rwt.client.service.UrlLauncher");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("Link", field.getName()).create();
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
		final String checkStatement = getCheckFragment();

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append(field.getName() + ".setText(\"<a>\" + " + modelObjectName + "." + getter + " + \"</a>\");\n");

		if (!checkStatement.isEmpty())
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
		final String getter = field.getDTOAttribute().getModelGetterName();
		final FormPanel panel = field.getPanel();
		final String checkStatement = getCheckFragment();

		b.append(field.getName() + " = new Link(" + panel.getName() + ", SWT.NONE);\n\n");
		b.append(field.getName() + ".addMouseListener(new MouseAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mouseDown(MouseEvent e)\n");
		b.append("{\n");
		b.append("final UrlLauncher launcher = RWT.getClient().getService(UrlLauncher.class);\n");

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append("launcher.openURL(");

		if (field.getFieldType() == FormFieldTypeEnumeration.MAIL_LINK)
			b.append("\"mailto:\" + ");

		b.append(modelObjectName + "." + getter + ");\n");
		b.append("}\n");
		b.append("});\n\n");

		return b.toString();
	}

}
