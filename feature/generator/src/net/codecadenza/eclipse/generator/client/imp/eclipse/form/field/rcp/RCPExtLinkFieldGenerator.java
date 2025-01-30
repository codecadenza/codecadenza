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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.rcp;

import net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;

/**
 * <p>
 * RCP implementation of a generator for web and email link fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RCPExtLinkFieldGenerator extends AbstractEclipseFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public RCPExtLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		formGenerator.importPackage("org.eclipse.swt.events");
		formGenerator.importPackage("net.codecadenza.runtime.richclient.util");
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
		final var msg = "Error while opening selected link in external application! Message: ";
		final var errorMsg = "This operating system doesn't support opening links in external applications!";

		b.append(field.getName() + " = new Link(" + panel.getName() + ", SWT.NONE);\n\n");
		b.append(field.getName() + ".addMouseListener(new MouseAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mouseDown(MouseEvent e)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append(getCheckFragment());
		b.append("new DesktopHelper(" + modelObjectName + "." + getter + ").");

		if (field.getFieldType() == FormFieldTypeEnumeration.WEB_LINK)
			b.append("openWebLink();\n");
		else
			b.append("openEmailLink();\n");

		b.append("}\n");
		b.append("catch (final IllegalArgumentException ex)\n");
		b.append("{\n");
		b.append("MessageDialog.openWarning(parentShell, " + i18n.getI18NMessage("msg_title_link", "Open link") + ", ");
		b.append(i18n.getI18NMessage("msg_err_link_arg", "Link must not contain an empty address!"));
		b.append(");\n");
		b.append("}\n");
		b.append("catch (final IllegalStateException ex)\n");
		b.append("{\n");
		b.append("MessageDialog.openInformation(parentShell, " + i18n.getI18NMessage("msg_title_link", "Open link") + ", ");
		b.append(i18n.getI18NMessage("msg_err_link_os", errorMsg) + ");\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Unexpected error while opening link!", "ex");

		b.append("\n");
		b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_link", "Open link") + ", ");
		b.append(i18n.getI18NMessage("msg_err_link_ex", msg) + " + ex.getMessage());\n");
		b.append("}\n");
		b.append("}\n");
		b.append("});\n\n");

		return b.toString();
	}

}
