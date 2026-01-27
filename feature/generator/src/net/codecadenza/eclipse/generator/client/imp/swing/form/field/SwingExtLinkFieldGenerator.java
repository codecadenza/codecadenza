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
public class SwingExtLinkFieldGenerator extends AbstractSwingFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public SwingExtLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		formGenerator.importClass("org.jdesktop.swingx.JXHyperlink");
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

		formGenerator.addPrivateField("JXHyperlink", field.getName()).create();
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
		b.append(field.getName() + ".setText(" + modelObjectName + "." + getter + ");\n");

		if (!checkStatement.isEmpty())
			b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.swing.form.field.AbstractSwingFieldGenerator#
	 * getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String getter = field.getDTOAttribute().getModelGetterName();
		final String formName = field.getPanel().getForm().getName();
		final var notSupportedMsg = "This operating system doesn't support opening links in external applications!";

		b.append(super.getFieldDefinitionFragment(hasOneColumn));
		b.append(field.getName() + ".addActionListener(_ ->\n");
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
		b.append("catch (final IllegalArgumentException e)\n");
		b.append("{\n");
		b.append("JOptionPane.showMessageDialog(" + formName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_link_arg", "Link must not contain an empty address!"));
		b.append(", " + i18n.getI18NMessage("msg_title_link", "Open link") + ", JOptionPane.WARNING_MESSAGE);\n");
		b.append("}\n");
		b.append("catch (final IllegalStateException e)\n");
		b.append("{\n");
		b.append("JOptionPane.showMessageDialog(" + formName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_link_os", notSupportedMsg));
		b.append(", " + i18n.getI18NMessage("msg_title_link", "Open link") + ", JOptionPane.INFORMATION_MESSAGE);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Unexpected error while opening external link!", "e");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(" + formName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_link_ex", "Error while opening selected link in external application! Message: "));
		b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_link", "Open link") + ", JOptionPane.ERROR_MESSAGE);\n");
		b.append("}\n");
		b.append("});\n\n");

		return b.toString();
	}

}
