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
public class JavaFXExtLinkFieldGenerator extends AbstractJavaFXFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JavaFXExtLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
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

		formGenerator.importPackage("net.codecadenza.runtime.richclient.util");
		formGenerator.importPackage("javafx.concurrent");
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
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final String getter = field.getDTOAttribute().getModelGetterName();
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		b.append(field.getName() + " = new Hyperlink();\n");
		b.append(field.getName() + ".setOnAction(event ->\n");
		b.append("{\n");
		b.append("final var task = new Task<Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#call()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Void call() throws Exception\n");
		b.append("{\n");
		b.append(getCheckFragment());
		b.append("new DesktopHelper(" + modelObjectName + "." + getter + ").");

		if (field.getFieldType() == FormFieldTypeEnumeration.WEB_LINK)
			b.append("openWebLink();\n");
		else
			b.append("openEmailLink();\n");

		if (!getCheckFragment().isEmpty())
			b.append("\n");

		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#failed()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void failed()\n");
		b.append("{\n");
		b.append("final String errorMessage = ");
		b.append(i18n.getI18NMessage("msg_err_link_ex", "Error while opening selected link in external application!") + ";\n\n");
		b.append("DialogUtil.openErrorDialog(" + form.getName() + ".this, errorMessage, getException());\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("new Thread(task).start();\n");
		b.append("});\n\n");
		b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
