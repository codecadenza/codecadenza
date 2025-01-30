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
package net.codecadenza.eclipse.generator.testing.gui.imp;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for Selenium page objects that represent grid panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumGridPanelPageObjectGenerator extends AbstractJavaSourceGenerator {
	private final FormPanel panel;
	private final Project project;

	/**
	 * Constructor
	 * @param panel
	 */
	public SeleniumGridPanelPageObjectGenerator(FormPanel panel) {
		super(panel.getPageObjectSourceFile());

		this.panel = panel;
		this.project = panel.getDTO().getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("net.codecadenza.runtime.selenium.junit");

		if (project.hasAngularClient())
			importPackage("net.codecadenza.runtime.selenium.page.imp.angular");
		else if (project.hasJSFClient())
			importPackage("net.codecadenza.runtime.selenium.page.imp.primefaces");
		else
			importPackage("net.codecadenza.runtime.selenium.page.imp.vaadin");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + panel.getName() + " extends DataTableComponent");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final var methodSignature = panel.getName() + "(SeleniumTestContext testContext, String gridPanelId)";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param testContext\n");
		b.append(" * @param gridPanelId\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super(testContext, gridPanelId);\n");
		b.append("}\n\n");

		addConstructor(methodSignature, b.toString());
	}

}
