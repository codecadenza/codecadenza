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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form;

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for grid panels of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFGridPanelGenerator extends AbstractJavaSourceGenerator {
	private final FormPanel panel;
	private final Project project;
	private final JSFI18NGenerator i18n;
	private final JSFSecurityGenerator securityHelper;
	private final JSFCommonDataTableGenerator tableGenerator;

	/**
	 * Constructor
	 * @param panel
	 */
	public JSFGridPanelGenerator(FormPanel panel) {
		super(panel.getSourceFile());

		this.panel = panel;
		this.project = panel.getDTO().getNamespace().getProject();
		this.securityHelper = new JSFSecurityGenerator(project);
		this.i18n = new JSFI18NGenerator(project);
		this.tableGenerator = new JSFCommonDataTableGenerator(this, panel, i18n, securityHelper);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(panel.getName()) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@ViewScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class ");
		b.append(panel.getName());
		b.append(" implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		tableGenerator.addImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		tableGenerator.addFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		tableGenerator.addMethods();

		i18n.save();
	}

	/**
	 * @return the content of the respective facelet file for this grid panel
	 */
	public String createXHTMLForm() {
		final var b = new StringBuilder();
		b.append("<ui:composition xmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txmlns:ui=\"jakarta.faces.facelets\"\n");
		b.append("\txmlns:h=\"jakarta.faces.html\"\n");
		b.append("\txmlns:f=\"jakarta.faces.core\"\n");
		b.append("\txmlns:p=\"http://primefaces.org/ui\">\n\n");
		b.append(tableGenerator.getXHTMLFragment());
		b.append("</ui:composition>\n");

		i18n.save();

		return b.toString();
	}

}
