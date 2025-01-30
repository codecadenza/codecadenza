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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.FORM_TITLE;

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for views of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFViewGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final Project project;
	private final JSFI18NGenerator i18n;
	private final JSFSecurityGenerator securityHelper;
	private final JSFCommonDataTableGenerator tableGenerator;

	/**
	 * Constructor
	 * @param form
	 */
	public JSFViewGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.project = form.getDTO().getNamespace().getProject();
		this.securityHelper = new JSFSecurityGenerator(project);
		this.i18n = new JSFI18NGenerator(project);
		this.tableGenerator = new JSFCommonDataTableGenerator(this, form, i18n, securityHelper);
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
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(form.getName()) + "\")\n");

		if (project.isJakartaEEApplication()) {
			if (tableGenerator.isSaveQueries() || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
				b.append("@ViewScoped\n");
			else
				b.append("@SessionScoped\n");
		}
		else
			b.append("@SessionScope\n");

		b.append("public class ");
		b.append(form.getName());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			b.append(" extends AbstractSearchableView");

		b.append(" implements Serializable");
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
	 * Create the XHTML form content
	 * @return the generated content
	 */
	public String createXHTMLForm() {
		final var b = new StringBuilder();
		final String managedBeanName = JSFGeneratorUtil.createManagedBeanName(form.getName());
		var initMethodName = "initView";

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			initMethodName = "fetch" + form.getDTO().getDomainObject().getNamePlural();

		b.append(JSFGeneratorUtil.createCompositeHeader());
		b.append("<f:metadata>\n");
		b.append("\t<f:viewAction action=\"#{" + managedBeanName + "." + initMethodName + "()}\"/>\n");
		b.append("</f:metadata>\n\n");
		b.append("<ui:define name=\"title\">#{" + managedBeanName + "." + FORM_TITLE + "}</ui:define>\n\n");
		b.append("<ui:define name=\"content\">\n");
		b.append("\t<h:panelGrid columns=\"3\">\n");
		b.append("\t\t<div class=\"pi pi-list\" style=\"font-size: 2em\"/>\n");

		// Entity &#160; is equal to &nbsp;!
		b.append("\t\t<h:outputText id=\"lblFormTitle\" value=\"&#160;#{" + managedBeanName);
		b.append("." + FORM_TITLE + "}\" styleClass=\"label-form-title\"/>\n");

		b.append("\t\t<p:ajaxStatus style=\"width:16px;height:16px;\">\n");
		b.append("\t\t\t<f:facet name=\"start\">\n");
		b.append("\t\t\t\t<h:graphicImage value=\"/images/ajaxloading.gif\"/>\n");
		b.append("\t\t\t</f:facet>\n\n");
		b.append("\t\t\t<f:facet name=\"complete\">\n");
		b.append("\t\t\t\t<h:outputText value=\"\"/>\n");
		b.append("\t\t\t</f:facet>\n");
		b.append("\t\t</p:ajaxStatus>\n");
		b.append("\t</h:panelGrid>\n\n");
		b.append(tableGenerator.getXHTMLFragment());
		b.append("</ui:define>\n");
		b.append("</ui:composition>\n");

		i18n.save();

		return b.toString();
	}

}
