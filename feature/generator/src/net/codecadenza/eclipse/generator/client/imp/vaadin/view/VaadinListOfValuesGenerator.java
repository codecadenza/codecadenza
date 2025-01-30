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
package net.codecadenza.eclipse.generator.client.imp.vaadin.view;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.List;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for list-of-values dialogs of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinListOfValuesGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final Project project;
	private final DTOBean dto;
	private final DTOBeanAttribute pkAttr;
	private final JavaType pkType;
	private final VaadinI18NGenerator i18n;
	private final BoundaryMethod method;

	/**
	 * Constructor
	 * @param form
	 */
	public VaadinListOfValuesGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.pkAttr = dto.getPKAttribute();
		this.pkType = pkAttr.getDomainAttribute().getJavaType();
		this.i18n = new VaadinI18NGenerator(project);
		this.method = form.getBoundaryMethod();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("java.util");
		importPackage("com.vaadin.flow.component.grid.Grid");
		importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");
		importPackage(dto.getNamespace().toString());

		if (dto.getDomainObject().isMandated()) {
			final var securityHelper = new VaadinSecurityHelper(project);

			addImports(securityHelper.getSecurityImports());
		}

		if (project.isSpringBootApplication())
			importClass("org.springframework.context.annotation.Scope");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isSpringBootApplication()) {
			b.append("@org.springframework.stereotype.Component\n");
			b.append("@Scope(\"prototype\")\n");
		}

		b.append("public class ");
		b.append(form.getName());
		b.append(" extends AbstractLOVDialog<" + dto.getName() + ", ");
		b.append(pkType.getWrapperTypeName());
		b.append(">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		new ServiceDeclarationGenerator(this, form.getBoundaryMethod().getBoundaryBean()).addField();

		if (dto.getDomainObject().isMandated())
			addProtectedField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final List<String> injectedFieldsOfSuperClass = List.of("I18NService i18n", "PreferencesStore preferences");

		addDefaultConstructorForInjection(injectedFieldsOfSuperClass);
		addConstructorForInjection(null, injectedFieldsOfSuperClass);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var methodSignature = "void setValues(" + dto.getName() + " selectedItem)";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractLOVDialog#setValues(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("selectedId = selectedItem." + pkAttr.getGetterName() + ";\n");

		if (dto.getDisplayAttribute() != null)
			b.append("selectedDisplayValue = selectedItem." + dto.getDisplayAttribute().getGetterName() + ";\n");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String getTitle()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractLOVDialog#getTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return " + i18n.getI18N(form) + ";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "List<" + dto.getName() + "> performSearchOperation(String filter)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.dialog.");
		b.append("AbstractLOVDialog#performSearchOperation(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(method, dto, b).addInvocation("filter");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void initGridColumns()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractLOVDialog#initGridColumns()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append(new VaadinCommonDataTableGenerator(this, form, i18n).createGridColumns());
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		// Add the method that implements getDialogLogger()
		addGetLoggerMethod("net.codecadenza.runtime.webclient.vaadin.dialog.AbstractLOVDialog", "getDialogLogger");

		i18n.save();
	}

}
