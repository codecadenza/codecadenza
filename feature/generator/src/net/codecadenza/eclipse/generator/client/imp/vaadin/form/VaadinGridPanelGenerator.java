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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for grid panels of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinGridPanelGenerator extends AbstractJavaSourceGenerator {
	private final FormPanel panel;
	private final Project project;
	private final DTOBean dto;
	private final VaadinI18NGenerator i18n;
	private final VaadinSecurityHelper securityHelper;
	private final VaadinCommonDataTableGenerator tableGenerator;
	private final BoundaryBean boundary;
	private final String parentIdType;

	/**
	 * Constructor
	 * @param panel
	 */
	public VaadinGridPanelGenerator(FormPanel panel) {
		super(panel.getSourceFile());

		this.panel = panel;
		this.dto = panel.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new VaadinI18NGenerator(project);
		this.securityHelper = new VaadinSecurityHelper(project);
		this.boundary = panel.getBoundaryMethod().getBoundaryBean();
		this.parentIdType = boundary.getDomainObject().getPKAttribute().getJavaType().getName();
		this.tableGenerator = new VaadinCommonDataTableGenerator(this, panel, i18n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("java.util");
		importPackage("net.codecadenza.runtime.webclient.vaadin.component");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");
		importClass("com.vaadin.flow.component.grid.Grid.Column");
		importPackage(dto.getNamespace().toString());

		if (project.isSpringBootApplication())
			importClass("org.springframework.context.annotation.Scope");

		addImports(securityHelper.getSecurityImports());

		tableGenerator.addActionImports();
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
		b.append(panel.getName());
		b.append(" extends AbstractDataGridPanel<" + dto.getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final Set<ServiceBean> services = new HashSet<>();
		services.add(boundary);

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		// Add a unique identifier
		addPublicConstant(JavaType.STRING, "ID",
				"\"" + project.getClientNamespace().toString() + PACK_CLIENT_PANEL + "." + panel.getName() + "\"").create();

		addPrivateField(parentIdType, "parentId").create();

		if (securityHelper.isSecurityAdded())
			addProtectedField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();

		new ServiceDeclarationGenerator(this, boundary).addField();

		for (final FormAction action : panel.getActions())
			if (action.getType() == ActionType.COPY || action.getType() == ActionType.DELETE) {
				final BoundaryBean actionBoundary = action.getBoundaryMethod().getBoundaryBean();

				if (!services.contains(actionBoundary)) {
					new ServiceDeclarationGenerator(this, actionBoundary).addField();
					services.add(actionBoundary);
				}
			}
			else if (!project.isBoundaryMode()
					&& (action.getType() == ActionType.DOWNLOAD_EXPORT || action.getType() == ActionType.UPLOAD_IMPORT)) {
				final DataExchangeMethod dataExchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
				final DataExchangeServiceBean dataExchangeBean = dataExchangeMethod.getDataExchangeServiceBean();

				if (!services.contains(dataExchangeBean)) {
					new ServiceDeclarationGenerator(this, action.getBoundaryMethod(), null).addField();
					services.add(dataExchangeBean);
				}
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var initFragment = "this.setId(\"grid" + panel.getName() + "\");\n";
		final List<String> injectedFieldsOfSuperClass = List.of("I18NService i18n", "PreferencesStore preferences");

		addDefaultConstructorForInjection(injectedFieldsOfSuperClass);
		addConstructorForInjection(initFragment, injectedFieldsOfSuperClass);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var methodSignature = "List<" + dto.getName() + "> fetchData()";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#fetchData()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(panel.getBoundaryMethod(), b).addInvocation("parentId");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void initGridColumns()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#initGridColumns()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append(tableGenerator.createGridColumns());
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		// Add the method that implements getDataGridLogger()
		addGetLoggerMethod("net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid", "getDataGridLogger");

		// Add the method for adding buttons to the button bar
		tableGenerator.createButtonBar();

		// Add the method for handling double-click events
		tableGenerator.createDoubleClick();

		// Add the getter and the setter for the parent ID
		addGetterAndSetter(parentIdType, "parentId", "the ID of the parent domain object");

		// Add the method for adding context menu items
		tableGenerator.addContextMenuItems();

		// Add methods for all form actions
		tableGenerator.addActionMethods();

		i18n.save();
	}

}
