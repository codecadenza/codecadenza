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

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.SAVED_QUERIES_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for a view that is responsible for managing saved queries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinSavedQueryViewGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final VaadinI18NGenerator i18n;
	private final VaadinSecurityHelper securityHelper;
	private final String logOnPKGetter;
	private final String locale;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinSavedQueryViewGenerator(Project project) {
		this.project = project;
		this.i18n = new VaadinI18NGenerator(project);
		this.securityHelper = new VaadinSecurityHelper(project);
		this.logOnPKGetter = project.getApplicationLogOnDTO().getPKAttribute().getModelGetterName();
		this.locale = i18n.getLocaleFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString();

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, SAVED_QUERIES_VIEW, packageName);
		javaFile.setComment("View for maintaining saved queries");

		return javaFile;
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
		importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");
		importPackage("com.vaadin.flow.component");
		importPackage("com.vaadin.flow.component.button");
		importPackage("com.vaadin.flow.component.html");
		importPackage("com.vaadin.flow.component.listbox");
		importPackage("com.vaadin.flow.component.orderedlayout");
		importPackage("com.vaadin.flow.router");
		importPackage(project.getRootNamespace().toString() + PACK_SERVICE);

		addImports(securityHelper.getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Route(value = " + SAVED_QUERIES_VIEW + ".ROUTE + \"/:id\", layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + SAVED_QUERIES_VIEW);
		b.append(" extends VerticalLayout implements BeforeEnterObserver, HasDynamicTitle");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPublicConstant(JavaType.STRING, "ROUTE", "\"user/" + SAVED_QUERIES_VIEW + "\"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateField(JavaType.STRING, "dialogTitle").create();
		addPrivateField("ListBox<String>", "listSavedQueries").withDefaultValue("new ListBox<>()").create();
		addPrivateField(JavaType.STRING, "viewId").create();
		addPrivateField("Navigator", "navigator").withDefaultValue("new Navigator(this)").create();
		addPrivateField(SAVED_QUERY_SERVICE, "queryManager").inject().create();
		addProtectedField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();
		addPrivateField("I18NService", "i18n").inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var noObjSelectedMessage = i18n.getI18NMessage("msg_no_object_selected", "No object selected!");
		final var errorMessage = i18n.getI18NMessage("msg_err_delete_query", "Could not delete selected saved query!");
		final var dialogTitle = i18n.getI18NMessage("msg_title_saved_queries", "Execute a saved query");

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void beforeEnter(BeforeEnterEvent event)\n");
		b.append("{\n");

		addDebugLog(b, "Initialize view");

		b.append("\n");
		b.append("dialogTitle = " + dialogTitle + ";\n");
		b.append("listSavedQueries.setWidth(500, Unit.PIXELS);\n");
		b.append("listSavedQueries.setHeight(400, Unit.PIXELS);\n\n");
		b.append("final var lblList = new NativeLabel(");
		b.append(i18n.getI18NMessage("field_savedqueryview_lblsavedqueries", "Available saved queries", true));
		b.append(");\n\n");
		b.append("final var panList = new BorderPanel(null);\n");
		b.append("panList.add(listSavedQueries);\n\n");
		b.append("final var cmdRun = new Button(" + i18n.getI18NMessage("cmd_run", "Run") + ");\n");
		b.append("cmdRun.addClickListener(_ -> openSavedQuery());\n\n");
		b.append("final var cmdDelete = new Button(" + i18n.getI18NMessage("cmd_delete", "Delete") + ");\n");
		b.append("cmdDelete.addClickListener(_ -> deleteSavedQuery());\n\n");
		b.append("final var cmdCancel = new Button(" + i18n.getI18NMessage("cmd_cancel", "Cancel") + ");\n");
		b.append("cmdCancel.addClickListener(_ -> navigator.navigateBack());\n\n");
		b.append("final var hlButtons = new HorizontalLayout();\n");
		b.append("hlButtons.add(cmdRun, cmdDelete, cmdCancel);\n\n");
		b.append("add(lblList, panList, hlButtons);\n\n");

		addDebugLog(b, "View initialization finished");

		b.append("\n");
		b.append("viewId = navigator.getStringIdParameter(event);\n\n");
		b.append("listSavedQueries.setItems(Collections.emptyList());\n\n");
		b.append("if(viewId != null && !viewId.isEmpty())\n");
		b.append("listSavedQueries.setItems(queryManager.getSavedQueries(" + MANAGED_SECURITY_MANAGER);
		b.append(".getLogOnDTO()." + logOnPKGetter + ", viewId));\n");
		b.append("}\n\n");

		addMethod("void beforeEnter(BeforeEnterEvent event)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.HasDynamicTitle#getPageTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getPageTitle()\n");
		b.append("{\n");
		b.append("return " + dialogTitle + ";\n");
		b.append("}\n\n");

		addMethod("String getPageTitle()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Open selected query\n");
		b.append(" */\n");
		b.append("@SuppressWarnings(\"unchecked\")\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void openSavedQuery()\n");
		b.append("{\n");
		b.append("if(listSavedQueries.getValue() == null)\n");
		b.append("{\n");
		b.append("new InfoMessageDialog(dialogTitle, " + noObjSelectedMessage + ", " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("navigator.navigateTo((Class<Component>) Class.forName(viewId), listSavedQueries.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Navigation to target view failed!", "e");

		b.append("}\n");
		b.append("}\n\n");

		addMethod("void openSavedQuery()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Delete selected query\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void deleteSavedQuery()\n");
		b.append("{\n");
		b.append("if(listSavedQueries.getValue() == null)\n");
		b.append("{\n");
		b.append("new InfoMessageDialog(dialogTitle, " + noObjSelectedMessage + ", " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("queryManager.deleteSavedQuery(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO().");
		b.append(logOnPKGetter + ", viewId, listSavedQueries.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while deleting saved query!", "e");

		b.append("\n");
		b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("// Refresh the list that contains the saved queries\n");
		b.append("listSavedQueries.setItems(queryManager.getSavedQueries(" + MANAGED_SECURITY_MANAGER);
		b.append(".getLogOnDTO()." + logOnPKGetter + ", viewId));\n");
		b.append("}\n\n");

		addMethod("void deleteSavedQuery()", b.toString());

		i18n.save();
	}

}
