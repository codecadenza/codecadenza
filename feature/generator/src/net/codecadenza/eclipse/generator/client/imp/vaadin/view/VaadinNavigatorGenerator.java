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

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.CHANGE_PASSWORD_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.PREFERENCES_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.TREE_NAVIGATOR;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the application tree navigator of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinNavigatorGenerator extends AbstractNavigatorGenerator {
	private static final String ROOT_NODE_NAME = "null";

	private final VaadinI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinNavigatorGenerator(Project project) {
		super(project, new VaadinSecurityHelper(project));

		this.i18n = new VaadinI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString();

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, TREE_NAVIGATOR, packageName);
		javaFile.setComment("Application tree view navigator");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("net.codecadenza.runtime.webclient.vaadin.tree");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");

		if (hasView)
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_VIEW);

		if (hasTree)
			importPackage(project.getClientNamespace() + PACK_CLIENT_TREE);

		addImports(securityHelper.getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + TREE_NAVIGATOR + " extends AbstractTreeNavigator");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		if (securityHelper.isSecurityAdded())
			addPrivateField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).withFinalModifier().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		var methodSignature = TREE_NAVIGATOR + "(I18NService i18n";

		if (securityHelper.isSecurityAdded())
			methodSignature += ", " + SECURITY_MANAGER + " " + MANAGED_SECURITY_MANAGER;

		methodSignature += ")";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param i18n\n");

		if (securityHelper.isSecurityAdded())
			b.append(" * @param " + MANAGED_SECURITY_MANAGER + "\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super(i18n);\n");

		if (securityHelper.isSecurityAdded()) {
			b.append("\n");
			b.append("this." + MANAGED_SECURITY_MANAGER + " = " + MANAGED_SECURITY_MANAGER + ";\n");
		}

		b.append("}\n\n");

		addConstructor(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final var methodSignature = "void addTreeItems()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeNavigator#addTreeItems()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("addViewItem(null, " + i18n.getI18NMessage("navigator_item_home", "Home") + ", HomeView.ROUTE);\n");
		b.append("addViewItem(null, " + i18n.getI18NMessage("navigator_item_preferences", "Preferences"));
		b.append(", " + PREFERENCES_VIEW + ".ROUTE);\n");

		if (securityHelper.isSecurityAdded()) {
			b.append("addViewItem(null, " + i18n.getI18NMessage("navigator_item_change_password", "Change password"));
			b.append(", " + CHANGE_PASSWORD_VIEW + ".ROUTE);\n");
		}

		b.append(createNavigatorTreeStructure());
		b.append("}\n");

		addMethod(methodSignature, b.toString());

		i18n.save();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator#getRootNodeName()
	 */
	@Override
	protected String getRootNodeName() {
		return ROOT_NODE_NAME;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator#addFormGroup(java.lang.String,
	 * java.lang.String, net.codecadenza.eclipse.model.client.FormGroup)
	 */
	@Override
	protected String addFormGroup(String parentNodeName, String nodeName, FormGroup group) {
		final var formGroupNode = new StringBuilder();
		formGroupNode.append("final TreeItem " + nodeName + " = addFolderItem(");
		formGroupNode.append(parentNodeName + ", " + i18n.getI18N(group) + ");\n");
		formGroupNode.append(addGroupsToParent(nodeName, group));

		return formGroupNode.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator#addView(java.lang.String,
	 * net.codecadenza.eclipse.model.client.Form)
	 */
	@Override
	protected String addView(String parentNodeName, Form form) {
		return "addViewItem(" + parentNodeName + ", " + i18n.getI18N(form) + ", " + form.getName() + ".ROUTE);\n";
	}

}
