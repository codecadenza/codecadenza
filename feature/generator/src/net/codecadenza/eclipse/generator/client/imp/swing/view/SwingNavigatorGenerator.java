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
package net.codecadenza.eclipse.generator.client.imp.swing.view;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.NAV_PANEL_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the application tree navigator of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingNavigatorGenerator extends AbstractNavigatorGenerator {
	private static final String ROOT_NODE_NAME = "rootNode";

	private final RichClientI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public SwingNavigatorGenerator(Project project) {
		super(project, new SwingSecurityHelper(project));

		this.i18n = new RichClientI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString();

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, NAV_PANEL_NAME, packageName);
		javaFile.setComment("Application tree view navigator");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importClass("javax.swing.JDesktopPane");
		importPackage("net.codecadenza.runtime.richclient.swing.image");
		importPackage("net.codecadenza.runtime.richclient.swing.widget");

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
		b.append("public class " + NAV_PANEL_NAME + " extends AbstractNavigationPanel");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final var methodSignature = NAV_PANEL_NAME + "(JDesktopPane desktop)";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param desktop\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super(desktop);\n");
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
		final var methodSignature = "initializeNodes(DataTreeNode " + getRootNodeName() + ")";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractNavigationPanel#");
		b.append("initializeNodes(net.codecadenza.runtime.richclient.swing.widget.DataTreeNode)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void " + methodSignature + "\n");
		b.append("{\n");
		b.append("var groupLabel = \"\";\n");

		if (hasView || hasTree)
			b.append("var viewLabel = \"\";\n");

		b.append(createNavigatorTreeStructure());
		b.append("}\n\n");

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
		formGroupNode.append("groupLabel = " + i18n.getI18N(group) + ";\n");
		formGroupNode.append("final var " + nodeName + " = new DataTreeNode(" + parentNodeName);
		formGroupNode.append(", groupLabel, ImageLoader.getImage(ImageLoader.FOLDER));\n");
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
		var viewNode = "viewLabel = " + i18n.getI18N(form) + ";\n";
		viewNode += "new DataTreeNode(" + parentNodeName + ", viewLabel, ImageLoader.getImage(ImageLoader.VIEW), new "
				+ form.getName() + "(";

		if (form.getFormType() != FormTypeEnumeration.TREE_VIEW)
			viewNode += "viewLabel";

		viewNode += "));\n";

		return viewNode;
	}

}
