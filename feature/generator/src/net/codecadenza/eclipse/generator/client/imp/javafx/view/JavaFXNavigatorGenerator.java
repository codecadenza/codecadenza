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
package net.codecadenza.eclipse.generator.client.imp.javafx.view;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.NAV_VIEW_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the application tree navigator of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXNavigatorGenerator extends AbstractNavigatorGenerator {
	private static final String ROOT_NODE_NAME = "rootItem";

	private final RichClientI18NGenerator i18n;
	private int viewIndex = 1;

	/**
	 * Constructor
	 * @param project
	 */
	public JavaFXNavigatorGenerator(Project project) {
		super(project, new JavaFXSecurityHelper(project));

		this.i18n = new RichClientI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, NAV_VIEW_NAME, project.getClientNamespace().toString());
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
		importStaticClass("net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_FOLDER");
		importStaticClass("net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage");
		importPackage("javafx.scene.control");
		importPackage("javafx.scene.image");
		importPackage("net.codecadenza.runtime.richclient.javafx.control");

		if (hasView)
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_VIEW);

		if (hasTree)
			importPackage(project.getClientNamespace() + PACK_CLIENT_TREE);

		if (hasTree || hasView)
			importStaticClass("net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_VIEW");

		addImports(securityHelper.getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + NAV_VIEW_NAME + " extends AbstractViewNavigator");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final var methodSignature = NAV_VIEW_NAME + "(TabPane viewParent)";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param viewParent\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super(viewParent);\n");
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
		final var methodSignature = "void addTreeItems(TreeItem<View> " + getRootNodeName() + ")";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
		b.append("AbstractViewNavigator#addTreeItems(javafx.scene.control.TreeItem)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
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
		formGroupNode.append("final var " + nodeName + " = new TreeItem<View>(new ViewGroup(" + i18n.getI18N(group) + "));\n");
		formGroupNode.append(nodeName + ".setGraphic(new ImageView(getImage(IMG_FOLDER)));\n");
		formGroupNode.append(nodeName + ".setExpanded(true);\n");
		formGroupNode.append(parentNodeName + ".getChildren().add(" + nodeName + ");\n");
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
		final var nodeName = "itemView" + indexFormat.format(viewIndex++);

		var viewNode = "final var " + nodeName + " = new TreeItem<View>(new " + form.getName() + "());\n";
		viewNode += nodeName + ".setGraphic(new ImageView(getImage(IMG_VIEW)));\n";
		viewNode += parentNodeName + ".getChildren().add(" + nodeName + ");\n";

		return viewNode;
	}

}
