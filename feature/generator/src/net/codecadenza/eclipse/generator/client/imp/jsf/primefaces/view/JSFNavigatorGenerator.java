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

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.shared.Constants.NAV_VIEW_NAME;

import net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the application tree navigator of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFNavigatorGenerator extends AbstractNavigatorGenerator {
	private static final String ROOT_NODE_NAME = "root";

	private final JSFI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public JSFNavigatorGenerator(Project project) {
		super(project, new JSFSecurityGenerator(project));

		this.i18n = new JSFI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, NAV_VIEW_NAME, project.getClientNamespace().toString());
		javaFile.setComment("Managed bean for tree navigator");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS);
		importStatic(project.getClientNamespace().toString() + "." + USER_SESSION_BEAN_TYPE);
		importClass("jakarta.annotation.PostConstruct");
		importPackage("org.primefaces.model");
		importPackage("java.util");
		importPackage("java.io");
		importPackage("jakarta.inject");
		importPackage("net.codecadenza.runtime.webclient.primefaces.tree");

		if (hasView || hasTree) {
			importPackage("jakarta.faces.context");
			importPackage("jakarta.servlet.http");
		}

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.context");
		else
			importClass("org.springframework.web.context.annotation.SessionScope");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(NAV_VIEW_NAME) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@SessionScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class " + NAV_VIEW_NAME + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateConstant(JavaType.STRING, "FOLDER_TYPE", "\"folder_type\"").create();
		addPrivateField("TreeNode<" + JavaType.STRING + ">", ROOT_NODE_NAME).withTransientModifier().create();
		addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();
		addPrivateField("ResourceBundle", "bundle").withTransientModifier().create();

		if (hasView || hasTree)
			addPrivateConstant(JavaType.STRING, "VIEW_TYPE", "\"view_type\"").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final var methodSignature = "void init()";

		b.append("/**\n");
		b.append(" * Initialize tree navigator\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@PostConstruct\n");
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (hasView || hasTree) {
			b.append("final FacesContext facesContext = FacesContext.getCurrentInstance();\n");
			b.append("final var req = (HttpServletRequest) facesContext.getExternalContext().getRequest();\n");
		}

		b.append("bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, " + USER_SESSION_BEAN + ".getLocale());\n");
		b.append("root = new DefaultTreeNode<>(\"" + getRootNodeName() + "\", null);\n\n");
		b.append(createNavigatorTreeStructure());
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		addGetter("TreeNode<" + JavaType.STRING + ">", getRootNodeName(), "the root element of the tree");

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
		formGroupNode.append("// Form group: " + group.getName() + "\n");
		formGroupNode.append("final var " + nodeName + " = new DefaultTreeNode<>(FOLDER_TYPE, ");
		formGroupNode.append("new TreeNavigatorItem(" + i18n.getI18NBundleFragment(group) + "), " + parentNodeName + ");\n");
		formGroupNode.append(nodeName + ".setExpanded(true);\n");
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
		var viewNode = "new DefaultTreeNode<>(VIEW_TYPE, new TreeNavigatorItem(" + i18n.getI18NBundleFragment(form);
		viewNode += ", req.getContextPath() + \"" + JSFGeneratorUtil.getFormRelativeURL(form) + "\"), " + parentNodeName + ");\n";

		return viewNode;
	}

}
