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
package net.codecadenza.eclipse.generator.client.imp.vaadin.security;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.LOGIN_VIEW;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for a listener that redirects unauthenticated requests to the login page
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinServiceInitListenerGenerator extends AbstractJavaSourceGenerator {
	public static final String CLASS_NAME = "ApplicationServiceInitListener";
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinServiceInitListenerGenerator(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString() + SUB_PACKAGE_UTIL;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, CLASS_NAME, packageName);
		javaFile.setComment("Listener that redirects unauthenticated requests to the login page");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage(project.getClientNamespace().toString());
		importPackage("org.springframework.security.authentication");
		importPackage("org.springframework.security.core");
		importPackage("org.springframework.security.core.context");
		importClass("org.springframework.stereotype.Component");
		importPackage("com.vaadin.flow.component");
		importPackage("com.vaadin.flow.router");
		importPackage("com.vaadin.flow.server");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Component\n");
		b.append("public class " + CLASS_NAME + " implements VaadinServiceInitListener");
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
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.server.VaadinServiceInitListener#serviceInit(com.vaadin.flow.server.ServiceInitEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void serviceInit(ServiceInitEvent event)\n");
		b.append("{\n");
		b.append("event.getSource().addUIInitListener(uiEvent -> {\n");
		b.append("final UI ui = uiEvent.getUI();\n");
		b.append("ui.addBeforeEnterListener(this::authenticateNavigation);\n");
		b.append("});\n");
		b.append("}\n\n");

		addMethod("void serviceInit(ServiceInitEvent event)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Check if the user is permitted to navigate to the requested target. ");
		b.append("Redirect the user to the login page if the authentication has not been performed yet!\n");
		b.append(" * @param event\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void authenticateNavigation(BeforeEnterEvent event)\n");
		b.append("{\n");
		b.append("if(!" + LOGIN_VIEW + ".class.equals(event.getNavigationTarget()) && !isAuthenticated())\n");
		b.append("event.rerouteTo(" + LOGIN_VIEW + ".class);\n");
		b.append("}\n\n");

		addMethod("void authenticateNavigation(BeforeEnterEvent event)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return true if the user is authenticated\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private boolean isAuthenticated()\n");
		b.append("{\n");
		b.append("final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();\n\n");
		b.append("return authentication != null && ");
		b.append("!(authentication instanceof AnonymousAuthenticationToken) && authentication.isAuthenticated();\n");
		b.append("}\n\n");

		addMethod("boolean isAuthenticated() ", b.toString());
	}

}
