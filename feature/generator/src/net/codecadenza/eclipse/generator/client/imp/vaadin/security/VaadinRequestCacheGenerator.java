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

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for a cache that caches internal Vaadin requests in a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinRequestCacheGenerator extends AbstractJavaSourceGenerator {
	public static final String CLASS_NAME = "ApplicationRequestCache";
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinRequestCacheGenerator(Project project) {
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
		javaFile.setComment("Cache for requests that have been issued by Vaadin");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("org.springframework.security.web.savedrequest");
		importPackage("com.vaadin.flow.server.HandlerHelper");
		importPackage("com.vaadin.flow.shared");
		importPackage("java.util.stream");
		importPackage("jakarta.servlet.http");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + CLASS_NAME + " extends HttpSessionRequestCache");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.springframework.security.web.savedrequest.HttpSessionRequestCache#saveRequest(");
		b.append("jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void saveRequest(HttpServletRequest request, HttpServletResponse response)\n");
		b.append("{\n");
		b.append("if(!isFrameworkInternalRequest(request))\n");
		b.append("super.saveRequest(request, response);\n");
		b.append("}\n\n");

		addMethod("void saveRequest(HttpServletRequest request, HttpServletResponse response)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @param request\n");
		b.append(" * @return true if the given HTTP request has been issued by Vaadin\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static boolean isFrameworkInternalRequest(HttpServletRequest request)\n");
		b.append("{\n");
		b.append("final String parameterValue = request.getParameter(ApplicationConstants.REQUEST_TYPE_PARAMETER);\n\n");
		b.append("return parameterValue != null && ");
		b.append("Stream.of(RequestType.values()).anyMatch(r -> r.getIdentifier().equals(parameterValue));\n");
		b.append("}\n\n");

		addMethod("boolean isFrameworkInternalRequest(HttpServletRequest request)", b.toString());
	}

}
