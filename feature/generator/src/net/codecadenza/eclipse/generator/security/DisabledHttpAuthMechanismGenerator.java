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
package net.codecadenza.eclipse.generator.security;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the class that disables the HTTP authentication mechanism of a rich-client application. As the transport protocol
 * internally uses the jakarta.security.enterprise.SecurityContext a security mechanism must be available. Otherwise, it could be
 * the case that the deployment will fail!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DisabledHttpAuthMechanismGenerator extends AbstractJavaSourceGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public DisabledHttpAuthMechanismGenerator(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getRootNamespace().toString();
		final var comment = "This class is necessary to avoid deployment problems when using the internal transport protocol if no security is in place!";

		final var javaFile = new JavaFile(project, BuildArtifactType.SERVER, "DisabledHttpAuthenticationMechanism", packageName);
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.enterprise.context");
		importPackage("jakarta.security.enterprise.authentication.mechanism.http");
		importPackage("jakarta.security.enterprise");
		importPackage("jakarta.servlet.http");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@ApplicationScoped\n");
		b.append("public class DisabledHttpAuthenticationMechanism implements HttpAuthenticationMechanism");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final var methodSignature = "AuthenticationStatus validateRequest(HttpServletRequest request, HttpServletResponse response, HttpMessageContext httpMessageContext)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.security.enterprise.authentication.mechanism.http.HttpAuthenticationMechanism#");
		b.append("validateRequest(jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse, ");
		b.append("jakarta.security.enterprise.authentication.mechanism.http.HttpMessageContext)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public " + methodSignature + " throws AuthenticationException\n");
		b.append("{\n");
		b.append("return AuthenticationStatus.SUCCESS;\n");
		b.append("}\n");

		addMethod(methodSignature, b.toString());
	}

}
