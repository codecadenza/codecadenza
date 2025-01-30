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
package net.codecadenza.eclipse.generator.basic.technology.spring;

import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the logging aspect of a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingAspectGenerator extends AbstractJavaSourceGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public LoggingAspectGenerator(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var comment = new StringBuilder();
		final String packageName = project.getRootNamespace().toString() + PACK_SERVICE;
		final String mode = project.isBoundaryMode() ? "boundary" : "facade";

		comment.append("Aspect that is responsible for logging ");
		comment.append(mode);
		comment.append(" method invocations");

		final var javaFile = new JavaFile(project, BuildArtifactType.SERVICE, "ApplicationLoggingAspect", packageName);
		javaFile.setComment(comment.toString());

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("net.codecadenza.runtime.server.logging");
		importClass("org.aspectj.lang.ProceedingJoinPoint");
		importClass("org.aspectj.lang.annotation.Aspect");
		importClass("org.springframework.context.annotation.Configuration");
		importClass("org.aspectj.lang.annotation.Around");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Aspect\n");
		b.append("@Configuration\n");
		b.append("public class ApplicationLoggingAspect extends LoggingAspect");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final StringBuilder b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param logger\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public ApplicationLoggingAspect(LoggingService logger)\n");
		b.append("{\n");
		b.append("super(logger);\n");
		b.append("}\n\n");

		addConstructor("ApplicationLoggingAspect(LoggingService logger)", b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final var methodSignature = "Object logAround(ProceedingJoinPoint joinPoint)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.server.logging.");
		b.append("LoggingAspect#logAround(org.aspectj.lang.ProceedingJoinPoint)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("@Around(\"within(" + project.getBoundaryNamespace().toString() + "..*)\")\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + " throws Throwable\n");
		b.append("{\n");
		b.append("return super.logAround(joinPoint);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
