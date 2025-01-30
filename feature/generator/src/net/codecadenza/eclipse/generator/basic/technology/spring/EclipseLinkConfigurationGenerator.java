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

import static net.codecadenza.eclipse.shared.Constants.ECLIPSE_LINK_CONFIG;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the configuration of EclipseLink in a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseLinkConfigurationGenerator extends AbstractJavaSourceGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public EclipseLinkConfigurationGenerator(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getDomainNamespace().toString();

		final var javaFile = new JavaFile(project, BuildArtifactType.DOMAIN, ECLIPSE_LINK_CONFIG, packageName);
		javaFile.setComment("Configuration class that is necessary for using EclipseLink");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("java.util");
		importClass("javax.sql.DataSource");
		importClass("org.springframework.beans.factory.ObjectProvider");
		importPackage("org.springframework.boot.autoconfigure.orm.jpa");
		importClass("org.springframework.context.annotation.Configuration");
		importPackage("org.springframework.orm.jpa.vendor");
		importPackage("org.springframework.transaction.jta");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Configuration\n");
		b.append("public class " + ECLIPSE_LINK_CONFIG + " extends JpaBaseConfiguration");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();

		var methodSignature = ECLIPSE_LINK_CONFIG + "(DataSource dataSource, JpaProperties properties, ";
		methodSignature += "ObjectProvider<JtaTransactionManager> jtaTransactionManager)";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param dataSource\n");
		b.append(" * @param properties\n");
		b.append(" * @param jtaTransactionManager\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super(dataSource, properties, jtaTransactionManager);\n");
		b.append("}\n\n");

		addConstructor(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		var methodSignature = "AbstractJpaVendorAdapter createJpaVendorAdapter()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.springframework.boot.autoconfigure.orm.jpa.JpaBaseConfiguration#createJpaVendorAdapter()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("return new EclipseLinkJpaVendorAdapter();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "Map<String, Object> getVendorProperties()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.springframework.boot.autoconfigure.orm.jpa.JpaBaseConfiguration#getVendorProperties()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var map = new HashMap<String, Object>();\n");

		for (final PersistenceUnitProperty property : project.getPersistenceUnitProperties())
			b.append("map.put(\"" + property.getName() + "\", \"" + property.getValue() + "\");\n");

		b.append("\n");
		b.append("return map;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
