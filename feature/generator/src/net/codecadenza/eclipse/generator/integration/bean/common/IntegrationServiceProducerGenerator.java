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
package net.codecadenza.eclipse.generator.integration.bean.common;

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SECURITY;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SEI;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for integration service producers
 * </p>
 * <p>
 * Copyright 2018 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationServiceProducerGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;
	private final Project project;
	private final IntegrationModule module;
	private final IntegrationTechnology integrationTechnology;
	private final boolean addCredentials;
	private final boolean hasClientField;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public IntegrationServiceProducerGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getProducerSourceFile());

		this.integrationBean = integrationBean;
		this.project = integrationBean.getNamespace().getProject();
		this.module = integrationBean.getIntegrationModule();
		this.integrationTechnology = integrationBean.getIntegrationTechnology();
		this.addCredentials = project.getApplicationLogOnDTO() != null && integrationTechnology != IntegrationTechnology.KAFKA
				&& integrationTechnology != IntegrationTechnology.JMS;
		this.hasClientField = integrationTechnology == IntegrationTechnology.REST
				|| integrationTechnology == IntegrationTechnology.KAFKA || integrationTechnology == IntegrationTechnology.JMS;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("java.io");
		importPackage(module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT);

		if (integrationTechnology == IntegrationTechnology.KAFKA)
			importPackage(module.getNamespace().toString() + SUB_PACKAGE_INT_SEI);
		else
			importPackage(module.getNamespace().toString());

		if (hasClientField)
			importClass("jakarta.annotation.PreDestroy");
		else if (integrationTechnology == IntegrationTechnology.RMI)
			importPackage("javax.naming");

		if (addCredentials)
			importPackage(module.getNamespace().toString() + SUB_PACKAGE_INT_SECURITY);

		if (project.isJakartaEEApplication()) {
			importPackage("jakarta.enterprise.inject");
			importPackage("jakarta.enterprise.context");
		}
		else {
			importPackage("org.springframework.context.annotation");
			importPackage("org.springframework.stereotype");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isSpringBootApplication()) {
			b.append("@Configuration\n");
			b.append("@Component\n");
		}
		else
			b.append("@SessionScoped\n");

		b.append("public class " + integrationBean.getProducerClassName() + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		if (hasClientField) {
			final JavaFieldGenerator fieldGenerator = addPrivateField(integrationBean.getClientClassName(), "client");

			if (integrationTechnology != IntegrationTechnology.REST)
				fieldGenerator.withTransientModifier();

			fieldGenerator.create();
		}

		if (addCredentials && project.isJakartaEEApplication())
			addPrivateField(module.getCredentialsProviderName(), "credentialsProvider").inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var label = integrationBean.getDomainObject().getLabel() + " " + integrationBean.getIntegrationTechnology().getName()
				+ " service";
		final var param = addCredentials && project.isSpringBootApplication()
				? module.getCredentialsProviderName() + " credentialsProvider" : "";
		var methodSignature = integrationBean.getInterfaceName() + " get" + integrationBean.getInterfaceName() + "(" + param + ")";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Producer method for injecting the " + label + "\n");

		if (addCredentials && project.isSpringBootApplication())
			b.append(" * @param credentialsProvider\n");

		b.append(" * @return an instance of the " + label + "\n");

		if (integrationTechnology == IntegrationTechnology.RMI)
			b.append(" * @throws IllegalStateException if the service could not be created\n");

		b.append(" */\n");

		if (project.isJakartaEEApplication()) {
			b.append("@Produces\n");
			b.append("@SessionScoped\n");
		}
		else
			b.append("@Bean\n");

		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Create service instance");

		b.append("\n");

		if (integrationTechnology == IntegrationTechnology.RMI) {
			b.append("try\n");
			b.append("{\n");
		}

		if (hasClientField)
			b.append("client = ");
		else
			b.append("return ");

		b.append("new " + integrationBean.getClientClassName() + "(");

		if (addCredentials)
			b.append("credentialsProvider.getUserName(), credentialsProvider.getPassword()");

		if (hasClientField) {
			b.append(");\n");

			if (integrationTechnology == IntegrationTechnology.JMS && !integrationBean.getMethods().isEmpty())
				b.append("client.init();\n");

			b.append("\n");

			if (integrationTechnology == IntegrationTechnology.REST)
				b.append("return client.getService();\n");
			else
				b.append("return client;\n");
		}
		else
			b.append(").getService();\n");

		if (integrationTechnology == IntegrationTechnology.RMI) {
			b.append("}\n");
			b.append("catch (final NamingException e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while creating service!", "e");

			b.append("\n");
			b.append("throw new IllegalStateException(e);\n");
			b.append("}\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (hasClientField) {
			methodSignature = "void close()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Release internal resources\n");
			b.append(" */\n");
			b.append("@PreDestroy\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			addDebugLog(b, "Release service resources");

			b.append("\n");
			b.append("client.close();\n");
			b.append("}\n");

			addMethod(methodSignature, b.toString());
		}
	}

}
