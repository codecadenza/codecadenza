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
package net.codecadenza.eclipse.generator.integration.bean.rest;

import static net.codecadenza.eclipse.shared.Constants.BASE_REST_CLIENT_CLASS_NAME;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.rest.RESTMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for REST integration clients
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RESTIntegrationClientGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;
	private final RESTIntegrationBean restBean;
	private final Project project;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public RESTIntegrationClientGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getClientSourceFile());

		this.integrationBean = integrationBean;
		this.restBean = (RESTIntegrationBean) integrationBean;
		this.project = integrationBean.getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage(integrationBean.getNamespace().toString());

		if (!integrationBean.getMethods().isEmpty()) {
			importPackage("jakarta.ws.rs.core");
			importClass("jakarta.ws.rs.core.Response.Status");

			for (final AbstractIntegrationMethod method : integrationBean.getMethods()) {
				final var restMethod = (RESTIntegrationMethod) method;

				if (restMethod.getContentParameter().isPresent() && (restMethod.getHttpMethod() == HttpMethodEnumeration.POST
						|| restMethod.getHttpMethod() == HttpMethodEnumeration.PUT)) {
					importPackage("jakarta.ws.rs.client");
					break;
				}
			}
		}

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.inject");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		// The client and the service producer implement the same interface. If the application is deployed on a Jakarta EE server
		// the implementation should be provided by the producer!
		if (project.isJakartaEEApplication())
			b.append("@Alternative\n");

		b.append("public class " + integrationBean.getClientClassName() + " extends " + BASE_REST_CLIENT_CLASS_NAME + " ");
		b.append("implements " + integrationBean.getInterfaceName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		// Create the constant that holds the resource path
		if (!integrationBean.getMethods().isEmpty())
			addPrivateConstant(JavaType.STRING, "RESOURCE_PATH", "\"" + restBean.getPath() + "\"").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var identifier = integrationBean.getClientClassName() + "()";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Default constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());

		identifier = integrationBean.getClientClassName() + "(String userName, String password)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("super(userName, password);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var identifier = integrationBean.getInterfaceName() + " getService()";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the service instance\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("return this;\n");
		b.append("}\n\n");

		addMethod(identifier, b.toString());

		// Add service methods
		for (final AbstractIntegrationMethod method : integrationBean.getMethods()) {
			final var restMethod = (RESTIntegrationMethod) method;
			identifier = RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getInterfaceMethodSignature(false, false);

			addImports(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getInterfaceImports());

			b = new StringBuilder();
			b.append(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).createCommentLink());
			b.append(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).createClientMethod());

			addMethod(identifier, b.toString());
		}
	}

}
