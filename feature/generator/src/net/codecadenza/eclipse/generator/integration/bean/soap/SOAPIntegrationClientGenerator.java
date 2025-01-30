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
package net.codecadenza.eclipse.generator.integration.bean.soap;

import static net.codecadenza.eclipse.shared.Constants.BASE_SOAP_CLIENT_CLASS_NAME;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;

/**
 * <p>
 * Generator for SOAP integration clients
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SOAPIntegrationClientGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public SOAPIntegrationClientGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getClientSourceFile());

		this.integrationBean = integrationBean;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("javax.xml.namespace");
		importPackage("jakarta.xml.ws");
		importPackage(integrationBean.getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@WebServiceClient(name = " + integrationBean.getInterfaceName() + ".SERVICE_NAME, ");
		b.append("targetNamespace = " + integrationBean.getInterfaceName() + ".NAMESPACE)\n");
		b.append("public class " + integrationBean.getClientClassName() + " extends " + BASE_SOAP_CLIENT_CLASS_NAME);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant("QName", "SERVICE", "new QName(" + integrationBean.getInterfaceName() + ".NAMESPACE, "
				+ integrationBean.getInterfaceName() + ".SERVICE_NAME)").create();
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
		b.append("super(AbstractSOAPClient.getWsdlURL(" + integrationBean.getInterfaceName());
		b.append(".SERVICE_NAME), SERVICE, userName, password);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final var identifier = integrationBean.getInterfaceName() + " getService()";

		b.append("/**\n");
		b.append(" * @return the service proxy\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@WebEndpoint(name = " + integrationBean.getInterfaceName() + ".PORT_NAME)\n");
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("final " + integrationBean.getInterfaceName());
		b.append(" service = super.getPort(new QName(" + integrationBean.getInterfaceName());
		b.append(".NAMESPACE, " + integrationBean.getInterfaceName() + ".PORT_NAME), ");
		b.append(integrationBean.getInterfaceName() + ".class);\n\n");
		b.append("addCredentials((BindingProvider) service);\n\n");
		b.append("return service;\n");
		b.append("}\n\n");

		addMethod(identifier, b.toString());
	}

}
