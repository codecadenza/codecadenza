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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.soap.BasicSOAPMethodGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.soap.SOAPMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for SOAP service end-point interfaces
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SOAPIntegrationSEIGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;
	private final SOAPIntegrationBean soapBean;
	private final Project project;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public SOAPIntegrationSEIGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getSEISourceFile());

		this.integrationBean = integrationBean;
		this.soapBean = (SOAPIntegrationBean) integrationBean;
		this.project = integrationBean.getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.jws");
		importPackage("jakarta.jws.soap");
		importPackage("jakarta.jws.soap.SOAPBinding");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@WebService(");
		b.append("name = " + integrationBean.getInterfaceName() + ".NAME, ");
		b.append("targetNamespace = " + integrationBean.getInterfaceName() + ".NAMESPACE, ");
		b.append("serviceName = " + integrationBean.getInterfaceName() + ".SERVICE_NAME, ");
		b.append("portName = " + integrationBean.getInterfaceName() + ".PORT_NAME");
		b.append(")\n");
		b.append("@SOAPBinding(parameterStyle = ParameterStyle.");

		if (soapBean.isBareParameterStyle())
			b.append("BARE");
		else
			b.append("WRAPPED");

		b.append(", style=Style.");

		if (soapBean.isRpcStype())
			b.append("RPC");
		else
			b.append("DOCUMENT");

		b.append(", use = Use.ENCODED)\n");
		b.append("public interface " + integrationBean.getInterfaceName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (soapBean.getPortTypeName() != null && !soapBean.getPortTypeName().isEmpty())
			addField(JavaType.STRING, "NAME", "\"" + soapBean.getPortTypeName() + "\"").create();
		else
			addField(JavaType.STRING, "NAME", "\"" + soapBean.getName() + "\"").create();

		addField(JavaType.STRING, "NAMESPACE", "\"" + project.getXmlNamespace() + "\"").create();
		addField(JavaType.STRING, "SERVICE_NAME", "\"" + soapBean.getServiceName() + "\"").create();
		addField(JavaType.STRING, "PORT_NAME", "\"" + soapBean.getPortName() + "\"").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		integrationBean.getMethods().stream().map(SOAPIntegrationMethod.class::cast).forEach(soapMethod -> {
			final var b = new StringBuilder();
			final BasicSOAPMethodGenerator soapMethodGenerator = SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this);

			addImports(soapMethodGenerator.getInterfaceImports());

			// Add the comment
			b.append(soapMethodGenerator.createComment());

			// Add the declaration
			b.append(getAnnotationForGeneratedElement());
			b.append(soapMethodGenerator.getMethodSignature(false, true, false));
			b.append(";\n\n");

			addMethod(soapMethodGenerator.getMethodSignature(false, false, false), b.toString());
		});
	}

}
