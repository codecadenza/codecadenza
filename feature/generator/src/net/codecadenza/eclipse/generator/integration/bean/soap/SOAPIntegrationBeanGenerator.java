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

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.soap.SOAPMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for SOAP integration beans
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SOAPIntegrationBeanGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;
	private final Project project;
	private final Map<String, ServiceBean> serviceMap = new HashMap<>();
	private final Map<String, String> resourceMap = new HashMap<>();
	private final Map<String, String> containerServiceSet = new HashMap<>();
	private final Map<String, String> constantMap = new HashMap<>();

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public SOAPIntegrationBeanGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getServiceBeanSourceFile());

		this.integrationBean = integrationBean;
		this.project = integrationBean.getNamespace().getProject();

		integrationBean.getMethods().stream().map(SOAPIntegrationMethod.class::cast).forEach(soapMethod -> {
			this.serviceMap.putAll(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).getServices());
			this.resourceMap.putAll(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).getResources());
			this.containerServiceSet.putAll(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).getContainerServices());
			this.constantMap.putAll(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).getConstants());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.jws");
		importPackage(integrationBean.getNamespace().toString());

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.context");
		else
			importClass("org.springframework.stereotype.Service");

		integrationBean.getMethods().forEach(method -> {
			final var soapMethod = (SOAPIntegrationMethod) method;

			addImports(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).getImports());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isJakartaEEApplication())
			b.append("@RequestScoped\n");
		else
			b.append("@Service\n");

		b.append("@WebService(");
		b.append("targetNamespace = " + integrationBean.getInterfaceName() + ".NAMESPACE, ");
		b.append("serviceName = " + integrationBean.getInterfaceName() + ".SERVICE_NAME, ");
		b.append("portName = " + integrationBean.getInterfaceName() + ".PORT_NAME");
		b.append(")\n");
		b.append("@HandlerChain(file = \"");

		// When deploying the application on Glassfish the name of the handler chain file must not start with a slash!
		if (!project.isDeployedOnGlassfish())
			b.append("/");

		b.append("handler-chain-server.xml\")\n");
		b.append("public class " + integrationBean.getName());
		b.append(" implements ");
		b.append(integrationBean.getInterfaceName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		// Add constants
		constantMap.entrySet().forEach(entry -> {
			final int delimiterPos = entry.getKey().lastIndexOf(" ");
			final String typeName = entry.getKey().substring(0, delimiterPos);
			final String fieldName = entry.getKey().substring(delimiterPos + 1);

			addPrivateConstant(typeName, fieldName, entry.getValue()).create();
		});

		// Add declarations for all services that must be injected
		serviceMap.keySet().forEach(serviceName -> {
			final ServiceBean serviceBean = serviceMap.get(serviceName);
			final String serviceType = serviceBean.getInterfaceName() != null ? serviceBean.getInterfaceName() : serviceBean.getName();

			importPackage(serviceBean.getNamespace().toString());

			addPrivateField(serviceType, serviceName).inject().create();
		});

		// Setter injection doesn't work for interfaces in SOAP beans!
		resourceMap.forEach((typeName, fieldName) -> addPrivateField(typeName, fieldName).asResource().create());

		containerServiceSet.forEach((typeName, fieldName) -> addPrivateField(typeName, fieldName).inject().create());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		integrationBean.getMethods().stream().map(SOAPIntegrationMethod.class::cast).forEach(soapMethod -> {
			final var b = new StringBuilder();
			final String identifier = SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).getMethodSignature(false, false,
					false);

			b.append(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).createCommentLink());
			b.append(SOAPMethodGeneratorFactory.getMethodGenerator(soapMethod, this).createMethod());

			addMethod(identifier, b.toString());
		});
	}

}
