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
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the SOAP configuration class of a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SOAPConfigGenerator extends AbstractJavaSourceGenerator {
	private final IntegrationModule module;
	private final Project project;

	/**
	 * Constructor
	 * @param module
	 */
	public SOAPConfigGenerator(IntegrationModule module) {
		this.module = module;
		this.project = module.getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = module.getNamespace().toString();

		final var soapConfig = new JavaFile(project, BuildArtifactType.INTEGRATION_IMP_SOAP, "SOAPConfig", packageName);
		soapConfig.setComment("Configuration class for all SOAP web services of this application");

		return soapConfig;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importClass("jakarta.xml.ws.Endpoint");
		importClass("org.apache.cxf.Bus");
		importClass("org.apache.cxf.bus.spring.SpringBus");
		importClass("org.apache.cxf.jaxws.EndpointImpl");
		importClass("org.apache.cxf.transport.servlet.CXFServlet");
		importClass("org.springframework.boot.web.servlet.ServletRegistrationBean");
		importPackage("org.springframework.context.annotation");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Configuration\n");
		b.append("public class SOAPConfig");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		var methodSignature = "ServletRegistrationBean<CXFServlet> getDispatcherServlet()";

		b.append("/**\n");
		b.append(" * @return the registration bean for the {@link CXFServlet}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return new ServletRegistrationBean<>(new CXFServlet(), \"/ws/*\");\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "SpringBus getSpringBus()";

		b.append("/**\n");
		b.append(" * @return a {@link SpringBus} instance\n");
		b.append(" */\n");
		b.append("@Bean(name = Bus.DEFAULT_BUS_ID)\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public SpringBus getSpringBus()\n");
		b.append("{\n");
		b.append("return new SpringBus();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		module.getNamespace().getJavaTypes().stream().map(SOAPIntegrationBean.class::cast).forEach(soapBean -> {
			final var methodName = "get" + soapBean.getDomainObject().getName() + "Endpoint";
			final var serviceName = soapBean.getDomainObject().getLowerCaseName() + "Service";
			final var signature = "Endpoint " + methodName + "(" + soapBean.getInterfaceName() + " " + serviceName + ")";
			final var method = new StringBuilder();

			method.append("/**\n");
			method.append(" * @param " + serviceName + "\n");
			method.append(" * @return the end point for the " + soapBean.getDomainObject().getLabel() + " SOAP web service\n");
			method.append(" */\n");
			method.append("@Bean\n");
			method.append(getAnnotationForGeneratedElement());
			method.append("public " + signature + "\n");
			method.append("{\n");
			method.append("final var endpoint = new EndpointImpl(getSpringBus(), " + serviceName + ");\n");
			method.append("endpoint.publish(\"/\" + " + soapBean.getInterfaceName() + ".SERVICE_NAME);\n\n");
			method.append("return endpoint;\n");
			method.append("}\n\n");

			addMethod(signature, method.toString());
		});

		b = new StringBuilder();
		methodSignature = "Endpoint getFileServiceEndpoint(" + module.getFileServiceName() + " fileService)";

		b.append("/**\n");
		b.append(" * @param fileService\n");
		b.append(" * @return the end point for the file SOAP web service\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var endpoint = new EndpointImpl(getSpringBus(), fileService);\n");
		b.append("endpoint.publish(\"/\" + " + module.getFileServiceName() + ".SERVICE_NAME);\n\n");
		b.append("return endpoint;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
