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

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.rest.RESTMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for REST integration beans
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RESTIntegrationBeanGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;
	private final RESTIntegrationBean restBean;
	private final Project project;
	private final Map<String, ServiceBean> serviceMap = new HashMap<>();
	private final Map<String, String> resourceMap = new HashMap<>();
	private final Map<String, String> containerServiceSet = new HashMap<>();
	private final Map<String, String> constantMap = new HashMap<>();

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public RESTIntegrationBeanGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getServiceBeanSourceFile());

		this.integrationBean = integrationBean;
		this.project = integrationBean.getNamespace().getProject();
		this.restBean = (RESTIntegrationBean) integrationBean;

		integrationBean.getMethods().stream().map(RESTIntegrationMethod.class::cast).forEach(restMethod -> {
			this.serviceMap.putAll(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getServices());
			this.resourceMap.putAll(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getResources());
			this.containerServiceSet.putAll(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getContainerServices());
			this.constantMap.putAll(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getConstants());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.ws.rs");

		if (!integrationBean.getMethods().isEmpty())
			importPackage("jakarta.ws.rs.core");

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.context");
		else
			importClass("org.springframework.stereotype.Service");

		integrationBean.getMethods().forEach(method -> {
			final var restMethod = (RESTIntegrationMethod) method;

			addImports(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getImports());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		final String primaryConsumedMediaType = restBean.getPrimaryConsumedMediaType();
		final String primaryProducedMediaType = restBean.getPrimaryProducedMediaType();

		if (project.isJakartaEEApplication())
			b.append("@RequestScoped\n");
		else
			b.append("@Service\n");

		b.append("@Path(\"" + restBean.getPath() + "\")\n");

		if (primaryConsumedMediaType != null)
			b.append("@Consumes(" + primaryConsumedMediaType + ")\n");

		if (primaryProducedMediaType != null)
			b.append("@Produces(" + primaryProducedMediaType + ")\n");

		b.append("public class " + integrationBean.getName());
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

		resourceMap.forEach((typeName, fieldName) -> addPrivateField(typeName, fieldName).create());

		containerServiceSet.forEach((typeName, fieldName) -> addPrivateField(typeName, fieldName).inject().create());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		resourceMap.forEach(this::addResourceSetterMethod);

		integrationBean.getMethods().stream().map(RESTIntegrationMethod.class::cast).forEach(restMethod -> {
			final var b = new StringBuilder();
			final String identifier = RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).getMethodSignature(false, false,
					false);

			b.append(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).createComment());
			b.append(RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this).createMethod());

			addMethod(identifier, b.toString());
		});
	}

}
