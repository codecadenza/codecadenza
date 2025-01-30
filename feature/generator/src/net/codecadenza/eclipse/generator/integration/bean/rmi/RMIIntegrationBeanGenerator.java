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
package net.codecadenza.eclipse.generator.integration.bean.rmi;

import static net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator.STANDARD_ERROR_LOG_CONSTANT;

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.rmi.RMIMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for RMI integration beans
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RMIIntegrationBeanGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;
	private final Map<String, ServiceBean> serviceMap = new HashMap<>();
	private final Map<String, String> resourceMap = new HashMap<>();
	private final Map<String, String> constantMap = new HashMap<>();

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public RMIIntegrationBeanGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getServiceBeanSourceFile());

		this.integrationBean = integrationBean;

		integrationBean.getMethods().stream().map(RMIIntegrationMethod.class::cast).forEach(rmiMethod -> {
			this.serviceMap.putAll(RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).getServices());
			this.resourceMap.putAll(RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).getResources());
			this.constantMap.putAll(RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).getConstants());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.ejb");
		importPackage(integrationBean.getNamespace().toString());

		integrationBean.getMethods().forEach(method -> {
			final var rmiMethod = (RMIIntegrationMethod) method;

			addImports(RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).getImports());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Stateless\n");
		b.append("@Remote(" + integrationBean.getInterfaceName() + ".class)\n");
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
		if (!integrationBean.getMethods().isEmpty())
			constantMap.put(JavaType.STRING + " " + STANDARD_ERROR_LOG_CONSTANT, "\"Error while processing RMI method invocation!\"");

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
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		resourceMap.forEach(this::addResourceSetterMethod);

		integrationBean.getMethods().stream().map(RMIIntegrationMethod.class::cast).forEach(rmiMethod -> {
			final var b = new StringBuilder();
			final String identifier = RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).getMethodSignature(false, false,
					false);

			b.append(RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).createCommentLink());
			b.append(RMIMethodGeneratorFactory.getMethodGenerator(rmiMethod, this).createMethod());

			addMethod(identifier, b.toString());
		});
	}

}
