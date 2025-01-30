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
package net.codecadenza.eclipse.generator.client.common.service;

import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE_LOCATOR;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE_LOCATOR_RAP;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for service declarations that is used in client generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServiceDeclarationGenerator {
	public static final String SERVICE_NAME_SUFFIX = "Service";

	protected final AbstractJavaSourceGenerator generator;
	protected final ServiceBean serviceBean;
	protected final Project project;
	protected final String serviceName;
	protected final String serviceInterfaceName;
	protected StringBuilder content;
	protected BoundaryBean rootBoundaryBean;

	/**
	 * Constructor
	 * @param generator
	 * @param boundaryBean
	 */
	public ServiceDeclarationGenerator(AbstractJavaSourceGenerator generator, BoundaryBean boundaryBean) {
		this.generator = generator;
		this.serviceBean = boundaryBean;
		this.project = boundaryBean.getNamespace().getProject();
		this.serviceInterfaceName = boundaryBean.getInterfaceName();
		this.serviceName = boundaryBean.getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;
	}

	/**
	 * Constructor
	 * @param generator
	 * @param boundaryBean
	 * @param rootBoundaryBean
	 */
	public ServiceDeclarationGenerator(AbstractJavaSourceGenerator generator, BoundaryBean boundaryBean,
			BoundaryBean rootBoundaryBean) {
		this(generator, boundaryBean);

		this.rootBoundaryBean = rootBoundaryBean;
	}

	/**
	 * Constructor
	 * @param generator
	 * @param boundaryBean
	 * @param content
	 */
	public ServiceDeclarationGenerator(AbstractJavaSourceGenerator generator, BoundaryBean boundaryBean, StringBuilder content) {
		this(generator, boundaryBean);

		this.content = content;
	}

	/**
	 * Constructor
	 * @param generator
	 * @param boundaryMethod
	 * @param content
	 */
	public ServiceDeclarationGenerator(AbstractJavaSourceGenerator generator, BoundaryMethod boundaryMethod,
			StringBuilder content) {
		this.generator = generator;
		this.project = boundaryMethod.getBoundaryBean().getNamespace().getProject();
		this.content = content;

		if (!project.isBoundaryMode() && boundaryMethod.getServiceMethod() instanceof final DataExchangeMethod dataExchangeMethod) {
			this.serviceBean = dataExchangeMethod.getDataExchangeServiceBean();
			this.serviceInterfaceName = serviceBean.getName();
			this.serviceName = serviceBean.getLowerCaseName();
		}
		else {
			this.serviceBean = boundaryMethod.getBoundaryBean();
			this.serviceInterfaceName = serviceBean.getInterfaceName();
			this.serviceName = serviceBean.getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;
		}
	}

	/**
	 * Add a private field for this service
	 * @param initialize
	 * @param addTransientModifier
	 */
	public void addField(boolean initialize, boolean addTransientModifier) {
		if (project.hasJSFOrVaadinClient()) {
			addImports(true);

			generator.addPrivateField(serviceInterfaceName, serviceName).setTransientModifier(addTransientModifier).inject().create();
			return;
		}

		addImports(false);

		final JavaFieldGenerator fieldGenerator = generator.addPrivateField(serviceInterfaceName, serviceName)
				.setTransientModifier(addTransientModifier);

		if (!initialize) {
			fieldGenerator.create();
			return;
		}

		fieldGenerator.withFinalModifier();

		if (project.isBoundaryMode())
			fieldGenerator.withDefaultValue("ServiceLocator.getService(" + serviceInterfaceName + ".class)").create();
		else if (rootBoundaryBean == null || serviceBean.equals(rootBoundaryBean))
			fieldGenerator.withDefaultValue("new " + serviceInterfaceName + "()").create();
		else {
			final String rootServiceName = rootBoundaryBean.getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;

			fieldGenerator.withDefaultValue("new " + serviceInterfaceName + "(" + rootServiceName + ".getEntityManager())").create();
		}
	}

	/**
	 * Add a private field for this service
	 * @param addTransientModifier
	 */
	public void addField(boolean addTransientModifier) {
		addField(false, addTransientModifier);
	}

	/**
	 * Add a private field for this service
	 */
	public void addField() {
		addField(false, true);
	}

	/**
	 * Initialize the service
	 */
	public void initField() {
		initField(false);
	}

	/**
	 * Add a local variable for this service
	 */
	public void addLocalVariable() {
		initField(true);
	}

	/**
	 * @return true if internal resources of a service must be closed
	 */
	public boolean needsCloseStatement() {
		return !project.isBoundaryMode();
	}

	/**
	 * Add the statement for closing service-related resources in a finally block
	 */
	public void addCloseStatementInFinallyBlock() {
		if (!needsCloseStatement())
			return;

		addCloseStatement(true);
	}

	/**
	 * Add the statement for closing service-related resources
	 */
	public void addCloseStatement() {
		addCloseStatement(false);
	}

	/**
	 * @return the name of the service
	 */
	public String getServiceName() {
		return serviceName;
	}

	/**
	 * Initialize either a field or a local variable
	 * @param asLocalVariable if set to true a local variable will be declared
	 */
	private void initField(boolean asLocalVariable) {
		addImports(false);

		if (asLocalVariable)
			content.append("final var ");

		if (!project.isBoundaryMode()) {
			content.append(serviceName + " = new " + serviceInterfaceName + "();\n");
			return;
		}

		content.append(serviceName + " = ServiceLocator.getService(" + serviceInterfaceName + ".class);\n");
	}

	/**
	 * Add service imports
	 * @param injectField
	 */
	private void addImports(boolean injectField) {
		generator.importPackage(serviceBean.getNamespace().toString());

		if (injectField)
			return;

		if (project.isBoundaryMode()) {
			if (project.hasRCPClient() || project.hasJavaFXClient() || project.hasSwingClient())
				generator.importPackage(PACK_SERVICE_LOCATOR);
			else if (project.hasRAPClient())
				generator.importClass(PACK_SERVICE_LOCATOR_RAP);
		}
	}

	/**
	 * Add the statement for closing service-related resources
	 * @param inFinallyBlock
	 */
	private void addCloseStatement(boolean inFinallyBlock) {
		if (inFinallyBlock) {
			content.append("finally\n");
			content.append("{\n");
		}

		if (needsCloseStatement())
			content.append(serviceName + ".closeEntityManager();\n");

		if (inFinallyBlock)
			content.append("}\n");
	}

}
