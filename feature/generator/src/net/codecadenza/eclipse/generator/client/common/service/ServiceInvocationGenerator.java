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

import static net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator.SERVICE_NAME_SUFFIX;

import java.util.Arrays;
import net.codecadenza.eclipse.generator.client.common.security.ClientParameterHelper;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.service.ServiceMethod;

/**
 * <p>
 * Generator for service method invocations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServiceInvocationGenerator {
	protected final StringBuilder content;
	protected final ServiceMethod method;
	protected String serviceName;
	protected ClientParameterHelper clientParameterHelper;

	/**
	 * Constructor
	 * @param method
	 * @param listDTO
	 * @param content
	 */
	public ServiceInvocationGenerator(BoundaryMethod method, DTOBean listDTO, StringBuilder content) {
		this.method = method;
		this.content = content;
		this.serviceName = method.getBoundaryBean().getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;

		if (listDTO != null)
			this.clientParameterHelper = new ClientParameterHelper(listDTO);
		else
			this.clientParameterHelper = new ClientParameterHelper(method);
	}

	/**
	 * Constructor
	 * @param method
	 * @param content
	 */
	public ServiceInvocationGenerator(BoundaryMethod method, StringBuilder content) {
		final boolean boundaryMode = method.getBoundaryBean().getNamespace().getProject().isBoundaryMode();

		this.method = method;
		this.content = content;
		this.clientParameterHelper = new ClientParameterHelper(method);

		if (!boundaryMode && method.getServiceMethod() instanceof final DataExchangeMethod dataExchangeMethod)
			this.serviceName = dataExchangeMethod.getDataExchangeServiceBean().getLowerCaseName();
		else
			this.serviceName = method.getBoundaryBean().getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;
	}

	/**
	 * Constructor
	 * @param method
	 * @param content
	 */
	public ServiceInvocationGenerator(RepositoryMethod method, StringBuilder content) {
		this.method = method;
		this.content = content;
		this.serviceName = method.getRepository().getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;
	}

	/**
	 * Add the service method invocation to the given content
	 */
	public void addInvocation() {
		addInvocation(new String[0]);
	}

	/**
	 * Add the service method invocation to the given content
	 * @param parameters
	 */
	public void addInvocation(String... parameters) {
		addInvocation(false, parameters);
	}

	/**
	 * Add the service method invocation to the given content
	 * @param omitSemicolon
	 * @param parameters
	 */
	public void addInvocation(boolean omitSemicolon, String... parameters) {
		final String clientParameter = clientParameterHelper != null ? clientParameterHelper.getClientParameter() : "";
		final String parameterList = addParameters(parameters);

		content.append(serviceName + "." + method.getName() + "(");
		content.append(parameterList);

		if (!parameterList.isEmpty() && !clientParameter.isEmpty())
			content.append(", ");

		content.append(clientParameter);
		content.append(")");

		if (!omitSemicolon)
			content.append(";\n");
	}

	/**
	 * @param parameters
	 * @return the generated content
	 */
	protected String addParameters(String... parameters) {
		return Arrays.asList(parameters).stream().filter(p -> !p.isEmpty()).reduce((p1, p2) -> p1 + ", " + p2).orElse("");
	}

}
