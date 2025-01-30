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
package net.codecadenza.eclipse.generator.client.imp.angular.service;

import java.util.Arrays;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;

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
public class AngularServiceInvocationGenerator {
	protected final BoundaryMethod method;
	protected final DTOBean listDTO;
	protected final Project project;
	protected final BoundaryBean serviceBean;
	protected final String serviceName;

	/**
	 * Constructor
	 * @param method
	 * @param listDTO
	 */
	public AngularServiceInvocationGenerator(BoundaryMethod method, DTOBean listDTO) {
		this.method = method;
		this.listDTO = listDTO;
		this.project = method.getBoundaryBean().getNamespace().getProject();
		this.serviceBean = method.getBoundaryBean();
		this.serviceName = serviceBean.getDomainObject().getLowerCaseName() + "Service";
	}

	/**
	 * Constructor
	 * @param method
	 */
	public AngularServiceInvocationGenerator(BoundaryMethod method) {
		this(method, null);
	}

	/**
	 * Create the service method invocation with an arbitrary number of parameters
	 * @param parameters
	 * @return the generated content
	 */
	public String createInvocation(String... parameters) {
		final var invocation = new StringBuilder();
		invocation.append("this." + serviceName + "." + method.getName() + "(");
		invocation.append(addParameters(parameters));
		invocation.append(")");

		return invocation.toString();
	}

	/**
	 * @return true if the method contains a parameter that is controlled by the authentication service
	 */
	public boolean isAuthServiceRequired() {
		return !createClientParam().isEmpty();
	}

	/**
	 * @param parameters
	 * @return the generated content
	 */
	private String addParameters(String... parameters) {
		final String params = Arrays.asList(parameters).stream().filter(p -> !p.isEmpty()).reduce((p1, p2) -> p1 + ", " + p2)
				.orElse("");
		final String clientParam = createClientParam();

		if (clientParam.isEmpty())
			return params;

		if (!params.isEmpty())
			return params + ", " + clientParam;

		return clientParam;
	}

	/**
	 * Create an additional parameter that either represents the ID of the client or the ID of the user
	 * @return the additional parameter or an empty string
	 */
	private String createClientParam() {
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		boolean addClientAttr = false;

		if (logOnDTO == null)
			return "";

		if (listDTO != null) {
			if (!listDTO.getDomainObject().isMandated())
				return "";

			addClientAttr = true;
		}

		if (addClientAttr || method.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
			final DTOBeanAttribute clientPkAttr = logOnDTO.getClientPKAttribute();
			final var convertToString = clientPkAttr.getDomainAttribute().getJavaType().isIntegerOrLong() ? ".toString()" : "";

			return "this.authService.getLoggedOnUser()." + clientPkAttr.getName() + convertToString;
		}
		else if (method.getDataFetchType() == BoundaryMethodDataFetchType.USER) {
			final DTOBeanAttribute userPkAttr = logOnDTO.getPKAttribute();
			final var convertToString = userPkAttr.getDomainAttribute().getJavaType().isIntegerOrLong() ? ".toString()" : "";

			return "this.authService.getLoggedOnUser()." + userPkAttr.getName() + convertToString;
		}

		return "";
	}

}
