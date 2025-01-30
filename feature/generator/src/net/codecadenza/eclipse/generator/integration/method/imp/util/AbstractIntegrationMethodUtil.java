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
package net.codecadenza.eclipse.generator.integration.method.imp.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Abstract base class for integration method utilities
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractIntegrationMethodUtil {
	public static final String OBJ_INSTANCE_NAME = "obj";
	public static final String LIST_INSTANCE_NAME = "resultList";

	protected final AbstractIntegrationMethod method;
	protected final BoundaryMethod boundaryMethod;
	protected final BoundaryMethodTypeEnumeration type;
	protected final IntegrationTechnology technology;
	protected final Project project;

	/**
	 * Constructor
	 * @param method
	 */
	protected AbstractIntegrationMethodUtil(AbstractIntegrationMethod method) {
		this.method = method;
		this.boundaryMethod = method.getBoundaryMethod();
		this.type = boundaryMethod.getMethodType();
		this.project = boundaryMethod.getBoundaryBean().getNamespace().getProject();
		this.technology = method.getIntegrationBean().getIntegrationTechnology();
	}

	/**
	 * @return the generated content
	 */
	public abstract String createConversionFragment();

	/**
	 * @return all necessary imports
	 */
	public Set<String> getImports() {
		return new HashSet<>();
	}

	/**
	 * @return a map containing additional services necessary for a specific operation
	 */
	public Map<String, ServiceBean> getServices() {
		return new HashMap<>();
	}

	/**
	 * @return the generated content
	 */
	protected String handleNullReturn() {
		// A REST integration method must not return null!
		if (technology == IntegrationTechnology.REST) {
			if (method.isStartNewThread())
				return "{\nresponse.resume(Response.status(Response.Status.NOT_FOUND).build());\nreturn;\n}\n";

			return "return Response.status(Response.Status.NOT_FOUND).build();\n";
		}

		return "return null;\n";
	}

	/**
	 * @param method
	 * @param serviceName
	 * @return the generated content
	 */
	public static String createServiceMethodInvocation(AbstractIntegrationMethod method, String serviceName) {
		final var b = new StringBuilder();
		boolean firstParam = true;

		b.append(serviceName + ".");

		if (!method.getIntegrationBean().getNamespace().getProject().isBoundaryMode()
				&& method.getBoundaryMethod().getServiceMethod() instanceof final DataExchangeMethod dataExchangeMethod)
			b.append(dataExchangeMethod.getName());
		else
			b.append(method.getBoundaryMethod().getName());

		b.append("(");

		for (final IntegrationMethodParameter param : method.getIntegrationParameters()) {
			if (param.isResponseParameter())
				continue;

			if (firstParam)
				firstParam = false;
			else
				b.append(",");

			b.append(param.getName());
		}

		b.append(");\n");

		return b.toString();
	}

	/**
	 * @param method
	 * @param clientMode
	 * @return true if the invocation of the respective service method can return null
	 */
	public static boolean canReturnNull(AbstractIntegrationMethod method, boolean clientMode) {
		final BoundaryMethodTypeEnumeration type = method.getBoundaryMethod().getMethodType();
		final boolean boundaryMode = method.getBoundaryMethod().getBoundaryBean().getNamespace().getProject().isBoundaryMode();

		if (type == BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY)
			return true;

		if (boundaryMode) {
			// Most boundary methods that return a single object (e.g. FIND_BY_ID) throw an exception if the respective object cannot be
			// found!
			if (type == BoundaryMethodTypeEnumeration.FIND_EXISTING)
				return true;
		}
		else if (clientMode && (type == BoundaryMethodTypeEnumeration.FIND_BY_OBJECT
				|| type == BoundaryMethodTypeEnumeration.FIND_EXISTING || type == BoundaryMethodTypeEnumeration.FIND_BY_ID)) {
			// When creating a client method the handling of null values should be added for respective method types. In case of
			// creating the service method the respective check must be omitted!
			return true;
		}

		return false;
	}

}
