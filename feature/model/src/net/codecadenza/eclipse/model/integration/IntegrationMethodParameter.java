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
package net.codecadenza.eclipse.model.integration;

import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;

/**
 * <p>
 * Meta-model class that holds additional information regarding parameters that belong to an integration method
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationMethodParameter {
	private final AbstractIntegrationMethod integrationMethod;
	private final String name;
	private String type;
	private String comment;
	private MethodParameter methodParameter;
	private boolean pathParameter;
	private boolean queryParameter;
	private boolean responseParameter;

	/**
	 * Constructor
	 * @param integrationMethod
	 * @param name
	 * @param type
	 */
	public IntegrationMethodParameter(AbstractIntegrationMethod integrationMethod, String name, String type) {
		this.integrationMethod = integrationMethod;
		this.name = name;
		this.type = type;
	}

	/**
	 * Constructor
	 * @param integrationMethod
	 * @param methodParameter
	 */
	public IntegrationMethodParameter(AbstractIntegrationMethod integrationMethod, MethodParameter methodParameter) {
		this(integrationMethod, methodParameter.getName(), methodParameter.getType().getName());

		this.methodParameter = methodParameter;
	}

	/**
	 * Constructor
	 * @param integrationMethod
	 * @param boundaryMethod
	 * @param methodParameter
	 */
	public IntegrationMethodParameter(AbstractIntegrationMethod integrationMethod, BoundaryMethod boundaryMethod,
			MethodParameter methodParameter) {
		this(integrationMethod, methodParameter);

		// Initialize the parameter with reasonable default values as the domain-model doesn't contain respective information!
		initParameter(boundaryMethod);
	}

	/**
	 * @param boundaryMethod
	 */
	private void initParameter(BoundaryMethod boundaryMethod) {
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();
		final MethodParameter firstParam = boundaryMethod.getFirstParameter(false);
		final IntegrationTechnology technology = integrationMethod.getIntegrationBean().getIntegrationTechnology();

		switch (methodType) {
			case ADD_TO_ASSOCIATION, CHANGE_ASSOCIATION, CHANGE_PARENT, UPLOAD:
				if (firstParam.getName().equals(methodParameter.getName()))
					pathParameter = true;

				break;
			case CHANGE_PASSWORD, FIND_EXISTING, REMOVE_FROM_ASSOCIATION:
				if (firstParam.getName().equals(methodParameter.getName()))
					pathParameter = true;
				else
					queryParameter = true;

				break;
			case EXISTS_BY_ID, DOWNLOAD, FIND_BY_ID, SEARCH_BY_FILTER, GET_ASSOCIATION, FIND_BY_PARENT, DELETE, SEARCH_BY_UNIQUE_KEY, FIND_BY_UNIQUE_KEY:
				pathParameter = true;
				break;
			case DOWNLOAD_EXPORT, LOG_ON, GET_LIST_OF_VALUES:
				queryParameter = true;
				break;
			case EXISTS_BY_UNIQUE_KEY, EXISTS_BY_UNIQUE_KEY_WITH_ID:
				if (technology == IntegrationTechnology.REST && methodParameter.getType() instanceof final DTOBean dto) {
					// When using REST, the type of the primary key attribute should be used, as a method may contain two DTO parameters
					// that would require a special handling as only one DTO can be used as content parameter!
					type = dto.getPKAttribute().getDomainAttribute().getJavaType().getName();
				}

				pathParameter = true;
				break;
			case UPLOAD_IMPORT:
				// The method parameter represents the content!
				break;
			default:
				break;
		}
	}

	/**
	 * @return the name of the parameter
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the name of a local variable that is used if the parameter type must be converted (e.g. JAX-RS doesn't support
	 *         parameters of type java.util.Date)
	 */
	public String getConvertedName() {
		return "param" + name.substring(0, 1).toUpperCase() + name.substring(1);
	}

	/**
	 * @return the parameter type name
	 */
	public String getType() {
		return type;
	}

	/**
	 * @return the method parameter this object is mapped to
	 */
	public MethodParameter getMethodParameter() {
		return methodParameter;
	}

	/**
	 * @return true if the parameter represents a JAX-RS path parameter
	 */
	public boolean isPathParameter() {
		return pathParameter;
	}

	/**
	 * @param pathParameter
	 */
	public void setPathParameter(boolean pathParameter) {
		this.pathParameter = pathParameter;
	}

	/**
	 * @return true if the parameter represents a JAX-RS query parameter
	 */
	public boolean isQueryParameter() {
		return queryParameter;
	}

	/**
	 * @param queryParameter
	 */
	public void setQueryParameter(boolean queryParameter) {
		this.queryParameter = queryParameter;
	}

	/**
	 * @return true if the parameter represents an asynchronous response parameter
	 */
	public boolean isResponseParameter() {
		return responseParameter;
	}

	/**
	 * @param responseParameter
	 */
	public void setResponseParameter(boolean responseParameter) {
		this.responseParameter = responseParameter;
	}

	/**
	 * @param comment
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

	/**
	 * @return the comment
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * @return a JAX-RS conform path if the parameter represents a path parameter. Otherwise it will return an empty string!
	 */
	public String getPath() {
		if (pathParameter)
			return "/{" + name + "}";

		return "";
	}

	/**
	 * @param addFinal flag that controls if the 'final' modifier should be added
	 * @return the generated content
	 */
	public String toString(boolean addFinal) {
		final var b = new StringBuilder();
		final IntegrationTechnology technology = integrationMethod.getIntegrationBean().getIntegrationTechnology();
		final String finalModifier = addFinal ? "final " : "";

		if (technology == IntegrationTechnology.REST) {
			if (pathParameter)
				b.append("@PathParam(\"" + name + "\") ");

			if (queryParameter)
				b.append("@QueryParam(\"" + name + "\") ");

			if (responseParameter)
				b.append("@Suspended ");

			// Unsupported JAX-RS parameter types must be replaced!
			if (type.equals(JavaType.DATE) || type.equals(JavaType.GREGORIAN_CAL))
				return b.append(finalModifier + JavaType.LONG_OBJ + " " + name).toString();
			else if (type.equals(JavaType.DOUBLE) || type.equals(JavaType.DOUBLE_OBJ) || type.equals(JavaType.FLOAT)
					|| type.equals(JavaType.FLOAT_OBJ) || type.equals(JavaType.BIG_DECIMAL) || type.equals(JavaType.CHAR)
					|| type.equals(JavaType.CHARACTER) || type.equals(JavaType.LOCAL_DATE) || type.equals(JavaType.LOCAL_DATE_TIME)
					|| (type.equals(JavaType.UUID) && !pathParameter && !queryParameter))
				return b.append(finalModifier + JavaType.STRING + " " + name).toString();
		}
		else if (technology == IntegrationTechnology.SOAP) {
			final var soapMethod = (SOAPIntegrationMethod) integrationMethod;

			if (soapMethod.isAddParameterAnnotations())
				b.append("@WebParam(name = \"" + name + "\") ");
		}

		b.append(finalModifier + type + " " + name);

		return b.toString();
	}

}
