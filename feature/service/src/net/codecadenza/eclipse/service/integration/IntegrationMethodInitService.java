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
package net.codecadenza.eclipse.service.integration;

import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.IntegrationFactory;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;

/**
 * <p>
 * Service that for initializing integration methods
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationMethodInitService {
	private static final String ALL = "/all";
	private static final String FIND = "/find";
	private static final String FIND_BY_OBJECT = "/find_by_object";
	private static final String FIND_EXISTING = "/find_existing";
	private static final String SEARCH = "/search";
	private static final String COUNT = "/count";
	private static final String LIST = "/list";
	private static final String EXPORT = "/export";
	private static final String IMPORT = "/import";
	private static final String UPLOAD = "/upload";
	private static final String DOWNLOAD = "/download";
	private static final String SAVE = "/save";
	private static final String OPERATION = "/operation";
	private static final String COPY = "/copy";
	private static final String EXISTS = "/exists";
	private static final String LOGON = "/logon";
	private static final String CHANGE_PASSWORD = "/changepassword";
	private static final String LOV = "/lov";
	private static final String BY = "_by_";
	private static final String AND = "_and_";

	/**
	 * @param integrationBean
	 * @param boundaryMethod
	 * @param mediaType
	 * @return the initialized integration method
	 */
	public AbstractIntegrationMethod initializeMethod(AbstractIntegrationBean integrationBean, BoundaryMethod boundaryMethod,
			MediaTypeEnumeration mediaType) {
		AbstractIntegrationMethod integrationMethod = null;

		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.SOAP)
			integrationMethod = IntegrationFactory.eINSTANCE.createSOAPIntegrationMethod();
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.REST) {
			final RESTIntegrationMethod restMethod = IntegrationFactory.eINSTANCE.createRESTIntegrationMethod();
			integrationMethod = restMethod;

			initRESTMethod(restMethod, integrationBean, boundaryMethod, mediaType);
		}
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.JMS) {
			final JMSIntegrationMethod jmsMethod = IntegrationFactory.eINSTANCE.createJMSIntegrationMethod();
			jmsMethod.setOperationID(boundaryMethod.getName());
			jmsMethod.setSendResponse(true);

			integrationMethod = jmsMethod;
		}
		else if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.RMI)
			integrationMethod = IntegrationFactory.eINSTANCE.createRMIIntegrationMethod();
		else {
			final String schemaNamePrefix = boundaryMethod.getName().substring(0, 1).toUpperCase()
					+ boundaryMethod.getName().substring(1);
			final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();

			final KafkaIntegrationMethod kafkaMethod = IntegrationFactory.eINSTANCE.createKafkaIntegrationMethod();
			kafkaMethod.setRequestSchemaName(schemaNamePrefix + "Request");
			kafkaMethod.setUseDedicatedPartition(true);
			kafkaMethod.setSendResponse(true);

			if (!boundaryMethod.getReturnType().isVoid() || methodType == BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION
					|| methodType == BoundaryMethodTypeEnumeration.CHANGE_ASSOCIATION
					|| methodType == BoundaryMethodTypeEnumeration.CHANGE_PARENT || methodType == BoundaryMethodTypeEnumeration.COPY
					|| methodType == BoundaryMethodTypeEnumeration.DELETE
					|| methodType == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION)
				kafkaMethod.setResponseSchemaName(schemaNamePrefix + "Response");
			else
				kafkaMethod.setResponseSchemaName("");

			integrationMethod = kafkaMethod;
		}

		integrationMethod.setName(boundaryMethod.getName());
		integrationMethod.setBoundaryMethod(boundaryMethod);
		integrationMethod.setReturnType(boundaryMethod.getReturnType());
		integrationMethod.setReturnTypeModifier(boundaryMethod.getReturnTypeModifier());
		integrationMethod.setComment(boundaryMethod.getComment());

		for (final MethodParameter param : boundaryMethod.getMethodParameters()) {
			final MethodParameter newParam = JavaFactory.eINSTANCE.createMethodParameter();
			newParam.setHint(param.getHint());
			newParam.setName(param.getName());
			newParam.setModifier(param.getModifier());
			newParam.setType(param.getType());

			integrationMethod.getMethodParameters().add(newParam);
		}

		return integrationMethod;
	}

	/**
	 * Initialize the path and the HTTP method fields of a REST method with reasonable default values
	 * @param restMethod
	 * @param integrationBean
	 * @param boundaryMethod
	 * @param mediaType
	 */
	private void initRESTMethod(final RESTIntegrationMethod restMethod, final AbstractIntegrationBean integrationBean,
			final BoundaryMethod boundaryMethod, final MediaTypeEnumeration mediaType) {
		final BoundaryMethodTypeEnumeration type = boundaryMethod.getMethodType();
		final boolean returnVoid = boundaryMethod.getReturnType().isVoid();
		final AbstractDomainAssociation assoc = boundaryMethod.getAssociation();
		boolean hasContentParameter = false;
		String returnTypeName = boundaryMethod.getReturnType().getName();
		MethodParameter firstParam = null;
		MethodParameter secondParam = null;
		boolean isFirstParam = true;

		for (final MethodParameter param : boundaryMethod.getMethodParameters()) {
			if (isFirstParam)
				firstParam = param;
			else
				secondParam = param;

			isFirstParam = false;

			if (!hasContentParameter && param.getType() instanceof DTOBean)
				hasContentParameter = true;
		}

		if (boundaryMethod.getReturnType() instanceof final DTOBean returnDTO) {
			returnTypeName = returnDTO.getDomainObject().getName().toLowerCase();

			if (boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_PARENT)
				returnTypeName = returnDTO.getDomainObject().getNamePlural().toLowerCase();
		}

		restMethod.setInputType(mediaType);
		restMethod.setOutputType(mediaType);

		switch (type) {
			case FIND_ALL:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(FIND + ALL);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case CHANGE_ASSOCIATION, CHANGE_PARENT, ADD_TO_ASSOCIATION:
				restMethod.setInputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath("/" + assoc.getName().toLowerCase());
				restMethod.setHttpMethod(HttpMethodEnumeration.PUT);
				break;
			case CHANGE_PASSWORD:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(OPERATION + CHANGE_PASSWORD);
				restMethod.setHttpMethod(HttpMethodEnumeration.PUT);
				break;
			case EXISTS_BY_ID:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(OPERATION + EXISTS);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case COPY:
				restMethod.setInputType(MediaTypeEnumeration.TEXT);
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(OPERATION + COPY);
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			case COUNT:
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(COUNT);
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			case COUNT_ALL:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(COUNT + ALL);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case DOWNLOAD:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(DOWNLOAD);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case DOWNLOAD_EXPORT:
				restMethod.setPath(EXPORT);
				restMethod.setInputType(MediaTypeEnumeration.NONE);

				if (boundaryMethod.getReturnType().isVoid())
					restMethod.setInputType(MediaTypeEnumeration.NONE);
				else if (boundaryMethod.getReturnType().isString()) {
					restMethod.setOutputType(MediaTypeEnumeration.TEXT);

					if (boundaryMethod.getServiceMethod() instanceof final DataExchangeMethod exchangeMethod
							&& exchangeMethod.returnsContent()) {
						if (exchangeMethod.getContentType() == ContentTypeEnumeration.JSON)
							restMethod.setOutputType(MediaTypeEnumeration.JSON);
						else if (exchangeMethod.getContentType() == ContentTypeEnumeration.XML)
							restMethod.setOutputType(MediaTypeEnumeration.XML);
					}
				}

				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case FIND_BY_ID:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(FIND);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case FIND_BY_OBJECT:
				restMethod.setPath(FIND_BY_OBJECT);
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			case FIND_EXISTING:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(FIND_EXISTING);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case SEARCH_BY_FILTER:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(LIST);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case GET_ASSOCIATION:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath("/" + returnTypeName + FIND);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case FIND_BY_PARENT:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath("/" + returnTypeName + LIST);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case LOG_ON:
				restMethod.setInputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(OPERATION + LOGON);
				restMethod.setHttpMethod(HttpMethodEnumeration.PUT);
				break;
			case GET_LIST_OF_VALUES:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(LOV);
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case REMOVE_FROM_ASSOCIATION:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath("/" + assoc.getName().toLowerCase());
				restMethod.setHttpMethod(HttpMethodEnumeration.DELETE);
				break;
			case UPDATE:
				restMethod.setHttpMethod(HttpMethodEnumeration.PUT);
				break;
			case CREATE:
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			case SAVE:
				restMethod.setHttpMethod(HttpMethodEnumeration.PUT);
				restMethod.setPath(SAVE);
				break;
			case DELETE:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setHttpMethod(HttpMethodEnumeration.DELETE);
				break;
			case DELETE_ALL:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(ALL);
				restMethod.setHttpMethod(HttpMethodEnumeration.DELETE);
				break;
			case SEARCH:
				restMethod.setPath(SEARCH);
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			case EXISTS_BY_UNIQUE_KEY:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(EXISTS + BY + firstParam.getName().toLowerCase());
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case SEARCH_BY_UNIQUE_KEY:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(SEARCH + BY + firstParam.getName().toLowerCase());
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case FIND_BY_UNIQUE_KEY:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setPath(FIND + BY + firstParam.getName().toLowerCase());
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case EXISTS_BY_UNIQUE_KEY_WITH_ID:
				restMethod.setInputType(MediaTypeEnumeration.NONE);
				restMethod.setOutputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(EXISTS + BY + secondParam.getName().toLowerCase() + AND + firstParam.getName().toLowerCase());
				restMethod.setHttpMethod(HttpMethodEnumeration.GET);
				break;
			case UPLOAD:
				restMethod.setInputType(MediaTypeEnumeration.TEXT);
				restMethod.setPath(UPLOAD);
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			case UPLOAD_IMPORT:
				// If the method has a parameter it either represents a path, a content string or a mapping object!
				if (firstParam == null)
					restMethod.setInputType(MediaTypeEnumeration.NONE);
				else if (firstParam.getType().isString())
					restMethod.setInputType(MediaTypeEnumeration.TEXT);

				restMethod.setPath(IMPORT);
				restMethod.setHttpMethod(HttpMethodEnumeration.POST);
				break;
			default:
				break;
		}

		if (returnVoid)
			restMethod.setOutputType(MediaTypeEnumeration.NONE);

		// Check if the combination of the path and the HTTP method is unique across all methods of the bean
		for (final AbstractIntegrationMethod integrationMethod : integrationBean.getMethods()) {
			final var existingMethod = (RESTIntegrationMethod) integrationMethod;
			boolean changePath = false;

			if (existingMethod.equals(restMethod) || existingMethod.getHttpMethod() != restMethod.getHttpMethod())
				continue;

			if (existingMethod.getPath() == null) {
				if (restMethod.getPath() == null)
					changePath = true;
			}
			else if (existingMethod.getPath().equals(restMethod.getPath()))
				changePath = true;

			if (changePath) {
				// Change the path in order to avoid problems at runtime!
				restMethod.setPath("/" + boundaryMethod.getName().toLowerCase());
				break;
			}
		}
	}

}
