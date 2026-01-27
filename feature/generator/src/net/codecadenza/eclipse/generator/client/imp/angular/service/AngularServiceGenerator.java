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

import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.ARRAY;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.NUMBER;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.OBSERVABLE;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.STRING;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.VOID;

import java.util.List;
import java.util.Optional;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.Constants;

/**
 * <p>
 * Generator for services of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularServiceGenerator extends AbstractTypeScriptSourceGenerator {
	private static final String SEARCH_INPUT_TYPE_BACKEND = "SearchInputBackend";

	private final DomainObject domainObject;
	private final String serviceName;
	private final Project project;
	private final boolean limitItems;

	/**
	 * Constructor
	 * @param boundaryBean
	 */
	public AngularServiceGenerator(BoundaryBean boundaryBean) {
		super(boundaryBean.getTypeScriptSourceFile(), "Service for " + boundaryBean.getDomainObject().getLabel() + " domain objects");

		this.domainObject = boundaryBean.getDomainObject();
		this.serviceName = domainObject.getName() + "Service";
		this.project = domainObject.getNamespace().getProject();
		this.limitItems = boundaryBean.getBoundaryMethods().stream()
				.anyMatch(m -> m.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importType("Injectable", "@angular/core");
		importType("environment", "../../environments/environment");
		importType(OBSERVABLE, "rxjs");

		if (limitItems)
			importType("map", "rxjs/operators");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("@Injectable({ providedIn: 'root' })");
		formatter.addLine("export class " + serviceName + " {");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (limitItems)
			addPrivateConstant("number", "MAX_LIST_SIZE", "100").create();

		addService("HttpClient", "httpClient", "@angular/common/http");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		// Search for the respective REST service
		final AbstractIntegrationBean integrationBean = project.searchIntegrationBean(IntegrationTechnology.REST, domainObject);

		if (integrationBean == null)
			throw new IllegalStateException("The REST service for domain object '" + domainObject.getName() + "' could not be found!");

		boolean firstMethod = true;

		for (final AbstractIntegrationMethod integrationMethod : integrationBean.getMethods()) {
			final var restMethod = (RESTIntegrationMethod) integrationMethod;

			if (firstMethod)
				firstMethod = false;
			else
				formatter.addBlankLine();

			addMethod((RESTIntegrationBean) integrationBean, restMethod, formatter);
		}
	}

	/**
	 * Add a service method based on the given REST integration method
	 * @param restBean
	 * @param restMethod
	 * @param formatter
	 */
	private void addMethod(RESTIntegrationBean restBean, RESTIntegrationMethod restMethod, AngularContentFormatter formatter) {
		final String httpMethod = restMethod.getHttpMethod().getName().toLowerCase();
		final List<IntegrationMethodParameter> params = restMethod.getIntegrationParameters();
		final Optional<IntegrationMethodParameter> contentParam = params.stream()
				.filter(p -> !p.isPathParameter() && !p.isQueryParameter() && !p.isResponseParameter()).findFirst();
		final var methodType = restMethod.getBoundaryMethod().getMethodType();
		boolean firstParam = true;
		String returnType = restMethod.getReturnType().getName();

		if (restMethod.getReturnType().getName().equals(JavaType.VOID))
			returnType = VOID;
		else if (project.getJavaTypeByName(returnType) != null && project.getJavaTypeByName(returnType).isNumber())
			returnType = NUMBER;
		else if (project.getAllSupportedTypes().stream().anyMatch(t -> t.equals(restMethod.getReturnType())))
			returnType = STRING;
		else
			importType(returnType, "../domain/" + returnType.toLowerCase() + ".interface");

		if (methodType == BoundaryMethodTypeEnumeration.COUNT || methodType == BoundaryMethodTypeEnumeration.SEARCH)
			importType(SEARCH_INPUT_TYPE_BACKEND, "../common/model/search-input-backend.model");

		if (restMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			returnType = ARRAY + "<" + returnType + ">";

		final var methodSignature = new StringBuilder();
		methodSignature.append(restMethod.getBoundaryMethod().getName());
		methodSignature.append("(");

		for (final IntegrationMethodParameter param : params) {
			if (param.isResponseParameter())
				continue;

			// A persist operation with initial one-to-many associations is currently not supported!
			if (methodType == BoundaryMethodTypeEnumeration.CREATE && !firstParam)
				break;

			// All standard types must be converted to String!
			final boolean useType = project.getAllSupportedTypes().stream().noneMatch(t -> t.getName().equals(param.getType()));

			if (firstParam)
				firstParam = false;
			else
				methodSignature.append(", ");

			methodSignature.append(param.getName() + ": ");

			if (useType) {
				if (methodType != BoundaryMethodTypeEnumeration.COUNT && methodType != BoundaryMethodTypeEnumeration.SEARCH)
					importType(param.getType(), "../domain/" + param.getType().toLowerCase() + ".interface");

				if (param.getType().equals(Constants.INTEGRATION_SEARCH_PARAM_TYPE))
					methodSignature.append(SEARCH_INPUT_TYPE_BACKEND);
				else
					methodSignature.append(param.getType());
			}
			else
				methodSignature.append(STRING);
		}

		methodSignature.append("): " + OBSERVABLE + "<" + returnType + "> {");

		formatter.addBlockComment(restMethod.getComment());
		formatter.addLine(methodSignature.toString());
		formatter.increaseIndent();

		// Create the service invocation URL
		final var url = new StringBuilder();
		url.append("const URL = environment.SERVICE_URL + '");
		url.append(restBean.getPath());

		if (restMethod.getPath() != null && !restMethod.getPath().isEmpty()) {
			if (!restBean.getPath().endsWith("/") && !restMethod.getPath().startsWith("/"))
				url.append("/");

			url.append(restMethod.getPath());
		}

		url.append("'");

		for (final IntegrationMethodParameter param : params) {
			if (!param.isPathParameter())
				continue;

			url.append(" + '/' + encodeURIComponent(" + param.getName() + ")");
		}

		url.append(";");

		formatter.addLine(url.toString());

		firstParam = true;

		// Add the query parameters
		final var queryParams = new StringBuilder();

		for (final IntegrationMethodParameter param : params) {
			if (!param.isQueryParameter())
				continue;

			importType("HttpParams", "@angular/common/http");

			if (firstParam) {
				queryParams.append("const params = new HttpParams()");
				firstParam = false;
			}

			queryParams.append(".set(");
			queryParams.append("'" + param.getName() + "', " + param.getName());
			queryParams.append(")");
		}

		if (!queryParams.isEmpty()) {
			queryParams.append(";");

			formatter.addLine(queryParams.toString());
		}

		// Create the service invocation
		final var invocation = new StringBuilder();
		invocation.append("return this.httpClient." + httpMethod);

		if (methodType == BoundaryMethodTypeEnumeration.DOWNLOAD || methodType == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT) {
			if (returnType.equals(VOID))
				invocation.append("<" + returnType + ">(URL");
			else
				invocation.append("(URL");

			if (!returnType.equals(VOID) || !queryParams.isEmpty()) {
				invocation.append(", { ");

				if (!returnType.equals(VOID)) {
					invocation.append("responseType: 'text'");

					if (!queryParams.isEmpty())
						invocation.append(", ");
				}

				if (!queryParams.isEmpty())
					invocation.append("params: params");

				invocation.append(" }");
			}
		}
		else {
			if (!returnType.equals(STRING))
				invocation.append("<" + returnType + ">");

			invocation.append("(URL");

			if (contentParam.isPresent())
				invocation.append(", " + contentParam.get().getName());
			else if (restMethod.getHttpMethod() == HttpMethodEnumeration.POST
					|| restMethod.getHttpMethod() == HttpMethodEnumeration.PUT)
				invocation.append(", null");

			if (!queryParams.isEmpty() || returnType.equals(STRING)) {
				invocation.append(", { ");

				if (!queryParams.isEmpty()) {
					invocation.append("params: params");

					if (returnType.equals(STRING))
						invocation.append(", ");
				}

				if (returnType.equals(STRING))
					invocation.append("responseType: 'text'");

				invocation.append(" }");
			}
		}

		invocation.append(")");

		if (methodType == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER)
			invocation.append(".pipe(map(items => items.slice(0, " + serviceName + ".MAX_LIST_SIZE)))");

		invocation.append(";");

		formatter.addBlankLine();
		formatter.addLine(invocation.toString());
		formatter.decreaseIndent();
		formatter.addLine("}");
	}

}
