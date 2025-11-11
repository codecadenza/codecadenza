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
package net.codecadenza.eclipse.generator.integration.method;

import static net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator.SERVICE_NAME_SUFFIX;
import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.LIST_INSTANCE_NAME;
import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.OBJ_INSTANCE_NAME;
import static net.codecadenza.eclipse.model.java.JavaType.UUID;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;
import static net.codecadenza.eclipse.shared.Constants.INTEGRATION_SEARCH_PARAM_TYPE;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_LANG;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;
import static net.codecadenza.eclipse.shared.Constants.REMOTE_OPERATION_EXCEPTION_NAME;
import static net.codecadenza.eclipse.shared.Constants.SEARCH_PARAM_NAME;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil;
import net.codecadenza.eclipse.generator.integration.method.imp.util.IntegrationMethodUtilFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Abstract base class for integration method generators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractIntegrationMethodGenerator {
	public static final String STANDARD_ERROR_LOG_CONSTANT = "STANDARD_ERROR_LOG_MSG";
	protected static final String PACK_SEARCH_DTO = "net.codecadenza.runtime.search.dto";
	protected static final String PACK_SEARCH_UTIL = "net.codecadenza.runtime.search.util";
	protected static final String PACK_COMMONS_REPOSITORY = "net.codecadenza.runtime.repository";
	protected static final String EXECUTOR_SERVICE_NAME = "managedExecutorService";

	protected final AbstractIntegrationMethod method;
	protected final AbstractJavaSourceGenerator parentGenerator;
	protected final Project project;

	/**
	 * Constructor
	 * @param method
	 * @param parentGenerator
	 */
	protected AbstractIntegrationMethodGenerator(AbstractIntegrationMethod method, AbstractJavaSourceGenerator parentGenerator) {
		this.method = method;
		this.parentGenerator = parentGenerator;
		this.project = method.getIntegrationBean().getNamespace().getProject();
	}

	/**
	 * @return the service name
	 */
	public String getServiceName() {
		if (project.isBoundaryMode())
			return method.getIntegrationBean().getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX;
		else if (method.getBoundaryMethod().getServiceMethod() instanceof final DataExchangeMethod dataExchangeMethod)
			return dataExchangeMethod.getDataExchangeServiceBean().getLowerCaseName();

		return DEFAULT_REPOSITORY;
	}

	/**
	 * @return the service method name
	 */
	public String getServiceMethodName() {
		if (!project.isBoundaryMode()
				&& method.getBoundaryMethod().getServiceMethod() instanceof final DataExchangeMethod dataExchangeMethod)
			return dataExchangeMethod.getName();

		return method.getName();
	}

	/**
	 * @return all necessary imports
	 */
	public Set<String> getImports() {
		final var imports = new HashSet<>(getIntegrationMethodParameterImports());

		if (method.getIntegrationBean().getIntegrationTechnology() == IntegrationTechnology.KAFKA
				|| method.getIntegrationBean().getIntegrationTechnology() == IntegrationTechnology.RMI)
			imports.add("import net.codecadenza.runtime.exception.*;");

		if (method.getBoundaryMethod().getReturnType() != null && !method.getBoundaryMethod().getReturnType().isPrimitive()
				&& method.getReturnType().getNamespace() != null)
			imports.add("import " + method.getBoundaryMethod().getReturnType().getNamespace().toString() + ".*;");

		if (method.getReturnType() != null && !method.getReturnType().isPrimitive()
				&& method.getReturnType().getNamespace() != null) {
			imports.add("import " + method.getReturnType().getNamespace().toString() + ".*;");

			if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				imports.add("import " + PACK_JAVA_UTIL + ".*;");
		}

		method.getMethodParameters().forEach(param -> {
			if (!param.getType().isPrimitive() && param.getType().getNamespace() != null)
				imports.add("import " + param.getType().getNamespace().toString() + ".*;");

			if (param.getModifier() != JavaTypeModifierEnumeration.NONE)
				imports.add("import " + PACK_JAVA_UTIL + ".*;");
		});

		final BoundaryMethodTypeEnumeration methodType = method.getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.COUNT || methodType == BoundaryMethodTypeEnumeration.SEARCH) {
			imports.add("import " + PACK_SEARCH_DTO + ".*;");
			imports.add("import " + PACK_SEARCH_UTIL + ".*;");
		}

		final AbstractIntegrationMethodUtil methodUtility = IntegrationMethodUtilFactory.getInstance(method);

		if (methodUtility != null)
			imports.addAll(methodUtility.getImports());

		if (method.isStartNewThread()) {
			if (project.isJakartaEEApplication())
				imports.add("import jakarta.enterprise.concurrent.*;");
			else
				imports.add("import org.springframework.scheduling.concurrent.*;");

			imports.add("import java.util.concurrent.*;");
		}

		if (addTransaction() && method.getIntegrationBean().getIntegrationTechnology() != IntegrationTechnology.RMI) {
			if (project.isSpringBootApplication()) {
				imports.add("import org.springframework.transaction.*;");
				imports.add("import org.springframework.transaction.support.*;");
			}
			else if (method.getIntegrationBean().getIntegrationTechnology() != IntegrationTechnology.JMS)
				imports.add("import jakarta.transaction.*;");
		}

		return imports;
	}

	/**
	 * @return all necessary imports for the interface
	 */
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(getIntegrationMethodParameterImports());

		for (final MethodParameter parameter : method.getMethodParameters())
			if (!parameter.getType().isPrimitive() && parameter.getType().getNamespace() != null)
				imports.add("import " + parameter.getType().getNamespace().toString() + ".*;");

		if (method.getReturnType() != null && !method.getReturnType().isPrimitive() && method.getReturnType().getNamespace() != null)
			imports.add("import " + method.getReturnType().getNamespace().toString() + ".*;");

		final BoundaryMethodTypeEnumeration methodType = method.getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.COUNT || methodType == BoundaryMethodTypeEnumeration.SEARCH)
			imports.add("import " + PACK_SEARCH_DTO + ".*;");

		return imports;
	}

	/**
	 * @param addQualifier
	 * @param addAnnotations if true the generator adds necessary annotations to all parameters
	 * @param generateFullSignature if true the generator adds final keywords to parameters and exceptions
	 * @return the method signature
	 */
	public String getMethodSignature(boolean addQualifier, boolean addAnnotations, boolean generateFullSignature) {
		final var b = new StringBuilder();
		final String returnType = method.getReturnType().getName();

		if (addQualifier)
			b.append("public ");

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE) {
			if (returnList())
				b.append(method.getReturnTypeModifier().getName() + "<" + returnType + ">");
			else
				b.append(returnType + "[]");
		}
		else
			b.append(returnType);

		b.append(" " + method.getName() + "(");
		b.append(addMethodParameters(addAnnotations, generateFullSignature));
		b.append(")");

		return b.toString();
	}

	/**
	 * @return the method's comment
	 */
	public String createComment() {
		final var b = new StringBuilder();
		final String returnComment = createReturnComment(method);

		b.append(method.generateBeginOfJavadocComment());

		method.getIntegrationParameters().forEach(param -> {
			b.append(" * @param " + param.getName());

			if (param.getComment() != null)
				b.append(" " + param.getComment());

			b.append("\n");
		});

		if (!returnComment.isEmpty())
			b.append(" * @return " + returnComment + "\n");

		b.append(" * @throws " + getExceptionName());
		b.append(" due to a network problem or if the call failed in the backend\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * Every non-abstract subclass must implement how the method should be generated
	 * @return the generated content
	 */
	public abstract String createMethod();

	/**
	 * Every non-abstract subclass must implement how the business logic should be generated
	 * @return the generated content
	 */
	protected abstract String createMethodLogic();

	/**
	 * Create the method body
	 * @return the generated content
	 */
	public String createMethodBody() {
		final var b = new StringBuilder();

		if (method.isStartNewThread()) {
			String returnType = method.getReturnType().getWrapperTypeName();

			if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				if (returnList())
					returnType = "List<" + returnType + ">";
				else
					returnType = returnType + " []";

			b.append("final Future<" + returnType + "> result = " + EXECUTOR_SERVICE_NAME + ".submit(() ->\n");
			b.append("{\n");
			b.append(createMethodLogic());

			if (method.getReturnType().isVoid())
				b.append("return null;\n");

			b.append("});\n\n");
			b.append("try\n");
			b.append("{\n");

			if (!method.getReturnType().isVoid())
				b.append("return ");

			b.append("result.get(DEFAULT_TIMEOUT_VALUE, DEFAULT_TIMEOUT_UNIT);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");
			b.append("if(e instanceof InterruptedException)\n");
			b.append("Thread.currentThread().interrupt();\n\n");
		}
		else {
			b.append("try\n");
			b.append("{\n");
			b.append(createMethodLogic());
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");
		}

		parentGenerator.addErrorLogFromConstant(b, STANDARD_ERROR_LOG_CONSTANT, "e");

		b.append("\n");
		b.append("throw new " + getExceptionName() + "(ExceptionHelper.getRootCause(e).getMessage());\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return false if the method return type should be converted into an array
	 */
	protected boolean returnList() {
		return true;
	}

	/**
	 * Create the fragment for converting search objects
	 * @param dto
	 * @return the generated content
	 */
	protected String createSearchObjectConversion(DTOBean dto) {
		final var b = new StringBuilder();
		b.append("final var converter = new SearchObjectConverter(" + SEARCH_PARAM_NAME + ");\n");

		dto.getAttributes().forEach(attr -> {
			final JavaType type = attr.getDomainAttribute().getJavaType();

			b.append("converter.addSearchField(" + attr.getSelectTokenConstant() + ", ");
			b.append(attr.getAttributeNameConstant() + ", ");
			b.append(attr.getDomainAttribute().getSearchFieldDataType());

			if (type.isLocalDate() || attr.getDomainAttribute().getTemporalType() == TemporalTypeEnumeration.DATE)
				b.append(", false");

			b.append(");\n");
		});

		b.append("\n");

		return b.toString();
	}

	/**
	 * @return the name of the exception that this method can throw
	 */
	protected String getExceptionName() {
		return REMOTE_OPERATION_EXCEPTION_NAME;
	}

	/**
	 * @param waitForResponseParamType
	 * @return the generated comment link
	 */
	public String createCommentLink(String waitForResponseParamType) {
		final var b = new StringBuilder();
		final String packageName = method.getIntegrationBean().getNamespace().toString();
		final String interfaceName = method.getIntegrationBean().getInterfaceName();
		boolean firstParam = true;

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + packageName + "." + interfaceName + "#" + method.getName() + "(");

		for (final IntegrationMethodParameter param : method.getIntegrationParameters()) {
			final MethodParameter methodParameter = param.getMethodParameter();

			if (param.isResponseParameter())
				continue;

			if (firstParam)
				firstParam = false;
			else
				b.append(", ");

			if (methodParameter != null) {
				if (!methodParameter.getType().isPrimitive()) {
					if (methodParameter.getType().getNamespace() != null)
						b.append(methodParameter.getType().getNamespace().toString());
					else
						b.append(PACK_JAVA_LANG);

					b.append(".");
				}

				b.append(methodParameter.getType().getName());
			}
			else {
				final String typeName = param.getType();

				if (typeName.equals(JavaType.STRING) || typeName.equals(JavaType.LONG_OBJ) || typeName.equals(JavaType.INTEGER))
					b.append(PACK_JAVA_LANG + ".");
				else if (typeName.equals(JavaType.UUID))
					b.append(PACK_JAVA_UTIL + ".");
				else if (typeName.equals(INTEGRATION_SEARCH_PARAM_TYPE))
					b.append(PACK_SEARCH_DTO + ".");

				b.append(typeName);
			}
		}

		if (waitForResponseParamType != null) {
			if (!firstParam)
				b.append(", ");

			b.append(waitForResponseParamType);
		}

		b.append(")\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * @return the generated comment link
	 */
	public String createCommentLink() {
		return createCommentLink(null);
	}

	/**
	 * @param addAnnotations if true the generator adds necessary annotations to all parameters
	 * @param addFinal if true the generator adds final keywords to all method parameters
	 * @return the generated content
	 */
	protected String addMethodParameters(boolean addAnnotations, boolean addFinal) {
		final var b = new StringBuilder();
		boolean firstParam = true;

		for (final IntegrationMethodParameter param : method.getIntegrationParameters()) {
			if (firstParam)
				firstParam = false;
			else
				b.append(", ");

			if (addAnnotations)
				b.append(param.toString(addFinal));
			else {
				if (addFinal)
					b.append("final ");

				b.append(param.getType() + " " + param.getName());
			}
		}

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	protected String createFacadeFragment() {
		final var b = new StringBuilder();
		final AbstractIntegrationMethodUtil methodUtility = IntegrationMethodUtilFactory.getInstance(method);

		if (methodUtility == null)
			return b.toString();

		b.append(methodUtility.createConversionFragment());

		return b.toString();
	}

	/**
	 * @return a map containing all application services necessary for performing a given operation
	 */
	public Map<String, ServiceBean> getServices() {
		final var serviceMap = new HashMap<String, ServiceBean>();
		final var boundaryMethod = method.getBoundaryMethod();
		final BoundaryBean boundaryBean = boundaryMethod.getBoundaryBean();
		final String serviceName = getServiceName();

		if (project.isBoundaryMode()
				|| !(method.getBoundaryMethod().getServiceMethod() instanceof final DataExchangeMethod dataExchangeMethod))
			serviceMap.put(serviceName, boundaryBean);
		else
			serviceMap.put(serviceName, dataExchangeMethod.getDataExchangeServiceBean());

		final AbstractIntegrationMethodUtil methodUtility = IntegrationMethodUtilFactory.getInstance(method);

		if (methodUtility != null)
			serviceMap.putAll(methodUtility.getServices());

		return serviceMap;
	}

	/**
	 * @return a map containing all container resources necessary for performing a given operation
	 */
	public Map<String, String> getResources() {
		final var resourceMap = new HashMap<String, String>();

		if (method.isStartNewThread() && project.isJakartaEEApplication())
			resourceMap.put("ManagedExecutorService", EXECUTOR_SERVICE_NAME);

		if (addTransaction() && project.isSpringBootApplication())
			resourceMap.put("PlatformTransactionManager", "transactionManager");

		return resourceMap;
	}

	/**
	 * @return a map containing all container services necessary for performing a given operation
	 */
	public Map<String, String> getContainerServices() {
		final var containerServiceMap = new HashMap<String, String>();

		if (!method.isStartNewThread() || !project.isSpringBootApplication())
			return containerServiceMap;

		containerServiceMap.put("ThreadPoolTaskExecutor", EXECUTOR_SERVICE_NAME);

		return containerServiceMap;
	}

	/**
	 * @return a map containing all constants for a specific method
	 */
	public Map<String, String> getConstants() {
		final var constants = new HashMap<String, String>();
		final var timeOutValueDec = "int DEFAULT_TIMEOUT_VALUE";
		final var timeOutUnitDec = "TimeUnit DEFAULT_TIMEOUT_UNIT";

		if (!method.isStartNewThread())
			return constants;

		constants.put(timeOutValueDec, "1");
		constants.put(timeOutUnitDec, "TimeUnit.SECONDS");

		return constants;
	}

	/**
	 * @param integrationMethod
	 * @return the return comment of an integration method
	 */
	public String createReturnComment(final AbstractIntegrationMethod integrationMethod) {
		final var boundaryMethod = integrationMethod.getBoundaryMethod();
		final BoundaryMethodTypeEnumeration type = boundaryMethod.getMethodType();
		final boolean returnList = boundaryMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE;
		final boolean returnVoid = boundaryMethod.getReturnType().isVoid();
		String returnObjectLabel = boundaryMethod.getBoundaryBean().getDomainObject().getLabel();
		String returnObjectPluralLabel = boundaryMethod.getBoundaryBean().getDomainObject().getLabelPlural();

		if (boundaryMethod.getReturnType() instanceof final MappingObject mappingObject) {
			returnObjectLabel = mappingObject.getDomainObject().getLabel();
			returnObjectPluralLabel = mappingObject.getDomainObject().getLabelPlural();
		}

		final var listReturnComment = "a list containing " + returnObjectPluralLabel;
		final var singleReturnComment = "the " + returnObjectLabel;

		switch (type) {
			case FIND_BY_PARENT, FIND_ALL, GET_LIST_OF_VALUES, SEARCH, SEARCH_BY_FILTER, SEARCH_BY_UNIQUE_KEY:
				return listReturnComment;
			case EXISTS_BY_ID:
				return "true if the " + returnObjectLabel + " already exists";
			case COPY:
				return "the id of the new object";
			case COUNT:
				return "the number of objects this query would return";
			case COUNT_ALL:
				return "the number of all existing " + returnObjectPluralLabel;
			case DOWNLOAD:
				return "the fully qualified path of the file on the server";
			case DOWNLOAD_EXPORT:
				final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();

				if (exchangeMethod.returnsPath())
					return "the absolute path of the generated file";
				else if (exchangeMethod.returnsContent())
					return "the generated content";
				else if (exchangeMethod.getExchangeMode() instanceof DirectExchangeMode) {
					if (exchangeMethod.isProcessSingleObject())
						return singleReturnComment;

					return "a container object that provides a list of " + returnObjectLabel + " objects";
				}
				else
					return "";
			case FIND_BY_ID, FIND_BY_OBJECT, FIND_EXISTING, FIND_BY_UNIQUE_KEY:
				return singleReturnComment;
			case GET_ASSOCIATION:
				if (returnList)
					return listReturnComment;

				return singleReturnComment;
			case LOG_ON:
				return "the " + returnObjectLabel;
			case UPDATE:
				if (!returnVoid)
					return "the updated " + returnObjectLabel;

				return "";
			case CREATE:
				if (!returnVoid)
					return "the new " + returnObjectLabel;

				return "";
			case SAVE:
				return "the saved " + returnObjectLabel;
			case EXISTS_BY_UNIQUE_KEY_WITH_ID, EXISTS_BY_UNIQUE_KEY:
				return "true if an object already exists for which the provided parameter values match the respective properties";
			default:
				return "";
		}
	}

	/**
	 * @return the name of the object that a method should return. An empty string is returned if the method return type is void!
	 */
	protected String getReturnObjectName() {
		var returnObjName = "";

		if (method.getReturnType().isVoid())
			return returnObjName;

		if (method.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE)
			returnObjName = OBJ_INSTANCE_NAME;
		else
			returnObjName = LIST_INSTANCE_NAME;

		final BoundaryMethodTypeEnumeration type = method.getBoundaryMethod().getMethodType();

		if (!project.isBoundaryMode() && (type == BoundaryMethodTypeEnumeration.CREATE || type == BoundaryMethodTypeEnumeration.UPDATE
				|| type == BoundaryMethodTypeEnumeration.SAVE))
			return method.getBoundaryMethod().getMethodParameters().get(0).getName();

		return returnObjName;
	}

	/**
	 * @return true if the method requires a transaction
	 */
	public boolean addTransaction() {
		if (project.isBoundaryMode())
			return false;

		final BoundaryMethodTypeEnumeration type = method.getBoundaryMethod().getMethodType();

		return type == BoundaryMethodTypeEnumeration.CREATE || type == BoundaryMethodTypeEnumeration.UPDATE
				|| type == BoundaryMethodTypeEnumeration.SAVE || type == BoundaryMethodTypeEnumeration.FIND_BY_ID
				|| type == BoundaryMethodTypeEnumeration.FIND_BY_OBJECT || type == BoundaryMethodTypeEnumeration.FIND_EXISTING
				|| type == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER;
	}

	/**
	 * @return a set that contains import statements required by the integration method parameters
	 */
	protected Set<String> getIntegrationMethodParameterImports() {
		final boolean importUtilPackage = method.getIntegrationParameters().stream().map(IntegrationMethodParameter::getType)
				.anyMatch(name -> name.equals(UUID));

		if (importUtilPackage)
			return Set.of("import " + PACK_JAVA_UTIL + ".*;");

		return Collections.emptySet();
	}

	/**
	 * Create null-check statements for all non-primitive parameters of a client method
	 * @return the generated content
	 */
	protected String createClientParameterChecks() {
		final var b = new StringBuilder();
		final BoundaryMethodTypeEnumeration methodType = method.getBoundaryMethod().getMethodType();
		final IntegrationTechnology technology = method.getIntegrationBean().getIntegrationTechnology();

		for (final IntegrationMethodParameter param : method.getIntegrationParameters()) {
			final JavaType type = project.getJavaTypeByName(param.getType());

			// Filter values for parameters of unique key methods that are mapped to optional associations are allowed to be null!
			// This rule doesn't apply for REST as respective path parameters must not be null!
			if (param.isResponseParameter()
					|| (technology != IntegrationTechnology.REST && (methodType == BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY
							|| methodType == BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY
							|| methodType == BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID
							|| methodType == BoundaryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)))
				continue;

			if (type == null || !type.isPrimitive()) {
				b.append("if(" + param.getName() + " == null)\n");
				b.append("throw new IllegalArgumentException(\"The parameter '" + param.getName() + "' must not be null!\");\n\n");
			}
		}

		return b.toString();
	}

}
