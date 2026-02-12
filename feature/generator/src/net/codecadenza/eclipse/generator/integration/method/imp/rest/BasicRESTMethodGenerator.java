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
package net.codecadenza.eclipse.generator.integration.method.imp.rest;

import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.LIST_INSTANCE_NAME;
import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.OBJ_INSTANCE_NAME;
import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.canReturnNull;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;
import static net.codecadenza.eclipse.shared.Constants.REMOTE_OPERATION_EXCEPTION_NAME;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;

/**
 * <p>
 * Base class for REST integration method generators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicRESTMethodGenerator extends AbstractIntegrationMethodGenerator {
	protected final RESTIntegrationMethod restMethod;

	/**
	 * Constructor
	 * @param restMethod
	 * @param parentGenerator
	 */
	public BasicRESTMethodGenerator(RESTIntegrationMethod restMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(restMethod, parentGenerator);

		this.restMethod = restMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethod()
	 */
	@Override
	public String createMethod() {
		final var b = new StringBuilder();
		final var restIntegrationBean = (RESTIntegrationBean) restMethod.getIntegrationBean();
		final String inputMediaType = restIntegrationBean.convertToMediaType(restMethod.getInputType());
		final String outputMediaType = restIntegrationBean.convertToMediaType(restMethod.getOutputType());
		final String primaryInputMediaType = restIntegrationBean.getPrimaryConsumedMediaType();
		final String primaryOutputMediaType = restIntegrationBean.getPrimaryProducedMediaType();

		// Add method annotations
		if (addTransaction() && !project.isSpringBootApplication())
			b.append("@Transactional\n");

		b.append("@" + restMethod.getHttpMethod().getName() + "\n");

		var path = restMethod.getPath() != null ? restMethod.getPath() : "";

		for (final IntegrationMethodParameter param : restMethod.getIntegrationParameters())
			path += param.getPath();

		if (path != null && !path.isEmpty())
			b.append("@Path(\"" + path + "\")\n");

		if (inputMediaType != null && !inputMediaType.equals(primaryInputMediaType))
			b.append("@Consumes(" + inputMediaType + ")\n");

		if (outputMediaType != null && !outputMediaType.equals(primaryOutputMediaType) && !restMethod.getReturnType().isVoid())
			b.append("@Produces(" + outputMediaType + ")\n");

		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append(getMethodSignature(true, true, true));
		b.append("\n{\n");
		b.append(createMethodBody());
		b.append("}\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final String returnType = method.getReturnType().getWrapperTypeName();

		if (restMethod.isStartNewThread()) {
			b.append("response.setTimeout(DEFAULT_TIMEOUT_VALUE, DEFAULT_TIMEOUT_UNIT);\n\n");
			b.append(EXECUTOR_SERVICE_NAME + ".submit(() ->\n");
			b.append("{\n");

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("final var def = new DefaultTransactionDefinition();\n");
				b.append("final TransactionStatus status = transactionManager.getTransaction(def);\n\n");
			}

			b.append("try\n");
			b.append("{\n");
			b.append(createMethodLogic());

			if (!restMethod.getReturnType().isVoid()) {
				if (restMethod.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE) {
					if (canReturnNull(restMethod, false)) {
						b.append("\n");
						b.append("if(" + getReturnObjectName() + " == null)\n");
						b.append("{\n");
						b.append("response.resume(Response.status(Response.Status.NOT_FOUND).build());\n");
						b.append("return;\n");
						b.append("}\n");
					}

					b.append("\n");
					b.append(createResponseEntity(returnType));
				}
				else {
					b.append("\n");
					b.append("final GenericEntity<List<" + returnType + ">> entity = new GenericEntity<>(");
					b.append(getReturnObjectName() + "){};\n");
				}

				b.append("\n");
				b.append("response.resume(Response.ok(entity).build());\n");
			}
			else {
				b.append("\n");
				b.append("response.resume(Response.noContent().build());\n");
			}

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			if (addTransaction() && project.isSpringBootApplication())
				b.append("transactionManager.rollback(status);\n\n");

			b.append("response.resume(e);\n");
			b.append("}\n");
			b.append("});\n");
		}
		else {
			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("final var def = new DefaultTransactionDefinition();\n");
				b.append("final TransactionStatus status = transactionManager.getTransaction(def);\n\n");
				b.append("try\n");
				b.append("{\n");
			}

			b.append(createMethodLogic());

			if (!restMethod.getReturnType().isVoid()) {
				if (restMethod.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE) {
					if (canReturnNull(restMethod, false)) {
						b.append("\n");
						b.append("if(" + getReturnObjectName() + " == null)\n");
						b.append("return Response.status(Response.Status.NOT_FOUND).build();\n");
					}

					b.append("\n");
					b.append(createResponseEntity(returnType));
				}
				else {
					b.append("\n");
					b.append("final GenericEntity<List<" + returnType + ">> entity = new GenericEntity<>(");
					b.append(getReturnObjectName() + "){};\n");
				}

				b.append("\n");
				b.append("return Response.ok(entity).build();\n");
			}

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");
				b.append("transactionManager.rollback(status);\n\n");
				b.append("throw e;\n");
				b.append("}\n");
			}
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethodLogic()
	 */
	@Override
	public String createMethodLogic() {
		final var b = new StringBuilder();
		final String facadeFragment = createFacadeFragment();
		final var paramConvMap = new HashSet<String>();
		boolean firstParam = true;

		// Search for parameters that needs a type conversion!
		restMethod.getIntegrationParameters().forEach(param -> {
			if (param.getType().equals(JavaType.GREGORIAN_CAL)) {
				b.append(JavaType.GREGORIAN_CAL + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append("{\n");
				b.append(param.getConvertedName() + " = new " + JavaType.GREGORIAN_CAL + "();\n");
				b.append(param.getConvertedName() + ".setTimeInMillis(" + param.getName() + ");\n");
				b.append("}\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.FLOAT)) {
				b.append(JavaType.FLOAT + " " + param.getConvertedName() + " = 0.0f;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append(param.getConvertedName() + " = Float.parseFloat(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.FLOAT_OBJ)) {
				b.append(JavaType.FLOAT_OBJ + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append(param.getConvertedName() + " = Float.parseFloat(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.DOUBLE)) {
				b.append(JavaType.DOUBLE + " " + param.getConvertedName() + " = 0.0;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append(param.getConvertedName() + " = Double.parseDouble(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.DOUBLE_OBJ)) {
				b.append(JavaType.DOUBLE_OBJ + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append(param.getConvertedName() + " = Double.parseDouble(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.DATE)) {
				b.append(JavaType.DATE + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append(param.getConvertedName() + " = new " + JavaType.DATE + "(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.BIG_DECIMAL)) {
				b.append(JavaType.BIG_DECIMAL + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null)\n");
				b.append(param.getConvertedName() + " = new " + JavaType.BIG_DECIMAL + "(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.CHAR)) {
				b.append(JavaType.CHAR + " " + param.getConvertedName() + " = ' ';\n\n");
				b.append("if(" + param.getName() + " != null && !" + param.getName() + ".isEmpty())\n");
				b.append(param.getConvertedName() + " = " + param.getName() + ".charAt(0);\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.CHARACTER)) {
				b.append(JavaType.CHARACTER + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null && !" + param.getName() + ".isEmpty())\n");
				b.append(param.getConvertedName() + " = " + param.getName() + ".charAt(0);\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.LOCAL_DATE)) {
				b.append(JavaType.LOCAL_DATE + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null && !" + param.getName() + ".isEmpty())\n");
				b.append(param.getConvertedName() + " = " + PACK_JAVA_TIME + ".LocalDate.parse(" + param.getName());
				b.append(", " + PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ISO_DATE);\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.LOCAL_DATE_TIME)) {
				b.append(JavaType.LOCAL_DATE_TIME + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null && !" + param.getName() + ".isEmpty())\n");
				b.append(param.getConvertedName() + " = " + PACK_JAVA_TIME + ".LocalDateTime.parse(" + param.getName());
				b.append(", " + PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ISO_DATE_TIME);\n\n");

				paramConvMap.add(param.getName());
			}
			else if (param.getType().equals(JavaType.UUID) && !param.isPathParameter() && !param.isQueryParameter()) {
				b.append(JavaType.UUID + " " + param.getConvertedName() + " = null;\n\n");
				b.append("if(" + param.getName() + " != null && !" + param.getName() + ".isEmpty())\n");
				b.append(param.getConvertedName() + " = UUID.fromString(" + param.getName() + ");\n\n");

				paramConvMap.add(param.getName());
			}
		});

		if (facadeFragment.isEmpty()) {
			if (!restMethod.getReturnType().isVoid() && !ignoreReturnValue()) {
				final String returnType = method.getReturnType().getWrapperTypeName();

				if (restMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
					b.append("final List<" + returnType + "> " + LIST_INSTANCE_NAME + " = ");
				else
					b.append("final " + returnType + " " + OBJ_INSTANCE_NAME + " = ");
			}

			b.append(getServiceName() + "." + getServiceMethodName() + "(");

			for (final IntegrationMethodParameter param : restMethod.getIntegrationParameters()) {
				if (param.isResponseParameter())
					continue;

				if (firstParam)
					firstParam = false;
				else
					b.append(",");

				if (paramConvMap.contains(param.getName()))
					b.append(param.getConvertedName());
				else
					b.append(param.getName());
			}

			b.append(");\n");
		}
		else
			b.append(facadeFragment);

		if (addTransaction() && project.isSpringBootApplication()) {
			b.append("\n");
			b.append("transactionManager.commit(status);\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getMethodSignature(boolean,
	 * boolean, boolean)
	 */
	@Override
	public String getMethodSignature(boolean addQualifier, boolean addAnnotations, boolean addFinal) {
		final var b = new StringBuilder();

		if (addQualifier)
			b.append("public ");

		if (restMethod.isStartNewThread() || restMethod.getReturnType().isVoid())
			b.append("void ");
		else
			b.append("Response ");

		b.append(restMethod.getName() + "(");
		b.append(addMethodParameters(addAnnotations, addFinal));
		b.append(")");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#addMethodParameters(boolean,
	 * boolean)
	 */
	@Override
	protected String addMethodParameters(boolean addAnnotations, boolean addFinal) {
		final var b = new StringBuilder();
		boolean firstParam = true;

		for (final IntegrationMethodParameter param : method.getIntegrationParameters()) {
			if (!firstParam)
				b.append(", ");

			if (addAnnotations) {
				final boolean isJerseyUsed = project.isSpringBootApplication() || project.isDeployedOnPayara();

				// Applications that use Eclipse Jersey require a mapping for all additional parameters of a 'CREATE' method!
				if (isJerseyUsed && method.getBoundaryMethod().getMethodType() == BoundaryMethodTypeEnumeration.CREATE && !firstParam)
					b.append("@HeaderParam(\"Additional-Content-" + param.getName().toUpperCase() + "\") ");

				b.append(param.toString(addFinal));
			}
			else {
				if (addFinal)
					b.append("final ");

				b.append(param.getType() + " " + param.getName());
			}

			if (firstParam)
				firstParam = false;
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(restMethod.generateBeginOfJavadocComment());

		restMethod.getIntegrationParameters().forEach(param -> {
			b.append(" * @param " + param.getName());

			if (param.getComment() != null && !param.getComment().isEmpty())
				b.append(" " + param.getComment());

			b.append("\n");
		});

		if (!restMethod.isStartNewThread())
			b.append(" * @return a response object\n");

		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final Set<String> imports = super.getImports();

		if (restMethod.isStartNewThread())
			imports.add("import jakarta.ws.rs.container.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());
		imports.add("import net.codecadenza.runtime.transport.*;");

		if (restMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			imports.add("import " + PACK_JAVA_UTIL + ".*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getExceptionName()
	 */
	@Override
	protected String getExceptionName() {
		return "WebApplicationException";
	}

	/**
	 * In case of REST the interface is only used by the respective client service! Thus, the REST bean doesn't implement this
	 * interface!
	 * @param addQualifier
	 * @param addFinal if true the generator adds final keywords to all method parameters
	 * @return the signature for the interface method
	 */
	public String getInterfaceMethodSignature(boolean addQualifier, boolean addFinal) {
		final var b = new StringBuilder();
		final String returnType = restMethod.getReturnType().getName();
		boolean firstParam = true;

		if (addQualifier)
			b.append("public ");

		if (restMethod.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE)
			b.append(returnType + " " + restMethod.getName() + "(");
		else
			b.append("List<" + returnType + "> " + restMethod.getName() + "(");

		for (final IntegrationMethodParameter param : restMethod.getIntegrationParameters()) {
			if (param.isResponseParameter())
				continue;

			if (firstParam)
				firstParam = false;
			else
				b.append(", ");

			if (addFinal)
				b.append("final ");

			b.append(param.getType() + " " + param.getName());
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * @return the comment for the interface method
	 */
	public String createInterfaceComment() {
		final var b = new StringBuilder();
		final String returnComment = createReturnComment(restMethod);

		b.append(restMethod.generateBeginOfJavadocComment());

		for (final IntegrationMethodParameter param : restMethod.getIntegrationParameters()) {
			if (param.isResponseParameter())
				continue;

			b.append(" * @param " + param.getName());

			if (param.getComment() != null && !param.getComment().isEmpty())
				b.append(" " + param.getComment());

			b.append("\n");
		}

		if (!returnComment.isEmpty())
			b.append(" * @return " + returnComment + "\n");

		b.append(" * @throws " + REMOTE_OPERATION_EXCEPTION_NAME);
		b.append(" due to a network problem or if the call failed in the backend\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createClientMethod() {
		final var b = new StringBuilder();
		final JavaType returnType = restMethod.getReturnType();
		final Optional<IntegrationMethodParameter> contentParam = restMethod.getContentParameter();
		final var restIntegrationBean = (RESTIntegrationBean) restMethod.getIntegrationBean();

		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append(getInterfaceMethodSignature(true, true));
		b.append("\n{\n");
		b.append(createClientParameterChecks());
		b.append("Response response = null;\n\n");
		b.append("try\n");
		b.append("{\n");

		if (!returnType.isVoid() && restMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			b.append("final GenericType<List<" + restMethod.getReturnType().getName() + ">> returnType = new GenericType<>() {};\n\n");

		b.append("response = ");
		b.append("webTarget.path(RESOURCE_PATH)");

		if (restMethod.getPath() != null && !restMethod.getPath().isEmpty())
			b.append(".path(\"" + restMethod.getPath() + "\")");

		// Add path and query parameters
		for (final IntegrationMethodParameter param : restMethod.getIntegrationParameters())
			if (param.isPathParameter() || param.isQueryParameter()) {
				final String paramType = param.getType();

				if (param.isQueryParameter())
					b.append(".queryParam(\"" + param.getName() + "\", ");
				else
					b.append(".path(");

				if (paramType.equals(JavaType.INT) || paramType.equals(JavaType.INTEGER))
					b.append("Integer.toString(" + param.getName() + ")");
				else if (paramType.equals(JavaType.LONG) || paramType.equals(JavaType.LONG_OBJ))
					b.append("Long.toString(" + param.getName() + ")");
				else if (paramType.equals(JavaType.BOOL) || paramType.equals(JavaType.BOOL_OBJ))
					b.append("Boolean.toString(" + param.getName() + ")");
				else if (paramType.equals(JavaType.DATE))
					b.append("Long.toString(" + param.getName() + ".getTime())");
				else if (paramType.equals(JavaType.GREGORIAN_CAL))
					b.append("Long.toString(" + param.getName() + ".getTimeInMillis())");
				else if (paramType.equals(JavaType.DOUBLE) || paramType.equals(JavaType.DOUBLE_OBJ))
					b.append("Double.toString(" + param.getName() + ")");
				else if (paramType.equals(JavaType.FLOAT) || paramType.equals(JavaType.FLOAT_OBJ))
					b.append("Float.toString(" + param.getName() + ")");
				else if (paramType.equals(JavaType.BIG_DECIMAL) || paramType.equals(JavaType.UUID))
					b.append(param.getName() + ".toString()");
				else if (paramType.equals(JavaType.CHAR) || paramType.equals(JavaType.CHARACTER))
					b.append("Character.toString(" + param.getName() + ")");
				else if (paramType.equals(JavaType.LOCAL_DATE))
					b.append(PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ISO_DATE.format(" + param.getName() + ")");
				else if (paramType.equals(JavaType.LOCAL_DATE_TIME))
					b.append(PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ISO_DATE_TIME.format(" + param.getName() + ")");
				else
					b.append(param.getName());

				b.append(")");
			}

		b.append(".request(");

		if (!returnType.isVoid())
			b.append(restIntegrationBean.convertToMediaType(restMethod.getOutputType()));

		b.append(")");
		b.append("." + restMethod.getHttpMethod().name().toLowerCase() + "(");

		if (restMethod.getHttpMethod() == HttpMethodEnumeration.POST || restMethod.getHttpMethod() == HttpMethodEnumeration.PUT)
			if (contentParam.isPresent()) {
				b.append("Entity.entity(" + contentParam.get().getName() + ", ");
				b.append(restIntegrationBean.convertToMediaType(restMethod.getInputType()) + ")");
			}
			else
				b.append("null");

		b.append(");\n\n");

		if (!returnType.isVoid() && restMethod.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE
				&& canReturnNull(restMethod, true)) {
			b.append("if(response.getStatusInfo().equals(Status.NOT_FOUND))\n");
			b.append("return null;\n");
			b.append("else ");
		}

		b.append("if(!response.getStatusInfo().equals(Status." + (returnType.isVoid() ? "NO_CONTENT" : "OK") + "))\n");
		b.append("throw new ");
		b.append(REMOTE_OPERATION_EXCEPTION_NAME);
		b.append("(response.getStatusInfo().getReasonPhrase());\n");

		if (!returnType.isVoid()) {
			b.append("\n");
			b.append("return response.readEntity(");

			if (restMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				b.append("returnType");
			else
				b.append(restMethod.getReturnType().getName() + ".class");

			b.append(");\n");
		}

		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("if(response != null)\n");
		b.append("response.close();\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return true if the return value of the service method must be ignored
	 */
	public boolean ignoreReturnValue() {
		return false;
	}

	/**
	 * Create a response entity based on the given return type
	 * @param returnType
	 * @return the generated content
	 */
	private String createResponseEntity(final String returnType) {
		final var b = new StringBuilder();

		if (returnType.equals(JavaType.UUID)) {
			b.append("final GenericEntity<" + JavaType.STRING + "> entity = new GenericEntity<>(");
			b.append(getReturnObjectName() + ".toString(), " + JavaType.STRING + ".class);\n");
		}
		else {
			b.append("final GenericEntity<" + returnType + "> entity = new GenericEntity<>(");
			b.append(getReturnObjectName() + ", " + returnType + ".class);\n");
		}

		return b.toString();
	}

}
