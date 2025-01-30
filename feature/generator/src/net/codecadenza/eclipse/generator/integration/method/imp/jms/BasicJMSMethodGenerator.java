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
package net.codecadenza.eclipse.generator.integration.method.imp.jms;

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;
import static net.codecadenza.eclipse.shared.Constants.SEARCH_PARAM_NAME;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Base class for JMS integration method generators
 * </p>
 * <p>
 * Copyright 2023 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicJMSMethodGenerator extends AbstractIntegrationMethodGenerator {
	private static final Pattern OPERATION_ID_PATTERN = Pattern.compile("([A-Z])");

	protected final JMSIntegrationMethod jmsMethod;
	protected final BoundaryMethodTypeEnumeration boundaryMethodType;
	protected final boolean hasSearchInputParameter;

	/**
	 * Constructor
	 * @param jmsMethod
	 * @param parentGenerator
	 */
	public BasicJMSMethodGenerator(JMSIntegrationMethod jmsMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(jmsMethod, parentGenerator);

		this.jmsMethod = jmsMethod;
		this.boundaryMethodType = jmsMethod.getBoundaryMethod().getMethodType();
		this.hasSearchInputParameter = this.boundaryMethodType == BoundaryMethodTypeEnumeration.SEARCH
				|| this.boundaryMethodType == BoundaryMethodTypeEnumeration.COUNT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import net.codecadenza.runtime.jms.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final BoundaryMethodTypeEnumeration methodType = jmsMethod.getBoundaryMethod().getMethodType();
		final var imports = new HashSet<>(getIntegrationMethodParameterImports());

		for (final MethodParameter parameter : jmsMethod.getMethodParameters())
			if (!parameter.getType().isPrimitive() && parameter.getType().getNamespace() != null)
				imports.add("import " + parameter.getType().getNamespace().toString() + ".*;");

		if (methodType == BoundaryMethodTypeEnumeration.COUNT || methodType == BoundaryMethodTypeEnumeration.SEARCH)
			imports.add("import " + PACK_SEARCH_DTO + ".*;");

		if (jmsMethod.isSendResponse()) {
			imports.add("import java.time.Duration;");

			if (jmsMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				imports.add("import " + PACK_JAVA_UTIL + ".*;");

			if (jmsMethod.getReturnType() != null && !jmsMethod.getReturnType().isPrimitive()
					&& jmsMethod.getReturnType().getNamespace() != null)
				imports.add("import " + method.getReturnType().getNamespace().toString() + ".*;");
		}

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		final String returnComment = createReturnComment(method);

		b.append(method.generateBeginOfJavadocComment());

		if (!method.getIntegrationParameters().isEmpty())
			b.append(" * @param requestMessage contains all necessary parameters for calling the respective service method\n");

		if (!returnComment.isEmpty())
			b.append(" * @return " + returnComment + "\n");

		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethod()
	 */
	@Override
	public String createMethod() {
		final var b = new StringBuilder();
		String returnType = JavaType.VOID;

		if (!method.getReturnType().getName().equals(JavaType.VOID)) {
			returnType = method.getReturnType().getName();

			if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE) {
				parentGenerator.importPackage(PACK_JAVA_UTIL);

				returnType = "List<" + returnType + ">";
			}
		}

		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("private " + returnType + " " + method.getName());

		if (method.getIntegrationParameters().isEmpty())
			b.append("()");
		else
			b.append("(RequestMessage requestMessage)");

		b.append("\n");
		b.append("{\n");
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

		for (final IntegrationMethodParameter parameter : jmsMethod.getIntegrationParameters()) {
			b.append("final var " + parameter.getName() + " = requestMessage.");

			if (parameter.getType().equals(JavaType.STRING))
				b.append("getNextStringParameter()");
			else if (parameter.getType().equals(JavaType.UUID))
				b.append("getNextUUIDParameter()");
			else if (parameter.getType().equals(JavaType.INT))
				b.append("getNextIntParameter()");
			else if (parameter.getType().equals(JavaType.INTEGER))
				b.append("getNextIntegerParameter()");
			else if (parameter.getType().equals(JavaType.LONG))
				b.append("getNextLongParameter()");
			else if (parameter.getType().equals(JavaType.LONG_OBJ))
				b.append("getNextLongObjectParameter()");
			else if (parameter.getType().equals(JavaType.FLOAT))
				b.append("getNextFloatParameter()");
			else if (parameter.getType().equals(JavaType.FLOAT_OBJ))
				b.append("getNextFloatObjectParameter()");
			else if (parameter.getType().equals(JavaType.DOUBLE))
				b.append("getNextDoubleParameter()");
			else if (parameter.getType().equals(JavaType.DOUBLE_OBJ))
				b.append("getNextDoubleObjectParameter()");
			else if (parameter.getType().equals(JavaType.BOOL))
				b.append("getNextBoolParameter()");
			else if (parameter.getType().equals(JavaType.BOOL_OBJ))
				b.append("getNextBooleanParameter()");
			else if (parameter.getType().equals(JavaType.CHAR))
				b.append("getNextCharParameter()");
			else
				b.append("getNextParameter(" + parameter.getType() + ".class)");

			b.append(";\n");
		}

		b.append(createMethodLogic());

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

		jmsMethod.getIntegrationParameters().forEach(param -> {
			if (hasSearchInputParameter && param.getName().equals(SEARCH_PARAM_NAME)) {
				BoundaryMethod searchMethod = null;

				if (boundaryMethodType == BoundaryMethodTypeEnumeration.COUNT) {
					final var countMethod = jmsMethod.getBoundaryMethod();
					searchMethod = countMethod.getSearchMethod();
				}
				else
					searchMethod = jmsMethod.getBoundaryMethod();

				if (!jmsMethod.getIntegrationParameters().isEmpty())
					b.append("\n");

				if (searchMethod == null) {
					b.append("// WARNING: Appropriate search method couldn't be found!\n");
					b.append("final var converter = new SearchObjectConverter(" + SEARCH_PARAM_NAME + ");\n\n");
				}
				else {
					final JavaType returnType = searchMethod.getReturnType();

					parentGenerator.importPackage(returnType.getNamespace().toString());

					b.append(createSearchObjectConversion((DTOBean) returnType));
				}
			}
		});

		if (facadeFragment.isEmpty()) {
			boolean firstParam = true;

			if (!jmsMethod.getIntegrationParameters().isEmpty())
				b.append("\n");

			if (!jmsMethod.getReturnType().isVoid())
				b.append("return ");

			b.append(getServiceName() + "." + getServiceMethodName() + "(");

			for (final var param : jmsMethod.getIntegrationParameters()) {
				if (firstParam)
					firstParam = false;
				else
					b.append(", ");

				if (hasSearchInputParameter && param.getName().equals(SEARCH_PARAM_NAME))
					b.append("converter.convert()");
				else
					b.append(param.getName());
			}

			b.append(");\n");
		}
		else {
			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("final var def = new DefaultTransactionDefinition();\n");
				b.append("final TransactionStatus status = transactionManager.getTransaction(def);\n\n");
				b.append("try\n");
				b.append("{\n");
			}
			else if (!jmsMethod.getIntegrationParameters().isEmpty())
				b.append("\n");

			b.append(facadeFragment);

			if (addTransaction() && project.isSpringBootApplication())
				b.append("transactionManager.commit(status);\n");

			if (!jmsMethod.getReturnType().isVoid()) {
				b.append("\n");
				b.append("return " + getReturnObjectName() + ";\n");
			}

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");
				b.append("transactionManager.rollback(status);\n");
				b.append("// Just rethrow the original exception!\n");
				b.append("throw e;\n");
				b.append("}\n");
			}
		}

		return b.toString();
	}

	/**
	 * Create the comment for a JMS interface method
	 * @param waitForResponse
	 * @return the generated content
	 */
	public String createInterfaceComment(boolean waitForResponse) {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment());

		method.getIntegrationParameters().forEach(param -> {
			b.append(" * @param " + param.getName());

			if (param.getComment() != null)
				b.append(" " + param.getComment());

			b.append("\n");
		});

		if (waitForResponse) {
			final String returnComment = createReturnComment(method);

			b.append(" * @param maxWaitTime the maximum time to wait for the response. If null, the timeout never expires!\n");

			if (!returnComment.isEmpty())
				b.append(" * @return " + returnComment + "\n");
		}

		b.append(" * @throws " + getExceptionName());
		b.append(" due to a network problem, a problem in the JMS broker, or if the call failed in the backend\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * Create the signature of the JMS client method
	 * @param waitForResponse
	 * @param addQualifier
	 * @param generateFullSignature
	 * @return signature of the JMS client method
	 */
	public String getClientSignature(boolean waitForResponse, boolean addQualifier, boolean generateFullSignature) {
		final var b = new StringBuilder();
		final String returnType = method.getReturnType().getName();

		if (addQualifier)
			b.append("public ");

		if (waitForResponse) {
			if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				b.append(method.getReturnTypeModifier().getName() + "<" + returnType + ">");
			else
				b.append(returnType);
		}
		else
			b.append(JavaType.VOID);

		b.append(" " + method.getName() + "(");
		b.append(addMethodParameters(false, generateFullSignature));

		if (waitForResponse) {
			if (!method.getIntegrationParameters().isEmpty())
				b.append(", ");

			if (generateFullSignature)
				b.append("final ");

			b.append("Duration maxWaitTime");
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * Create the JMS client method
	 * @param waitForResponse
	 * @return the generated content
	 */
	public String createClientMethod(boolean waitForResponse) {
		final var b = new StringBuilder();
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());

		if (waitForResponse && jmsMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			b.append("@SuppressWarnings(\"unchecked\")\n");

		b.append("public " + getClientSignature(waitForResponse, false, true));
		b.append("\n{\n");
		b.append(createClientParameterChecks());
		b.append("final var requestMessage = new RequestMessage(" + createOperationIDConstant(jmsMethod.getBoundaryMethod()) + ")");

		// Add all parameters to the request message
		jmsMethod.getIntegrationParameters().forEach(param -> b.append("\n.withParameter(" + param.getName() + ")"));

		b.append(";\n\n");

		if (waitForResponse)
			b.append("final var correlationId = ");

		b.append("sendMessage(requestDestination, requestMessage);\n");

		if (waitForResponse) {
			final String returnType = jmsMethod.getReturnType().getName();

			if (!returnType.equals(JavaType.VOID)) {
				b.append("return (");

				if (jmsMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
					b.append(jmsMethod.getReturnTypeModifier() + "<" + returnType + ">");
				else
					b.append(returnType);

				b.append(") ");
			}

			b.append("receiveResponse(responseDestination, correlationId, maxWaitTime);\n");
		}

		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * Get a map for creating all necessary operation ID constants for a given {@link JMSIntegrationBean}
	 * @param jmsBean
	 * @return the new map
	 */
	public static Map<JMSIntegrationMethod, String> getOperationIDConstants(JMSIntegrationBean jmsBean) {
		return jmsBean.getMethods().stream().map(JMSIntegrationMethod.class::cast)
				.collect(Collectors.toMap(a -> a, method -> createOperationIDConstant(method.getBoundaryMethod())));
	}

	/**
	 * Create the name of an operation ID constant
	 * @param boundaryMethod
	 * @return the name of the operation ID constant
	 */
	private static String createOperationIDConstant(BoundaryMethod boundaryMethod) {
		return OPERATION_ID_PATTERN.matcher(boundaryMethod.getName()).replaceAll("_$1").toUpperCase();
	}

}
