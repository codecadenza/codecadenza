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
package net.codecadenza.eclipse.generator.integration.method.imp.kafka;

import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.LIST_INSTANCE_NAME;
import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.OBJ_INSTANCE_NAME;
import static net.codecadenza.eclipse.model.java.JavaType.UUID;
import static net.codecadenza.eclipse.shared.Constants.INTEGRATION_SEARCH_PARAM_TYPE;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_LANG;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;
import static net.codecadenza.eclipse.shared.Constants.REMOTE_OPERATION_EXCEPTION_NAME;
import static net.codecadenza.eclipse.shared.Constants.SEARCH_PARAM_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SEI;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.mapping.MappingObject;

/**
 * <p>
 * Base class for Kafka integration method generators
 * </p>
 * <p>
 * Copyright 2021 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicKafkaMethodGenerator extends AbstractIntegrationMethodGenerator {
	private static final String REQUEST_OBJ_NAME = "request";
	private static final String RESPONSE_OBJ_NAME = "response";

	protected final boolean hasResponseSchema;
	protected final boolean hasSearchInputParameter;
	protected final BoundaryMethodTypeEnumeration boundaryMethodType;
	protected final KafkaIntegrationMethod kafkaMethod;

	/**
	 * Constructor
	 * @param kafkaMethod
	 * @param parentGenerator
	 */
	public BasicKafkaMethodGenerator(KafkaIntegrationMethod kafkaMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(kafkaMethod, parentGenerator);

		this.kafkaMethod = kafkaMethod;
		this.hasResponseSchema = kafkaMethod.getResponseSchemaName() != null && !kafkaMethod.getResponseSchemaName().isEmpty();
		this.boundaryMethodType = kafkaMethod.getBoundaryMethod().getMethodType();
		this.hasSearchInputParameter = boundaryMethodType == BoundaryMethodTypeEnumeration.SEARCH
				|| boundaryMethodType == BoundaryMethodTypeEnumeration.COUNT;
	}

	/**
	 * Determine additional fields that should be added to a Kafka response message. This is necessary to provide important data
	 * regarding an operation for listeners that might have not originally triggered the corresponding request.
	 * @param integrationMethod
	 * @return a map that contains all additional fields
	 */
	public static Map<String, JavaType> getAdditionalResponseFields(KafkaIntegrationMethod integrationMethod) {
		final var responseFields = new HashMap<String, JavaType>();
		final BoundaryMethod boundaryMethod = integrationMethod.getBoundaryMethod();
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.CHANGE_PARENT || methodType == BoundaryMethodTypeEnumeration.COPY
				|| methodType == BoundaryMethodTypeEnumeration.DELETE || methodType == BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION
				|| methodType == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION)
			boundaryMethod.getMethodParameters().forEach(param -> responseFields.put(param.getName(), param.getType()));

		return responseFields;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		final boolean importUtilPackage = method.getMethodParameters().stream().map(MethodParameter::getModifier)
				.anyMatch(modifier -> modifier != JavaTypeModifierEnumeration.NONE);

		if (importUtilPackage || (hasResponseSchema && method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE))
			imports.add("import " + PACK_JAVA_UTIL + ".*;");

		if (hasSearchInputParameter) {
			imports.add("import " + PACK_SEARCH_DTO + ".*;");
			imports.add("import " + PACK_SEARCH_UTIL + ".*;");
		}

		if (!kafkaMethod.getIntegrationParameters().isEmpty() || (hasResponseSchema && !kafkaMethod.getReturnType().isVoid()))
			imports.add("import net.codecadenza.runtime.avro.util.*;");

		if (hasResponseSchema && method.getReturnType() instanceof MappingObject)
			imports.add("import " + method.getReturnType().getNamespace().toString() + "." + method.getReturnType().getName() + ";");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(getIntegrationMethodParameterImports());
		final boolean importUtilPackage = method.getMethodParameters().stream().map(MethodParameter::getModifier)
				.anyMatch(modifier -> modifier != JavaTypeModifierEnumeration.NONE);

		imports.add("import " + kafkaMethod.getIntegrationBean().getNamespace().toString() + SUB_PACKAGE_INT_AVRO + ".*;");

		if (importUtilPackage || (hasResponseSchema && method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE))
			imports.add("import " + PACK_JAVA_UTIL + ".*;");

		if (kafkaMethod.isSendResponse())
			imports.add("import net.codecadenza.runtime.transport.*;");

		if (boundaryMethodType == BoundaryMethodTypeEnumeration.SEARCH || boundaryMethodType == BoundaryMethodTypeEnumeration.COUNT)
			imports.add("import net.codecadenza.runtime.avro.search.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getMethodSignature(boolean,
	 * boolean, boolean)
	 */
	@Override
	public String getMethodSignature(boolean addQualifier, boolean addAnnotations, boolean generateFullSignature) {
		final var b = new StringBuilder();

		if (addQualifier)
			b.append("public ");

		if (hasResponseSchema) {
			if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				b.append(method.getReturnTypeModifier().getName() + "<" + method.getReturnType().getName() + ">");
			else
				b.append(method.getReturnType().getName());
		}
		else
			b.append(JavaType.VOID);

		b.append(" " + method.getName() + "(");
		b.append(addMethodParameters(addAnnotations, generateFullSignature));
		b.append(")");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(kafkaMethod.generateBeginOfJavadocComment());

		if (!kafkaMethod.getIntegrationParameters().isEmpty())
			b.append(" * @param payload a byte array that contains the message payload\n");

		if (kafkaMethod.isSendResponse()) {
			b.append(" * @param correlationId the correlation ID provided by the corresponding request message\n");

			if (kafkaMethod.isUseDedicatedPartition())
				b.append(" * @param partitionId the ID of the partition in which the response should be saved\n");
		}

		if (!kafkaMethod.getIntegrationParameters().isEmpty())
			b.append(" * @throws AvroObjectDeserializationException if the payload could not be deserialized\n");

		if (kafkaMethod.isSendResponse()) {
			b.append(" * @throws AvroObjectSerializationException if the response object could not be serialized\n");
			b.append(" * @throws KafkaSenderException if the response message could not be sent\n");
		}

		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createCommentLink()
	 */
	@Override
	public String createCommentLink() {
		final var b = new StringBuilder();
		final String packageName = method.getIntegrationBean().getNamespace().toString() + SUB_PACKAGE_INT_SEI;
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
					if (methodParameter.getType().getNamespace() != null) {
						if (methodParameter.getType() instanceof MappingObject)
							b.append(kafkaMethod.getIntegrationBean().getNamespace().toString() + SUB_PACKAGE_INT_AVRO);
						else
							b.append(methodParameter.getType().getNamespace().toString());
					}
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
					b.append("net.codecadenza.runtime.avro.search.");

				b.append(typeName);
			}
		}

		b.append(")\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * Create the signature of the Kafka client method
	 * @param waitForResponse
	 * @param addQualifier
	 * @param generateFullSignature
	 * @return signature of the Kafka client method
	 */
	public String getClientSignature(boolean waitForResponse, boolean addQualifier, boolean generateFullSignature) {
		final var b = new StringBuilder();

		if (addQualifier)
			b.append("public ");

		if (waitForResponse && hasResponseSchema) {
			final String returnType = method.getReturnType().getName();

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

			b.append("boolean waitForResponse");
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * Create the comment for a Kafka interface method
	 * @param waitForResponse
	 * @return the comment for the given interface method
	 */
	public String createInterfaceComment(boolean waitForResponse) {
		final var b = new StringBuilder();
		final String returnComment = createReturnComment(method);

		b.append("/**\n");

		if (method.getComment() != null && !method.getComment().isEmpty())
			b.append(" * " + method.getComment() + "\n");

		method.getIntegrationParameters().forEach(param -> {
			b.append(" * @param " + param.getName());

			if (param.getComment() != null)
				b.append(" " + param.getComment());

			b.append("\n");
		});

		if (waitForResponse) {
			b.append(" * @param waitForResponse flag that controls if the method should wait for the response message\n");

			if (hasResponseSchema && !returnComment.isEmpty())
				b.append(" * @return " + returnComment + "\n");

			b.append(" * @throws AvroObjectSerializationException if the request message could not be serialized\n");
			b.append(" * @throws AvroObjectDeserializationException if the response message could not be deserialized\n");
			b.append(" * @throws " + REMOTE_OPERATION_EXCEPTION_NAME);
			b.append(" if either the request message could not be sent or the respective response message ");
			b.append("could not be received or the call failed in the backend\n");
		}
		else {
			b.append(" * @throws AvroObjectSerializationException if the request message could not be serialized\n");
			b.append(" * @throws " + REMOTE_OPERATION_EXCEPTION_NAME + " if the request message could not be sent\n");
		}

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
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("private " + getInternalMethodSignature());
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

		if (!kafkaMethod.getIntegrationParameters().isEmpty()) {
			b.append("final var schema = " + kafkaMethod.getRequestSchemaName() + ".getClassSchema();\n");
			b.append("final var " + REQUEST_OBJ_NAME + " = new AvroObjectDeserializer<");
			b.append(kafkaMethod.getRequestSchemaName() + ">(schema).deserialize(payload);\n");
		}

		if (kafkaMethod.isSendResponse()) {
			b.append("var responseStatus = ResponseStatusInitializer.withSuccessStatus();\n");

			if (hasResponseSchema) {
				b.append("final var " + RESPONSE_OBJ_NAME + " = " + kafkaMethod.getResponseSchemaName());
				b.append(".newBuilder().setResponseStatus(responseStatus).build();\n");
			}

			b.append("RuntimeException exception = null;\n");
		}

		if (!kafkaMethod.getIntegrationParameters().isEmpty() || kafkaMethod.isSendResponse())
			b.append("\n");

		if (kafkaMethod.isSendResponse()) {
			b.append("try\n");
			b.append("{\n");
		}

		b.append(createMethodLogic());

		if (kafkaMethod.isSendResponse()) {
			b.append("}\n");
			b.append("catch (RuntimeException e)\n");
			b.append("{\n");
			b.append("exception = e;\n");
			b.append("responseStatus = ResponseStatusInitializer.fromException(e);\n");

			if (hasResponseSchema)
				b.append(RESPONSE_OBJ_NAME + ".setResponseStatus(responseStatus);\n");

			b.append("}\n\n");

			if (hasResponseSchema)
				b.append("kafkaSender.sendResponse(RESPONSE_TOPIC, " + RESPONSE_OBJ_NAME + ", correlationId");
			else
				b.append("kafkaSender.sendResponse(RESPONSE_TOPIC, responseStatus, correlationId");

			if (kafkaMethod.isUseDedicatedPartition())
				b.append(", partitionId");

			b.append(");\n\n");
			b.append("if(exception != null)\n");
			b.append("throw exception;\n");
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
		boolean firstParam = true;

		// Convert the data from the request
		kafkaMethod.getIntegrationParameters().forEach(param -> {
			final var paramGetter = "get" + param.getName().substring(0, 1).toUpperCase() + param.getName().substring(1) + "()";

			b.append("final var " + param.getName() + " = ");

			final MethodParameter methodParameter = kafkaMethod.getMethodParameters().stream()
					.filter(p -> p.getName().equals(param.getName())).findFirst().orElse(null);

			if (methodParameter != null && methodParameter.getType() instanceof MappingObject) {
				final JavaType paramType = methodParameter.getType();
				final var conversionMethod = methodParameter.getModifier() == JavaTypeModifierEnumeration.NONE ? "toObject"
						: "toObjectList";

				parentGenerator.importClass(paramType.getNamespace().toString() + "." + paramType.getName());

				b.append("AvroObjectConverter." + conversionMethod + "(" + REQUEST_OBJ_NAME);
				b.append("." + paramGetter + ", " + paramType.getName() + ".class);\n");
			}
			else if (methodParameter != null && methodParameter.getType().isUUID() || param.getType().equals(UUID)) {
				parentGenerator.importPackage("net.codecadenza.runtime.avro.util");

				b.append("UuidConverter.getUUID(" + REQUEST_OBJ_NAME + "." + paramGetter + ");\n");
			}
			else if (hasSearchInputParameter && param.getName().equals(SEARCH_PARAM_NAME)) {
				final var getter = "get" + SEARCH_PARAM_NAME.substring(0, 1).toUpperCase() + SEARCH_PARAM_NAME.substring(1) + "()";
				BoundaryMethod searchMethod = null;

				b.append("AvroObjectConverter.toObject(" + REQUEST_OBJ_NAME);
				b.append("." + getter + ", SearchInput.class);\n\n");

				if (boundaryMethodType == BoundaryMethodTypeEnumeration.COUNT) {
					final var countMethod = kafkaMethod.getBoundaryMethod();
					searchMethod = countMethod.getSearchMethod();
				}
				else
					searchMethod = kafkaMethod.getBoundaryMethod();

				if (searchMethod == null) {
					b.append("// WARNING: Appropriate search method couldn't be found!\n");
					b.append("final var converter = new SearchObjectConverter(" + SEARCH_PARAM_NAME + ");\n\n");
				}
				else {
					final JavaType returnType = searchMethod.getReturnType();

					parentGenerator.importClass(returnType.getNamespace().toString() + "." + returnType.getName());

					b.append(createSearchObjectConversion((DTOBean) returnType));
				}
			}
			else
				b.append(REQUEST_OBJ_NAME + "." + paramGetter + ";\n");
		});

		// Invoke the actual boundary method
		if (hasResponseSchema && !kafkaMethod.getReturnType().isVoid()) {
			final String returnType = kafkaMethod.getReturnType().getWrapperTypeName();

			if (kafkaMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				b.append("final List<" + returnType + "> " + LIST_INSTANCE_NAME + " = ");
			else
				b.append("final " + returnType + " " + OBJ_INSTANCE_NAME + " = ");
		}

		b.append(getServiceName() + "." + getServiceMethodName() + "(");

		for (final var param : kafkaMethod.getIntegrationParameters()) {
			if (firstParam)
				firstParam = false;
			else
				b.append(",");

			if (hasSearchInputParameter && param.getName().equals(SEARCH_PARAM_NAME))
				b.append("converter.convert()");
			else
				b.append(param.getName());
		}

		b.append(");\n");

		if (hasResponseSchema) {
			final Map<String, JavaType> additionalFields = getAdditionalResponseFields(kafkaMethod);

			if (!kafkaMethod.getReturnType().isVoid() || !additionalFields.isEmpty())
				b.append("\n");

			if (!kafkaMethod.getReturnType().isVoid()) {
				if (kafkaMethod.getReturnType() instanceof MappingObject) {
					final JavaType returnType = kafkaMethod.getReturnType();
					final var avroType = kafkaMethod.getIntegrationBean().getNamespace().toString() + SUB_PACKAGE_INT_AVRO + "."
							+ returnType.getName();
					final String conversionMethod;
					final String invocationResult;

					if (kafkaMethod.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE) {
						conversionMethod = "toAvro";
						invocationResult = OBJ_INSTANCE_NAME;
					}
					else {
						conversionMethod = "toAvroList";
						invocationResult = LIST_INSTANCE_NAME;
					}

					b.append("final var responseData = AvroObjectConverter.");
					b.append(conversionMethod + "(" + invocationResult + ", " + avroType + ".class);\n");
					b.append(RESPONSE_OBJ_NAME + ".setResponseData(responseData);\n");
				}
				else if (kafkaMethod.getReturnType().isUUID()) {
					parentGenerator.importPackage("net.codecadenza.runtime.avro.util");
					parentGenerator.importPackage(PACK_JAVA_UTIL);

					b.append(RESPONSE_OBJ_NAME + ".setResponseData(UuidConverter.from(" + OBJ_INSTANCE_NAME + "));\n");
				}
				else
					b.append(RESPONSE_OBJ_NAME + ".setResponseData(" + OBJ_INSTANCE_NAME + ");\n");
			}

			for (final var entrySet : additionalFields.entrySet()) {
				final String fieldName = entrySet.getKey();
				final var fieldSetter = "set" + fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);

				b.append(RESPONSE_OBJ_NAME + "." + fieldSetter + "(");

				if (entrySet.getValue().isUUID()) {
					parentGenerator.importPackage("net.codecadenza.runtime.avro.util");

					b.append("UuidConverter.from(" + fieldName + ")");
				}
				else
					b.append(fieldName);

				b.append(");\n");
			}
		}

		return b.toString();
	}

	/**
	 * @return the method signature that is used in the message listener
	 */
	public String getInternalMethodSignature() {
		final var b = new StringBuilder();
		b.append(JavaType.VOID);
		b.append(" " + method.getName() + "(");

		if (!method.getIntegrationParameters().isEmpty())
			b.append(JavaType.BYTE_ARRAY + " payload");

		if (kafkaMethod.isSendResponse()) {
			if (!method.getIntegrationParameters().isEmpty())
				b.append(", ");

			b.append(JavaType.BYTE_ARRAY + " correlationId");

			if (kafkaMethod.isUseDedicatedPartition())
				b.append(", " + JavaType.INT + " partitionId");
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * @param waitForResponse
	 * @return the generated content
	 */
	public String createClientMethod(boolean waitForResponse) {
		final var b = new StringBuilder();
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append(getClientSignature(waitForResponse, true, true));
		b.append("\n{\n");
		b.append(createClientParameterChecks());

		if (hasSearchInputParameter) {
			parentGenerator.importClass("java.util.Collections");
			parentGenerator.importClass("java.text.DecimalFormatSymbols");

			b.append("if(" + SEARCH_PARAM_NAME + ".getSearchFields() == null)\n");
			b.append(SEARCH_PARAM_NAME + ".setSearchFields(Collections.emptyList());\n\n");
			b.append("if(" + SEARCH_PARAM_NAME + ".getDecimalSeparator() == null)\n");
			b.append(SEARCH_PARAM_NAME);
			b.append(".setDecimalSeparator(String.valueOf(DecimalFormatSymbols.getInstance().getDecimalSeparator()));\n\n");
			b.append("if(" + SEARCH_PARAM_NAME + ".getGroupingSeparator() == null)\n");
			b.append(SEARCH_PARAM_NAME);
			b.append(".setGroupingSeparator(String.valueOf(DecimalFormatSymbols.getInstance().getGroupingSeparator()));\n\n");
		}

		if (kafkaMethod.isSendResponse()) {
			b.append("final var correlationId = UUID.randomUUID();\n");

			if (kafkaMethod.isUseDedicatedPartition())
				b.append("final var partitionId = getRandomPartition(RESPONSE_TOPIC);\n");
		}

		b.append("final var operation = " + kafkaMethod.getRequestSchemaName() + ".newBuilder()");

		kafkaMethod.getIntegrationParameters().forEach(param -> {
			final var requestParamSetter = "set" + param.getName().substring(0, 1).toUpperCase() + param.getName().substring(1);

			b.append("." + requestParamSetter + "(");

			if (param.getType().equals(UUID)) {
				parentGenerator.importPackage("net.codecadenza.runtime.avro.util");

				b.append("UuidConverter.from(" + param.getName() + ")");
			}
			else
				b.append(param.getName());

			b.append(")");
		});

		b.append(".build();\n\n");
		b.append("sendRequest(REQUEST_TOPIC, operation");

		if (kafkaMethod.isSendResponse()) {
			b.append(", correlationId");

			if (kafkaMethod.isUseDedicatedPartition())
				b.append(", partitionId");
		}

		b.append(");\n");

		if (waitForResponse) {
			b.append("\n");
			b.append("if(waitForResponse)\n");
			b.append("{\n");
			b.append("final var response = getResponse(RESPONSE_TOPIC, new ");

			if (hasResponseSchema)
				b.append(kafkaMethod.getResponseSchemaName());
			else
				b.append("ResponseStatus");

			b.append("(), correlationId");

			if (kafkaMethod.isUseDedicatedPartition())
				b.append(", partitionId");

			b.append(");\n\n");

			if (hasResponseSchema) {
				b.append("if(response.getResponseStatus().getCode() == ResponseCode.SUCCESS)\n");

				if (kafkaMethod.getReturnType().isVoid())
					b.append("return;\n\n");
				else if (kafkaMethod.getReturnType().isUUID()) {
					parentGenerator.importPackage("net.codecadenza.runtime.avro.util");

					b.append("return UuidConverter.getUUID(response.getResponseData());\n\n");
				}
				else
					b.append("return response.getResponseData();\n\n");

				b.append("throw new RemoteOperationException(response.getResponseStatus().getMessage());\n");
			}
			else {
				b.append("if(response.getCode() == ResponseCode.SUCCESS)\n");
				b.append("return;\n\n");
				b.append("throw new RemoteOperationException(response.getMessage());\n");
			}

			b.append("}\n");

			if (hasResponseSchema && !kafkaMethod.getReturnType().isVoid()) {
				b.append("\n");
				b.append("return " + kafkaMethod.getReturnType().getLocalVariableDefaultValue() + ";\n");
			}
		}

		b.append("}\n\n");

		return b.toString();
	}

}
