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
package net.codecadenza.eclipse.generator.testing.integration;

import static net.codecadenza.eclipse.generator.testing.integration.util.FileHandlingGenerator.DOWNLOAD_FILE_CONSTANT;
import static net.codecadenza.eclipse.generator.testing.integration.util.FileHandlingGenerator.FILE_SERVICE_NAME;
import static net.codecadenza.eclipse.generator.testing.integration.util.FileHandlingGenerator.UPLOAD_FILE_PATH;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.testing.integration.util.FileHandlingGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;

/**
 * <p>
 * Generator for integration test cases
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestCaseGenerator extends AbstractJavaSourceGenerator {
	private static final String DOWNLOAD_FILE_CONSTANT_VALUE = "\"testfile\"";
	private static final String EXPECTED_RESULT_OBJECT = "expectedResultObject";
	private static final String EXPECTED_RESULT_LIST = "expectedResultList";
	private static final String ACTUAL_RESULT_OBJECT = "actualResultObject";
	private static final String ACTUAL_RESULT_LIST = "actualResultList";

	private final IntegrationTestCase testCase;
	private final Map<IntegrationMethodTestInvocation, Map<TestDataAttribute, String>> invocationAttributes = new HashMap<>();
	private final Set<AbstractIntegrationBean> integrationBeans;
	private final Project project;
	private final boolean addCompletionHandler;
	private final boolean addFileHandling;
	private final boolean addPreProcessingStatements;
	private final boolean addPostProcessingStatements;
	private final IntegrationTechnology integrationTechnology;
	private String fileServiceClientName;
	private boolean importAssertThat;

	/**
	 * Constructor
	 * @param testCase
	 */
	public IntegrationTestCaseGenerator(IntegrationTestCase testCase) {
		super(testCase.getSourceFile());

		this.testCase = testCase;
		this.project = testCase.getNamespace().getProject();
		this.integrationBeans = testCase.getMethodInvocations().stream().map(IntegrationMethodTestInvocation::getIntegrationMethod)
				.map(AbstractIntegrationMethod::getIntegrationBean).collect(Collectors.toSet());
		this.addCompletionHandler = testCase.getMethodInvocations().stream().anyMatch(
				i -> !i.isExpectToFail() && i.getPostProcessingStatement() != null && !i.getPostProcessingStatement().isEmpty());
		this.addPreProcessingStatements = testCase.getPreProcessingStatements() != null
				&& !testCase.getPreProcessingStatements().isEmpty();
		this.addPostProcessingStatements = testCase.getPostProcessingStatements() != null
				&& !testCase.getPostProcessingStatements().isEmpty();
		this.addFileHandling = testCase.isFileHandlingRequired();
		this.integrationTechnology = integrationBeans.stream().map(AbstractIntegrationBean::getIntegrationTechnology).findFirst()
				.orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		if (testCase.getMethodInvocations().isEmpty())
			return;

		importPackage("java.io");
		importPackage("net.codecadenza.runtime.ddt.service.data");
		importPackage("org.junit.jupiter.api");

		if (addCompletionHandler)
			importPackage("net.codecadenza.runtime.ddt.service.completion");

		if (addPreProcessingStatements || addPostProcessingStatements)
			importPackage("net.codecadenza.runtime.ddt.service.preparation");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("class " + testCase.getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (testCase.getMethodInvocations().isEmpty())
			return;

		final String xmlPath = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";

		addPrivateConstant(JavaType.STRING, "XML_FILE_PATH", "\"" + xmlPath + "\"").create();
		addPrivateField("ITestDataProvider", "testDataProvider").withStaticModifier().create();

		if (addCompletionHandler)
			addPrivateField("IInvocationCompletionHandler", "completionHandler").withStaticModifier().create();

		addConstantsForTrackingAttributes();
		addConstantsForFieldsWithExpectedSize();

		if (addPreProcessingStatements || addPostProcessingStatements)
			addPrivateField("IStatementProcessor", "statementProcessor").withStaticModifier().create();

		if (testCase.addCredentials() || addPreProcessingStatements || addPostProcessingStatements)
			addPrivateField("TestData", "testData").withStaticModifier().create();

		for (final AbstractIntegrationBean integrationBean : integrationBeans) {
			final String serviceName = integrationBean.getDomainObject().getLowerCaseName() + "Service";

			if (integrationTechnology == IntegrationTechnology.SOAP || integrationTechnology == IntegrationTechnology.RMI) {
				importPackage(integrationBean.getNamespace().toString());

				addPrivateField(integrationBean.getInterfaceName(), serviceName).withStaticModifier().create();
			}
			else
				addPrivateField(integrationBean.getClientClassName(), serviceName).withStaticModifier().create();
		}

		if (addFileHandling) {
			final AbstractIntegrationMethod integrationMethod = testCase.getMethodInvocations().getFirst().getIntegrationMethod();
			final AbstractIntegrationBean integrationBean = integrationMethod.getIntegrationBean();
			final IntegrationModule integrationModule = integrationBean.getIntegrationModule();

			fileServiceClientName = integrationModule.getFileServiceClientName();

			if (integrationTechnology == IntegrationTechnology.SOAP || integrationTechnology == IntegrationTechnology.RMI) {
				importPackage(integrationBean.getNamespace().toString());
				importPackage("java.nio.file");

				if (integrationTechnology == IntegrationTechnology.RMI)
					importPackage("net.codecadenza.runtime.transport.file");

				addPrivateField(integrationModule.getFileServiceName(), FILE_SERVICE_NAME).withStaticModifier().create();
			}
			else {
				if (integrationTechnology == IntegrationTechnology.JMS)
					importPackage("java.time");

				addPrivateField(fileServiceClientName, FILE_SERVICE_NAME).withStaticModifier().create();
			}

			for (final IntegrationMethodTestInvocation invocation : testCase.getMethodInvocations())
				if (invocation.isDownloadFile()) {
					importPackage("org.junit.jupiter.api.io");
					importPackage("java.nio.file");

					addPrivateConstant(JavaType.STRING, DOWNLOAD_FILE_CONSTANT, DOWNLOAD_FILE_CONSTANT_VALUE).create();
					addPrivateField("Path", "testDir").withAnnotations("@TempDir\n").create();
					break;
				}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (testCase.getMethodInvocations().isEmpty())
			return;

		addInitMethod();

		addBasicWorkflowMethod();

		for (final IntegrationMethodTestInvocation invocation : testCase.getMethodInvocations())
			addTestMethod(invocation);

		if (addPostProcessingStatements)
			addCleanUpMethod();
	}

	/**
	 * Add all constants for the attribute IDs that should be tracked
	 */
	private void addConstantsForTrackingAttributes() {
		final Set<String> constantNames = new HashSet<>();
		final Map<String, String> attributeIds = new HashMap<>();

		for (final IntegrationMethodTestInvocation invocation : testCase.getMethodInvocations()) {
			final TestDataAttribute trackedAttribute = invocation.getTrackedAttribute();

			if (trackedAttribute != null) {
				final MappingAttribute mappingAttribute = trackedAttribute.getMappingAttribute();
				final List<String> ids = invocation.getIdsOfTrackedAttributes(mappingAttribute);
				String initialConstantName;
				int counter = 0;

				if (mappingAttribute != null)
					initialConstantName = "IDS_OF_FIELD_" + mappingAttribute.getName().toUpperCase();
				else
					initialConstantName = "IDS_OF_RETURN_VALUE";

				String newConstantName = initialConstantName;

				while (true) {
					if (counter > 0)
						newConstantName = initialConstantName + "_" + counter;

					if (constantNames.contains(newConstantName)) {
						counter++;
						continue;
					}

					constantNames.add(newConstantName);

					invocationAttributes.computeIfAbsent(invocation, key -> new HashMap<>());
					invocationAttributes.get(invocation).put(trackedAttribute, newConstantName);
					break;
				}

				attributeIds.put(newConstantName, ids.stream().map(id -> "\"" + id + "\"").collect(Collectors.joining(",")));
			}
		}

		for (final var constantName : constantNames) {
			final String constantValue = "new LinkedList<>(List.of(" + attributeIds.get(constantName) + "))";

			importPackage("java.util");

			addPrivateConstant("LinkedList<String>", constantName, constantValue).create();
		}
	}

	/**
	 * Add all constants for the fields where the expected size should be checked
	 */
	private void addConstantsForFieldsWithExpectedSize() {
		final List<String> attributeIds = new ArrayList<>();

		for (final IntegrationMethodTestInvocation invocation : testCase.getMethodInvocations())
			attributeIds.addAll(invocation.getIdsOfExpectedSizeFields());

		if (!attributeIds.isEmpty()) {
			importPackage("java.util");

			final String ids = attributeIds.stream().map(id -> "\"" + id + "\"").collect(Collectors.joining(","));
			final String constantValue = "new LinkedList<>(List.of(" + ids + "))";

			addPrivateConstant("LinkedList<String>", "EXPECTED_SIZE_IDS", constantValue).create();
		}
	}

	/**
	 * Create the method that initializes the test
	 */
	private void addInitMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void init()";

		b.append("/**\n");
		b.append(" * Initialize the {@link ITestDataProvider} and the services that should be tested\n");
		b.append(" */\n");
		b.append("@BeforeAll\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("static " + methodSignature);

		if (integrationTechnology == IntegrationTechnology.RMI) {
			importClass("javax.naming.NamingException");

			b.append(" throws NamingException");
		}

		b.append("\n{\n");
		b.append("final var testDataProviderProperties = new TestDataProviderProperties();\n");
		b.append("testDataProviderProperties.load();\n\n");
		b.append("testDataProvider = TestDataProviderFactory.getTestDataProvider(");
		b.append("new File(XML_FILE_PATH), testDataProviderProperties);\n");

		if (testCase.addCredentials() || addPreProcessingStatements || addPostProcessingStatements) {
			importClass("net.codecadenza.runtime.ddt.model.TestData");

			b.append("testData = testDataProvider.loadTestData();\n");
		}

		b.append("\n");

		if (addCompletionHandler) {
			b.append("final var invocationHandlerProperties = new InvocationCompletionHandlerProperties();\n");
			b.append("invocationHandlerProperties.load();\n\n");
			b.append("completionHandler = InvocationCompletionHandlerFactory.getPostProcessor(invocationHandlerProperties);\n\n");
		}

		if (addPreProcessingStatements || addPostProcessingStatements) {
			b.append("final var statementProcessorProperties = new StatementProcessorProperties();\n");
			b.append("statementProcessorProperties.load();\n\n");
			b.append("statementProcessor = StatementProcessorFactory.getStatementProcessor(statementProcessorProperties);\n");
			b.append("statementProcessor.executeStatements(testData.getPreProcessingStatements());\n\n");
		}

		for (final AbstractIntegrationBean integrationBean : integrationBeans) {
			final String serviceName = integrationBean.getDomainObject().getLowerCaseName() + "Service";

			b.append(serviceName + " = new " + integrationBean.getClientClassName() + "(");

			if (testCase.addCredentials())
				b.append("testData.getUserName(), testData.getPassword()");

			b.append(")");

			if (integrationTechnology == IntegrationTechnology.SOAP || integrationTechnology == IntegrationTechnology.RMI)
				b.append(".getService()");

			b.append(";\n");

			if (integrationTechnology == IntegrationTechnology.JMS)
				b.append(serviceName + ".init();\n");
		}

		if (addFileHandling) {
			b.append(FILE_SERVICE_NAME + " = new " + fileServiceClientName + "(");

			if (testCase.addCredentials())
				b.append("testData.getUserName(), testData.getPassword()");

			b.append(")");

			if (integrationTechnology == IntegrationTechnology.SOAP || integrationTechnology == IntegrationTechnology.RMI)
				b.append(".getService()");

			b.append(";\n");

			if (integrationTechnology == IntegrationTechnology.JMS)
				b.append(FILE_SERVICE_NAME + ".init();\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method for running the post-processing statements
	 */
	private void addCleanUpMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void cleanUp()";

		b.append("/**\n");
		b.append(" * Run all post-processing statements\n");
		b.append(" */\n");
		b.append("@AfterAll\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("static " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(statementProcessor != null)\n");
		b.append("statementProcessor.executeStatements(testData.getPostProcessingStatements());\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the method that performs all method invocations
	 */
	private void addBasicWorkflowMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void testBasicWorkflow()";
		IntegrationMethodTestInvocation previousInvocation = null;

		b.append("/**\n");
		b.append(" * Perform the actual test\n");
		b.append(" */\n");
		b.append("@Test\n");
		b.append(getAnnotationForGeneratedElement());
		b.append(methodSignature + "\n");
		b.append("{\n");
		b.append("MethodInvocation invocation = testDataProvider.getNextInvocation();\n\n");

		for (final IntegrationMethodTestInvocation methodInvocation : testCase.getMethodInvocations()) {
			importClass("net.codecadenza.runtime.ddt.model.MethodInvocation");

			if (previousInvocation != null) {
				b.append("\n");

				if (previousInvocation.getNestedInvocations().isEmpty())
					b.append("invocation = testDataProvider.getNextInvocation();\n\n");
			}

			if (!methodInvocation.getNestedInvocations().isEmpty()) {
				importPackage("java.util");

				b.append("while(true)\n");
				b.append("{\n");
				b.append("final UUID groupId = invocation.getGroupId();\n\n");
				b.append(methodInvocation.getTestMethodName() + "(invocation);\n\n");
				b.append("invocation = testDataProvider.getNextInvocation();\n\n");
				b.append("if(invocation == null || !groupId.equals(invocation.getGroupId()))\n");
				b.append("break;\n");
				b.append("}\n");
			}
			else
				b.append(methodInvocation.getTestMethodName() + "(invocation);\n");

			previousInvocation = methodInvocation;
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the test method for the given {@link IntegrationMethodTestInvocation}
	 * @param methodInvocation
	 */
	private void addTestMethod(IntegrationMethodTestInvocation methodInvocation) {
		final var b = new StringBuilder();
		final var methodSignature = "void " + methodInvocation.getTestMethodName() + "(MethodInvocation invocation)";
		final AbstractIntegrationMethod integrationMethod = methodInvocation.getIntegrationMethod();
		final String serviceName = integrationMethod.getIntegrationBean().getDomainObject().getLowerCaseName() + "Service";
		final String validationResult = validateResult(methodInvocation);

		b.append("/**\n");
		b.append(" * Test method " + integrationMethod.getIntegrationBean().getClientClassName());
		b.append("::" + integrationMethod.getName());

		if (methodInvocation.isExpectToFail())
			b.append(" which is expected to fail");

		b.append("\n");
		b.append(" * @param invocation\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		for (final MethodInvocationParameter parameter : methodInvocation.getParameters()) {
			final JavaType paramType = parameter.getType();

			if (paramType == null) {
				if (integrationTechnology == IntegrationTechnology.KAFKA)
					importClass("net.codecadenza.runtime.avro.search.SearchInput");
				else
					importClass("net.codecadenza.runtime.search.dto.SearchInput");

				b.append("final var searchInput = testDataProvider.getNextParameter(SearchInput.class);\n");
			}
			else {
				importType(integrationMethod, paramType);

				b.append("final var " + parameter.getName() + " = ");
				b.append("testDataProvider.getNextParameter(" + paramType.getName() + ".class);\n");
			}
		}

		if (methodInvocation.isUploadFile())
			b.append(new FileHandlingGenerator(methodInvocation).addFileUpload());

		if (validationResult.contains(EXPECTED_RESULT_OBJECT) || validationResult.contains(EXPECTED_RESULT_LIST)) {
			final JavaType returnType = methodInvocation.getIntegrationMethod().getReturnType();

			importType(integrationMethod, returnType);

			b.append("final var ");

			if (methodInvocation.isReturnList())
				b.append(EXPECTED_RESULT_LIST + " = testDataProvider.getReturnListValue");
			else
				b.append(EXPECTED_RESULT_OBJECT + " = testDataProvider.getReturnValue");

			b.append("(" + returnType.getName() + ".class);\n\n");
		}

		if (validationResult.contains(ACTUAL_RESULT_OBJECT) || validationResult.contains(ACTUAL_RESULT_LIST)
				|| methodInvocation.isDownloadFile()) {
			b.append("final var ");

			if (methodInvocation.isReturnList())
				b.append(ACTUAL_RESULT_LIST);
			else
				b.append(ACTUAL_RESULT_OBJECT);

			b.append(" = ");
		}

		// Ignore the timeout if the method is expected to fail!
		if (methodInvocation.isExpectToFail()) {
			importStaticClass("org.junit.jupiter.api.Assertions.assertThrows");

			b.append("assertThrows(Exception.class, () -> ");
		}
		else if (methodInvocation.getTimeout() != null && !methodInvocation.isWaitForResponseParameterRequired()) {
			importStaticClass("org.junit.jupiter.api.Assertions.assertTimeoutPreemptively");

			b.append("assertTimeoutPreemptively(invocation.getTimeout(), () -> ");
		}

		b.append(serviceName + "." + integrationMethod.getName() + "(");
		b.append(createParameterList(methodInvocation));
		b.append(")");

		if (methodInvocation.isExpectToFail()
				|| (methodInvocation.getTimeout() != null && !methodInvocation.isWaitForResponseParameterRequired()))
			b.append(")");

		b.append(";\n");

		if (methodInvocation.isDownloadFile()) {
			importAssertThat = true;

			b.append(new FileHandlingGenerator(methodInvocation).addFileDownload());
		}
		else if (!validationResult.isEmpty()) {
			b.append("\n");
			b.append(validationResult);
		}
		else
			importAssertThat = false;

		if (!methodInvocation.isExpectToFail() && methodInvocation.getPostProcessingStatement() != null
				&& !methodInvocation.getPostProcessingStatement().isEmpty())
			b.append(addPostProcessing(methodInvocation));

		b.append("}\n\n");

		if (importAssertThat)
			importStaticClass("org.assertj.core.api.Assertions.assertThat");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Import the required type
	 * @param integrationMethod
	 * @param type
	 */
	private void importType(final AbstractIntegrationMethod integrationMethod, final JavaType type) {
		if (type.isPrimitive() || type.getNamespace() == null)
			return;

		if (integrationTechnology == IntegrationTechnology.KAFKA) {
			final AbstractIntegrationBean integrationBean = integrationMethod.getIntegrationBean();
			final String avroPackage = integrationBean.getNamespace().toString() + SUB_PACKAGE_INT_AVRO;

			importClass(avroPackage + "." + type.getName());
		}
		else
			importClass(type.getNamespace().toString() + "." + type.getName());
	}

	/**
	 * Create the list of parameters necessary for invoking an integration client method
	 * @param methodInvocation
	 * @return the generated content
	 */
	private String createParameterList(IntegrationMethodTestInvocation methodInvocation) {
		if (methodInvocation.isUploadFile()) {
			final AbstractIntegrationMethod integrationMethod = methodInvocation.getIntegrationMethod();
			final BoundaryMethodTypeEnumeration methodType = integrationMethod.getBoundaryMethod().getMethodType();

			if (methodType == BoundaryMethodTypeEnumeration.UPLOAD || methodType == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT) {
				final int size = methodInvocation.getParameters().size();
				final StringJoiner parameterList = new StringJoiner(", ");

				for (int i = 0; i < size; i++) {
					final String parameterName = methodInvocation.getParameters().get(i).getName();

					parameterList.add(i == size - 1 ? UPLOAD_FILE_PATH : parameterName);
				}

				return parameterList.toString();
			}
		}

		String parameterList = methodInvocation.getParameters().stream().map(MethodInvocationParameter::getName)
				.collect(Collectors.joining(", "));

		if (methodInvocation.isWaitForResponseParameterRequired()) {
			if (!parameterList.isEmpty())
				parameterList += ", ";

			if (integrationTechnology == IntegrationTechnology.KAFKA)
				parameterList += "true";
			else if (integrationTechnology == IntegrationTechnology.JMS)
				parameterList += "invocation.getTimeout()";
		}

		return parameterList;
	}

	/**
	 * Add the post-processing for the given method invocation
	 * @param methodInvocation
	 * @return the generated content
	 */
	private String addPostProcessing(IntegrationMethodTestInvocation methodInvocation) {
		final var b = new StringBuilder("\n");
		final AbstractIntegrationMethod integrationMethod = methodInvocation.getIntegrationMethod();

		if (methodInvocation.isReturnVoid() && invocationAttributes.containsKey(methodInvocation)
				&& methodInvocation.getTrackedAttribute() != null) {
			// Execute the given statement and inject the generated primary key value into the test data
			final JavaType returnType = integrationMethod.getBoundaryMethod().getFirstParameter(true).getType();
			JavaType generatedType = returnType;

			if (returnType instanceof final MappingObject mappingObject) {
				final MappingAttribute pkAttr;

				if (mappingObject instanceof final ExchangeMappingObject exchangeMappingObject)
					pkAttr = exchangeMappingObject.getPKAttribute();
				else if (mappingObject instanceof final DTOBean dto)
					pkAttr = dto.getPKAttribute();
				else
					throw new IllegalStateException("Unsupported type for mapping object " + mappingObject.getName());

				generatedType = pkAttr.getDomainAttribute().getJavaType();
			}

			final String idConstant = invocationAttributes.get(methodInvocation).get(methodInvocation.getTrackedAttribute());

			if (generatedType.getNamespace() != null)
				importPackage(generatedType.getNamespace().toString());

			b.append("final " + generatedType.getName() + " generatedId = ");
			b.append("completionHandler.waitForFinish(invocation.getPostProcessingStatement(), " + generatedType.getName() + ".class");
			b.append(addWaitForFinishParameters(methodInvocation));
			b.append(");\n");
			b.append("testDataProvider.setGeneratedFieldValue(UUID.fromString(");
			b.append(idConstant + ".poll()), generatedId, " + generatedType.getName() + ".class);\n");
		}
		else {
			b.append("completionHandler.waitForFinish(invocation.getPostProcessingStatement() ");
			b.append(addWaitForFinishParameters(methodInvocation));
			b.append(");\n");
		}

		return b.toString();
	}

	/**
	 * Create the parameter list for invoking waitForFinish() with the values provided by the method parameters
	 * @param methodInvocation
	 * @return the generated content
	 */
	private String addWaitForFinishParameters(IntegrationMethodTestInvocation methodInvocation) {
		final var b = new StringBuilder();

		for (final String attributeName : methodInvocation.extractAttributeNamesFromStatement()) {
			boolean found = false;

			for (final MethodInvocationParameter param : methodInvocation.getParameters()) {
				if (param.isRepresentsList())
					continue;

				if (param.getType() instanceof MappingObject) {
					final TestDataObject testDataObject = param.getParameterValues().getFirst();

					for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
						if (attribute.getMappingAttribute() == null || attribute.getMappingAttribute().getDomainAttribute() == null)
							continue;

						if (attribute.getMappingAttribute().getDomainAttribute().getName().equals(attributeName)) {
							b.append(", " + param.getName() + "." + attribute.getMappingAttribute().getGetterName());
							found = true;
							break;
						}
					}
				}
				else if (param.getName().equals(attributeName)) {
					b.append(", " + param.getName());
					found = true;
					break;
				}

				if (found)
					break;
			}

			if (!found)
				b.append(", null");
		}

		return b.toString();
	}

	/**
	 * Create the statements to validate the result of the given {@link IntegrationMethodTestInvocation}
	 * @param methodInvocation
	 * @return the generated content
	 */
	private String validateResult(IntegrationMethodTestInvocation methodInvocation) {
		final var b = new StringBuilder();

		// A JMS method that sends a response without a specified timeout is returning void!
		if (methodInvocation.isReturnVoid() || (methodInvocation.getTimeout() == null
				&& methodInvocation.getIntegrationMethod() instanceof final JMSIntegrationMethod jmsMethod && jmsMethod.isSendResponse()))
			return b.toString();

		final TestDataAttribute trackedAttribute = methodInvocation.getTrackedAttribute();

		if (trackedAttribute != null) {
			final JavaType attributeType = trackedAttribute.getJavaType();
			final String constantName = invocationAttributes.get(methodInvocation).get(trackedAttribute);

			if (!attributeType.isPrimitive() && attributeType.getNamespace() != null)
				importClass(attributeType.getNamespace().toString() + "." + attributeType.getName());

			b.append("testDataProvider.setGeneratedFieldValue(UUID.fromString(" + constantName + ".poll()), ");
			b.append(ACTUAL_RESULT_OBJECT);

			// A copy operation just returns the tracked value!
			if (trackedAttribute.getMappingAttribute() != null)
				b.append("." + trackedAttribute.getMappingAttribute().getGetterName());

			b.append(", " + trackedAttribute.getJavaType().getName() + ".class);\n");
		}

		final String validationFragment;

		if (methodInvocation.isReturnList())
			validationFragment = validateListResult(methodInvocation, ACTUAL_RESULT_LIST, EXPECTED_RESULT_LIST);
		else
			validationFragment = validateSingleResult(methodInvocation);

		if (!validationFragment.isEmpty()) {
			if (trackedAttribute != null)
				b.append("\n");

			b.append(validationFragment);
		}

		return b.toString();
	}

	/**
	 * Create the statements to validate the returned list of objects
	 * @param methodInvocation
	 * @param actualResultList
	 * @param expectedResultList
	 * @return the generated content
	 */
	private String validateListResult(IntegrationMethodTestInvocation methodInvocation, String actualResultList,
			String expectedResultList) {
		final var b = new StringBuilder();

		if (methodInvocation.getExpectedSize() != null) {
			if (project.isSpringBootApplication() && integrationTechnology == IntegrationTechnology.SOAP
					&& methodInvocation.getExpectedSize() == 0) {
				// When using Spring Boot, SOAP returns null instead of an empty list!
				b.append(addAssertThatWithNullCheck(actualResultList, AssertionOperator.IS_NULL));
			}
			else {
				String expression = actualResultList + ".";

				if (integrationTechnology == IntegrationTechnology.SOAP)
					expression += "length";
				else
					expression += "size()";

				b.append(addAssertThat(expression));
				b.append(convertOperatorToMethod(methodInvocation.getExpectedSizeOperator()));
				b.append("(invocation.getExpectedSize());\n");
			}
		}

		if (methodInvocation.getReturnValues().isEmpty())
			return b.toString();

		final TestDataObject testDataObject = methodInvocation.getReturnValues().getFirst();

		b.append(validateListResult(testDataObject, actualResultList, expectedResultList));

		return b.toString();
	}

	/**
	 * Create the statements to validate the returned list of objects
	 * @param testDataObject
	 * @param actualResultList
	 * @param expectedResultList
	 * @return the generated content
	 */
	private String validateListResult(TestDataObject testDataObject, String actualResultList, String expectedResultList) {
		final var b = new StringBuilder();
		final TestDataAttribute pkAttr = testDataObject.getPKAttribute();
		final TestDataAttribute displayAttr = testDataObject.getDisplayAttribute();
		boolean firstAttribute = true;

		importPackage("java.util");

		b.append(addAssertThat(expectedResultList) + "isNotEmpty().allSatisfy(expectedItem -> {\n");
		b.append(addAssertThat(actualResultList) + "filteredOn(actualItem ->\n");
		b.append("Objects.equals(actualItem." + pkAttr.getMappingAttribute().getGetterName() + ", ");
		b.append("expectedItem." + pkAttr.getMappingAttribute().getGetterName() + ")");

		if (displayAttr != null) {
			b.append(" || Objects.equals(actualItem." + displayAttr.getMappingAttribute().getGetterName() + ", ");
			b.append("expectedItem." + displayAttr.getMappingAttribute().getGetterName() + ")");
		}

		b.append(")\n");
		b.append(".singleElement()\n");
		b.append(".satisfies(actualItem -> {\n");

		for (final TestDataAttribute attribute : testDataObject.getAttributes()) {
			final MappingAttribute mappingAttribute = attribute.getMappingAttribute();
			final String getter = generateAttributeGetter(attribute);
			final String defaultValue = mappingAttribute.getDomainAttribute().getJavaType().getLocalVariableDefaultValue();
			final boolean nullCheck = attribute.getOperator() == AssertionOperator.IS_NULL
					|| attribute.getOperator() == AssertionOperator.IS_NOT_NULL;

			// Skip the validation of the primary key and the display attribute for inappropriate operators
			if (attribute.skip() || ((attribute.equals(pkAttr) || attribute.equals(displayAttr)) && nullCheck))
				continue;

			if (firstAttribute)
				firstAttribute = false;
			else
				b.append("\n");

			if (!nullCheck)
				b.append("if(expectedItem." + getter + " !=  " + defaultValue + ")\n");

			b.append(validateTestAttribute(attribute, "actualItem." + getter, "expectedItem." + getter));
		}

		b.append("});\n");
		b.append("});\n");

		return b.toString();
	}

	/**
	 * Create the statements to validate the single result after invoking an integration client method
	 * @param methodInvocation
	 * @return the generated content
	 */
	private String validateSingleResult(IntegrationMethodTestInvocation methodInvocation) {
		final var b = new StringBuilder();

		if (methodInvocation.isExpectedReturnNull())
			return addAssertThatWithNullCheck(ACTUAL_RESULT_OBJECT, AssertionOperator.IS_NULL);

		if (methodInvocation.getReturnValues().size() != 1)
			return b.toString();

		final TestDataObject returnedObject = methodInvocation.getReturnValues().getFirst();

		if (returnedObject.getMappingObject() != null) {
			for (final TestDataAttribute attribute : returnedObject.getAttributes()) {
				if (attribute.skip())
					continue;

				final String getter = generateAttributeGetter(attribute);

				b.append(validateTestAttribute(attribute, ACTUAL_RESULT_OBJECT + "." + getter, EXPECTED_RESULT_OBJECT + "." + getter));
			}
		}
		else if (!returnedObject.getAttributes().isEmpty() && !returnedObject.getAttributes().getFirst().skip())
			b.append(addAssertThat(ACTUAL_RESULT_OBJECT, EXPECTED_RESULT_OBJECT, returnedObject.getAttributes().getFirst()));

		return b.toString();
	}

	/**
	 * Create the statements to validated the given {@link TestDataAttribute}
	 * @param testDataAttribute
	 * @param actualGetter
	 * @param expectedGetter
	 * @return the generated content
	 */
	private String validateTestAttribute(TestDataAttribute testDataAttribute, String actualGetter, String expectedGetter) {
		final var b = new StringBuilder();
		final AssertionOperator operator = testDataAttribute.getOperator();

		if (testDataAttribute.getExpectedSize() != null) {
			importPackage("java.util");

			b.append(addAssertThat(actualGetter + ".size()"));
			b.append(convertOperatorToMethod(testDataAttribute.getExpectedSizeOperator()));
			b.append("(testDataProvider.getExpectedSizeOfField(UUID.fromString(EXPECTED_SIZE_IDS.poll())));\n");
		}

		if (testDataAttribute.isMappedToElementCollection()) {
			b.append(addAssertThat(actualGetter));
			b.append("containsAll(" + expectedGetter + ");\n");
		}
		else if (testDataAttribute.isMappedToList()) {
			if (!testDataAttribute.getReferencedObjects().isEmpty()) {
				final TestDataObject listObject = testDataAttribute.getReferencedObjects().getFirst();

				b.append(validateListResult(listObject, actualGetter, expectedGetter));
			}
		}
		else if (!testDataAttribute.getReferencedObjects().isEmpty()) {
			final TestDataObject referencedObject = testDataAttribute.getReferencedObjects().getFirst();

			if (operator == AssertionOperator.IS_NULL || operator == AssertionOperator.IS_NOT_NULL)
				b.append(addAssertThatWithNullCheck(actualGetter, operator));
			else {
				String referencedObjectValidation = null;

				for (final TestDataAttribute refAttribute : referencedObject.getAttributes()) {
					final JavaType javaType = refAttribute.getJavaType();

					// Do not create an assertion statement for primitive primary key attributes of an optional association
					if (refAttribute.skip() || (javaType != null && javaType.isPrimitive() && refAttribute.getMappingAttribute() != null
							&& refAttribute.getMappingAttribute().getDomainAttribute() != null
							&& refAttribute.getMappingAttribute().getDomainAttribute().isPk()
							&& ((javaType.isLong() && Long.toString(Long.MIN_VALUE).equals(refAttribute.getValue()))
									|| (javaType.isInteger() && Integer.toString(Integer.MIN_VALUE).equals(refAttribute.getValue())))))
						continue;

					referencedObjectValidation = validateTestAttribute(refAttribute,
							actualGetter + "." + generateAttributeGetter(refAttribute),
							expectedGetter + "." + generateAttributeGetter(refAttribute));
				}

				if (referencedObjectValidation != null && !referencedObjectValidation.isEmpty()) {
					b.append(addAssertThatWithNullCheck(actualGetter, AssertionOperator.IS_NOT_NULL));
					b.append(referencedObjectValidation);
				}
			}
		}
		else
			b.append(addAssertThat(actualGetter, expectedGetter, testDataAttribute));

		return b.toString();
	}

	/**
	 * Add an assertThat statement for the given expression
	 * @param expression the expression to be validated
	 * @return the generated content
	 */
	private String addAssertThat(String expression) {
		importAssertThat = true;

		return "assertThat(" + expression + ").";
	}

	/**
	 * Add an assertThat statement for the given expression
	 * @param actual the expression to be validated
	 * @param operator the operator that should be used. Only NULL or NOT_NULL must be used!
	 * @return the generated content
	 */
	private String addAssertThatWithNullCheck(String actual, AssertionOperator operator) {
		final var b = new StringBuilder();
		b.append(addAssertThat(actual));
		b.append(convertOperatorToMethod(operator));
		b.append("();\n");

		return b.toString();
	}

	/**
	 * Add an assertThat statement based on the given {@link TestDataAttribute}
	 * @param actual the actual expression to be validated
	 * @param expected the expected expression
	 * @param testDataAttribute the test data attribute
	 * @return the generated content
	 */
	private String addAssertThat(String actual, String expected, TestDataAttribute testDataAttribute) {
		final var b = new StringBuilder("assertThat(" + actual);
		final AssertionOperator operator = testDataAttribute.getOperator();
		final JavaType javaType = testDataAttribute.getJavaType();

		importAssertThat = true;

		if (javaType != null && javaType.isCalendar())
			b.append(".getTime()");

		b.append(").");

		if (javaType != null && (javaType.isFloat() || javaType.isDouble())
				&& (operator == AssertionOperator.EQUAL || operator == AssertionOperator.NONE)) {
			importStaticClass("org.assertj.core.data.Offset.offset");

			b.append("isCloseTo(" + expected + ", offset(0.01");

			if (javaType.isFloat())
				b.append("f");

			b.append("));\n");
		}
		else {
			b.append(convertOperatorToMethod(operator));

			if (operator == AssertionOperator.IS_NULL || operator == AssertionOperator.IS_NOT_NULL
					|| operator == AssertionOperator.IS_EMPTY)
				b.append("();\n");
			else if (javaType != null && javaType.isCalendar())
				b.append("(" + expected + ".getTime());\n");
			else
				b.append("(" + expected + ");\n");
		}

		return b.toString();
	}

	/**
	 * Convert the given operator to the corresponding AssertJ method name
	 * @param operator
	 * @return the method name
	 */
	private String convertOperatorToMethod(AssertionOperator operator) {
		String methodName = "undefined";

		switch (operator) {
			case AssertionOperator.NONE -> methodName = "isEqualTo";
			case AssertionOperator.EQUAL -> methodName = "isEqualTo";
			case AssertionOperator.CONTAINS -> methodName = "contains";
			case AssertionOperator.STARTS_WITH -> methodName = "startsWith";
			case AssertionOperator.ENDS_WITH -> methodName = "endsWith";
			case AssertionOperator.GREATER -> methodName = "isGreaterThan";
			case AssertionOperator.GREATER_OR_EQUAL -> methodName = "isGreaterThanOrEqualTo";
			case AssertionOperator.SMALLER -> methodName = "isLessThan";
			case AssertionOperator.SMALLER_OR_EQUAL -> methodName = "isLessThanOrEqualTo";
			case AssertionOperator.IS_NULL -> methodName = "isNull";
			case AssertionOperator.IS_NOT_NULL -> methodName = "isNotNull";
			case AssertionOperator.IS_EMPTY -> methodName = "isEmpty";
		}

		return methodName;
	}

	/**
	 * Create the getter for the given {@link TestDataAttribute}
	 * @param testDataAttribute
	 * @return the generated getter
	 */
	private String generateAttributeGetter(TestDataAttribute testDataAttribute) {
		// The Avro IDL compiler generates a getter starting with "get" for every field, regardless of the field's type!
		if (integrationTechnology == IntegrationTechnology.KAFKA && testDataAttribute.getJavaType().isBoolean())
			return "get" + testDataAttribute.getMappingAttribute().getUpperCaseName() + "()";

		return testDataAttribute.getMappingAttribute().getGetterName();
	}

}
