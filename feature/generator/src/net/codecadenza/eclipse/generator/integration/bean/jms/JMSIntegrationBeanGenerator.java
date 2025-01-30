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
package net.codecadenza.eclipse.generator.integration.bean.jms;

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.jms.BasicJMSMethodGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for JMS integration beans
 * </p>
 * <p>
 * Copyright 2023 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JMSIntegrationBeanGenerator extends AbstractJavaSourceGenerator {
	private final JMSIntegrationBean jmsBean;
	private final Map<String, ServiceBean> serviceMap = new HashMap<>();
	private final Map<String, String> resourceMap = new HashMap<>();
	private final Map<String, String> constantMap = new HashMap<>();
	private final List<JMSIntegrationMethod> jmsMethods;
	private final Map<JMSIntegrationMethod, String> operationConstants;
	private final Project project;
	private final boolean sendResponse;
	private boolean checkSendResponse;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public JMSIntegrationBeanGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getServiceBeanSourceFile());

		this.project = integrationBean.getNamespace().getProject();
		this.jmsBean = (JMSIntegrationBean) integrationBean;
		this.jmsMethods = integrationBean.getMethods().stream().map(JMSIntegrationMethod.class::cast).toList();
		this.operationConstants = BasicJMSMethodGenerator.getOperationIDConstants(this.jmsBean);
		this.sendResponse = this.jmsBean.isSendResponse();

		this.jmsMethods.forEach(jmsMethod -> {
			final var methodGenerator = new BasicJMSMethodGenerator(jmsMethod, this);

			this.serviceMap.putAll(methodGenerator.getServices());
			this.resourceMap.putAll(methodGenerator.getResources());
			this.constantMap.putAll(methodGenerator.getConstants());
		});

		if (this.sendResponse)
			this.checkSendResponse = this.jmsMethods.stream().anyMatch(method -> !method.isSendResponse());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		final String utilPackageName = jmsBean.getIntegrationModule().getNamespace().toString() + SUB_PACKAGE_UTIL;

		importStaticClass(utilPackageName + ".RequestMessageConverter.getCorrelationID");
		importStaticClass(utilPackageName + ".RequestMessageConverter.validateAndConvert");

		if (project.isJakartaEEApplication()) {
			importPackage("jakarta.ejb");
			importClass("jakarta.jms.JMSConnectionFactoryDefinition");
			importClass("jakarta.jms.MessageListener");

			if (sendResponse)
				importClass("jakarta.jms.Destination");
		}
		else {
			importClass("org.springframework.stereotype.Component");
			importClass("org.springframework.jms.annotation.JmsListener");
		}

		importClass("jakarta.jms.Message");
		importPackage("net.codecadenza.runtime.jms");

		if (!jmsMethods.isEmpty() && sendResponse) {
			importClass("java.io.Serializable");
			importPackage(utilPackageName);
		}

		jmsMethods.forEach(method -> {
			final var methodGenerator = new BasicJMSMethodGenerator(method, this);

			addImports(methodGenerator.getImports());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		final JMSResource requestDestination = jmsBean.getRequestDestination();
		final String requestDestinationType = requestDestination.isTopic() ? "Topic" : "Queue";

		if (project.isJakartaEEApplication()) {
			b.append("@JMSConnectionFactoryDefinition(name=\"java:comp/jms/DefaultConnectionFactory\", ");
			b.append("className=\"jakarta.jms.ConnectionFactory\")\n");
			b.append("@MessageDriven(activationConfig={\n");
			b.append("@ActivationConfigProperty(propertyName=\"destinationLookup\", propertyValue=\"");
			b.append(requestDestination.getName() + "\"),\n");
			b.append("@ActivationConfigProperty(propertyName=\"destinationType\", ");
			b.append("propertyValue=\"jakarta.jms." + requestDestinationType + "\")})\n");
		}
		else
			b.append("@Component\n");

		b.append("public class " + jmsBean.getName());

		if (project.isJakartaEEApplication())
			b.append(" implements MessageListener");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final JMSResource requestDestination = jmsBean.getRequestDestination();
		final JMSResource responseDestination = jmsBean.getResponseDestination();

		if (project.isSpringBootApplication()) {
			constantMap.put(JavaType.STRING + " REQUEST_DESTINATION", "\"" + requestDestination.getName() + "\"");

			if (sendResponse)
				constantMap.put(JavaType.STRING + " RESPONSE_DESTINATION", "\"" + responseDestination.getName() + "\"");
		}

		// Create the constants for all operation IDs
		operationConstants
				.forEach((method, name) -> constantMap.put(JavaType.STRING + " " + name, "\"" + method.getOperationID() + "\""));

		// Add constants
		constantMap.entrySet().forEach(entry -> {
			final int delimiterPos = entry.getKey().lastIndexOf(" ");
			final String typeName = entry.getKey().substring(0, delimiterPos);
			final String fieldName = entry.getKey().substring(delimiterPos + 1);

			addPrivateConstant(typeName, fieldName, entry.getValue()).create();
		});

		// Add declarations for all services that must be injected
		serviceMap.keySet().forEach(serviceName -> {
			final ServiceBean serviceBean = serviceMap.get(serviceName);
			final String serviceType = serviceBean.getInterfaceName() != null ? serviceBean.getInterfaceName() : serviceBean.getName();

			importPackage(serviceBean.getNamespace().toString());

			addPrivateField(serviceType, serviceName).inject().create();
		});

		if (sendResponse) {
			if (project.isJakartaEEApplication())
				addPrivateField("Destination", "responseDestination").create();

			addPrivateField("ResponseMessageSender", "responseMessageSender").inject().create();
		}

		resourceMap.forEach((typeName, fieldName) -> addPrivateField(typeName, fieldName).create());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (sendResponse && project.isJakartaEEApplication())
			addResourceSetterMethod("Destination", "responseDestination", jmsBean.getResponseDestination().getName());

		resourceMap.forEach(this::addResourceSetterMethod);

		addListenerMethod();

		jmsMethods.forEach(method -> {
			final var methodGenerator = new BasicJMSMethodGenerator(method, this);
			final String identifier = methodGenerator.getMethodSignature(false, false, false);

			final var b = new StringBuilder();
			b.append(methodGenerator.createComment());
			b.append(methodGenerator.createMethod());

			addMethod(identifier, b.toString());
		});
	}

	/**
	 * Create the method that listens for incoming messages
	 */
	private void addListenerMethod() {
		final var methodSignature = "void onMessage(Message message)";
		final var b = new StringBuilder();

		if (project.isJakartaEEApplication()) {
			b.append("/*\n");
			b.append(" * (non-Javadoc)\n");
			b.append(" * @see jakarta.jms.MessageListener#onMessage(jakarta.jms.Message)\n");
			b.append(" */\n");
			b.append("@Override\n");
		}
		else {
			final String containerFactory = jmsBean.getRequestDestination().isTopic() ? "defaultTopicFactory" : "defaultQueueFactory";

			b.append("/**\n");
			b.append(" * Consume the incoming {@link Message}\n");
			b.append(" * @param message the message to be consumed\n");
			b.append(" */\n");
			b.append("@JmsListener(destination=REQUEST_DESTINATION, containerFactory=\"" + containerFactory + "\")\n");
		}

		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final RequestMessage requestMessage = validateAndConvert(message);\n\n");
		b.append("if(requestMessage == null)\n");
		b.append("return;\n\n");
		b.append("final String operationID = requestMessage.getOperationID();\n");
		b.append("final String correlationID = getCorrelationID(message);\n");

		if (sendResponse) {
			b.append("ResponseStatus status = ResponseStatus.ERROR;\n");
			b.append("String statusMessage = null;\n");
			b.append("Serializable requestResult = null;\n");

			if (checkSendResponse)
				b.append("boolean sendResponse = false;\n");
		}

		b.append("\n");
		b.append("try\n");
		b.append("{\n");

		if (!jmsMethods.isEmpty()) {
			boolean firstMethod = true;

			b.append("// Invoke the respective service method depending on the provided operation ID\n");

			for (final JMSIntegrationMethod jmsMethod : jmsMethods) {
				if (!firstMethod)
					b.append("else ");
				else
					firstMethod = false;

				b.append("if(operationID.equals(" + operationConstants.get(jmsMethod) + "))\n");

				if (checkSendResponse && jmsMethod.isSendResponse()) {
					b.append("{\n");
					b.append("sendResponse = true;\n");
				}

				if (jmsMethod.isSendResponse() && !jmsMethod.getReturnType().getName().equals(JavaType.VOID)) {
					b.append("requestResult = ");

					if (jmsMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
						b.append("(Serializable) ");
				}

				b.append(jmsMethod.getName());

				if (jmsMethod.getIntegrationParameters().isEmpty())
					b.append("();\n");
				else
					b.append("(requestMessage);\n");

				if (checkSendResponse && jmsMethod.isSendResponse())
					b.append("}\n");
			}

			b.append("else\n");

			if (sendResponse) {
				b.append("{\n");

				if (checkSendResponse)
					b.append("sendResponse = true;\n");

				b.append("status = ResponseStatus.INVALID_OPERATION;\n");
			}

			b.append("throw new IllegalStateException(\"The operation '\" + operationID + \"' is not supported!\");\n");

			if (sendResponse) {
				b.append("}\n\n");
				b.append("status = ResponseStatus.SUCCESS;\n");
			}
		}

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		if (sendResponse)
			b.append("statusMessage = ex.getMessage();\n");

		addErrorLog(b, "Error while processing '{}' message with ID {}!", "ex", "operationID", "correlationID");

		b.append("}\n");

		if (sendResponse) {
			b.append("\n");

			if (checkSendResponse)
				b.append("if(sendResponse)\n");

			b.append("responseMessageSender.");

			if (project.isSpringBootApplication()) {
				if (jmsBean.getResponseDestination().isTopic())
					b.append("sendResponseToTopic(");
				else
					b.append("sendResponseToQueue(");

				b.append("RESPONSE_DESTINATION");
			}
			else
				b.append("sendResponse(responseDestination");

			b.append(", operationID, correlationID, status, statusMessage, requestResult);\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
