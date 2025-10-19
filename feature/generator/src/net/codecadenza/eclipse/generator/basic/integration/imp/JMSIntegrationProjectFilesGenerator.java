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
package net.codecadenza.eclipse.generator.basic.integration.imp;

import static net.codecadenza.eclipse.shared.Constants.BASE_JMS_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for basic source and configuration files necessary for supporting JMS
 * </p>
 * <p>
 * Copyright 2023 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JMSIntegrationProjectFilesGenerator extends AbstractIntegrationProjectFilesGenerator {
	private static final String FILE_REQUEST_QUEUE = "jms/FileRequestQueue";
	private static final String FILE_RESPONSE_QUEUE = "jms/FileResponseQueue";
	private static final String JMS_FILE_CONSUMER = "FileJMSConsumer";
	private static final String MESSAGE_CONVERTER_CLASS_NAME = "RequestMessageConverter";
	private static final String MESSAGE_SENDER_CLASS_NAME = "ResponseMessageSender";
	private static final String CONFIG_PROPERTIES_CLASS_NAME = "JMSProperties";
	private static final String JMS_CONFIG_CLASS_NAME = "JMSConfiguration";

	/**
	 * Constructor
	 * @param module
	 * @param artifactType
	 */
	public JMSIntegrationProjectFilesGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		super(module, artifactType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final var sourceFiles = new ArrayList<JavaFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_IMP_JMS) {
			final String utilPackageName = module.getNamespace().toString() + SUB_PACKAGE_UTIL;
			final String beanPackageName = module.getNamespace().toString() + SUB_PACKAGE_BEAN;
			final var converterComment = "Validate and convert the given {@link Message} into a {@link RequestMessage}";
			final var messageSenderComment = "Component for sending a {@link ResponseMessage}";

			final var messageConverter = new JavaFile(project, artifactType, MESSAGE_CONVERTER_CLASS_NAME, utilPackageName);
			messageConverter.setComment(converterComment);
			messageConverter.setContent(createRequestMessageConverter());

			final var messageSender = new JavaFile(project, artifactType, MESSAGE_SENDER_CLASS_NAME, utilPackageName);
			messageSender.setComment(messageSenderComment);
			messageSender.setContent(createResponseMessageSender());

			final var fileConsumer = new JavaFile(project, artifactType, JMS_FILE_CONSUMER, beanPackageName);
			fileConsumer.setComment("JMS consumer for file operations");
			fileConsumer.setContent(createFileConsumer());

			sourceFiles.add(messageConverter);
			sourceFiles.add(messageSender);
			sourceFiles.add(fileConsumer);

			if (project.isSpringBootApplication()) {
				final var configPropertiesComment = "Configuration properties for JMS listeners and JMS templates";
				final var jmsConfigComment = "Class for the configuration of JMS consumers and JMS templates";

				final var configProperties = new JavaFile(project, artifactType, CONFIG_PROPERTIES_CLASS_NAME, utilPackageName);
				configProperties.setComment(configPropertiesComment);
				configProperties.setContent(createConfigProperties());

				final var jmsConfiguration = new JavaFile(project, artifactType, JMS_CONFIG_CLASS_NAME, utilPackageName);
				jmsConfiguration.setComment(jmsConfigComment);
				jmsConfiguration.setContent(createJMSConfigurationBean());

				sourceFiles.add(configProperties);
				sourceFiles.add(jmsConfiguration);
			}
		}
		else if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_JMS) {
			final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;
			final var comment = "Abstract base class for clients that communicate with JMS destinations via JNDI";

			final var baseClient = new JavaFile(project, artifactType, BASE_JMS_CLIENT_CLASS_NAME, packageName);
			baseClient.setComment(comment);
			baseClient.setContent(createBaseClient());

			final var fileClient = new JavaFile(project, artifactType, module.getFileServiceClientName(), packageName);
			fileClient.setComment("Client for uploading and downloading files via JMS queues");
			fileClient.setContent(createFileServiceClient());

			sourceFiles.add(baseClient);
			sourceFiles.add(fileClient);

			if (module.isAddProducers())
				sourceFiles.add(createFileServiceProducer());
		}

		return sourceFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> createConfigurationFiles() {
		final var fileList = new ArrayList<WorkspaceFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_JMS) {
			final var path = project.getConfigFolder(artifactType) + "/" + JNDI_PROPERTIES_FILE;

			fileList.add(new WorkspaceFile(project, artifactType, path, createJNDIProperties(Collections.emptyList())));
		}

		return fileList;
	}

	/**
	 * Rebuild the jndi.properties file
	 * @throws Exception if the file could not be rebuilt
	 */
	public void rebuildJNDIProperties() throws Exception {
		final String projectName = project.getTargetProjectName(artifactType);
		final var path = project.getConfigFolder(artifactType) + "/" + JNDI_PROPERTIES_FILE;
		final var existingContent = EclipseIDEService.getFileContent(projectName, path);
		final String newContent = createJNDIProperties(existingContent);

		EclipseIDEService.overwriteFileContent(projectName, path, newContent);
	}

	/**
	 * Create the converter for request messages
	 * @return the generated content
	 */
	private String createRequestMessageConverter() {
		final var b = new StringBuilder();
		b.append("import java.lang.invoke.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import jakarta.jms.*;\n");
		b.append("import net.codecadenza.runtime.jms.*;\n\n");
		b.append("public class " + MESSAGE_CONVERTER_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n\n");
		b.append("/**\n");
		b.append(" * Private constructor\n");
		b.append(" */\n");
		b.append("private " + MESSAGE_CONVERTER_CLASS_NAME + "()\n");
		b.append("{\n");
		b.append("// Prevent instantiation\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Validate and convert the given {@link Message} into a {@link RequestMessage}\n");
		b.append(" * @param message the {@link Message} to be converted\n");
		b.append(" * @return the {@link RequestMessage} or null if the {@link Message} could not be converted\n");
		b.append(" */\n");
		b.append("public static RequestMessage validateAndConvert(Message message)\n");
		b.append("{\n");
		b.append("final RequestMessage requestMessage;\n");
		b.append("final String operationID;\n");
		b.append("final String correlationID = getCorrelationID(message);\n\n");
		b.append("if(correlationID == null || correlationID.isEmpty())\n");
		b.append("{\n");
		b.append("logger.warn(\"Skip processing of message without correlation ID\");\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var objectMessage = (ObjectMessage) message;\n");
		b.append("requestMessage = (RequestMessage) objectMessage.getObject();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while converting incoming message!\", e);\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("if(requestMessage == null)\n");
		b.append("{\n");
		b.append("logger.warn(\"No request message available!\");\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("operationID = requestMessage.getOperationID();\n\n");
		b.append("if(operationID == null || operationID.isEmpty())\n");
		b.append("{\n");
		b.append("logger.warn(\"Skip processing of message without operation ID\");\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("logger.info(\"Converted '{}' request message with ID {}\", operationID, correlationID);\n\n");
		b.append("return requestMessage;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Extract the correlation ID from the provided {@link Message}\n");
		b.append(" * @param message {@link Message} from which the correlation ID should be extracted\n");
		b.append(" * @return the correlation ID or null if the message doesn't contain a correlation ID\n");
		b.append(" */\n");
		b.append("public static String getCorrelationID(Message message)\n");
		b.append("{\n");
		b.append("final String correlationID;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("correlationID = message.getJMSCorrelationID();\n");
		b.append("}\n");
		b.append("catch (JMSException e)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while extracting correlation ID!\", e);\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("return correlationID;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the class for the configuration of JMS listeners and JMS templates of a Spring Boot application
	 * @return the generated content
	 */
	private String createJMSConfigurationBean() {
		final var b = new StringBuilder();
		b.append("import jakarta.jms.ConnectionFactory;\n");
		b.append("import jakarta.jms.JMSException;\n");
		b.append("import org.apache.activemq.artemis.jms.client.ActiveMQConnectionFactory;\n");
		b.append("import org.springframework.boot.autoconfigure.jms.DefaultJmsListenerContainerFactoryConfigurer;\n");
		b.append("import org.springframework.boot.context.properties.ConfigurationProperties;\n");
		b.append("import org.springframework.context.annotation.Bean;\n");
		b.append("import org.springframework.context.annotation.Configuration;\n");
		b.append("import org.springframework.jms.config.DefaultJmsListenerContainerFactory;\n");
		b.append("import org.springframework.jms.config.JmsListenerContainerFactory;\n");
		b.append("import org.springframework.jms.core.JmsTemplate;\n");
		b.append("import org.springframework.jms.listener.DefaultMessageListenerContainer;\n\n");
		b.append("@Configuration\n");
		b.append("public class " + JMS_CONFIG_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * @return the JMS properties\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("@ConfigurationProperties(prefix = \"spring.artemis\")\n");
		b.append("public JMSProperties jmsProperties()\n");
		b.append("{\n");
		b.append("return new JMSProperties();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the default connection factory for creating JMS listeners and templates\n");
		b.append(" * @throws JMSException if the connection factory could not be created\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("public ActiveMQConnectionFactory defaultConnectionFactory() throws JMSException\n");
		b.append("{\n");
		b.append("final JMSProperties jmsProperties = jmsProperties();\n\n");
		b.append("final var connectionFactory = new ActiveMQConnectionFactory();\n");
		b.append("connectionFactory.setBrokerURL(\"tcp://\" + jmsProperties.getHost() + \":\" + jmsProperties.getPort());\n");
		b.append("connectionFactory.setUser(jmsProperties.getUser());\n");
		b.append("connectionFactory.setPassword(jmsProperties.getPassword());\n");
		b.append("return connectionFactory;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a default {@link JmsListenerContainerFactory} for JMS topic listeners\n");
		b.append(" * @param connectionFactory\n");
		b.append(" * @param configurer\n");
		b.append(" * @return a new {@link JmsListenerContainerFactory}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("public JmsListenerContainerFactory<DefaultMessageListenerContainer> defaultTopicFactory(");
		b.append("ConnectionFactory connectionFactory, DefaultJmsListenerContainerFactoryConfigurer configurer)\n");
		b.append("{\n");
		b.append("final var factory = new DefaultJmsListenerContainerFactory();\n\n");
		b.append("configurer.configure(factory, connectionFactory);\n\n");
		b.append("factory.setPubSubDomain(true);\n");
		b.append("return factory;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a default {@link JmsListenerContainerFactory} for JMS queue listeners\n");
		b.append(" * @param connectionFactory\n");
		b.append(" * @param configurer\n");
		b.append(" * @return a new {@link JmsListenerContainerFactory}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("public JmsListenerContainerFactory<DefaultMessageListenerContainer> defaultQueueFactory(");
		b.append("ConnectionFactory connectionFactory, DefaultJmsListenerContainerFactoryConfigurer configurer)\n");
		b.append("{\n");
		b.append("final var factory = new DefaultJmsListenerContainerFactory();\n\n");
		b.append("configurer.configure(factory, connectionFactory);\n\n");
		b.append("return factory;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a JMS template for sending messages to a queue\n");
		b.append(" * @return a new JMS template\n");
		b.append(" * @throws JMSException if the connection factory could not be created\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("public JmsTemplate defaultQueueTemplate() throws JMSException\n");
		b.append("{\n");
		b.append("return createJMSTemplate(false);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a JMS template for sending messages to a topic\n");
		b.append(" * @return a new JMS template\n");
		b.append(" * @throws JMSException if the connection factory could not be created\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("public JmsTemplate defaultTopicTemplate() throws JMSException\n");
		b.append("{\n");
		b.append("return createJMSTemplate(true);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create a JMS template for sending messages to a JMS destination\n");
		b.append(" * @param topic flag that controls if the JMS template should be used for a topic\n");
		b.append(" * @return a new JMS template\n");
		b.append(" * @throws JMSException if the connection factory could not be created\n");
		b.append(" */\n");
		b.append("protected JmsTemplate createJMSTemplate(boolean topic) throws JMSException\n");
		b.append("{\n");
		b.append("final var template = new JmsTemplate();\n");
		b.append("template.setConnectionFactory(defaultConnectionFactory());\n");
		b.append("template.setPubSubDomain(topic);\n");
		b.append("template.setDeliveryPersistent(true);\n");
		b.append("return template;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the class for the JMS configuration properties of a Spring Boot application
	 * @return the generated content
	 */
	private String createConfigProperties() {
		final var b = new StringBuilder();
		b.append("public class " + CONFIG_PROPERTIES_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private String host;\n");
		b.append("private int port;\n");
		b.append("private String user;\n");
		b.append("private String password;\n\n");
		b.append("/**\n");
		b.append(" * @return the host name\n");
		b.append(" */\n");
		b.append("public String getHost()\n");
		b.append("{\n");
		b.append("return host;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param host\n");
		b.append(" */\n");
		b.append("public void setHost(String host)\n");
		b.append("{\n");
		b.append("this.host = host;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the port\n");
		b.append(" */\n");
		b.append("public int getPort()\n");
		b.append("{\n");
		b.append("return port;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param port\n");
		b.append(" */\n");
		b.append("public void setPort(int port)\n");
		b.append("{\n");
		b.append("this.port = port;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the user name\n");
		b.append(" */\n");
		b.append("public String getUser()\n");
		b.append("{\n");
		b.append("return user;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param user\n");
		b.append(" */\n");
		b.append("public void setUser(String user)\n");
		b.append("{\n");
		b.append("this.user = user;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the password\n");
		b.append(" */\n");
		b.append("public String getPassword()\n");
		b.append("{\n");
		b.append("return password;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("public void setPassword(String password)\n");
		b.append("{\n");
		b.append("this.password = password;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the component for sending response messages
	 * @return the generated content
	 */
	private String createResponseMessageSender() {
		final StringBuilder b = new StringBuilder();
		b.append("import jakarta.jms.*;\n");
		b.append("import java.io.*;\n");
		b.append("import java.lang.invoke.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import net.codecadenza.runtime.jms.*;\n");

		if (project.isSpringBootApplication()) {
			b.append("import org.springframework.beans.factory.annotation.*;\n");
			b.append("import org.springframework.jms.core.*;\n");
			b.append("import org.springframework.stereotype.*;\n\n");
		}
		else {
			b.append("import jakarta.inject.*;\n");
			b.append("import jakarta.ejb.*;\n\n");
		}

		if (project.isJakartaEEApplication())
			b.append("@Stateless\n");
		else
			b.append("@Component\n");

		b.append("public class " + MESSAGE_SENDER_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("@JMSConnectionFactory(\"java:comp/jms/DefaultConnectionFactory\")\n");
			b.append("@Inject\n");
			b.append("private JMSContext context;\n\n");
		}
		else {
			b.append("@Autowired\n");
			b.append("@Qualifier(\"defaultQueueTemplate\")\n");
			b.append("private JmsTemplate jmsQueueTemplate;\n\n");
			b.append("@Autowired\n");
			b.append("@Qualifier(\"defaultTopicTemplate\")\n");
			b.append("private JmsTemplate jmsTopicTemplate;\n\n");
			b.append("/**\n");
			b.append(" * Create and send a {@link ResponseMessage} based on the provided data to the given JMS queue\n");
			b.append(" * @param queueName the name of the JMS queue the message has to be sent to\n");
			b.append(" * @param operationID the operation ID\n");
			b.append(" * @param correlationID the correlation ID\n");
			b.append(" * @param status the response message status\n");
			b.append(" * @param text an optional, human-readable text that is primarily used for providing an error message\n");
			b.append(" * @param object the optional result created by a specific operation\n");
			b.append(" */\n");
			b.append("public void sendResponseToQueue(String queueName, String operationID, String correlationID, ");
			b.append("ResponseStatus status, String text, Serializable object)\n");
			b.append("{\n");
			b.append("sendResponse(queueName, false, operationID, correlationID, status, text, object);\n");
			b.append("}\n\n");
			b.append("/**\n");
			b.append(" * Create and send a {@link ResponseMessage} based on the provided data to the given JMS topic\n");
			b.append(" * @param topicName the name of the JMS topic the message has to be sent to\n");
			b.append(" * @param operationID the operation ID\n");
			b.append(" * @param correlationID the correlation ID\n");
			b.append(" * @param status the response message status\n");
			b.append(" * @param text an optional, human-readable text that is primarily used for providing an error message\n");
			b.append(" * @param object the optional result created by a specific operation\n");
			b.append(" */\n");
			b.append("public void sendResponseToTopic(String topicName, String operationID, String correlationID, ");
			b.append("ResponseStatus status, String text, Serializable object)\n");
			b.append("{\n");
			b.append("sendResponse(topicName, true, operationID, correlationID, status, text, object);\n");
			b.append("}\n\n");
		}

		b.append("/**\n");
		b.append(" * Create and send a {@link ResponseMessage} based on the provided data\n");
		b.append(" * @param responseDestination the ");

		if (project.isSpringBootApplication())
			b.append("name of the ");

		b.append("JMS destination the message has to be sent to\n");

		if (project.isSpringBootApplication())
			b.append(" * @param toTopic flag that controls if the message should be sent to a topic\n");

		b.append(" * @param operationID the operation ID\n");
		b.append(" * @param correlationID the correlation ID\n");
		b.append(" * @param status the response message status\n");
		b.append(" * @param text an optional, human-readable text that is primarily used for providing an error message\n");
		b.append(" * @param object the optional result created by a specific operation\n");
		b.append(" */\n");
		b.append("public void sendResponse(");

		if (project.isJakartaEEApplication())
			b.append("Destination");
		else
			b.append("String");

		b.append(" responseDestination, ");

		if (project.isSpringBootApplication())
			b.append("boolean toTopic, ");

		b.append("String operationID, String correlationID, ResponseStatus status, String text, Serializable object)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");

		if (project.isSpringBootApplication())
			b.append("final JmsTemplate jmsTemplate = toTopic ? jmsTopicTemplate : jmsQueueTemplate;\n\n");

		b.append("final var responseMessage = new ResponseMessage(operationID);\n");
		b.append("responseMessage.setObject(object);\n");
		b.append("responseMessage.setStatus(status);\n");
		b.append("responseMessage.setText(text);\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("final var messageProducer = context.createProducer();\n");
			b.append("messageProducer.setJMSCorrelationID(correlationID);\n");
			b.append("messageProducer.send(responseDestination, responseMessage);\n\n");
		}
		else {
			b.append("jmsTemplate.send(responseDestination, messageCreator -> {\n");
			b.append("final ObjectMessage objectMessage = messageCreator.createObjectMessage(responseMessage);\n");
			b.append("objectMessage.setJMSCorrelationID(correlationID);\n");
			b.append("return objectMessage;\n");
			b.append("});\n\n");
		}

		b.append("logger.debug(\"Sent '{}' response message with ID {}\", operationID, correlationID);\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("logger.error(\"Error while sending '{}' response with ID {}!\", operationID, correlationID, ex);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the base class for all JMS clients
	 * @return the generated content
	 */
	private String createBaseClient() {
		final StringBuilder b = new StringBuilder();
		final String standardImportPrefix = project.isJakartaEEApplication() ? "jakarta" : "javax";

		b.append("import " + standardImportPrefix + ".jms.ConnectionFactory;\n");
		b.append("import " + standardImportPrefix + ".jms.Destination;\n");
		b.append("import " + standardImportPrefix + ".jms.JMSConsumer;\n");
		b.append("import " + standardImportPrefix + ".jms.JMSContext;\n");
		b.append("import " + standardImportPrefix + ".jms.JMSException;\n");
		b.append("import " + standardImportPrefix + ".jms.JMSProducer;\n");
		b.append("import " + standardImportPrefix + ".jms.ObjectMessage;\n");
		b.append("import java.io.*;\n");
		b.append("import java.time.*;\n");
		b.append("import java.util.*;\n");
		b.append("import java.util.concurrent.locks.*;\n");
		b.append("import javax.naming.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n");
		b.append("import net.codecadenza.runtime.jms.*;\n\n");
		b.append("public abstract class " + BASE_JMS_CLIENT_CLASS_NAME + " implements AutoCloseable\n");
		b.append("{\n");

		if (project.isJakartaEEApplication()) {
			if (project.isDeployedOnGlassfish())
				b.append("private static final String CONNECTION_FACTORY_NAME = \"jms/__defaultConnectionFactory\";\n");
			else
				b.append("private static final String CONNECTION_FACTORY_NAME = \"jms/RemoteConnectionFactory\";\n");
		}
		else
			b.append("private static final String CONNECTION_FACTORY_NAME = \"ConnectionFactory\";\n");

		b.append("\n");
		b.append("protected final ReentrantLock lock = new ReentrantLock();\n");
		b.append("protected InitialContext initialContext;\n");
		b.append("protected ConnectionFactory connectionFactory;\n");
		b.append("protected JMSProducer messageProducer;\n");
		b.append("protected JMSContext context;\n");
		b.append("protected boolean initialized;\n");
		b.append("private final String userName;\n");
		b.append("private final String password;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("protected " + BASE_JMS_CLIENT_CLASS_NAME + "()\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("protected " + BASE_JMS_CLIENT_CLASS_NAME + "(String userName, String password)\n");
		b.append("{\n");
		b.append("this.userName = userName;\n");
		b.append("this.password = password;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Initialize the {@link InitialContext}, the {@link JMSContext} and a {@link JMSProducer}\n");
		b.append(" * @throws NamingException if a context operation has failed\n");
		b.append(" */\n");
		b.append("public void init() throws NamingException\n");
		b.append("{\n");
		b.append("initialContext = createInitialContext();\n");
		b.append("connectionFactory = (ConnectionFactory) initialContext.lookup(CONNECTION_FACTORY_NAME);\n");
		b.append("context = connectionFactory.createContext();\n");
		b.append("messageProducer = context.createProducer();\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see java.lang.AutoCloseable#close()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void close()\n");
		b.append("{\n");
		b.append("lock.lock();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(context != null)\n");
		b.append("context.close();\n\n");
		b.append("initialized = false;\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("lock.unlock();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * An implementation must provide a {@link Logger}\n");
		b.append(" * @return a logger\n");
		b.append(" */\n");
		b.append("protected abstract Logger getLogger();\n\n");
		b.append("/**\n");
		b.append(" * Send a {@link RequestMessage} to the given JMS destination\n");
		b.append(" * @param destination the JMS destination that should receive the message\n");
		b.append(" * @param requestMessage the request message to be sent\n");
		b.append(" * @return a correlation ID that can be used to search for a corresponding response message\n");
		b.append(" * @throws RemoteOperationException if the message could not be sent\n");
		b.append(" * @throws IllegalStateException if the internal JMS resources has not yet been initialized\n");
		b.append(" */\n");
		b.append("protected String sendMessage(Destination destination, RequestMessage requestMessage)\n");
		b.append("{\n");
		b.append("final var correlationID = UUID.randomUUID().toString();\n\n");
		b.append("// The initialization is not being checked in the receiveResponse() method as it is\n");
		b.append("// assumed that a client doesn't invoke receiveResponse() before sending a message!\n");
		b.append("checkInitialization();\n\n");
		b.append("getLogger().debug(\"Sending '{}' message with correlation ID {}\", ");
		b.append("requestMessage.getOperationID(), correlationID);\n\n");
		b.append("lock.lock();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final ObjectMessage objectMessage = context.createObjectMessage();\n");
		b.append("objectMessage.setObject(requestMessage);\n");
		b.append("objectMessage.setJMSCorrelationID(correlationID);\n\n");
		b.append("messageProducer.send(destination, objectMessage);\n\n");
		b.append("getLogger().debug(\"Sent '{}' message with correlation ID {}\", ");
		b.append("requestMessage.getOperationID(), correlationID);\n\n");
		b.append("return correlationID;\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("lock.unlock();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Receive a {@link ResponseMessage} from the given JMS destination\n");
		b.append(" * @param destination the JMS destination to be used\n");
		b.append(" * @param correlationID the correlation ID\n");
		b.append(" * @param maxWaitTime the maximum time to wait for the response. If null, the timeout never expires!\n");
		b.append(" * @return the actual payload of the {@link ResponseMessage}, or null if the response doesn't contain a payload\n");
		b.append(" * @throws RemoteOperationException if the response message could not be received (e.g. due to a timeout)\n");
		b.append(" */\n");
		b.append("protected Serializable receiveResponse(Destination destination, String correlationID, Duration maxWaitTime)\n");
		b.append("{\n");
		b.append("final ObjectMessage responseMessage;\n");
		b.append("final String selector = \"JMSCorrelationID='\" + correlationID + \"'\";\n");
		b.append("final long waitTimeMillis = maxWaitTime == null ? 0L : maxWaitTime.toMillis();\n\n");
		b.append("getLogger().debug(\"Fetching response message with correlation ID {}\", correlationID);\n\n");
		b.append("lock.lock();\n\n");
		b.append("try(final JMSConsumer messageConsumer = context.createConsumer(destination, selector))\n");
		b.append("{\n");
		b.append("responseMessage = (ObjectMessage) messageConsumer.receive(waitTimeMillis);\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("lock.unlock();\n");
		b.append("}\n\n");
		b.append("return handleResponseMessage(correlationID, responseMessage);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Convert the payload of the given {@link ObjectMessage} and extract the response data\n");
		b.append(" * @param correlationID the correlation ID\n");
		b.append(" * @param objectMessage the JMS message that contains the {@link ResponseMessage}\n");
		b.append(" * @return the actual payload of the {@link ResponseMessage}, or null if the response doesn't contain a payload\n");
		b.append(" * @throws RemoteOperationException if the response message is ");
		b.append("either invalid or the response status indicates an error\n");
		b.append(" * @throws IllegalStateException if the provided operation ID is unknown to the message consumer\n");
		b.append(" */\n");
		b.append("private Serializable handleResponseMessage(String correlationID, final ObjectMessage objectMessage)\n");
		b.append("{\n");
		b.append("final ResponseMessage responseMessage;\n\n");
		b.append("if(objectMessage == null)\n");
		b.append("throw new RemoteOperationException(\"Timeout waiting for response with ID \" + correlationID);\n\n");
		b.append("getLogger().debug(\"Received response message with correlation ID {}\", correlationID);\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("responseMessage = (ResponseMessage) objectMessage.getObject();\n");
		b.append("}\n");
		b.append("catch (JMSException e)\n");
		b.append("{\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n\n");
		b.append("if(ResponseStatus.INVALID_OPERATION == responseMessage.getStatus())\n");
		b.append("throw new IllegalStateException(\"The operation '\" + responseMessage.getOperationID() ");
		b.append("+ \"' is not supported!\");\n");
		b.append("else if(ResponseStatus.SUCCESS != responseMessage.getStatus())\n");
		b.append("{\n");
		b.append("final String errorMessage = responseMessage.getText();\n\n");
		b.append("if(errorMessage == null)\n");
		b.append("throw new RemoteOperationException(\"Unknown error!\");\n\n");
		b.append("throw new RemoteOperationException(errorMessage);\n");
		b.append("}\n\n");
		b.append("return responseMessage.getObject();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create initial context\n");
		b.append(" * @return the created context\n");
		b.append(" * @throws NamingException if the {@link InitialContext} could not be created\n");
		b.append(" */\n");
		b.append("private InitialContext createInitialContext() throws NamingException\n");
		b.append("{\n");
		b.append("final var props = new Hashtable<>();\n\n");
		b.append("if(userName != null)\n");
		b.append("props.put(Context.SECURITY_PRINCIPAL, userName);\n\n");
		b.append("if(password != null)\n");
		b.append("props.put(Context.SECURITY_CREDENTIALS, password);\n\n");
		b.append("return new InitialContext(props);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Check if the internal JMS resources has been initialized\n");
		b.append(" * @throws IllegalStateException if the internal JMS resources has not yet been initialized\n");
		b.append(" */\n");
		b.append("private void checkInitialization()\n");
		b.append("{\n");
		b.append("lock.lock();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(!initialized)\n");
		b.append("throw new IllegalStateException(\"Client has not yet been initialized!\");\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("lock.unlock();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the JMS file consumer
	 * @return the generated content
	 */
	private String createFileConsumer() {
		final String utilPackageName = module.getNamespace().toString() + SUB_PACKAGE_UTIL;
		final var b = new StringBuilder();

		b.append("import static " + utilPackageName + ".RequestMessageConverter.getCorrelationID;\n");
		b.append("import static " + utilPackageName + ".RequestMessageConverter.validateAndConvert;\n");
		b.append("import " + utilPackageName + ".*;\n");
		b.append("import java.io.*;\n");
		b.append("import java.lang.invoke.*;\n");
		b.append("import java.nio.*;\n");
		b.append("import java.nio.channels.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import org.slf4j.*;\n");

		if (project.isJakartaEEApplication()) {
			b.append("import jakarta.annotation.*;\n");
			b.append("import jakarta.ejb.*;\n");
			b.append("import jakarta.jms.JMSConnectionFactoryDefinition;\n");
			b.append("import jakarta.jms.MessageListener;\n");
			b.append("import jakarta.jms.Queue;\n");
		}
		else {
			b.append("import org.springframework.stereotype.Component;\n");
			b.append("import org.springframework.jms.annotation.JmsListener;\n");
		}

		if (project.isJakartaEEApplication())
			b.append("import jakarta.inject.*;\n");

		b.append("import jakarta.jms.Message;\n");
		b.append("import net.codecadenza.runtime.property.*;\n");
		b.append("import net.codecadenza.runtime.jms.*;\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("@JMSConnectionFactoryDefinition(name=\"java:comp/jms/DefaultConnectionFactory\", ");
			b.append("className=\"jakarta.jms.ConnectionFactory\")\n");
			b.append("@MessageDriven(activationConfig={\n");
			b.append("@ActivationConfigProperty(propertyName=\"destinationLookup\", propertyValue=\"");
			b.append(FILE_REQUEST_QUEUE + "\"),\n");
			b.append("@ActivationConfigProperty(propertyName=\"destinationType\", propertyValue=\"jakarta.jms.Queue\")})\n");
		}
		else
			b.append("@Component\n");

		b.append("public class " + JMS_FILE_CONSUMER);

		if (project.isJakartaEEApplication())
			b.append(" implements MessageListener");

		b.append("\n{\n");
		b.append("private static final String PATH_EXCHANGE_FOLDER = new PropertyService()\n");
		b.append(".getStringProperty(PropertyService.PROP_EXCHANGE_FOLDER);\n");

		if (project.isSpringBootApplication()) {
			b.append("private static final String REQUEST_QUEUE_NAME = \"" + FILE_REQUEST_QUEUE + "\";\n");
			b.append("private static final String RESPONSE_QUEUE_NAME = \"" + FILE_RESPONSE_QUEUE + "\";\n");
		}

		b.append("private static final String OPERATION_UPLOAD = \"upload\";\n");
		b.append("private static final String OPERATION_DOWNLOAD = \"download\";\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");

		if (project.isJakartaEEApplication())
			b.append("private Queue responseQueue;\n");

		b.append("private final ResponseMessageSender responseMessageSender;\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append("public " + JMS_FILE_CONSUMER + "()\n");
			b.append("{\n");
			b.append("this.responseMessageSender = null;\n");
			b.append("}\n\n");
		}

		b.append("/**\n");
		b.append(" * Constructor for injecting the response message sender\n");
		b.append(" * @param logger\n");
		b.append(" */\n");

		if (project.isJakartaEEApplication())
			b.append("@Inject\n");

		b.append("public " + JMS_FILE_CONSUMER + "(ResponseMessageSender responseMessageSender)\n");
		b.append("{\n");
		b.append("this.responseMessageSender = responseMessageSender;\n");
		b.append("}\n\n");

		if (project.isJakartaEEApplication()) {
			final String prefix = project.isDeployedOnJBoss() ? "java:/" : "";

			b.append("/**\n");
			b.append(" * Inject the response queue\n");
			b.append(" * @param responseQueue\n");
			b.append(" */\n");
			b.append("@Resource(name=\"" + prefix + FILE_RESPONSE_QUEUE + "\")\n");
			b.append("public void setResponseQueue(Queue responseQueue)\n");
			b.append("{\n");
			b.append("this.responseQueue = responseQueue;\n");
			b.append("}\n\n");
			b.append("/*\n");
			b.append(" * (non-Javadoc)\n");
			b.append(" * @see jakarta.jms.MessageListener#onMessage(jakarta.jms.Message)\n");
			b.append(" */\n");
			b.append("@Override\n");
		}
		else {
			b.append("/**\n");
			b.append(" * Consume the incoming {@link Message}\n");
			b.append(" * @param message the message to be consumed\n");
			b.append(" */\n");
			b.append("@JmsListener(destination=REQUEST_QUEUE_NAME, containerFactory=\"defaultQueueFactory\")\n");
		}

		b.append("public void onMessage(Message message)\n");
		b.append("{\n");
		b.append("final RequestMessage requestMessage = validateAndConvert(message);\n\n");
		b.append("if(requestMessage == null)\n");
		b.append("return;\n\n");
		b.append("final String operationID = requestMessage.getOperationID();\n");
		b.append("final String correlationID = getCorrelationID(message);\n");
		b.append("ResponseStatus status = ResponseStatus.ERROR;\n");
		b.append("String statusMessage = null;\n");
		b.append("Serializable requestResult = null;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(operationID.equals(OPERATION_UPLOAD))\n");
		b.append("requestResult = saveFile(requestMessage);\n");
		b.append("else if (operationID.equals(OPERATION_DOWNLOAD))\n");
		b.append("requestResult = downloadFile(requestMessage);\n");
		b.append("else\n");
		b.append("{\n");
		b.append("status = ResponseStatus.INVALID_OPERATION;\n");
		b.append("throw new IllegalStateException(\"The operation '\" + operationID + \"' is not supported!\");\n");
		b.append("}\n\n");
		b.append("status = ResponseStatus.SUCCESS;\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("statusMessage = ex.getMessage();\n");
		b.append("logger.error(\"Error while processing '{}' message with ID {}!\", operationID, correlationID, ex);\n");
		b.append("}\n\n");
		b.append("responseMessageSender.");

		if (project.isJakartaEEApplication())
			b.append("sendResponse(responseQueue");
		else
			b.append("sendResponseToQueue(RESPONSE_QUEUE_NAME");

		b.append(", operationID, correlationID, status, statusMessage, requestResult);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Save either a single file or a part of the file\n");
		b.append(" * @param requestMessage the {@link RequestMessage} that contains all necessary data\n");
		b.append(" * @return the fully-qualified path of the file where the content has been saved\n");
		b.append(" * @throws IOException if the file either could not be created or appended\n");
		b.append(" */\n");
		b.append("private String saveFile(RequestMessage requestMessage) throws IOException\n");
		b.append("{\n");
		b.append("final File targetFile;\n");
		b.append("final byte[] content = requestMessage.getNextParameter(byte[].class);\n");
		b.append("String path = requestMessage.getNextStringParameter();\n");
		b.append("final String fileName = requestMessage.getNextStringParameter();\n\n");
		b.append("if(path != null)\n");
		b.append("{\n");
		b.append("// For larger files that are filled by multiple requests the ");
		b.append("message must contain the actual path of the target file!\n");
		b.append("targetFile = new File(path);\n\n");
		b.append("if(!Files.exists(targetFile.toPath()))\n");
		b.append("throw new IllegalArgumentException(\"The file '\" + targetFile.getName() + \"' could not be found!\");\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("// Create an initial target file if the message doesn't contain the respective path\n");
		b.append("if(!PATH_EXCHANGE_FOLDER.isEmpty())\n");
		b.append("{\n");
		b.append("// Create the target file in the exchange folder\n");
		b.append("targetFile = new File(PATH_EXCHANGE_FOLDER + fileName + System.currentTimeMillis());\n\n");
		b.append("Files.createFile(targetFile.toPath());\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("// Create a temporary file where the content should be saved to\n");
		b.append("targetFile = Files.createTempFile(fileName, Long.toString(System.currentTimeMillis())).toFile();\n");
		b.append("}\n\n");
		b.append("path = targetFile.getAbsolutePath();\n");
		b.append("}\n\n");
		b.append("logger.info(\"Write {} byte(s) to {}\", content.length, path);\n\n");
		b.append("Files.write(targetFile.toPath(), content, StandardOpenOption.APPEND);\n\n");
		b.append("return path;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Download the requested chunk of a file\n");
		b.append(" * @param requestMessage the {@link RequestMessage} that contains all necessary data\n");
		b.append(" * @return a byte array that represents either the full content of the file or a part of it\n");
		b.append(" * @throws IOException if the file could not be read\n");
		b.append(" */\n");
		b.append("private byte[] downloadFile(RequestMessage requestMessage) throws IOException\n");
		b.append("{\n");
		b.append("final String path = requestMessage.getNextStringParameter();\n");
		b.append("final long startIndex = requestMessage.getNextLongParameter();\n");
		b.append("final int requestedSize = requestMessage.getNextIntParameter();\n");
		b.append("final File file = new File(path);\n\n");
		b.append("try(final FileChannel fileChannel = FileChannel.open(file.toPath(), StandardOpenOption.READ))\n");
		b.append("{\n");
		b.append("final int size = (int) Math.min(fileChannel.size(), requestedSize);\n");
		b.append("final var buffer = ByteBuffer.allocate(size);\n");
		b.append("final var bytesRead = fileChannel.read(buffer, startIndex);\n\n");
		b.append("if(bytesRead > 0)\n");
		b.append("{\n");
		b.append("buffer.flip();\n\n");
		b.append("logger.info(\"Read {} byte(s) from {}\", buffer.limit(), file.getName());\n\n");
		b.append("final var content = new byte[buffer.limit()];\n");
		b.append("buffer.get(content);\n\n");
		b.append("return content;\n");
		b.append("}\n");
		b.append("else\n");
		b.append("return new byte[0];\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the client for handling files
	 * @return the generated content
	 */
	private String createFileServiceClient() {
		final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;
		final StringBuilder b = new StringBuilder();

		if (project.isJakartaEEApplication())
			b.append("import jakarta.jms.Queue;\n");
		else
			b.append("import javax.jms.Queue;\n");

		b.append("import java.io.*;\n");
		b.append("import java.time.*;\n");
		b.append("import java.lang.invoke.*;\n");
		b.append("import java.nio.*;\n");
		b.append("import java.nio.channels.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n");
		b.append("import net.codecadenza.runtime.transport.file.*;\n");
		b.append("import net.codecadenza.runtime.jms.*;\n\n");
		b.append("public class " + module.getFileServiceClientName() + " extends " + BASE_JMS_CLIENT_CLASS_NAME);
		b.append(" implements " + module.getFileServiceName() + "\n");
		b.append("{\n");
		b.append("private static final String REQUEST_QUEUE_NAME = \"" + FILE_REQUEST_QUEUE + "\";\n");
		b.append("private static final String RESPONSE_QUEUE_NAME = \"" + FILE_RESPONSE_QUEUE + "\";\n");
		b.append("private static final String OPERATION_UPLOAD = \"upload\";\n");
		b.append("private static final String OPERATION_DOWNLOAD = \"download\";\n");
		b.append("private static final int BUFFER_SIZE = 1 << 18;\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n\n");
		b.append("private Queue requestQueue;\n");
		b.append("private Queue responseQueue;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + module.getFileServiceClientName() + "()\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("public " + module.getFileServiceClientName() + "(String userName, String password)\n");
		b.append("{\n");
		b.append("super(userName, password);\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see " + packageName + "." + BASE_JMS_CLIENT_CLASS_NAME + "#getLogger()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected Logger getLogger()\n");
		b.append("{\n");
		b.append("return logger;\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see " + packageName + "." + BASE_JMS_CLIENT_CLASS_NAME + "#init()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void init()\n");
		b.append("{\n");
		b.append("lock.lock();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("super.init();\n\n");
		b.append("requestQueue = (Queue) initialContext.lookup(REQUEST_QUEUE_NAME);\n");
		b.append("responseQueue = (Queue) initialContext.lookup(RESPONSE_QUEUE_NAME);\n");
		b.append("initialized = true;\n");
		b.append("}\n");
		b.append("catch (Exception ex)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(ex);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("lock.unlock();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#uploadFile(java.io.File, java.time.Duration)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String uploadFile(File file, Duration maxWaitTime)\n");
		b.append("{\n");
		b.append("String path = null;\n\n");
		b.append("logger.info(\"Upload file {}\", file.getName());\n\n");
		b.append("try(final FileChannel fileChannel = FileChannel.open(file.toPath(), StandardOpenOption.READ))\n");
		b.append("{\n");
		b.append("int chunkIndex = 0;\n");
		b.append("long size = fileChannel.size();\n\n");
		b.append("while(size > 0)\n");
		b.append("{\n");
		b.append("final ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);\n");
		b.append("final int bytesRead = fileChannel.read(buffer);\n\n");
		b.append("if(bytesRead <= 0)\n");
		b.append("break;\n\n");
		b.append("buffer.flip();\n\n");
		b.append("final var content = new byte[buffer.limit()];\n");
		b.append("buffer.get(content);\n");
		b.append("size -= bytesRead;\n\n");
		b.append("logger.debug(\"Upload chunk {} with {} byte(s) of file {}\", chunkIndex, bytesRead, file.getName());\n\n");
		b.append("final var request = new RequestMessage(OPERATION_UPLOAD)\n");
		b.append(".withParameter(content)\n");
		b.append(".withParameter(path)\n");
		b.append(".withParameter(file.getName());\n");
		b.append("final var correlationId = sendMessage(requestQueue, request);\n\n");
		b.append("path = (String) receiveResponse(responseQueue, correlationId, maxWaitTime);\n\n");
		b.append("chunkIndex++;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n\n");
		b.append("logger.info(\"Finished upload of file {}. The remote storage path is {}\", file.getName(), path);\n");
		b.append("return path;\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#downloadFile(java.lang.String, java.io.File, java.time.Duration)\n");
		b.append(" */\n");
		b.append("public void downloadFile(String pathOnServer, File targetFile, Duration maxWaitTime)\n");
		b.append("{\n");
		b.append("long startIndex = 0;\n\n");
		b.append("logger.info(\"Download file {}\", targetFile.getName());\n\n");
		b.append("try(FileOutputStream output = new FileOutputStream(targetFile, true))\n");
		b.append("{\n");
		b.append("while(true)\n");
		b.append("{\n");
		b.append("logger.debug(\"Request content for file {} from start position {} \", targetFile.getName(), startIndex);\n\n");
		b.append("final var request = new RequestMessage(OPERATION_DOWNLOAD)");
		b.append(".withParameter(pathOnServer)\n.withParameter(startIndex)\n");
		b.append(".withParameter(BUFFER_SIZE);\n");
		b.append("final var correlationId = sendMessage(requestQueue, request);\n\n");
		b.append("byte[] content = (byte[]) receiveResponse(responseQueue, correlationId, maxWaitTime);\n\n");
		b.append("logger.debug(\"Received {} byte(s) for file {}\", content.length, targetFile.getName());\n\n");
		b.append("// Append the target file with the full content of every chunk\n");
		b.append("output.write(content);\n\n");
		b.append("// Exit the loop as soon as the returned content doesn't fully fill the internal buffer\n");
		b.append("if(content.length < BUFFER_SIZE)\n");
		b.append("break;\n\n");
		b.append("startIndex += BUFFER_SIZE;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("throw new RemoteOperationException(e.getMessage());\n");
		b.append("}\n\n");
		b.append("logger.info(\"Finished download of file {}\", targetFile.getName());\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the jndi.properties file for the client artifact
	 * @param existingContent
	 * @return the generated content
	 */
	private String createJNDIProperties(List<String> existingContent) {
		final var b = new StringBuilder();

		if (project.isSpringBootApplication()) {
			final var jmsBeans = module.getNamespace().getJavaTypes().stream().map(JMSIntegrationBean.class::cast).toList();
			boolean connectionFactoryURLFound = false;

			// Avoid automatically overwriting of the connection URL, which may have been changed manually!
			for (final String line : existingContent)
				if (line.startsWith("connectionFactory.ConnectionFactory")) {
					b.append(line);
					connectionFactoryURLFound = true;
					break;
				}

			if (!connectionFactoryURLFound)
				b.append("connectionFactory.ConnectionFactory=tcp://localhost:61616\n");

			b.append("java.naming.factory.initial=org.apache.activemq.artemis.jndi.ActiveMQInitialContextFactory\n");
			b.append("queue." + FILE_REQUEST_QUEUE + "=" + FILE_REQUEST_QUEUE + "\n");
			b.append("queue." + FILE_RESPONSE_QUEUE + "=" + FILE_RESPONSE_QUEUE + "\n");

			for (final JMSIntegrationBean jmsBean : jmsBeans) {
				final JMSResource requestDestination = jmsBean.getRequestDestination();

				if (requestDestination.isTopic())
					b.append("topic." + requestDestination.getName() + "=" + requestDestination.getName() + "\n");
				else
					b.append("queue." + requestDestination.getName() + "=" + requestDestination.getName() + "\n");

				if (jmsBean.isSendResponse()) {
					final JMSResource responseDesintation = jmsBean.getResponseDestination();

					if (responseDesintation.isTopic())
						b.append("topic." + responseDesintation.getName() + "=" + responseDesintation.getName() + "\n");
					else
						b.append("queue." + responseDesintation.getName() + "=" + responseDesintation.getName() + "\n");
				}
			}
		}
		else
			b.append(super.createJNDIProperties());

		return b.toString();
	}

}
