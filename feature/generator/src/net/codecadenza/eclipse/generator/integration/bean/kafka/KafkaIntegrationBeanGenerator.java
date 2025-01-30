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
package net.codecadenza.eclipse.generator.integration.bean.kafka;

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.kafka.BasicKafkaMethodGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for Kafka integration beans
 * </p>
 * <p>
 * Copyright 2021 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class KafkaIntegrationBeanGenerator extends AbstractJavaSourceGenerator {
	private final KafkaIntegrationBean kafkaIntegrationBean;
	private final Map<String, ServiceBean> serviceMap = new HashMap<>();
	private final Map<String, String> constantMap = new HashMap<>();
	private final List<KafkaIntegrationMethod> kafkaMethods;
	private boolean sendResponse;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public KafkaIntegrationBeanGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getServiceBeanSourceFile());

		this.kafkaIntegrationBean = (KafkaIntegrationBean) integrationBean;
		this.kafkaMethods = kafkaIntegrationBean.getMethods().stream().map(KafkaIntegrationMethod.class::cast).toList();

		this.kafkaMethods.forEach(method -> {
			final var methodGenerator = new BasicKafkaMethodGenerator(method, this);

			this.serviceMap.putAll(methodGenerator.getServices());
			this.constantMap.putAll(methodGenerator.getConstants());

			if (method.isSendResponse())
				this.sendResponse = true;
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importClass("org.springframework.stereotype.Component");
		importClass("org.springframework.kafka.annotation.KafkaListener");
		importClass("org.apache.kafka.clients.consumer.ConsumerRecord");
		importStaticClass("net.codecadenza.runtime.spring.kafka.KafkaHeaderHelper.getRequestSchemaFromHeader");

		if (!kafkaMethods.isEmpty())
			importPackage(kafkaIntegrationBean.getNamespace().toString() + SUB_PACKAGE_INT_AVRO);

		if (sendResponse) {
			importPackage("net.codecadenza.runtime.avro.response");
			importClass("net.codecadenza.runtime.spring.kafka.KafkaSender");
			importStaticClass("net.codecadenza.runtime.spring.kafka.KafkaHeaderHelper.getCorrelationIdFromHeader");

			if (kafkaMethods.stream().anyMatch(KafkaIntegrationMethod::isUseDedicatedPartition))
				importStaticClass("net.codecadenza.runtime.spring.kafka.KafkaHeaderHelper.getResponsePartitionIdFromHeader");
		}

		kafkaMethods.forEach(method -> addImports(new BasicKafkaMethodGenerator(method, this).getImports()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Component\n");
		b.append("public class " + kafkaIntegrationBean.getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		constantMap.put(JavaType.STRING + " REQUEST_TOPIC", "\"" + kafkaIntegrationBean.getRequestTopic() + "\"");

		if (sendResponse)
			constantMap.put(JavaType.STRING + " RESPONSE_TOPIC", "\"" + kafkaIntegrationBean.getResponseTopic() + "\"");

		constantMap.put(JavaType.STRING + " GROUP_ID", "\"" + kafkaIntegrationBean.getConsumerGroup() + "\"");

		constantMap.entrySet().forEach(entry -> {
			final int delimiterPos = entry.getKey().lastIndexOf(" ");
			final String typeName = entry.getKey().substring(0, delimiterPos);
			final String fieldName = entry.getKey().substring(delimiterPos + 1);

			addPrivateConstant(typeName, fieldName, entry.getValue()).create();
		});

		if (sendResponse)
			addPrivateField("KafkaSender", "kafkaSender").inject().create();

		// Add declarations for all services that must be injected
		serviceMap.keySet().forEach(serviceName -> {
			final ServiceBean serviceBean = serviceMap.get(serviceName);
			final String serviceType = serviceBean.getInterfaceName() != null ? serviceBean.getInterfaceName() : serviceBean.getName();

			importPackage(serviceBean.getNamespace().toString());

			addPrivateField(serviceType, serviceName).inject().create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		addListenerMethod();

		kafkaMethods.forEach(method -> {
			final var b = new StringBuilder();
			final var methodGenerator = new BasicKafkaMethodGenerator(method, this);
			final String identifier = methodGenerator.getMethodSignature(false, false, false);

			b.append(methodGenerator.createComment());
			b.append(methodGenerator.createMethod());

			addMethod(identifier, b.toString());
		});
	}

	/**
	 * Create the method that listens for incoming messages
	 */
	private void addListenerMethod() {
		final var methodSignature = "void onMessageReceived(ConsumerRecord<String, byte[]> consumerRecord)";

		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Listen for incoming messages from the request topic\n");
		b.append(" * @param consumerRecord the incoming record\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@KafkaListener(groupId = GROUP_ID, topics = REQUEST_TOPIC)\n");
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var payload = consumerRecord.value();\n");
		b.append("final var schema = getRequestSchemaFromHeader(consumerRecord);\n\n");
		b.append("if(payload != null)\n");

		addDebugLog(b, "Received {} byte(s) with schema '{}'", "payload.length", "schema");

		b.append("\n");

		if (!kafkaIntegrationBean.getMethods().isEmpty()) {
			boolean firstMethod = true;

			b.append("// Perform an operation depending on the provided request schema\n");

			for (final KafkaIntegrationMethod method : kafkaMethods) {
				if (!firstMethod)
					b.append("else ");
				else
					firstMethod = false;

				b.append("if(schema.equals(" + method.getRequestSchemaName() + ".getClassSchema().getName()))\n");
				b.append(method.getName() + "(");

				if (!method.getIntegrationParameters().isEmpty())
					b.append("payload");

				if (method.isSendResponse()) {
					if (!method.getIntegrationParameters().isEmpty())
						b.append(", ");

					b.append("getCorrelationIdFromHeader(consumerRecord)");

					if (method.isUseDedicatedPartition())
						b.append(", getResponsePartitionIdFromHeader(consumerRecord)");
				}

				b.append(");\n");
			}

			b.append("else\n");
			b.append("throw new IllegalStateException(\"The schema '\" + schema + \"' is not supported!\");\n");
		}

		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("// Only log the exception so that the listener can commit the message offset\n");

		addErrorLog(b, "Error while processing request message!", "e");

		b.append("}\n");
		b.append("}\n");

		addMethod(methodSignature, b.toString());
	}

}
