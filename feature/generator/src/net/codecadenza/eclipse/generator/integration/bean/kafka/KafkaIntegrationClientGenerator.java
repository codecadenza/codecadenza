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

import static net.codecadenza.eclipse.shared.Constants.BASE_KAFKA_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_AVRO;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SEI;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.kafka.BasicKafkaMethodGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for Kafka integration clients
 * </p>
 * <p>
 * Copyright 2021 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class KafkaIntegrationClientGenerator extends AbstractJavaSourceGenerator {
	private final KafkaIntegrationBean kafkaBean;
	private final boolean sendResponse;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public KafkaIntegrationClientGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getClientSourceFile());

		this.kafkaBean = (KafkaIntegrationBean) integrationBean;
		this.sendResponse = kafkaBean.getMethods().stream().map(KafkaIntegrationMethod.class::cast)
				.anyMatch(KafkaIntegrationMethod::isSendResponse);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage(kafkaBean.getNamespace().toString() + SUB_PACKAGE_INT_SEI);

		if (!kafkaBean.getMethods().isEmpty())
			importPackage(kafkaBean.getNamespace().toString() + SUB_PACKAGE_INT_AVRO);

		if (sendResponse) {
			importPackage(PACK_JAVA_UTIL);
			importPackage("net.codecadenza.runtime.avro.response");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + kafkaBean.getClientClassName() + " extends " + BASE_KAFKA_CLIENT_CLASS_NAME + " ");
		b.append("implements " + kafkaBean.getInterfaceName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (!kafkaBean.getMethods().isEmpty())
			addPrivateConstant(JavaType.STRING, "REQUEST_TOPIC", "\"" + kafkaBean.getRequestTopic() + "\"").create();

		if (sendResponse)
			addPrivateConstant(JavaType.STRING, "RESPONSE_TOPIC", "\"" + kafkaBean.getResponseTopic() + "\"").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		kafkaBean.getMethods().stream().map(KafkaIntegrationMethod.class::cast).forEach(kafkaMethod -> {
			if (kafkaMethod.isSendResponse()) {
				// Create a method that waits for the corresponding response message
				createMethod(kafkaMethod, kafkaMethod.isSendResponse());
			}

			// Create a method that just sends a request message
			createMethod(kafkaMethod, false);
		});
	}

	/**
	 * Create the Kafka client method
	 * @param kafkaMethod
	 * @param waitForResponse
	 */
	private void createMethod(KafkaIntegrationMethod kafkaMethod, boolean waitForResponse) {
		final var methodGenerator = new BasicKafkaMethodGenerator(kafkaMethod, this);

		addImports(methodGenerator.getInterfaceImports());

		final var b = new StringBuilder();
		b.append(methodGenerator.createCommentLink(waitForResponse ? "boolean" : null));
		b.append(methodGenerator.createClientMethod(waitForResponse));

		addMethod(methodGenerator.getClientSignature(waitForResponse, false, true), b.toString());
	}

}
