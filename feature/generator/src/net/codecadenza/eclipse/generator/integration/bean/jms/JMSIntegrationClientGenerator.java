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

import static net.codecadenza.eclipse.shared.Constants.BASE_JMS_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;

import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.jms.BasicJMSMethodGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for JMS integration clients
 * </p>
 * <p>
 * Copyright 2023 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JMSIntegrationClientGenerator extends AbstractJavaSourceGenerator {
	private final JMSIntegrationBean jmsBean;
	private final Map<JMSIntegrationMethod, String> operationConstants;
	private final Project project;
	private final boolean sendResponse;
	private final String basePackage;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public JMSIntegrationClientGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getClientSourceFile());

		this.project = integrationBean.getNamespace().getProject();
		this.jmsBean = (JMSIntegrationBean) integrationBean;
		this.operationConstants = BasicJMSMethodGenerator.getOperationIDConstants(this.jmsBean);
		this.sendResponse = this.jmsBean.isSendResponse();
		this.basePackage = this.jmsBean.getIntegrationModule().getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage(jmsBean.getNamespace().toString());

		if (!jmsBean.getMethods().isEmpty()) {
			importPackage("net.codecadenza.runtime.jms");

			if (project.isJakartaEEApplication())
				importClass("jakarta.jms.Destination");
			else
				importClass("javax.jms.Destination");
		}

		jmsBean.getMethods().stream().map(JMSIntegrationMethod.class::cast).forEach(method -> {
			final var methodGenerator = new BasicJMSMethodGenerator(method, this);

			addImports(methodGenerator.getInterfaceImports());
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + jmsBean.getClientClassName() + " extends " + BASE_JMS_CLIENT_CLASS_NAME + " ");
		b.append("implements " + jmsBean.getInterfaceName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (!jmsBean.getMethods().isEmpty()) {
			final String destinationName = jmsBean.getRequestDestination().getName();

			addPrivateConstant(JavaType.STRING, "REQUEST_DESTINATION", "\"" + destinationName + "\"").create();
		}

		if (sendResponse) {
			final String destinationName = jmsBean.getResponseDestination().getName();

			addPrivateConstant(JavaType.STRING, "RESPONSE_DESTINATION", "\"" + destinationName + "\"").create();
		}

		// Create the constants for all operation IDs
		operationConstants
				.forEach((method, name) -> addPrivateConstant(JavaType.STRING, name, "\"" + method.getOperationID() + "\"").create());

		if (!jmsBean.getMethods().isEmpty())
			addPrivateField("Destination", "requestDestination").create();

		if (sendResponse)
			addPrivateField("Destination", "responseDestination").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var identifier = jmsBean.getClientClassName() + "()";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Default constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());

		identifier = jmsBean.getClientClassName() + "(String userName, String password)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("super(userName, password);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		addGetLoggerMethod(basePackage + "." + BASE_JMS_CLIENT_CLASS_NAME);

		if (!jmsBean.getMethods().isEmpty())
			addInitMethod();

		jmsBean.getMethods().stream().map(JMSIntegrationMethod.class::cast).forEach(jmsMethod -> {
			if (jmsMethod.isSendResponse()) {
				// Create a method that waits for the corresponding response message
				createMethod(jmsMethod, jmsMethod.isSendResponse());
			}

			// Create a method that just sends a request message
			createMethod(jmsMethod, false);
		});
	}

	/**
	 * Create the JMS client method
	 * @param jmsMethod
	 * @param waitForResponse
	 */
	private void createMethod(JMSIntegrationMethod jmsMethod, boolean waitForResponse) {
		final var methodGenerator = new BasicJMSMethodGenerator(jmsMethod, this);

		final var b = new StringBuilder();
		b.append(methodGenerator.createCommentLink(waitForResponse ? "java.time.Duration" : null));
		b.append(methodGenerator.createClientMethod(waitForResponse));

		addMethod(methodGenerator.getClientSignature(waitForResponse, false, true), b.toString());
	}

	/**
	 * Create the method to initialize the client
	 */
	private void addInitMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void init()";

		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see " + basePackage + "." + BASE_JMS_CLIENT_CLASS_NAME + "#init()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("lock.lock();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("super.init();\n\n");
		b.append("requestDestination = (Destination) initialContext.lookup(REQUEST_DESTINATION);\n");

		if (sendResponse)
			b.append("responseDestination = (Destination) initialContext.lookup(RESPONSE_DESTINATION);\n");

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

		addMethod(methodSignature, b.toString());
	}

}
