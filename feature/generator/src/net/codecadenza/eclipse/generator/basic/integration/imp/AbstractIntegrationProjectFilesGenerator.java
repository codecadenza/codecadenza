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

import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_PRODUCER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_SECURITY;

import net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.common.LoggingGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Abstract base class for generators that create basic source and configuration files that are necessary for integration
 * artifacts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractIntegrationProjectFilesGenerator implements IIntegrationProjectFilesGenerator {
	protected static final String JNDI_PROPERTIES_FILE = "jndi.properties";

	protected final IntegrationModule module;
	protected final Project project;
	protected final BuildArtifactType artifactType;

	/**
	 * Constructor
	 * @param module
	 * @param artifactType
	 */
	protected AbstractIntegrationProjectFilesGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		this.module = module;
		this.project = module.getProject();
		this.artifactType = artifactType;
	}

	/**
	 * Create the credentials provider
	 * @return the internal representation of the credentials provider
	 */
	public JavaFile createCredentialsProvider() {
		final var b = new StringBuilder();

		if (project.isSpringBootApplication()) {
			b.append("import org.springframework.stereotype.*;\n");
			b.append("import org.springframework.web.context.annotation.*;\n");
		}
		else
			b.append("import jakarta.enterprise.context.*;\n");

		b.append("import java.io.*;\n\n");

		if (project.isSpringBootApplication()) {
			b.append("@SessionScope\n");
			b.append("@Service\n");
		}
		else
			b.append("@SessionScoped\n");

		b.append("public class " + module.getCredentialsProviderName() + " implements Serializable\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n");
		b.append("private String userName;\n");
		b.append("private String password;\n\n");
		b.append("/**\n");
		b.append(" * @return the user name\n");
		b.append(" */\n");
		b.append("public String getUserName()\n");
		b.append("{\n");
		b.append("return userName;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param userName\n");
		b.append(" */\n");
		b.append("public void setUserName(String userName)\n");
		b.append("{\n");
		b.append("this.userName = userName;\n");
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
		b.append("}\n");
		b.append("}\n");

		final var comment = "This class holds the credentials that are necessary for injected services to provide proper login data when they are accessing protected resources.";
		final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_SECURITY;

		final var credentialsProvider = new JavaFile(project, artifactType, module.getCredentialsProviderName(), packageName);
		credentialsProvider.setComment(comment);
		credentialsProvider.setContent(b.toString());

		return credentialsProvider;
	}

	/**
	 * Create the file service producer
	 * @return the internal representation of the file service producer
	 */
	public JavaFile createFileServiceProducer() {
		final var b = new StringBuilder();
		final IntegrationTechnology integrationTechnology = module.getTechnology();
		final boolean addCredentials = project.getApplicationLogOnDTO() != null
				&& integrationTechnology != IntegrationTechnology.KAFKA && integrationTechnology != IntegrationTechnology.JMS;
		final var param = addCredentials && project.isSpringBootApplication()
				? module.getCredentialsProviderName() + " credentialsProvider" : "";
		final boolean hasClientField = integrationTechnology == IntegrationTechnology.REST
				|| integrationTechnology == IntegrationTechnology.KAFKA || integrationTechnology == IntegrationTechnology.JMS;

		b.append("import java.io.*;\n");
		b.append("import " + module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT + ".*;\n");

		if (hasClientField)
			b.append("import jakarta.annotation.PreDestroy;\n");
		else if (integrationTechnology == IntegrationTechnology.RMI)
			b.append("import javax.naming.*;\n");

		if (integrationTechnology != IntegrationTechnology.SOAP)
			b.append("import net.codecadenza.runtime.transport.file.*;\n");
		else
			b.append("import " + module.getNamespace().toString() + ".*;\n");

		if (addCredentials) {
			b.append("import " + module.getNamespace().toString() + SUB_PACKAGE_INT_SECURITY + ".*;\n");

			if (project.isJakartaEEApplication())
				b.append("import jakarta.inject.Inject;\n");
		}

		if (project.isJakartaEEApplication()) {
			b.append("import jakarta.enterprise.inject.*;\n");
			b.append("import jakarta.enterprise.context.*;\n");
		}
		else {
			b.append("import org.springframework.context.annotation.*;\n");
			b.append("import org.springframework.stereotype.*;\n");
		}

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");

		if (project.isSpringBootApplication()) {
			b.append("@Configuration\n");
			b.append("@Component\n");
		}
		else
			b.append("@SessionScoped\n");

		b.append("public class " + module.getFileServiceProducerName() + " implements Serializable\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final long serialVersionUID = 1L;\n");

		if (hasClientField)
			b.append("private transient " + module.getFileServiceClientName() + " client;\n");

		if (addCredentials && project.isJakartaEEApplication()) {
			b.append("private final " + module.getCredentialsProviderName() + " credentialsProvider;\n\n");
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append("public " + module.getFileServiceProducerName() + "()\n");
			b.append("{\n");
			b.append("this.credentialsProvider = null;\n");
			b.append("}\n\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" * @param credentialsProvider\n");
			b.append(" */\n");
			b.append("@Inject\n");
			b.append("public " + module.getFileServiceProducerName() + "(");
			b.append(module.getCredentialsProviderName() + " credentialsProvider)\n");
			b.append("{\n");
			b.append("this.credentialsProvider = credentialsProvider;\n");
			b.append("}\n");
		}

		b.append("\n");
		b.append("/**\n");
		b.append(" * Producer method for injecting the file " + integrationTechnology.getName() + " service\n");

		if (addCredentials && project.isSpringBootApplication())
			b.append(" * @param credentialsProvider\n");

		b.append(" * @return an instance of the " + integrationTechnology.getName() + " file service\n");

		if (integrationTechnology == IntegrationTechnology.RMI)
			b.append(" * @throws IllegalStateException if the service could not be created\n");

		b.append(" */\n");

		if (project.isJakartaEEApplication()) {
			b.append("@Produces\n");
			b.append("@SessionScoped\n");
		}
		else
			b.append("@Bean\n");

		b.append("public " + module.getFileServiceName() + " get" + module.getFileServiceName() + "(" + param + ")\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Create service instance");

		b.append("\n");

		if (integrationTechnology == IntegrationTechnology.RMI) {
			b.append("try\n");
			b.append("{\n");
		}

		if (hasClientField)
			b.append("client = ");
		else
			b.append("return ");

		b.append("new " + module.getFileServiceClientName() + "(");

		if (addCredentials)
			b.append("credentialsProvider.getUserName(), credentialsProvider.getPassword()");

		if (hasClientField) {
			b.append(");\n");

			if (integrationTechnology == IntegrationTechnology.JMS)
				b.append("client.init();\n");

			b.append("\n");

			if (integrationTechnology == IntegrationTechnology.REST)
				b.append("return client.getService();\n");
			else
				b.append("return client;\n");
		}
		else
			b.append(").getService();\n");

		if (integrationTechnology == IntegrationTechnology.RMI) {
			b.append("}\n");
			b.append("catch (final NamingException e)\n");
			b.append("{\n");

			LoggingGenerator.addErrorLog(b, "Error while creating service!", "e");

			b.append("\n");
			b.append("throw new IllegalStateException(e);\n");
			b.append("}\n");
		}

		b.append("}\n\n");

		if (hasClientField) {
			b.append("/**\n");
			b.append(" * Release internal resources\n");
			b.append(" */\n");
			b.append("@PreDestroy\n");
			b.append("public void close()\n");
			b.append("{\n");

			LoggingGenerator.addDebugLog(b, "Release service resources");

			b.append("\n");
			b.append("client.close();\n");
			b.append("}\n");
		}

		b.append("}\n");

		final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_PRODUCER;

		final var fileProducer = new JavaFile(project, artifactType, module.getFileServiceProducerName(), packageName);
		fileProducer.setComment("Producer for the file " + module.getTechnology().getName() + " service");
		fileProducer.setContent(b.toString());

		return fileProducer;
	}

	/**
	 * Create the jndi.properties file for the client artifact
	 * @return the generated content
	 */
	protected String createJNDIProperties() {
		final var b = new StringBuilder();

		if (project.isDeployedOnGlassfish()) {
			b.append("java.naming.factory.initial=com.sun.enterprise.naming.SerialInitContextFactory\n");
			b.append("java.naming.factory.url.pkgs=com.sun.enterprise.naming\n");
			b.append("java.naming.factory.state=com.sun.corba.ee.impl.presentation.rmi.JNDIStateFactoryImpl\n");
			b.append("org.omg.CORBA.ORBInitialHost=localhost\n");
			b.append("org.omg.CORBA.ORBInitialPort=3700\n");
		}
		else {
			b.append("java.naming.factory.initial=org.wildfly.naming.client.WildFlyInitialContextFactory\n");
			b.append("java.naming.provider.url=http-remoting://localhost:8080\n");
			b.append("jboss.naming.client.connect.options.org.xnio.Options.SASL_POLICY_NOPLAINTEXT=false\n");
			b.append("jboss.naming.client.ejb.context=true\n");
		}

		return b.toString();
	}

}
