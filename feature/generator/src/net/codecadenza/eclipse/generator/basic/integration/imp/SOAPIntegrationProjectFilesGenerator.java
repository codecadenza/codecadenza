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

import static net.codecadenza.eclipse.shared.Constants.BASE_SOAP_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.REMOTE_OPERATION_EXCEPTION_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.generator.common.LoggingGenerator;
import net.codecadenza.eclipse.generator.integration.bean.soap.SOAPConfigGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for basic source and configuration files necessary for supporting SOAP
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SOAPIntegrationProjectFilesGenerator extends AbstractIntegrationProjectFilesGenerator {
	public static final String FILE_SERVICE_BEAN = "FileSOAPServiceBean";
	public static final String AUTH_HANDLER = "AuthenticationHandler";
	public static final String LOGGING_HANDLER = "LoggingHandler";
	public static final String HANDLER_CHAIN = "handler-chain-server.xml";

	/**
	 * Constructor
	 * @param module
	 * @param artifactType
	 */
	public SOAPIntegrationProjectFilesGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		super(module, artifactType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final var sourceFiles = new ArrayList<JavaFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_SOAP) {
			final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;

			final var baseClientClass = new JavaFile(project, artifactType, BASE_SOAP_CLIENT_CLASS_NAME, packageName);
			baseClientClass.setComment("Abstract base class for all SOAP clients");
			baseClientClass.setContent(createBaseClient());

			sourceFiles.add(baseClientClass);

			final var fileClientClass = new JavaFile(project, artifactType, module.getFileServiceClientName(), packageName);
			fileClientClass.setComment("Client for file operations via SOAP");
			fileClientClass.setContent(createFileServiceClient());

			sourceFiles.add(fileClientClass);

			if (module.isAddProducers())
				sourceFiles.add(createFileServiceProducer());
		}
		else if (artifactType == BuildArtifactType.INTEGRATION_IMP_SOAP) {
			sourceFiles.add(createFileServiceBean());
			sourceFiles.add(createLoggingHandler());

			if (project.isSpringBootApplication())
				try {
					new SOAPConfigGenerator(module).createSourceFile();
				}
				catch (final Exception e) {
					throw new IllegalStateException(e);
				}
		}
		else {
			final String packageName = module.getNamespace().toString();

			final var fileSEI = new JavaFile(project, artifactType, module.getFileServiceName(), packageName);
			fileSEI.setComment("Service interface for file operations via SOAP");
			fileSEI.setContent(createFileSEI());

			sourceFiles.add(fileSEI);
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

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_SOAP) {
			final var path = project.getConfigFolder(artifactType) + "/soap_config.properties";

			fileList.add(new WorkspaceFile(project, artifactType, path, createClientConfig()));
		}
		else if (artifactType == BuildArtifactType.INTEGRATION_IMP_SOAP)
			fileList.add(createHandlerChain());

		return fileList;
	}

	/**
	 * Create the basic SOAP client
	 * @return the generated content
	 */
	private String createBaseClient() {
		final var b = new StringBuilder();

		if (project.isSpringBootApplication()) {
			b.append("import static net.codecadenza.runtime.crypto.HashGenerator.*;\n");
			b.append("import java.security.NoSuchAlgorithmException;");
		}

		b.append("import java.net.*;\n");
		b.append("import java.util.*;\n");
		b.append("import javax.xml.namespace.*;\n");
		b.append("import jakarta.xml.ws.handler.*;\n");
		b.append("import jakarta.xml.ws.*;\n");
		b.append("import net.codecadenza.runtime.property.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public class " + BASE_SOAP_CLIENT_CLASS_NAME + " extends Service\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String PROP_FILE_NAME = \"config/soap_config.properties\";\n");
		b.append("private static final String PROP_NAME_SERVICE_URL = \"service_url\";\n");
		b.append("private static final String SERVICE_URL = ");
		b.append("new PropertyService(PROP_FILE_NAME).getStringProperty(PROP_NAME_SERVICE_URL);\n\n");
		b.append("protected String userName;\n");
		b.append("protected String password;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param wsdlDocumentLocation\n");
		b.append(" * @param serviceName\n");
		b.append(" */\n");
		b.append("protected " + BASE_SOAP_CLIENT_CLASS_NAME + "(URL wsdlDocumentLocation, QName serviceName)\n");
		b.append("{\n");
		b.append("super(wsdlDocumentLocation, serviceName);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param wsdlDocumentLocation\n");
		b.append(" * @param serviceName\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("protected " + BASE_SOAP_CLIENT_CLASS_NAME + "(URL wsdlDocumentLocation, ");
		b.append("QName serviceName, String userName, String password)\n");
		b.append("{\n");
		b.append("super(wsdlDocumentLocation, serviceName);\n\n");
		b.append("this.userName = userName;\n");
		b.append("this.password = password;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param serviceName\n");
		b.append(" * @return the URL where the WSDL file can be found\n");
		b.append(" */\n");
		b.append("protected static URL getWsdlURL(String serviceName)\n");
		b.append("{\n");
		b.append("final var serviceURL = SERVICE_URL + serviceName + \"?wsdl\";\n\n");

		LoggingGenerator.addDebugLog(b, "Initialize service with end-point URL '{}'", "serviceURL");

		b.append("\n");
		b.append("try\n");
		b.append("{\n");
		b.append("return URI.create(serviceURL).toURL();\n");
		b.append("}\n");
		b.append("catch (final MalformedURLException e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "The end-point URL '{}' is malformed!", "e", "serviceURL");

		b.append("}\n\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param bindingProvider\n");
		b.append(" */\n");
		b.append("protected void addCredentials(BindingProvider bindingProvider)\n");
		b.append("{\n");
		b.append("if(userName == null || password == null)\n");
		b.append("return;\n\n");

		LoggingGenerator.addDebugLog(b, "Add authorization information for user '{}'", "userName");

		b.append("\n");
		b.append("final var headers = new HashMap<String, List<String>>();\n");
		b.append("headers.put(\"username\", Collections.singletonList(userName));\n");

		if (project.isSpringBootApplication()) {
			b.append("\n");
			b.append("try\n");
			b.append("{\n");
			b.append("headers.put(\"password\", Collections.singletonList(encryptSHA256(password)));\n");
			b.append("}\n");
			b.append("catch (NoSuchAlgorithmException e)\n");
			b.append("{\n");
			b.append("logger.error(\"Error while encrypting password!\", e);\n");
			b.append("}\n\n");
		}
		else
			b.append("headers.put(\"password\", Collections.singletonList(password));\n\n");

		b.append("final Map<String, Object> requestContext = bindingProvider.getRequestContext();\n");
		b.append("requestContext.put(MessageContext.HTTP_REQUEST_HEADERS, headers);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param userName\n");
		b.append(" */\n");
		b.append("public void setUserName(String userName)\n");
		b.append("{\n");
		b.append("this.userName = userName;\n");
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
	 * Create the SEI for file operations
	 * @return the generated content
	 */
	private String createFileSEI() {
		final var b = new StringBuilder();
		b.append("import jakarta.jws.*;\n");
		b.append("import jakarta.jws.soap.*;\n");
		b.append("import jakarta.jws.soap.SOAPBinding.*;\n");
		b.append("import net.codecadenza.runtime.transport.file.*;\n\n");
		b.append("@WebService(name = " + module.getFileServiceName() + ".NAME, targetNamespace = ");
		b.append(module.getFileServiceName() + ".NAMESPACE, ");
		b.append("serviceName = " + module.getFileServiceName() + ".SERVICE_NAME, portName = ");
		b.append(module.getFileServiceName() + ".PORT_NAME)\n");
		b.append("@SOAPBinding(parameterStyle = ParameterStyle.WRAPPED, style = Style.DOCUMENT, use = Use.ENCODED)\n");
		b.append("public interface " + module.getFileServiceName() + " extends FileSOAPService\n");
		b.append("{\n");
		b.append("String NAME = \"FileServicePortType\";\n");
		b.append("String NAMESPACE = \"" + project.getXmlNamespace() + "\";\n");
		b.append("String SERVICE_NAME = \"FileService\";\n");
		b.append("String PORT_NAME = \"FileServicePort\";\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the file service bean
	 * @return the generated Java file
	 */
	public JavaFile createFileServiceBean() {
		final var b = new StringBuilder();
		b.append("import java.io.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import jakarta.activation.*;\n");
		b.append("import jakarta.mail.util.*;\n");
		b.append("import jakarta.ws.rs.core.*;\n");
		b.append("import jakarta.xml.ws.soap.*;\n");
		b.append("import jakarta.jws.*;\n");

		if (project.isJakartaEEApplication())
			b.append("import jakarta.enterprise.context.*;\n");
		else
			b.append("import org.springframework.stereotype.Service;");

		b.append("import net.codecadenza.runtime.property.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n");
		b.append("import " + module.getNamespace().toString() + ".*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");

		if (project.isJakartaEEApplication())
			b.append("@RequestScoped\n");
		else
			b.append("@Service\n");

		b.append("@WebService(targetNamespace = " + module.getFileServiceName() + ".NAMESPACE, serviceName = ");
		b.append(module.getFileServiceName() + ".SERVICE_NAME, portName = " + module.getFileServiceName() + ".PORT_NAME)\n");
		b.append("@HandlerChain(file = \"");

		if (!project.isDeployedOnPayara())
			b.append("/");

		b.append(HANDLER_CHAIN + "\")\n");
		b.append("@MTOM\n");
		b.append("public class " + FILE_SERVICE_BEAN + " implements " + module.getFileServiceName() + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String PATH_EXCHANGE_FOLDER = ");
		b.append("new PropertyService().getStringProperty(PropertyService.PROP_EXCHANGE_FOLDER);\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file.FileSOAPService#downloadFile(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public DataHandler downloadFile(String pathOnServer)\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Download file {}", "pathOnServer");

		b.append("\n");
		b.append("try(final var fin = new FileInputStream(new File(pathOnServer)))\n");
		b.append("{\n");
		b.append("final var dataSource = new ByteArrayDataSource(fin, MediaType.APPLICATION_OCTET_STREAM);\n\n");
		b.append("return new DataHandler(dataSource);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new " + REMOTE_OPERATION_EXCEPTION_NAME + "(e.getMessage());\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file.FileSOAPService#uploadFile(java.lang.String, byte[])\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String uploadFile(String fileName, byte[] data)\n");
		b.append("{\n");
		b.append("final File file;\n\n");

		LoggingGenerator.addDebugLog(b, "Save file {} with {} byte(s)", "fileName", "data.length");

		b.append("\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(!PATH_EXCHANGE_FOLDER.isEmpty())\n");
		b.append("{\n");
		b.append("// Save file in exchange folder\n");
		b.append("file = new File(PATH_EXCHANGE_FOLDER + fileName + System.currentTimeMillis());\n");
		b.append("file.createNewFile();\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("// Create temporary file\n");
		b.append("file = File.createTempFile(fileName, Long.toString(System.currentTimeMillis()));\n");
		b.append("}\n\n");
		b.append("Files.copy(new ByteArrayInputStream(data), file.toPath(), StandardCopyOption.REPLACE_EXISTING);\n\n");
		b.append("return file.getAbsolutePath();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new " + REMOTE_OPERATION_EXCEPTION_NAME + "(e.getMessage());\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		final var fileServiceBean = new JavaFile(project, artifactType, FILE_SERVICE_BEAN,
				module.getNamespace().toString() + SUB_PACKAGE_BEAN);
		fileServiceBean.setComment("File service implementation");
		fileServiceBean.setContent(b.toString());

		return fileServiceBean;
	}

	/**
	 * Create the file service SOAP client
	 * @return the generated content
	 */
	private String createFileServiceClient() {
		final var b = new StringBuilder();
		b.append("import javax.xml.namespace.*;\n");
		b.append("import jakarta.xml.ws.*;\n");
		b.append("import " + module.getNamespace().toString() + ".*;\n\n");
		b.append("@WebServiceClient(name = " + module.getFileServiceName() + ".SERVICE_NAME, targetNamespace = ");
		b.append(module.getFileServiceName() + ".NAMESPACE)\n");
		b.append("public class " + module.getFileServiceClientName() + " extends " + BASE_SOAP_CLIENT_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private static final QName SERVICE = new QName(" + module.getFileServiceName() + ".NAMESPACE, ");
		b.append(module.getFileServiceName() + ".SERVICE_NAME);\n\n");
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
		b.append("super(" + BASE_SOAP_CLIENT_CLASS_NAME + ".getWsdlURL(" + module.getFileServiceName());
		b.append(".SERVICE_NAME), SERVICE, userName, password);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the service proxy\n");
		b.append(" */\n");
		b.append("@WebEndpoint(name = " + module.getFileServiceName() + ".PORT_NAME)\n");
		b.append("public " + module.getFileServiceName() + " getService()\n");
		b.append("{\n");
		b.append("final " + module.getFileServiceName() + " service = super.getPort(new QName(" + module.getFileServiceName());
		b.append(".NAMESPACE, " + module.getFileServiceName() + ".PORT_NAME), " + module.getFileServiceName() + ".class);\n\n");
		b.append("addCredentials((BindingProvider) service);\n\n");
		b.append("return service;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the authorization handler
	 * @return the generated Java file
	 */
	public JavaFile createAuthHandler() {
		final var b = new StringBuilder();
		b.append("import java.util.*;\n");
		b.append("import jakarta.servlet.http.*;\n");
		b.append("import javax.xml.namespace.*;\n");
		b.append("import jakarta.xml.ws.handler.*;\n");
		b.append("import jakarta.xml.ws.handler.soap.*;\n");

		if (project.isJakartaEEApplication()) {
			b.append("import jakarta.inject.*;\n");
			b.append("import jakarta.security.enterprise.*;\n");
			b.append("import jakarta.security.enterprise.credential.*;\n");
			b.append("import jakarta.security.enterprise.authentication.mechanism.http.*;\n");
		}

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public class " + AUTH_HANDLER + " implements SOAPHandler<SOAPMessageContext>\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");

		if (project.isJakartaEEApplication()) {
			b.append("private final SecurityContext securityContext;\n\n");
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append("public " + AUTH_HANDLER + "()\n");
			b.append("{\n");
			b.append("this.securityContext = null;\n");
			b.append("}\n\n");
			b.append("/**\n");
			b.append(" * Constructor for injecting the security context\n");
			b.append(" * @param securityContext\n");
			b.append(" */\n");
			b.append("@Inject\n");
			b.append("public " + AUTH_HANDLER + "(SecurityContext securityContext)\n");
			b.append("{\n");
			b.append("this.securityContext = securityContext;\n");
			b.append("}\n");
		}

		b.append("\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.Handler#handleMessage(jakarta.xml.ws.handler.MessageContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public boolean handleMessage(final SOAPMessageContext context)\n");
		b.append("{\n");
		b.append("final var outbound = (Boolean) context.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY);\n");
		b.append("var userName = \"\";\n\n");
		b.append("if(Boolean.TRUE.equals(outbound))\n");
		b.append("return true;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var httpHeaders = (Map<?, ?>) context.get(MessageContext.HTTP_REQUEST_HEADERS);\n\n");
		b.append("if(!httpHeaders.containsKey(\"username\") || !httpHeaders.containsKey(\"password\"))\n");
		b.append("{\n");

		LoggingGenerator.addInfoLog(b, "Cannot perform log in! The request header doesn't contain appropriate data!");

		b.append("\n");
		b.append("return false;\n");
		b.append("}\n\n");
		b.append("final var userList = (List<?>) httpHeaders.get(\"username\");\n");
		b.append("final var passwordList = (List<?>) httpHeaders.get(\"password\");\n");
		b.append("final String password = passwordList.stream().map(String.class::cast).findFirst().orElse(\"\");\n");

		if (project.isJakartaEEApplication())
			b.append("final var response = (HttpServletResponse) context.get(MessageContext.SERVLET_RESPONSE);\n");

		b.append("final var request = (HttpServletRequest) context.get(MessageContext.SERVLET_REQUEST);\n\n");
		b.append("userName = userList.stream().map(String.class::cast).findFirst().orElse(\"\");\n\n");

		b.append("if(request.getUserPrincipal() == null)\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Login user '{}'", "userName");

		b.append("\n");

		if (project.isJakartaEEApplication()) {
			b.append("final var credential = new UsernamePasswordCredential(userName, new Password(password));\n");
			b.append("final AuthenticationStatus status = securityContext.authenticate");
			b.append("(request, response, AuthenticationParameters.withParams().credential(credential));\n\n");

			LoggingGenerator.addDebugLog(b, "Authentication status: {}", "status");

			b.append("\n");
			b.append("if(!status.equals(AuthenticationStatus.SEND_CONTINUE) && !status.equals(AuthenticationStatus.SUCCESS))\n");
			b.append("return false;\n");
		}
		else
			b.append("request.login(userName, password);\n");

		b.append("}\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while performing login of user '{}'!", "e", "userName");

		b.append("\n");
		b.append("return false;\n");
		b.append("}\n\n");
		b.append("return true;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.Handler#handleFault(jakarta.xml.ws.handler.MessageContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public boolean handleFault(final SOAPMessageContext context)\n");
		b.append("{\n");
		b.append("return true;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.Handler#close(jakarta.xml.ws.handler.MessageContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void close(final MessageContext context)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.soap.SOAPHandler#getHeaders()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Set<QName> getHeaders()\n");
		b.append("{\n");
		b.append("return Collections.emptySet();\n");
		b.append("}\n\n");
		b.append("}\n");

		final var authHandler = new JavaFile(project, artifactType, AUTH_HANDLER, module.getNamespace().toString());
		authHandler.setComment("Handler to authenticate the user by using the application's login mechanism");
		authHandler.setContent(b.toString());

		return authHandler;
	}

	/**
	 * Create the logging handler
	 * @return the generated source file
	 */
	private JavaFile createLoggingHandler() {
		final var b = new StringBuilder();
		b.append("import java.lang.invoke.*;\n");
		b.append("import java.util.*;\n");
		b.append("import javax.xml.namespace.*;\n");
		b.append("import org.slf4j.*;\n");
		b.append("import jakarta.xml.soap.*;\n");
		b.append("import jakarta.xml.ws.handler.*;\n");
		b.append("import jakarta.xml.ws.handler.soap.*;\n\n");
		b.append("public class " + LOGGING_HANDLER + " implements SOAPHandler<SOAPMessageContext>\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.Handler#handleMessage(jakarta.xml.ws.handler.MessageContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public boolean handleMessage(final SOAPMessageContext context)\n");
		b.append("{\n");
		b.append("if(logger.isDebugEnabled())\n");
		b.append("{\n");
		b.append("final var outbound = (Boolean) context.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY);\n");
		b.append("final var operation = (QName) context.get(MessageContext.WSDL_OPERATION);\n");
		b.append("final var service = (QName) context.get(MessageContext.WSDL_SERVICE);\n\n");
		b.append("if(Boolean.TRUE.equals(outbound))\n");

		LoggingGenerator.addDebugLog(b, "Finish {}::{}", "service.getLocalPart()", "operation.getLocalPart()");

		b.append("else\n");

		LoggingGenerator.addDebugLog(b, "Process {}::{}", "service.getLocalPart()", "operation.getLocalPart()");

		b.append("}\n\n");
		b.append("return true;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.Handler#handleFault(jakarta.xml.ws.handler.MessageContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public boolean handleFault(final SOAPMessageContext context)\n");
		b.append("{\n");
		b.append("final SOAPMessage msg = context.getMessage();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final SOAPFault fault = msg.getSOAPBody().getFault();\n\n");
		b.append("if(fault != null)\n");
		b.append("{\n");
		b.append("final var service = (QName) context.get(MessageContext.WSDL_SERVICE);\n");
		b.append("final var operation = (QName) context.get(MessageContext.WSDL_OPERATION);\n\n");

		LoggingGenerator.addWarningLog(b, "Error while processing {}:{}! Message: {}", "service.getLocalPart()",
				"operation.getLocalPart()", "fault.getFaultString()");

		b.append("}\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("final var service = (QName) context.get(MessageContext.WSDL_SERVICE);\n");
		b.append("final var operation = (QName) context.get(MessageContext.WSDL_OPERATION);\n\n");

		LoggingGenerator.addErrorLog(b, "Error while extracting SOAP body for {}::{}", "e", "service.getLocalPart()",
				"operation.getLocalPart()");

		b.append("}\n\n");
		b.append("return true;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.Handler#close(jakarta.xml.ws.handler.MessageContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void close(final MessageContext context)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.xml.ws.handler.soap.SOAPHandler#getHeaders()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Set<QName> getHeaders()\n");
		b.append("{\n");
		b.append("return Collections.emptySet();\n");
		b.append("}\n\n");
		b.append("}\n");

		final var loggingHandler = new JavaFile(project, artifactType, LOGGING_HANDLER, module.getNamespace().toString());
		loggingHandler.setComment("Handler that is responsible for logging SOAP requests");
		loggingHandler.setContent(b.toString());

		return loggingHandler;
	}

	/**
	 * Create the handler chain configuration file
	 * @return the generated file
	 */
	public WorkspaceFile createHandlerChain() {
		final var b = new StringBuilder();
		final String packageName = module.getNamespace().toString();
		final var path = project.getResourceFolder() + "/" + HANDLER_CHAIN;

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\" standalone=\"yes\"?>\n");
		b.append("<jakartaee:handler-chains xmlns:jakartaee=\"https://jakarta.ee/xml/ns/jakartaee\" ");
		b.append("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">\n");
		b.append("\t<jakartaee:handler-chain>\n");

		if (module.isAddSecurityHandler() && project.getApplicationLogOnDTO() != null) {
			b.append("\t\t<jakartaee:handler>\n");
			b.append("\t\t\t<jakartaee:handler-name>" + AUTH_HANDLER + "</jakartaee:handler-name>\n");
			b.append("\t\t\t<jakartaee:handler-class>" + packageName + "." + AUTH_HANDLER + "</jakartaee:handler-class>\n");
			b.append("\t\t</jakartaee:handler>\n");
		}

		b.append("\t\t<jakartaee:handler>\n");
		b.append("\t\t\t<jakartaee:handler-name>" + LOGGING_HANDLER + "</jakartaee:handler-name>\n");
		b.append("\t\t\t<jakartaee:handler-class>" + packageName + "." + LOGGING_HANDLER + "</jakartaee:handler-class>\n");
		b.append("\t\t</jakartaee:handler>\n");
		b.append("\t</jakartaee:handler-chain>\n");
		b.append("</jakartaee:handler-chains>\n");

		return new WorkspaceFile(project, artifactType, path, b.toString());
	}

	/**
	 * Create the soap_config.properties file for the client artifact
	 * @return the generated content
	 */
	private String createClientConfig() {
		final var b = new StringBuilder();
		final String context;

		if (project.isSpringBootApplication())
			context = project.getCode() + "/ws";
		else
			context = project.getCode();

		b.append("service_url=http://localhost:8080/" + context + "/\n");

		return b.toString();
	}

}
