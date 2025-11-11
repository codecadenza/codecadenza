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

import static net.codecadenza.eclipse.shared.Constants.BASE_REST_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.REMOTE_OPERATION_EXCEPTION_NAME;
import static net.codecadenza.eclipse.shared.Constants.REST_APPLICATION_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.generator.common.LoggingGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for basic source and configuration files necessary for supporting REST
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RESTIntegrationProjectFilesGenerator extends AbstractIntegrationProjectFilesGenerator {
	private static final String FILE_SERVICE_BEAN = "FileRESTServiceBean";
	private static final String AUTH_FILTER = "AuthenticationFilter";

	/**
	 * Constructor
	 * @param module
	 * @param artifactType
	 */
	public RESTIntegrationProjectFilesGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		super(module, artifactType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final var sourceFiles = new ArrayList<JavaFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_REST) {
			final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;

			final var baseClientClass = new JavaFile(project, artifactType, BASE_REST_CLIENT_CLASS_NAME, packageName);
			baseClientClass.setComment("Abstract base class for all REST clients");
			baseClientClass.setContent(createBaseClient());

			sourceFiles.add(baseClientClass);

			final var fileClientClass = new JavaFile(project, artifactType, module.getFileServiceClientName(), packageName);
			fileClientClass.setComment("Client for file operations via REST");
			fileClientClass.setContent(createFileServiceClient());

			sourceFiles.add(fileClientClass);

			if (module.isAddProducers())
				sourceFiles.add(createFileServiceProducer());
		}
		else if (artifactType == BuildArtifactType.INTEGRATION_IMP_REST) {
			final String packageName = module.getNamespace().toString();
			final JavaFile applicationClass = createRESTApplication();

			sourceFiles.add(applicationClass);

			final var fileServiceBean = new JavaFile(project, artifactType, FILE_SERVICE_BEAN, packageName + SUB_PACKAGE_BEAN);
			fileServiceBean.setComment("REST file resource");
			fileServiceBean.setContent(createFileServiceBean());

			sourceFiles.add(fileServiceBean);
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

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_REST) {
			final var path = project.getConfigFolder(artifactType) + "/rest_config.properties";

			fileList.add(new WorkspaceFile(project, artifactType, path, createClientConfig()));
		}

		return fileList;
	}

	/**
	 * Create the basic REST client
	 * @return the generated content
	 */
	private String createBaseClient() {
		final var b = new StringBuilder();
		b.append("import java.io.*;\n");
		b.append("import java.text.*;\n");
		b.append("import jakarta.annotation.*;\n");
		b.append("import jakarta.ws.rs.client.*;\n");
		b.append("import jakarta.ws.rs.core.*;\n");
		b.append("import jakarta.ws.rs.ext.*;\n");
		b.append("import net.codecadenza.runtime.property.*;\n");
		b.append("import com.fasterxml.jackson.databind.*;\n");
		b.append("import com.fasterxml.jackson.datatype.jsr310.*;\n");

		if (project.isJakartaEEApplication())
			b.append("import static net.codecadenza.runtime.authentication.BasicAuthentication.*;\n");
		else
			b.append("import static net.codecadenza.runtime.crypto.HashGenerator.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public abstract class " + BASE_REST_CLIENT_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String PROP_FILE_NAME = \"config/rest_config.properties\";\n");
		b.append("private static final String PROP_NAME_SERVICE_URL = \"service_url\";\n");
		b.append("private static final String SERVICE_URL = ");
		b.append("new PropertyService(PROP_FILE_NAME).getStringProperty(PROP_NAME_SERVICE_URL);\n\n");
		b.append("protected final Client client;\n");
		b.append("protected final WebTarget webTarget;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("protected " + BASE_REST_CLIENT_CLASS_NAME + "()\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("protected " + BASE_REST_CLIENT_CLASS_NAME + "(String userName, String password)\n");
		b.append("{\n");
		b.append("client = ClientBuilder.newClient();\n");
		b.append("client.register(new JacksonConfig());\n");
		b.append("client.register(new ClientLogFilter());\n\n");
		b.append("if(userName != null && password != null)\n");
		b.append("client.register(new ClientRESTAuthFilter(userName, password));\n\n");
		b.append("webTarget = client.target(SERVICE_URL);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Release internal resources (e.g. socket connections)\n");
		b.append(" */\n");
		b.append("@PreDestroy\n");
		b.append("public void close()\n");
		b.append("{\n");
		b.append("if(client != null)\n");
		b.append("client.close();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Configuration of the {@link ObjectMapper} for the conversion of date fields\n");
		b.append(" */\n");
		b.append("private class JacksonConfig implements ContextResolver<ObjectMapper>\n");
		b.append("{\n");
		b.append("private static final String DATE_FORMAT = \"yyyy-MM-dd'T'HH:mm:ss'Z[UTC]'\";\n\n");
		b.append("private final ObjectMapper objectMapper;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public JacksonConfig()\n");
		b.append("{\n");
		b.append("this.objectMapper = new ObjectMapper();\n");
		b.append("this.objectMapper.registerModule(new JavaTimeModule());");
		b.append("this.objectMapper.setDateFormat(new SimpleDateFormat(DATE_FORMAT));\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.ws.rs.ext.ContextResolver#getContext(java.lang.Class)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public ObjectMapper getContext(Class<?> objectType)\n");
		b.append("{\n");
		b.append("return objectMapper;\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Logging filter\n");
		b.append(" */\n");
		b.append("private class ClientLogFilter implements ClientRequestFilter, ClientResponseFilter\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.ws.rs.client.ClientRequestFilter#filter(jakarta.ws.rs.client.ClientRequestContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void filter(final ClientRequestContext requestContext) throws IOException\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Send request to '{}' ({}) ", "requestContext.getUri()", "requestContext.getMethod()");

		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.ws.rs.client.ClientResponseFilter#");
		b.append("filter(jakarta.ws.rs.client.ClientRequestContext, jakarta.ws.rs.client.ClientResponseContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void filter(ClientRequestContext requestContext, ");
		b.append("ClientResponseContext responseContext) throws IOException\n");
		b.append("{\n");

		final var logMsg = "Received status code '{}' from '{}' ({})";

		LoggingGenerator.addDebugLog(b, logMsg, "responseContext.getStatus()", "requestContext.getUri()",
				"requestContext.getMethod()");

		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Filter for adding user credentials to request header\n");
		b.append(" */\n");
		b.append("private class ClientRESTAuthFilter implements ClientRequestFilter\n");
		b.append("{\n");
		b.append("private final String userName;\n");
		b.append("private final String password;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("public ClientRESTAuthFilter(final String userName, final String password)\n");
		b.append("{\n");
		b.append("this.userName = userName;\n");
		b.append("this.password = password;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.ws.rs.client.ClientRequestFilter#filter(jakarta.ws.rs.client.ClientRequestContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void filter(final ClientRequestContext requestContext) throws IOException\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Add authorization information for user '{}'", "userName");

		b.append("\n");
		b.append("final MultivaluedMap<String, Object> headers = requestContext.getHeaders();\n");

		if (project.isSpringBootApplication()) {
			b.append("headers.add(\"username\", userName);\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("headers.add(\"password\", encryptSHA256(password));\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			LoggingGenerator.addErrorLog(b, "Error while encrypting password!", "e");

			b.append("}\n");
		}
		else
			b.append("headers.add(HTTP_HEADER_AUTHORIZATION, createAuthentication(userName, password));\n");

		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the REST application
	 * @return the generated content
	 */
	public JavaFile createRESTApplication() {
		final var b = new StringBuilder();
		final String packageName = module.getNamespace().toString();
		final var comment = "Configuration for all JAX-RS end-points of this application";

		if (project.isSpringBootApplication()) {
			b.append("import " + module.getNamespace().toString() + SUB_PACKAGE_BEAN + ".*;\n");
			b.append("import jakarta.ws.rs.ApplicationPath;\n");
			b.append("import net.codecadenza.runtime.rest.filters.*;\n");
			b.append("import net.codecadenza.runtime.rest.mappers.*;\n");
			b.append("import org.glassfish.jersey.server.ResourceConfig;\n");
			b.append("import org.springframework.stereotype.Component;\n\n");
			b.append("@Component\n");
			b.append("@ApplicationPath(\"rest\")\n");
			b.append("public class " + REST_APPLICATION_CLASS_NAME + " extends ResourceConfig\n");
			b.append("{\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + REST_APPLICATION_CLASS_NAME + "()\n");
			b.append("{\n");

			if (project.getApplicationLogOnDTO() != null && module.isAddSecurityHandler())
				b.append("register(" + AUTH_FILTER + ".class);\n");

			if (project.hasAngularClient())
				b.append("register(CORSFilter.class);\n");

			b.append("register(LoggingFilter.class);\n");
			b.append("register(StandardExceptionMapper.class);\n");
			b.append("register(WebApplicationExceptionMapper.class);\n");
			b.append("register(" + FILE_SERVICE_BEAN + ".class);\n");

			module.getNamespace().getJavaTypes().forEach(t -> b.append("register(" + t.getName() + ".class);\n"));

			b.append("}\n\n");
		}
		else {
			b.append("import jakarta.ws.rs.*;\n");
			b.append("import jakarta.ws.rs.core.*;\n\n");
			b.append("@ApplicationPath(\"rest\")\n");
			b.append("public class " + REST_APPLICATION_CLASS_NAME + " extends Application\n");
			b.append("{\n");
		}

		b.append("}\n");

		final var applicationClass = new JavaFile(project, artifactType, REST_APPLICATION_CLASS_NAME, packageName);
		applicationClass.setComment(comment);
		applicationClass.setContent(b.toString());

		return applicationClass;
	}

	/**
	 * Create the file service bean
	 * @return the generated content
	 */
	private String createFileServiceBean() {
		final var b = new StringBuilder();
		b.append("import java.io.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import jakarta.ws.rs.*;\n");
		b.append("import jakarta.ws.rs.core.*;\n");
		b.append("import jakarta.ws.rs.Path;\n");

		if (project.isJakartaEEApplication())
			b.append("import jakarta.enterprise.context.*;\n");
		else
			b.append("import org.springframework.stereotype.Service;");

		b.append("import net.codecadenza.runtime.property.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("@Path(\"/file\")\n");

		if (project.isJakartaEEApplication())
			b.append("@RequestScoped\n");
		else
			b.append("@Service\n");

		b.append("public class " + FILE_SERVICE_BEAN + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String HEADER_PARAM_FILE_NAME = \"filename\";\n");
		b.append("private static final String HEADER_PARAM_FILE_PATH = \"path\";\n");
		b.append("private static final String PATH_EXCHANGE_FOLDER = ");
		b.append("new PropertyService().getStringProperty(PropertyService.PROP_EXCHANGE_FOLDER);\n\n");
		b.append("/**\n");
		b.append(" * Upload file\n");
		b.append(" * @param inputStream\n");
		b.append(" * @param fileName\n");
		b.append(" * @return a response object\n");
		b.append(" */\n");
		b.append("@POST\n");
		b.append("@Consumes(MediaType.APPLICATION_OCTET_STREAM)\n");
		b.append("public Response uploadFile(InputStream inputStream, ");
		b.append("final @HeaderParam(HEADER_PARAM_FILE_NAME) String fileName) throws IOException\n");
		b.append("{\n");
		b.append("final File file;\n\n");

		LoggingGenerator.addDebugLog(b, "Upload file '{}'", "fileName");

		b.append("\n");
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
		b.append("Files.copy(inputStream, file.toPath(), StandardCopyOption.REPLACE_EXISTING);\n\n");
		b.append("return Response.ok(new GenericEntity<>(file.getAbsolutePath(), String.class)).build();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Download a file that is defined by the fully qualified path on the server. Attention: In production ");
		b.append("environments the access to either system or inappropriate directories must be prohibited!\n");
		b.append(" * @param path\n");
		b.append(" * @return a response object\n");
		b.append(" */\n");
		b.append("@GET\n");
		b.append("@Produces(MediaType.APPLICATION_OCTET_STREAM)\n");
		b.append("public Response downloadFile(final @HeaderParam(HEADER_PARAM_FILE_PATH) String path)\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Download file from '{}'", "path");

		b.append("\n");
		b.append("final var streamingOutput = new StreamingOutput()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.ws.rs.core.StreamingOutput#write(java.io.OutputStream)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void write(OutputStream output) throws IOException\n");
		b.append("{\n");
		b.append("final var outputFile = new File(path);\n\n");
		b.append("Files.copy(outputFile.toPath(), output);\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("return Response.ok(streamingOutput, MediaType.APPLICATION_OCTET_STREAM).build();\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the file service REST client
	 * @return the generated content
	 */
	private String createFileServiceClient() {
		final var b = new StringBuilder();
		b.append("import java.io.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import jakarta.ws.rs.client.*;\n");
		b.append("import jakarta.ws.rs.core.*;\n");
		b.append("import jakarta.ws.rs.core.Response.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n");
		b.append("import net.codecadenza.runtime.transport.file.*;\n");

		if (project.isJakartaEEApplication())
			b.append("import jakarta.enterprise.inject.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");

		if (project.isJakartaEEApplication())
			b.append("@Alternative\n");

		b.append("public class " + module.getFileServiceClientName() + " extends " + BASE_REST_CLIENT_CLASS_NAME);
		b.append(" implements " + module.getFileServiceName() + ", Serializable\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final long serialVersionUID = 1L;\n");
		b.append("private static final String HEADER_PARAM_FILE_PATH = \"path\";\n");
		b.append("private static final String HEADER_PARAM_FILE_NAME = \"filename\";\n");
		b.append("private static final String RESOURCE_PATH = \"file\";\n\n");
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
		b.append("/**\n");
		b.append(" * @return the service instance\n");
		b.append(" */\n");
		b.append("public " + module.getFileServiceName() + " getService()\n");
		b.append("{\n");
		b.append("return this;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#downloadFile(java.lang.String, java.io.File)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void downloadFile(String pathOnServer, File targetFile) throws IOException\n");
		b.append("{\n");
		b.append("Response response = null;\n\n");
		b.append("if(pathOnServer == null || pathOnServer.isEmpty())\n");
		b.append("throw new IllegalArgumentException(\"No download file selected!\");\n\n");
		b.append("if(targetFile == null)\n");
		b.append("throw new IllegalArgumentException(\"No target file selected!\");\n\n");
		b.append("if(targetFile.isDirectory())\n");
		b.append("throw new IllegalArgumentException(\"Target file must not be a directory!\");\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("response = webTarget.path(RESOURCE_PATH).request().header(HEADER_PARAM_FILE_PATH, pathOnServer).get();\n");
		b.append("final InputStream inputStream = response.readEntity(InputStream.class);\n\n");
		b.append("Files.copy(inputStream, targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("if(response != null)\n");
		b.append("response.close();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#uploadFile(java.io.File)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String uploadFile(File file) throws FileNotFoundException\n");
		b.append("{\n");
		b.append("final var fin = new FileInputStream(file);\n");
		b.append("Response response = null;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("response = webTarget.path(RESOURCE_PATH).request().header(HEADER_PARAM_FILE_NAME, file.getName())");
		b.append(".post(Entity.entity(fin, MediaType.APPLICATION_OCTET_STREAM));\n\n");
		b.append("if(!response.getStatusInfo().equals(Status.OK))\n");
		b.append("throw new " + REMOTE_OPERATION_EXCEPTION_NAME + "(response.getStatusInfo().getReasonPhrase());\n\n");
		b.append("return response.readEntity(String.class);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("fin.close();\n");
		b.append("}\n");
		b.append("catch (final IOException e)\n");
		b.append("{\n");

		LoggingGenerator.addWarningLog(b, "Could not close file input stream!", "e");

		b.append("}\n\n");
		b.append("if(response != null)\n");
		b.append("response.close();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the authorization filter
	 * @return the generated Java file
	 */
	public JavaFile createAuthFilter() {
		final var b = new StringBuilder();
		final String packageName = module.getNamespace().toString() + SUB_PACKAGE_BEAN;

		b.append("import java.io.*;\n");
		b.append("import jakarta.annotation.*;\n");
		b.append("import jakarta.servlet.http.*;\n");
		b.append("import jakarta.ws.rs.*;\n");
		b.append("import jakarta.ws.rs.container.*;\n");
		b.append("import jakarta.ws.rs.core.*;\n");
		b.append("import jakarta.ws.rs.core.Response.Status;\n");
		b.append("import jakarta.ws.rs.ext.Provider;\n");

		if (project.isJakartaEEApplication()) {
			b.append("import jakarta.inject.*;\n");
			b.append("import jakarta.security.enterprise.*;\n");
			b.append("import jakarta.security.enterprise.SecurityContext;\n");
			b.append("import jakarta.security.enterprise.credential.*;\n");
			b.append("import jakarta.security.enterprise.authentication.mechanism.http.*;\n");
			b.append("import static net.codecadenza.runtime.authentication.BasicAuthentication.*;\n");
		}

		b.append("\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("@Provider\n");
		b.append("@Priority(Priorities.AUTHORIZATION)\n");
		b.append("public class " + AUTH_FILTER + " implements ContainerRequestFilter\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private @Context HttpServletRequest request;\n");

		if (project.isJakartaEEApplication()) {
			b.append("private @Context HttpServletResponse response;\n");
			b.append("private SecurityContext securityContext;\n\n");
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append("public " + AUTH_FILTER + "()\n");
			b.append("{\n");
			b.append("this.securityContext = null;\n");
			b.append("}\n\n");
			b.append("/**\n");
			b.append(" * Constructor for injecting the security context\n");
			b.append(" * @param securityContext\n");
			b.append(" */\n");
			b.append("@Inject\n");
			b.append("public " + AUTH_FILTER + "(SecurityContext securityContext)\n");
			b.append("{\n");
			b.append("this.securityContext = securityContext;\n");
			b.append("}\n");
		}

		b.append("\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.ws.rs.container.ContainerRequestFilter#filter(jakarta.ws.rs.container.ContainerRequestContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void filter(final ContainerRequestContext requestContext) throws IOException\n");
		b.append("{\n");
		b.append("var userName = \"\";\n\n");

		if (project.hasAngularClient()) {
			b.append("// Skip the authentication check for requests with HTTP method OPTIONS!\n");
			b.append("if(requestContext.getMethod().equals(HttpMethod.OPTIONS))\n");
			b.append("return;\n\n");
		}

		b.append("try\n");
		b.append("{\n");

		if (project.isJakartaEEApplication()) {
			b.append("final String password = getPassword(requestContext.getHeaderString(HTTP_HEADER_AUTHORIZATION));\n");
			b.append("userName = getUserName(requestContext.getHeaderString(HTTP_HEADER_AUTHORIZATION));\n\n");
		}
		else {
			b.append("final String password = requestContext.getHeaderString(\"password\");\n");
			b.append("userName = requestContext.getHeaderString(\"username\");\n\n");
		}

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
			b.append("throw new IllegalStateException(\"Authentication for user '\" + userName + \"' failed!\");\n");
		}
		else
			b.append("request.login(userName, password);\n");

		b.append("}\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new NotAuthorizedException(Status.UNAUTHORIZED.getReasonPhrase(),");
		b.append("Response.status(Status.UNAUTHORIZED).build(), e);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		final var authFilter = new JavaFile(project, artifactType, AUTH_FILTER, packageName);
		authFilter.setComment("Handler to authenticate the user by using the application's login mechanism");
		authFilter.setContent(b.toString());

		return authFilter;
	}

	/**
	 * Create the rest_config.properties file for the client artifact
	 * @return the generated content
	 */
	private String createClientConfig() {
		final var b = new StringBuilder();
		b.append("service_url=http://localhost:8080/" + project.getCode() + "/rest/\n");

		return b.toString();
	}

}
