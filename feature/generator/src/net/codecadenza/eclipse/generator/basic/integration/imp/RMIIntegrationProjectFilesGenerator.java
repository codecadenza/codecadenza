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

import static net.codecadenza.eclipse.shared.Constants.BASE_RMI_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.REMOTE_OPERATION_EXCEPTION_NAME;
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
 * Generator for basic source and configuration files necessary for supporting RMI
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RMIIntegrationProjectFilesGenerator extends AbstractIntegrationProjectFilesGenerator {
	private static final String FILE_SERVICE_BEAN = "FileRMIServiceBean";

	/**
	 * Constructor
	 * @param module
	 * @param artifactType
	 */
	public RMIIntegrationProjectFilesGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		super(module, artifactType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.integration.IIntegrationProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final var sourceFiles = new ArrayList<JavaFile>();

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_RMI) {
			final String packageName = module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT;

			final var baseClientClass = new JavaFile(project, artifactType, BASE_RMI_CLIENT_CLASS_NAME, packageName);
			baseClientClass.setComment("Abstract base class for all RMI clients");
			baseClientClass.setContent(createBaseClient());

			sourceFiles.add(baseClientClass);

			final var fileClientClass = new JavaFile(project, artifactType, module.getFileServiceClientName(), packageName);
			fileClientClass.setComment("Client for file operations via RMI");
			fileClientClass.setContent(createFileServiceClient());

			sourceFiles.add(fileClientClass);

			if (module.isAddProducers())
				sourceFiles.add(createFileServiceProducer());
		}
		else if (artifactType == BuildArtifactType.INTEGRATION_IMP_RMI) {
			final String packageName = module.getNamespace().toString() + SUB_PACKAGE_BEAN;

			final var fileServiceBean = new JavaFile(project, artifactType, FILE_SERVICE_BEAN, packageName);
			fileServiceBean.setComment("File service implementation");
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

		if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_RMI) {
			final var path = project.getConfigFolder(artifactType) + "/" + JNDI_PROPERTIES_FILE;

			fileList.add(new WorkspaceFile(project, artifactType, path, createJNDIProperties()));
		}

		return fileList;
	}

	/**
	 * Create the basic RMI client
	 * @return the generated content
	 */
	private String createBaseClient() {
		final var b = new StringBuilder();
		b.append("import java.util.*;\n");
		b.append("import jakarta.annotation.*;\n");
		b.append("import javax.naming.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public abstract class " + BASE_RMI_CLIENT_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("protected static final String NAMING_PREFIX = \"");

		if (project.isDeployedOnPayara())
			b.append("java:global/");

		b.append(project.getCode() + "/\";\n\n");
		b.append("private final String userName;\n");
		b.append("private final String password;\n");
		b.append("private InitialContext context;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("protected " + BASE_RMI_CLIENT_CLASS_NAME + "()\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append("protected " + BASE_RMI_CLIENT_CLASS_NAME + "(String userName, String password)\n");
		b.append("{\n");
		b.append("this.userName = userName;\n");
		b.append("this.password = password;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Create initial context\n");
		b.append(" * @return the created context\n");
		b.append(" * @throws NamingException\n");
		b.append(" */\n");
		b.append("protected InitialContext getInitialContext() throws NamingException\n");
		b.append("{\n");
		b.append("final var props = new Hashtable<>();\n\n");
		b.append("if(userName != null)\n");
		b.append("props.put(Context.SECURITY_PRINCIPAL, userName);\n\n");
		b.append("if(password != null)\n");
		b.append("props.put(Context.SECURITY_CREDENTIALS, password);\n\n");
		b.append("return new InitialContext(props);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param <T> the type of the service that should be returned\n");
		b.append(" * @param cl\n");
		b.append(" * @return the service that is bound to the given interface\n");
		b.append(" * @throws NamingException\n");
		b.append(" */\n");
		b.append("@SuppressWarnings(\"unchecked\")\n");
		b.append("public synchronized <T> T getService(Class<T> cl) throws NamingException\n");
		b.append("{\n");
		b.append("final String jndiName = getJNDIServiceName();\n\n");
		b.append("if(context == null)\n");
		b.append("context = getInitialContext();\n\n");

		LoggingGenerator.addDebugLog(b, "Lookup for service '{}'", "jndiName");

		b.append("\n");
		b.append("return (T) context.lookup(jndiName);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the JNDI name of the service\n");
		b.append(" */\n");
		b.append("public abstract String getJNDIServiceName();\n\n");
		b.append("/**\n");
		b.append(" * Release resources\n");
		b.append(" */\n");
		b.append("@PreDestroy\n");
		b.append("public void close()\n");
		b.append("{\n");
		b.append("if(context != null)\n");
		b.append("try\n");
		b.append("{\n");
		b.append("context.close();\n");
		b.append("}\n");
		b.append("catch (final NamingException e)\n");
		b.append("{\n");

		LoggingGenerator.addWarningLog(b, "Could not close the initial context!", "e");

		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the file service bean
	 * @return the generated content
	 */
	private String createFileServiceBean() {
		final var b = new StringBuilder();
		b.append("import java.io.*;\n");
		b.append("import java.nio.file.*;\n");
		b.append("import jakarta.ejb.*;\n");
		b.append("import net.codecadenza.runtime.property.*;\n");
		b.append("import net.codecadenza.runtime.transport.*;\n");
		b.append("import net.codecadenza.runtime.transport.file.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("@Stateless\n");
		b.append("@Remote(" + module.getFileServiceName() + ".class)\n");
		b.append("public class " + FILE_SERVICE_BEAN + " implements " + module.getFileServiceName() + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String PATH_EXCHANGE_FOLDER = ");
		b.append("new PropertyService().getStringProperty(PropertyService.PROP_EXCHANGE_FOLDER);\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#downloadFile(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public byte[] downloadFile(String pathOnServer)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var outputFile = new File(pathOnServer);\n");
		b.append("final var baos = new ByteArrayOutputStream();\n\n");
		b.append("Files.copy(outputFile.toPath(), baos);\n\n");
		b.append("return baos.toByteArray();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while downloading file '{}'!", "e", "pathOnServer");

		b.append("\n");
		b.append("throw new " + REMOTE_OPERATION_EXCEPTION_NAME + "(e.getMessage());\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.transport.file." + module.getFileServiceName());
		b.append("#uploadFile(java.lang.String, byte[])\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String uploadFile(String fileName, byte[] data)\n");
		b.append("{\n");
		b.append("final File file;\n\n");
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

		LoggingGenerator.addErrorLog(b, "Error while saving file '{}'!", "e", "fileName");

		b.append("\n");
		b.append("throw new " + REMOTE_OPERATION_EXCEPTION_NAME + "(e.getMessage());\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the file service RMI client
	 * @return the generated content
	 */
	private String createFileServiceClient() {
		final var b = new StringBuilder();
		b.append("import javax.naming.NamingException;\n");
		b.append("import net.codecadenza.runtime.transport.file.*;\n\n");
		b.append("public class " + module.getFileServiceClientName() + " extends " + BASE_RMI_CLIENT_CLASS_NAME + "\n");
		b.append("{\n");
		b.append("private static final String EJB_NAME = \"" + FILE_SERVICE_BEAN + "!\";\n\n");
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
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + module.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT + ".");
		b.append(BASE_RMI_CLIENT_CLASS_NAME + "#getJNDIServiceName()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getJNDIServiceName()\n");
		b.append("{\n");
		b.append("return NAMING_PREFIX + EJB_NAME + " + module.getFileServiceName() + ".class.getName();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the service\n");
		b.append(" * @throws NamingException\n");
		b.append(" */\n");
		b.append("public " + module.getFileServiceName() + " getService() throws NamingException\n");
		b.append("{\n");
		b.append("return super.getService(" + module.getFileServiceName() + ".class);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

}
