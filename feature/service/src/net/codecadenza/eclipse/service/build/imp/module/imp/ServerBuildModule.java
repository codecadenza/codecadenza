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
package net.codecadenza.eclipse.service.build.imp.module.imp;

import java.util.List;
import net.codecadenza.eclipse.generator.basic.project.ProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.ServerProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.generator.basic.technology.spring.EclipseLinkConfigurationGenerator;
import net.codecadenza.eclipse.generator.basic.technology.spring.SpringBootApplicationGenerator;
import net.codecadenza.eclipse.generator.security.DisabledHttpAuthMechanismGenerator;
import net.codecadenza.eclipse.generator.security.SecurityConfigurationGenerator;
import net.codecadenza.eclipse.generator.service.LoggingConfigurationGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Build module for server artifacts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServerBuildModule extends AbstractBuildModule {
	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public ServerBuildModule(BuildArtifact buildArtifact) {
		super(buildArtifact);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getFolders()
	 */
	@Override
	public List<String> getFolders() {
		final List<String> folderNames = super.getFolders();
		folderNames.add(project.getWebInfFolder());
		folderNames.add(project.getConfigFolder(BuildArtifactType.SERVER));

		if (project.isJakartaEEApplication())
			folderNames.add(project.getSchemaFolder());

		return folderNames;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> getConfigurationFiles() {
		final List<WorkspaceFile> configFiles = super.getConfigurationFiles();
		final IServerProjectFilesGenerator serverFileGenerator = ServerProjectFilesGeneratorFactory.getGenerator(project);
		final ProjectFilesGenerator projectFileGenerator = new ProjectFilesGenerator(project);

		var path = project.getConfigFolder(BuildArtifactType.SERVER) + "/application.properties";
		String content = projectFileGenerator.createPropertiesFile(BuildArtifactType.SERVER);
		final var propertiesFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

		configFiles.add(propertiesFile);

		if (project.isJakartaEEApplication()) {
			path = project.getWebInfFolder() + "/web.xml";
			content = serverFileGenerator.createWebXML();
			final var webXmlFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			configFiles.add(webXmlFile);

			path = project.getWebInfFolder() + "/ejb-jar.xml";
			content = serverFileGenerator.createEJBJarXML();
			final var ejbJarFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			configFiles.add(ejbJarFile);

			path = project.getWebInfFolder() + "/" + BEANS_XML;
			content = projectFileGenerator.createBeansXML();
			final var beansXmlFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			configFiles.add(beansXmlFile);

			path = project.getWebInfFolder() + "/" + serverFileGenerator.getVendorWebXMLName();
			content = serverFileGenerator.createVendorWebXML();
			final var vendorWebXmlFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			configFiles.add(vendorWebXmlFile);
		}

		if (project.getDataSourceFileName() != null) {
			path = project.getWebInfFolder() + "/" + project.getDataSourceFileName();
			content = serverFileGenerator.createDataSource();
			final var dataSourceFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			configFiles.add(dataSourceFile);
		}

		if (project.isDeployedOnWildfly()) {
			path = project.getWebInfFolder() + "/jboss-deployment-structure.xml";
			content = serverFileGenerator.createDeploymentStructureXML();
			final var deploymentStructureXMLFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			configFiles.add(deploymentStructureXMLFile);
		}

		// Create the logging configuration file
		path = project.getResourceFolder() + "/log4j2.xml";
		content = new LoggingConfigurationGenerator(buildArtifact).createContent();
		final var loggingConfFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

		configFiles.add(loggingConfFile);

		return configFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getPackages()
	 */
	@Override
	public List<String> getPackages() {
		final List<String> packages = super.getPackages();
		packages.add(project.getRootNamespace().toString());

		return packages;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#createInitialSourceFiles()
	 */
	@Override
	public void createInitialSourceFiles() throws Exception {
		if (project.isSpringBootApplication()) {
			new SpringBootApplicationGenerator(project).createSourceFile();
			new SecurityConfigurationGenerator(project).createSourceFile();

			if (project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK)
				new EclipseLinkConfigurationGenerator(project).createSourceFile();
		}
		else
			new DisabledHttpAuthMechanismGenerator(project).createSourceFile();
	}

}
