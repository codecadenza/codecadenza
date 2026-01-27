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

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_COMMON_MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.ANGULAR_DOMAIN_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.ANGULAR_ENVIRONMENTS_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.ANGULAR_PAGE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.APPLICATION_MODEL_NAME;
import static net.codecadenza.eclipse.shared.Constants.CONTRIBUTION_XML;
import static net.codecadenza.eclipse.shared.Constants.CONT_PATH_JUNIT;
import static net.codecadenza.eclipse.shared.Constants.CONT_PATH_PDE;
import static net.codecadenza.eclipse.shared.Constants.JAVA_NATURE_ID;
import static net.codecadenza.eclipse.shared.Constants.LIB_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.META_INF_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.OSGI_INF_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_CONVERTER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.PDE_NATURE_ID;
import static net.codecadenza.eclipse.shared.Constants.PLUGIN_XML;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;
import static net.codecadenza.eclipse.shared.Constants.TRANSLATION_FILE_NAME;
import static net.codecadenza.eclipse.shared.Constants.UI_APP_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_DIALOG_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_LOV_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_PANEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_TREE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_VIEW_FOLDER;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.generator.basic.client.ClientProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.generator.basic.client.imp.AngularClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.EclipseClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.JSFClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.VaadinClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.project.ProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.ServerProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.generator.basic.technology.spring.EclipseLinkConfigurationGenerator;
import net.codecadenza.eclipse.generator.basic.technology.spring.SpringBootApplicationGenerator;
import net.codecadenza.eclipse.generator.security.SecurityConfigurationGenerator;
import net.codecadenza.eclipse.generator.service.LoggingConfigurationGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Build module for GUI artifacts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUIBuildModule extends AbstractBuildModule {
	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public GUIBuildModule(BuildArtifact buildArtifact) {
		super(buildArtifact);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getFolders()
	 */
	@Override
	public List<String> getFolders() {
		final var folderNames = new ArrayList<String>();

		if (project.hasAngularClient()) {
			folderNames.add(ANGULAR_DOMAIN_FOLDER);
			folderNames.add(ANGULAR_PAGE_FOLDER);

			return folderNames;
		}

		folderNames.addAll(super.getFolders());
		folderNames.add(project.getConfigFolder(BuildArtifactType.GUI));

		if (project.hasJSFOrVaadinClient()) {
			if (project.isJakartaEEApplication())
				folderNames.add(project.getSchemaFolder());

			folderNames.add(project.getWebInfFolder());
		}

		if (project.hasJSFClient()) {
			folderNames.add(project.getWebAppFolder() + UI_APP_FOLDER);
			folderNames.add(project.getWebAppFolder() + UI_DIALOG_FOLDER);
			folderNames.add(project.getWebAppFolder() + UI_VIEW_FOLDER);
			folderNames.add(project.getWebAppFolder() + UI_LOV_FOLDER);
			folderNames.add(project.getWebAppFolder() + UI_TREE_FOLDER);
			folderNames.add(project.getWebAppFolder() + UI_PANEL_FOLDER);
		}
		else if (project.hasEclipseClient()) {
			folderNames.add(LIB_FOLDER);
			folderNames.add(META_INF_FOLDER);
		}

		if (project.hasRAPClient())
			folderNames.add(OSGI_INF_FOLDER);

		return folderNames;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getNatures()
	 */
	@Override
	public List<String> getNatures() {
		if (project.hasAngularClient())
			return Collections.emptyList();

		final var natures = new ArrayList<String>();

		if (project.hasEclipseClient()) {
			natures.add(JAVA_NATURE_ID);
			natures.add(PDE_NATURE_ID);
		}
		else
			natures.addAll(super.getNatures());

		return natures;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getClassPathEntries(java.lang.String)
	 */
	@Override
	public HashSet<IClasspathEntry> getClassPathEntries(String projectName) {
		if (project.hasAngularClient())
			return new HashSet<>();

		if (project.hasEclipseClient()) {
			final var classPathEntries = new HashSet<IClasspathEntry>();
			classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getSourceFolder())));
			classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getResourceFolder())));
			classPathEntries.add(JavaCore.newSourceEntry(new Path("/" + projectName + "/" + project.getTestSourceFolder())));
			classPathEntries.add(JavaCore.newContainerEntry(new Path(CONT_PATH_PDE)));
			classPathEntries.add(JavaCore.newContainerEntry(new Path(CONT_PATH_JUNIT)));

			project.getBuildConfiguration().stream().forEach(artifact -> {
				final BuildArtifactType artifactType = artifact.getType();

				if (artifactType == BuildArtifactType.CLIENT_INTERFACE || artifactType == BuildArtifactType.DTO
						|| artifactType == BuildArtifactType.SHARED)
					classPathEntries.add(JavaCore.newProjectEntry(new Path("/" + artifact.getName()), true));
			});

			return classPathEntries;
		}

		return super.getClassPathEntries(projectName);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getConfigurationFiles()
	 */
	@Override
	public List<WorkspaceFile> getConfigurationFiles() {
		final IServerProjectFilesGenerator serverFileGenerator = ServerProjectFilesGeneratorFactory.getGenerator(project);
		final ProjectFilesGenerator projectFileGenerator = new ProjectFilesGenerator(project);
		final List<WorkspaceFile> configFiles;
		String path;
		String content;

		if (project.hasAngularClient()) {
			final var angularGenerator = new AngularClientProjectFilesGenerator(project);

			path = ANGULAR_ENVIRONMENTS_FOLDER + "/environment.ts";
			content = angularGenerator.createEnvironmentFile(true);

			configFiles = new ArrayList<>();
			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

			path = ANGULAR_ENVIRONMENTS_FOLDER + "/environment.development.ts";
			content = angularGenerator.createEnvironmentFile(false);

			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

			path = ANGULAR_COMMON_MODEL_FOLDER + "/role.enum.ts";
			content = angularGenerator.createRoleEnum();

			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

			return configFiles;
		}

		configFiles = super.getConfigurationFiles();

		path = project.getConfigFolder(BuildArtifactType.GUI) + "/application.properties";
		content = projectFileGenerator.createPropertiesFile(BuildArtifactType.GUI);

		configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

		if (project.hasJSFOrVaadinClient()) {
			if (project.isJakartaEEApplication()) {
				path = project.getWebInfFolder() + "/" + serverFileGenerator.getVendorWebXMLName();
				content = serverFileGenerator.createVendorWebXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

				path = project.getWebInfFolder() + "/ejb-jar.xml";
				content = serverFileGenerator.createEJBJarXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
			}

			if (project.getDataSourceFileName() != null) {
				path = project.getWebInfFolder() + "/" + project.getDataSourceFileName();
				content = serverFileGenerator.createDataSource();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
			}

			if (project.isDeployedOnJBoss()) {
				path = project.getWebInfFolder() + "/jboss-deployment-structure.xml";
				content = serverFileGenerator.createDeploymentStructureXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
			}

			if (addBeansXML(BuildArtifactType.GUI)) {
				path = project.getWebInfFolder() + "/" + BEANS_XML;
				content = projectFileGenerator.createBeansXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
			}
		}

		if (project.hasJSFClient()) {
			final var jsfGenerator = new JSFClientProjectFilesGenerator(project);

			path = project.getWebInfFolder() + "/faces-config.xml";
			content = jsfGenerator.createFacesConfigXML();

			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

			if (project.isJakartaEEApplication()) {
				path = project.getWebInfFolder() + "/web.xml";
				content = jsfGenerator.createWebXMLFile();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
			}
		}
		else if (project.hasVaadinClient()) {
			final var vaadinGenerator = new VaadinClientProjectFilesGenerator(project);
			final String stylePath;

			if (project.isJakartaEEApplication()) {
				stylePath = project.getWebAppFolder() + "/styles.css";
				path = project.getWebInfFolder() + "/web.xml";
				content = vaadinGenerator.createWebXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
			}
			else
				stylePath = project.getMetaInfFolder() + "/resources/styles.css";

			content = vaadinGenerator.createStylesheet();

			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, stylePath, content));
		}
		else if (project.hasEclipseClient()) {
			final var eclipseGenerator = new EclipseClientProjectFilesGenerator(project);

			path = APPLICATION_MODEL_NAME;
			content = eclipseGenerator.createApplicationModelFile();

			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

			if (project.hasRCPClient()) {
				path = PLUGIN_XML;
				content = eclipseGenerator.createPluginXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

				path = "application.product";
				content = eclipseGenerator.createProductFile();
			}
			else {
				path = OSGI_INF_FOLDER + "/" + CONTRIBUTION_XML;
				content = eclipseGenerator.createContributionXML();

				configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

				path = "application.launch";
				content = eclipseGenerator.createLaunchFile();
			}

			configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));
		}

		// Create the translation file
		path = "/" + project.getResourceFolder() + "/" + TRANSLATION_FILE_NAME;

		if (project.hasJSFClient())
			content = new JSFClientProjectFilesGenerator(project).createI18NDefaultResourceFile();
		else
			content = "#" + new Date(System.currentTimeMillis()).toString();

		configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

		// Create the logging configuration file
		path = project.getResourceFolder() + "/log4j2.xml";
		content = new LoggingConfigurationGenerator(buildArtifact).createContent();

		configFiles.add(new WorkspaceFile(project, BuildArtifactType.GUI, path, content));

		return configFiles;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getPackages()
	 */
	@Override
	public List<String> getPackages() {
		if (project.hasAngularClient())
			return Collections.emptyList();

		final List<String> packages = super.getPackages();
		packages.add(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
		packages.add(project.getClientNamespace().toString() + PACK_CLIENT_LOV);
		packages.add(project.getClientNamespace().toString() + PACK_CLIENT_VIEW);
		packages.add(project.getClientNamespace().toString() + SUB_PACKAGE_UTIL);
		packages.add(project.getClientNamespace().toString() + PACK_CLIENT_TREE);
		packages.add(project.getClientNamespace().toString() + PACK_CLIENT_PANEL);

		if (project.hasJSFClient())
			packages.add(project.getClientNamespace().toString() + PACK_CLIENT_CONVERTER);

		return packages;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#createInitialSourceFiles()
	 */
	@Override
	public void createInitialSourceFiles() throws Exception {
		// Build the application tree navigator
		new FormService(project).rebuildNavigator();

		if (project.hasAngularClient())
			return;

		// Create the GUI source files
		for (final JavaFile javaFile : ClientProjectFilesGeneratorFactory.getGenerator(project).createSourceFiles())
			EclipseIDEService.createJavaFile(javaFile);

		if (project.isSpringBootApplication() && project.hasJSFOrVaadinClient()) {
			new SpringBootApplicationGenerator(project).createSourceFile();
			new SecurityConfigurationGenerator(project).createSourceFile();

			if (project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK)
				new EclipseLinkConfigurationGenerator(project).createSourceFile();
		}
	}

}
