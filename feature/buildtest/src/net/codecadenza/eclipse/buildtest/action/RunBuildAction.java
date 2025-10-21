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
package net.codecadenza.eclipse.buildtest.action;

import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.MODEL_ROOT_FILE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import net.codecadenza.eclipse.buildtest.CodeCadenzaBuildTestPlugin;
import net.codecadenza.eclipse.buildtest.dialog.ProjectSelectionDialog;
import net.codecadenza.eclipse.buildtest.mapping.ConfigurationGroupRoot;
import net.codecadenza.eclipse.buildtest.mapping.ProjectConfiguration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.util.file.ZipFileUtil;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.xml.sax.InputSource;

/**
 * <p>
 * Action that performs the build of predefined internal reference projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RunBuildAction implements IViewActionDelegate {
	private static final String PROJ_CONFIG_FILE = "/config/projects.xml";
	private static final String PROP_DIR_GLASSFISH = "glassfish_folder";
	private static final String PROP_DIR_JBOSS = "jboss_folder";
	private static final String PROP_DIR_TOMCAT = "tomcat_folder";
	private static final String PROP_JDBC_DIR = "jdbc_lib_folder";
	private static final String PROP_SELENIUM_DRIVER_DIR = "selenium_driver_folder";
	private static final String DEFAULT_EXCHANGE_PATH = "/development/test/exchange";

	private Properties properties;

	/**
	 * @param projectConf
	 * @return the default build configuration based on the given project settings
	 */
	private List<BuildArtifact> getDefaultBuildConfiguration(ProjectConfiguration projectConf) {
		// Create a temporary project instance and fill it with required data
		final Project project = ProjectFactory.eINSTANCE.createProject();
		project.setBoundaryMode(projectConf.isBoundaryMode());
		project.setBuildTool(projectConf.getBuildTool());
		project.setName(projectConf.getProjectName());
		project.setClientPlatform(projectConf.getClientPlatform());
		project.setTechnology(projectConf.getTechnologyPlatform());

		return ProjectBuildFactory.getBuildService(project).getDefaultBuildConfiguration();
	}

	/**
	 * Build the project based on the provided configuration
	 * @param projectConf
	 * @param monitor
	 * @throws Exception if an internal error has occurred
	 */
	private void buildProject(ProjectConfiguration projectConf, IProgressMonitor monitor) throws Exception {
		final var metaModelFilePath = "/templates/" + projectConf.getProjectTemplateName() + ".zip";
		final URL metaModelFileURL = CodeCadenzaBuildTestPlugin.getInstance().getBundle().getEntry(metaModelFilePath);
		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		final List<BuildArtifact> buildConfig = getDefaultBuildConfiguration(projectConf);
		var deploySubFolder = "";
		Project project = null;

		// Delete all existing workbench projects
		for (final BuildArtifact buildArtifact : buildConfig) {
			final IProject existingProject = workspaceRoot.getProject(buildArtifact.getName());

			if (existingProject.exists())
				existingProject.delete(true, monitor);
		}

		// Save the meta-data template files in a temporary directory
		final Path metaModelTempPath = Files.createTempDirectory(MODEL_FOLDER);

		ZipFileUtil.unzip(metaModelFileURL.openStream(), metaModelTempPath);

		final var modelRootFile = new File(metaModelTempPath.toFile(), MODEL_ROOT_FILE);
		final var resourceSet = new ResourceSetImpl();
		final URI namespaceURI = URI.createFileURI(modelRootFile.getAbsolutePath());

		// Get the resource object
		final Resource projectResource = resourceSet.getResource(namespaceURI, true);

		// Search for the project meta-data object
		for (final EObject e : projectResource.getContents()) {
			if (e instanceof final Project proj) {
				project = proj;

				project.setName(projectConf.getProjectName());
				project.setBoundaryMode(projectConf.isBoundaryMode());
				project.setServerPlatform(projectConf.getServerPlatform());
				project.setClientPlatform(projectConf.getClientPlatform());
				project.setTechnology(projectConf.getTechnologyPlatform());
				project.setPersistenceProvider(projectConf.getPersistenceProvider());
				project.setBuildTool(projectConf.getBuildTool());
				project.setProtectManualChanges(projectConf.isProtectManualChanges());
				project.getBuildConfiguration().addAll(buildConfig);

				if (project.isDeployedOnGlassfish())
					deploySubFolder = properties.getProperty(PROP_DIR_GLASSFISH);
				else if (project.isDeployedOnJBoss())
					deploySubFolder = properties.getProperty(PROP_DIR_JBOSS);
				else if (project.isDeployedOnTomcat())
					deploySubFolder = properties.getProperty(PROP_DIR_TOMCAT);

				if (properties.getProperty(PROP_JDBC_DIR) != null) {
					String jdbcPath = properties.getProperty(PROP_JDBC_DIR);
					jdbcPath += project.getDatabase().getVendorGroup().getName().toLowerCase();

					final var jdbcDir = new File(System.getProperty("user.home"), jdbcPath);

					// Add JDBC libraries from the local system
					if (jdbcDir.exists() && jdbcDir.isDirectory())
						for (final File libFile : jdbcDir.listFiles())
							if (libFile.isFile() && libFile.exists() && !libFile.isHidden())
								project.getDataSource().getDriverList().add(libFile.getAbsolutePath());
				}

				// Copy all existing integration modules of the template to a new list
				final var integrationModules = new ArrayList<>(project.getIntegrationModules());
				final var buildArtifacts = new ArrayList<>(project.getBuildConfiguration());

				for (final IntegrationModule module : integrationModules) {
					// Check if this integration module must be removed
					final boolean remove = (module.getTechnology() == IntegrationTechnology.REST && !projectConf.isAddREST())
							|| (module.getTechnology() == IntegrationTechnology.SOAP && !projectConf.isAddSOAP())
							|| (module.getTechnology() == IntegrationTechnology.RMI && !projectConf.isAddRMI())
							|| (module.getTechnology() == IntegrationTechnology.KAFKA && !projectConf.isAddKafka())
							|| (module.getTechnology() == IntegrationTechnology.JMS && !projectConf.isAddJMS());

					if (!remove) {
						if (module.getTechnology() == IntegrationTechnology.JMS && project.isDeployedOnJBoss()) {
							// When deploying the application on Wildfly all JMS response destination names must be corrected!
							module.getNamespace().getJavaTypes().stream().map(JMSIntegrationBean.class::cast)
									.map(JMSIntegrationBean::getResponseDestination)
									.forEach(destination -> destination.setName("java:/" + destination.getName()));
						}

						continue;
					}

					final Namespace integrationNamespace = module.getNamespace();

					// Copy all integration beans of this module to a new list
					final var integrationBeans = new ArrayList<>(integrationNamespace.getJavaTypes());

					for (final JavaType type : integrationBeans) {
						// Remove the integration bean from the respective namespace!
						integrationNamespace.getJavaTypes().remove(type);

						// Delete the integration bean
						project.eResource().getContents().remove(type);
					}

					adaptExchangePaths(project);

					module.setNamespace(null);

					project.getIntegrationModules().remove(module);

					// Remove all required build artifacts
					for (final BuildArtifact buildArtifact : buildArtifacts) {
						final var type = buildArtifact.getType();

						if ((module.getTechnology() == IntegrationTechnology.REST
								&& (type == BuildArtifactType.INTEGRATION_CLIENT_REST || type == BuildArtifactType.INTEGRATION_IMP_REST
										|| type == BuildArtifactType.INTEGRATION_SEI_REST || type == BuildArtifactType.INTEGRATION_TEST_REST))
								|| (module.getTechnology() == IntegrationTechnology.SOAP
										&& (type == BuildArtifactType.INTEGRATION_CLIENT_SOAP || type == BuildArtifactType.INTEGRATION_IMP_SOAP
												|| type == BuildArtifactType.INTEGRATION_SEI_SOAP || type == BuildArtifactType.INTEGRATION_TEST_SOAP))
								|| (module.getTechnology() == IntegrationTechnology.RMI
										&& (type == BuildArtifactType.INTEGRATION_CLIENT_RMI || type == BuildArtifactType.INTEGRATION_IMP_RMI
												|| type == BuildArtifactType.INTEGRATION_SEI_RMI || type == BuildArtifactType.INTEGRATION_TEST_RMI))
								|| (module.getTechnology() == IntegrationTechnology.KAFKA
										&& (type == BuildArtifactType.INTEGRATION_CLIENT_KAFKA || type == BuildArtifactType.INTEGRATION_IMP_KAFKA
												|| type == BuildArtifactType.INTEGRATION_SEI_KAFKA || type == BuildArtifactType.INTEGRATION_TEST_KAFKA))
								|| (module.getTechnology() == IntegrationTechnology.JMS
										&& (type == BuildArtifactType.INTEGRATION_CLIENT_JMS || type == BuildArtifactType.INTEGRATION_IMP_JMS
												|| type == BuildArtifactType.INTEGRATION_SEI_JMS || type == BuildArtifactType.INTEGRATION_TEST_JMS)))
							project.getBuildConfiguration().remove(buildArtifact);
					}

					// Delete the integration namespace physically!
					project.eResource().getContents().remove(integrationNamespace);
				}

				// Remove the respective build artifact if no GUI should be added!
				if (!project.hasClient())
					for (final BuildArtifact buildArtifact : buildArtifacts)
						if (buildArtifact.getType() == BuildArtifactType.GUI) {
							project.getBuildConfiguration().remove(buildArtifact);
							break;
						}

				final AbstractTestModule seleniumTestModule = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);
				final var deployPath = new File(System.getProperty("user.home"), deploySubFolder);

				// Set the path to the default Selenium driver if the project contains a respective test module
				if (seleniumTestModule != null && properties.getProperty(PROP_SELENIUM_DRIVER_DIR) != null) {
					final var driverFile = new File(System.getProperty("user.home"), properties.getProperty(PROP_SELENIUM_DRIVER_DIR));

					if (driverFile.exists() && driverFile.isFile())
						((SeleniumTestModule) seleniumTestModule).setDriverPath(driverFile.getAbsolutePath());

					if (!projectConf.isAddSelenium())
						removeTestModule(seleniumTestModule);
				}

				// Remove unused integration test modules
				if (!projectConf.isAddREST() || !projectConf.isAddIntegrationTests())
					removeTestModule(project.getTestModuleByArtifact(BuildArtifactType.INTEGRATION_TEST_REST));

				if (!projectConf.isAddSOAP() || !projectConf.isAddIntegrationTests())
					removeTestModule(project.getTestModuleByArtifact(BuildArtifactType.INTEGRATION_TEST_SOAP));

				if (!projectConf.isAddRMI() || !projectConf.isAddIntegrationTests())
					removeTestModule(project.getTestModuleByArtifact(BuildArtifactType.INTEGRATION_TEST_RMI));

				if (!projectConf.isAddKafka() || !projectConf.isAddIntegrationTests())
					removeTestModule(project.getTestModuleByArtifact(BuildArtifactType.INTEGRATION_TEST_KAFKA));

				if (!projectConf.isAddJMS() || !projectConf.isAddIntegrationTests())
					removeTestModule(project.getTestModuleByArtifact(BuildArtifactType.INTEGRATION_TEST_JMS));

				// Run the integration bean synchronization as it might be the case that some methods are missing!
				if (!project.getIntegrationModules().isEmpty())
					new IntegrationBeanSyncService(project).sync(false);

				ProjectBuildFactory.getBuildService(project).buildProject(deployPath.getAbsolutePath(), monitor);

				// Save the project
				EclipseIDEService.saveProjectMetaData(project);

				// Copy the meta-model files to the model folder of the domain module
				final IProject domainProject = workspaceRoot.getProject(project.getTargetProjectName(BuildArtifactType.DOMAIN));
				final IFolder projectModelFolder = domainProject.getFolder(MODEL_FOLDER);

				for (final File file : metaModelTempPath.toFile().listFiles()) {
					if (file.isDirectory())
						continue;

					final File targetFile = projectModelFolder.getLocation().append(file.getName()).toFile();

					Files.copy(file.toPath(), targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
				}

				projectModelFolder.refreshLocal(IResource.DEPTH_ONE, monitor);

				// In case of Eclipse RCP/RAP applications we must add all views to the application model file!
				if (project.hasEclipseClient()) {
					final String guiProjectName = project.getTargetProjectName(BuildArtifactType.GUI);

					for (final Form form : project.getAllFormsOfProject()) {
						final FormTypeEnumeration t = form.getFormType();

						if (t != FormTypeEnumeration.SIMPLE_VIEW && t != FormTypeEnumeration.SEARCHABLE_VIEW
								&& t != FormTypeEnumeration.TREE_VIEW)
							continue;

						String packageName = project.getClientNamespace().toString();

						if (t == FormTypeEnumeration.SIMPLE_VIEW || t == FormTypeEnumeration.SEARCHABLE_VIEW)
							packageName += PACK_CLIENT_VIEW;
						else
							packageName += PACK_CLIENT_TREE;

						// Add the view to the application model
						EclipseIDEService.addViewToApplicationModel(guiProjectName, form.getTitle(), packageName + "." + form.getName());
					}
				}

				break;
			}
		}

		// Rebuild all objects of this project
		ProjectBuildFactory.getBuildService(project).rebuildAllObjects(monitor);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		final Display display = Display.getDefault();
		final Shell shell = display.getActiveShell();
		final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		ConfigurationGroupRoot configRoot;

		try (InputStream in = classLoader.getResourceAsStream("plugin.properties")) {
			if (in == null)
				throw new IllegalStateException("The properties could not be loaded!");

			properties = new Properties();
			properties.load(in);

			final URL fileURL = CodeCadenzaBuildTestPlugin.getInstance().getBundle().getEntry(PROJ_CONFIG_FILE);
			final Unmarshaller unmarshaller = JAXBContext.newInstance(ConfigurationGroupRoot.class).createUnmarshaller();
			final var is = new InputSource(new InputStreamReader(fileURL.openStream(), StandardCharsets.UTF_8));
			configRoot = (ConfigurationGroupRoot) unmarshaller.unmarshal(is);
		}
		catch (final Exception e) {
			CodeCadenzaBuildTestPlugin.getInstance().handleInternalError(e);
			return;
		}

		// Open the dialog to select the projects that should be created
		final var dlg = new ProjectSelectionDialog(shell, configRoot);
		dlg.open();

		if (Dialog.OK != dlg.getReturnCode())
			return;

		final var job = new Job("Building CodeCadenza test projects...") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(final IProgressMonitor monitor) {
				try {
					final long start = System.currentTimeMillis();

					// Iterate over all test projects
					for (final ProjectConfiguration projectConf : dlg.getSelectedProjects()) {
						monitor.beginTask("Create project " + projectConf.getProjectName(), IProgressMonitor.UNKNOWN);
						buildProject(projectConf, monitor);
					}

					final long end = System.currentTimeMillis();
					final Duration duration = Duration.ofMillis(end - start);
					final String durationInMinutes = String.format("%01d:%02d", duration.toMinutes(), duration.toSecondsPart());
					final String createdProjects = "Created " + dlg.getSelectedProjects().size() + " CodeCadenza project(s)";

					CodeCadenzaBuildTestPlugin.getInstance().logInfo(createdProjects + " in " + durationInMinutes + " minutes");
				}
				catch (final Exception e) {
					display.syncExec(() -> CodeCadenzaBuildTestPlugin.getInstance().handleInternalError(e));
				}

				monitor.done();
				return Status.OK_STATUS;
			}
		};

		job.schedule();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	@Override
	public void init(IViewPart view) {
		// No implementation required!
	}

	/**
	 * Adapt the path of all data exchange methods for the given project in order to ease testing on different operating systems
	 * @param project the project that contains the data exchange methods
	 */
	private void adaptExchangePaths(Project project) {
		for (final DataExchangeServiceBean exchangeService : project.getAllExchangeServices()) {
			for (final DataExchangeMethod method : exchangeService.getDataExchangeMethods()) {
				if (method.getExchangeMode() instanceof final FileExchangeMode fileExchangeMode && fileExchangeMode.getPath() != null
						&& !fileExchangeMode.getPath().isEmpty()) {
					final var userHome = System.getProperty("user.home");
					final var newPath = Path.of(userHome, DEFAULT_EXCHANGE_PATH);

					fileExchangeMode.setPath(newPath.toAbsolutePath().toString().replace("\\", "\\\\"));
				}
			}
		}
	}

	/**
	 * Remove the provided test module from the project meta-data
	 * @param testModule
	 */
	private void removeTestModule(AbstractTestModule testModule) {
		final Project project = testModule.getProject();
		final List<BuildArtifact> buildArtifacts = new ArrayList<>(project.getBuildConfiguration());
		final Namespace testPackage = testModule.getNamespace();

		for (final BuildArtifact buildArtifact : buildArtifacts)
			if (buildArtifact.getType() == testModule.getArtifactType())
				project.getBuildConfiguration().remove(buildArtifact);

		testModule.setNamespace(null);

		project.getTestModules().remove(testModule);
		project.eResource().getContents().remove(testPackage);
	}

}
