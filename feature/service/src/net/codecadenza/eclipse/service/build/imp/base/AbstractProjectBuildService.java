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
package net.codecadenza.eclipse.service.build.imp.base;

import static net.codecadenza.eclipse.shared.Constants.CONT_PATH_JRE;
import static net.codecadenza.eclipse.shared.Constants.RCP_ICON_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.zip.ZipFile;
import net.codecadenza.eclipse.generator.basic.project.ProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.ServerProjectFilesGeneratorFactory;
import net.codecadenza.eclipse.generator.basic.technology.spring.EclipseLinkConfigurationGenerator;
import net.codecadenza.eclipse.generator.basic.technology.spring.LoggingAspectGenerator;
import net.codecadenza.eclipse.generator.service.LoggingServiceGenerator;
import net.codecadenza.eclipse.generator.service.SavedQueryServiceBeanGenerator;
import net.codecadenza.eclipse.generator.service.SavedQueryServiceGenerator;
import net.codecadenza.eclipse.generator.service.ServiceConfigurationGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.build.IProjectBuildService;
import net.codecadenza.eclipse.service.build.imp.module.BuildModuleFactory;
import net.codecadenza.eclipse.service.build.imp.module.IBuildModule;
import net.codecadenza.eclipse.service.build.imp.packaging.BuildFileFactory;
import net.codecadenza.eclipse.service.build.imp.resolve.DependencyResolverService;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.exchange.DataExchangeBeanService;
import net.codecadenza.eclipse.service.exchange.ExchangeMappingObjectService;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.service.java.JavaEnumService;
import net.codecadenza.eclipse.service.java.PackageInfoService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.service.security.SecurityService;
import net.codecadenza.eclipse.service.testing.TestSuiteService;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import net.codecadenza.eclipse.tools.angular.AngularProjectBuildService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.util.file.ZipFileUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

/**
 * <p>
 * Abstract base class for project build services
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractProjectBuildService implements IProjectBuildService {
	protected static final String INTEGR_SUFFIX_CLIENT = "-client";
	protected static final String INTEGR_SUFFIX_SEI = "-sei";
	protected static final String INTEGR_SUFFIX_IMP = "-imp";
	protected static final String INTEGR_TEST_SUFFIX = "-test";

	protected Project project;
	protected PackageInfoService packageInfoService;
	protected FormService formService;
	protected GUITestCaseService guiTestCaseService;

	/**
	 * Constructor
	 * @param project
	 */
	protected AbstractProjectBuildService(Project project) {
		this.project = project;
		this.packageInfoService = new PackageInfoService(project);
		this.formService = new FormService(project);
		this.guiTestCaseService = new GUITestCaseService(project);
	}

	/**
	 * Import additional files for a given client platform
	 * @param wsProject
	 * @param monitor
	 * @throws Exception if the import of the files has failed
	 */
	protected void importAdditionalContentFiles(IProject wsProject, IProgressMonitor monitor) throws Exception {
		if (!project.hasClient() || project.hasSwingClient())
			return;

		final URL path = CodeCadenzaResourcePlugin.getContentResourcePath(project);
		IPath destPath = wsProject.getFullPath();

		try (ZipFile zipFile = new ZipFile(FileLocator.toFileURL(path).getFile())) {
			if (project.hasEclipseClient())
				destPath = destPath.append("/" + RCP_ICON_FOLDER);
			else if (project.hasJSFClient())
				destPath = destPath.append("/" + project.getWebAppFolder());
			else if (project.hasAngularClient() || project.hasJavaFXClient())
				destPath = destPath.append("/");

			ZipFileUtil.importFilesFromZip(zipFile, destPath, SubMonitor.convert(monitor, 1));
		}
	}

	/**
	 * Get all internal libraries that are not provided by public repositories
	 * @param artifact
	 * @param wsProject
	 * @return a hash set containing internal libraries that are not provided by public repositories
	 * @throws Exception if the operation has failed
	 */
	@SuppressWarnings("unused")
	protected HashSet<IClasspathEntry> getInternalLibraries(final BuildArtifact artifact, final IProject wsProject)
			throws Exception {
		return new HashSet<>();
	}

	/**
	 * Create custom build files depending on the selected build tool
	 * @param artifact
	 * @param wsProject
	 * @param classPathEntries
	 * @throws Exception if the creation of custom build files has failed
	 */
	@SuppressWarnings("unused")
	protected void createCustomBuildFiles(BuildArtifact artifact, IProject wsProject, HashSet<IClasspathEntry> classPathEntries)
			throws Exception {

	}

	/**
	 * @return the output location where class files are ordinarily generated
	 */
	protected abstract String getOutputLocation();

	/**
	 * Create the default build artifact
	 * @param artifactType
	 * @return the new build artifact
	 */
	protected BuildArtifact createDefaultArtifact(BuildArtifactType artifactType) {
		return createDefaultArtifact(artifactType, artifactType.getName());
	}

	/**
	 * Create the default build artifact
	 * @param artifactType
	 * @param name
	 * @return the new build artifact
	 */
	protected BuildArtifact createDefaultArtifact(BuildArtifactType artifactType, String name) {
		final BuildArtifact artifact = ProjectFactory.eINSTANCE.createBuildArtifact();
		artifact.setName(project.getName().toLowerCase() + "-" + name.replace('_', '-').toLowerCase());
		artifact.setType(artifactType);

		return artifact;
	}

	/**
	 * Create the default root build artifact
	 * @param artifactType
	 * @return the new build artifact
	 */
	protected BuildArtifact createDefaultRootArtifact(BuildArtifactType artifactType) {
		final BuildArtifact artifact = createDefaultArtifact(artifactType);
		artifact.setName(project.getName().toLowerCase());

		return artifact;
	}

	/**
	 * Create an Eclipse workspace project based on the given build artifact
	 * @param artifact
	 * @param deployDir
	 * @param monitor
	 * @throws Exception if the project could not be created
	 */
	protected void buildArtifact(final BuildArtifact artifact, String deployDir, IProgressMonitor monitor) throws Exception {
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

		// Get the respective build module
		final IBuildModule buildModule = BuildModuleFactory.getBuildModule(artifact, artifact.getType());

		// Get the natures of this build module
		final List<String> natures = buildModule.getNatures();

		IProjectDescription projectDescription = root.getWorkspace().newProjectDescription(artifact.getName());
		projectDescription.setLocation(null);

		if (createSubModule(project, artifact)) {
			// Save the module in a subdirectory of the root project
			final String rootProjectName = project.getTargetProjectName(BuildArtifactType.MASTER);
			final IPath location = root.getProject(rootProjectName).getLocation().append(artifact.getModuleName());

			projectDescription.setLocation(location);
		}

		// Create the project in the workspace
		final IProject wsProject = EclipseIDEService.createNewProject(root, projectDescription, UTF_8, monitor);

		// The project natures must be added after creating the project in the workspace!
		projectDescription = wsProject.getDescription();
		projectDescription.setNatureIds(natures.toArray(new String[natures.size()]));

		wsProject.setDescription(projectDescription, monitor);

		// Add the folders that are defined by this build module
		for (final String folderName : buildModule.getFolders())
			EclipseIDEService.createFolder(wsProject.getName(), folderName);

		for (final BuildArtifactType buildType : artifact.getContainedArtifacts()) {
			final IBuildModule containedModule = BuildModuleFactory.getBuildModule(artifact, buildType);

			// Add the folders for this contained build module
			for (final String folderName : containedModule.getFolders())
				EclipseIDEService.createFolder(wsProject.getName(), folderName);
		}

		// Resolve all dependencies
		final List<IClasspathEntry> resolvedClassPathEntries = new DependencyResolverService(artifact).resolveDependencies(monitor);

		// Determine all required packages
		final List<String> packages = buildModule.getPackages();
		final List<String> testPackages = buildModule.getTestPackages();
		final List<WorkspaceFile> configFiles = buildModule.getConfigurationFiles();

		final HashSet<IClasspathEntry> classPathEntries = buildModule.getClassPathEntries(artifact.getName());
		classPathEntries.addAll(resolvedClassPathEntries);
		classPathEntries.addAll(getInternalLibraries(artifact, wsProject));

		// Create the build file
		final WorkspaceFile buildFile = BuildFileFactory.getBuildFileService(artifact).createBuildFile(deployDir);

		if (buildFile != null)
			EclipseIDEService.createOrUpdateFile(buildFile);

		// Create further build files depending on the respective build tool
		createCustomBuildFiles(artifact, wsProject, classPathEntries);

		artifact.getContainedArtifacts().forEach(buildType -> {
			final IBuildModule containedModule = BuildModuleFactory.getBuildModule(artifact, buildType);

			configFiles.addAll(containedModule.getConfigurationFiles());

			// Add the classpath entries of the contained build artifact
			classPathEntries.addAll(containedModule.getClassPathEntries(artifact.getName()));

			// Add the packages
			packages.addAll(containedModule.getPackages());
			testPackages.addAll(containedModule.getTestPackages());
		});

		// Create the source packages
		for (final String packageName : packages)
			EclipseIDEService.createPackage(artifact.getName(), project.getSourceFolder(), packageName);

		// Create the test packages
		for (final String packageName : testPackages)
			EclipseIDEService.createPackage(artifact.getName(), project.getTestSourceFolder(), packageName);

		if (!classPathEntries.isEmpty()) {
			final IJavaProject wsJavaProject = JavaCore.create(wsProject);
			wsJavaProject.setOutputLocation(wsJavaProject.getPath().append(getOutputLocation()), monitor);

			final var classPathArray = new IClasspathEntry[classPathEntries.size() + 1];
			int i = 0;

			// The JRE classpath container must be the first element in the array in order to avoid compilation problems!
			classPathArray[i++] = JavaCore.newContainerEntry(new Path(CONT_PATH_JRE));

			for (final IClasspathEntry entry : classPathEntries) {
				classPathArray[i] = entry;
				i++;
			}

			wsJavaProject.setRawClasspath(classPathArray, monitor);

			// Set the compiler options
			buildModule.getCompilerOptions().keySet()
					.forEach(optionName -> wsJavaProject.setOption(optionName, buildModule.getCompilerOptions().get(optionName)));
		}

		if (artifact.getType() == BuildArtifactType.GUI) {
			if (project.hasAngularClient()) {
				new AngularProjectBuildService(wsProject).build();

				wsProject.refreshLocal(IResource.DEPTH_INFINITE, monitor);
			}

			importAdditionalContentFiles(wsProject, monitor);
		}

		// Create the configuration files
		for (final WorkspaceFile configFile : configFiles)
			EclipseIDEService.createOrUpdateFile(configFile);

		buildModule.createInitialSourceFiles();

		// Create the source files of the contained build modules
		for (final BuildArtifactType buildType : artifact.getContainedArtifacts()) {
			final IBuildModule containedModule = BuildModuleFactory.getBuildModule(artifact, buildType);
			containedModule.createInitialSourceFiles();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#buildProject(java.lang.String,
	 * org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void buildProject(String deployDir, IProgressMonitor monitor) throws Exception {
		addPersistenceUnitProperties();

		for (final BuildArtifact artifact : project.getBuildConfiguration())
			buildArtifact(artifact, deployDir, monitor);

		// Create the package-info files
		packageInfoService.rebuildPackageInfoFiles();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildORMXML()
	 */
	@Override
	public void rebuildORMXML() throws Exception {
		if (project.isSpringBootApplication())
			return;

		final String content = new ProjectFilesGenerator(project).createORMXML();
		final var path = project.getMetaInfFolder() + "/orm.xml";
		final var ormFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, content);

		EclipseIDEService.createOrUpdateFile(ormFile);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildPersistenceUnit()
	 */
	@Override
	public void rebuildPersistenceUnit() throws Exception {
		if (project.isSpringBootApplication())
			return;

		final String content = new ProjectFilesGenerator(project).createPersistenceXML();
		final var path = project.getMetaInfFolder() + "/persistence.xml";
		final var persistenceFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, content);

		EclipseIDEService.createOrUpdateFile(persistenceFile);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildDataSourceFile()
	 */
	@Override
	public void rebuildDataSourceFile() throws Exception {
		// If the name of the data source file is null it should not be created for this project!
		if (project.getDataSourceFileName() == null)
			return;

		final IServerProjectFilesGenerator generator = ServerProjectFilesGeneratorFactory.getGenerator(project);
		final String content = generator.createDataSource();
		final var path = project.getWebInfFolder() + "/" + project.getDataSourceFileName();

		if (project.hasJSFOrVaadinClient()) {
			final var dataSourceFile = new WorkspaceFile(project, BuildArtifactType.GUI, path, content);

			EclipseIDEService.createOrUpdateFile(dataSourceFile);
		}
		else {
			final var dataSourceFile = new WorkspaceFile(project, BuildArtifactType.SERVER, path, content);

			EclipseIDEService.createOrUpdateFile(dataSourceFile);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildSecurity()
	 */
	@Override
	public void rebuildSecurity() throws Exception {
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();

		if (logOnDTO == null)
			return;

		// Rebuild the application tree navigator
		if (project.hasClient())
			formService.rebuildNavigator();

		new SecurityService(project).rebuildAllSecurityRelatedFiles();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#buildObjectsOfNamespace(net.codecadenza.eclipse.model.java.
	 * Namespace, boolean, org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void buildObjectsOfNamespace(Namespace namespace, boolean rebuildPersitenceXML, IProgressMonitor monitor)
			throws Exception {
		for (final JavaType type : namespace.getJavaTypes()) {
			if (type instanceof final BoundaryBean boundaryBean) {
				monitor.beginTask("Building boundary " + boundaryBean.getInterfaceName(), IProgressMonitor.UNKNOWN);
				new BoundaryService(project).rebuildBoundarySourceFiles(boundaryBean);
			}
			else if (type instanceof final DTOBean dto) {
				monitor.beginTask("Building DTO " + dto.getName(), IProgressMonitor.UNKNOWN);
				new DTOBeanService(project).rebuildDTOBeanSourceFiles(dto);
			}
			else if (type instanceof final DomainObject domainObject) {
				monitor.beginTask("Building domain object " + domainObject.getName(), IProgressMonitor.UNKNOWN);
				new DomainObjectService(project).rebuildDomainObjectSourceFiles(domainObject, rebuildPersitenceXML);
			}
			else if (type instanceof final JavaEnum javaEnum) {
				monitor.beginTask("Building enumeration " + javaEnum.getName(), IProgressMonitor.UNKNOWN);
				new JavaEnumService(project).rebuildEnumerationSourceFile(javaEnum);
			}
			else if (type instanceof final Repository repository) {
				monitor.beginTask("Building repository " + repository.getName(), IProgressMonitor.UNKNOWN);
				new RepositoryService(project).rebuildRepositorySourceFiles(repository);
			}
			else if (type instanceof final ExchangeMappingObject mappingObject) {
				monitor.beginTask("Building mapping object " + mappingObject.getName(), IProgressMonitor.UNKNOWN);
				new ExchangeMappingObjectService().rebuildExchangeMappingObject(mappingObject);
			}
			else if (type instanceof final DataExchangeServiceBean exchangeService) {
				monitor.beginTask("Building data exchange service " + exchangeService.getName(), IProgressMonitor.UNKNOWN);
				new DataExchangeBeanService(project).rebuildDataExchangeServiceBeanSourceFiles(exchangeService);
			}
			else if (type instanceof final AbstractIntegrationBean integrationBean) {
				monitor.beginTask("Building integration service " + integrationBean.getName(), IProgressMonitor.UNKNOWN);
				new IntegrationBeanService(project).rebuildIntegrationBeanSourceFiles(integrationBean);
			}
			else if (type instanceof final GUITestCase testCase) {
				monitor.beginTask("Building GUI test case " + testCase.getName(), IProgressMonitor.UNKNOWN);
				new GUITestCaseService(project).rebuildTestCaseSourceFiles(testCase);
			}
			else if (type instanceof final TestSuite testSuite) {
				monitor.beginTask("Building test suite " + testSuite.getName(), IProgressMonitor.UNKNOWN);
				new TestSuiteService(project).rebuildTestSuiteSourceFile(testSuite);
			}
			else if (type instanceof final IntegrationTestCase testCase) {
				monitor.beginTask("Building integration test case " + testCase.getName(), IProgressMonitor.UNKNOWN);
				new IntegrationTestCaseService((IntegrationTestModule) testCase.getTestModule()).rebuildTestCaseSourceFiles(testCase);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildLoggingService()
	 */
	@Override
	public void rebuildLoggingService() throws Exception {
		if (project.isJavaSEApplication())
			return;

		final DomainObject loggingObject = project.getDomainObjectByTag(DomainTagEnumeration.LOGGING);

		// In case of Jakarta EE we must provide an empty implementation of this service!
		if (loggingObject == null && !project.isJakartaEEApplication())
			return;

		new LoggingServiceGenerator(project).createSourceFile();

		if (project.isSpringBootApplication())
			new LoggingAspectGenerator(project).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildSavedQueryService()
	 */
	@Override
	public void rebuildSavedQueryService() throws Exception {
		if (!project.hasJSFOrVaadinClient())
			return;

		if (project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) == null)
			return;

		// Create the service interface
		new SavedQueryServiceGenerator(project).createSourceFile();

		// Create the service implementation
		new SavedQueryServiceBeanGenerator(project).createSourceFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildAllObjects(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void rebuildAllObjects(IProgressMonitor monitor) throws Exception {
		// Rebuild all domain objects
		for (final Namespace ns : project.getDomainNamespace().getChildNamespaces())
			buildObjectsOfNamespace(ns, false, monitor);

		// Rebuild all data transfer objects
		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			buildObjectsOfNamespace(ns, false, monitor);

		// Rebuild all repositories
		if (project.isBoundaryMode())
			for (final Namespace ns : project.getRepositoryNamespace().getChildNamespaces())
				buildObjectsOfNamespace(ns, false, monitor);

		// Rebuild all boundary beans
		for (final Namespace ns : project.getBoundaryNamespace().getChildNamespaces())
			buildObjectsOfNamespace(ns, false, monitor);

		// Rebuild all exchange services
		for (final Namespace ns : project.getExchangeNamespace().getChildNamespaces())
			buildObjectsOfNamespace(ns, false, monitor);

		for (final IntegrationModule module : project.getIntegrationModules()) {
			// Rebuild all integration services
			buildObjectsOfNamespace(module.getNamespace(), false, monitor);

			if (module.getTechnology() == IntegrationTechnology.KAFKA)
				new IntegrationBeanService(project).generateJavaClassesFromAvroIDLFiles();
		}

		// Rebuild all integration test cases
		for (final AbstractTestModule module : project.getTestModules())
			buildObjectsOfNamespace(module.getNamespace(), false, monitor);

		if (project.hasJSFOrVaadinClient() || project.hasAngularClient())
			for (final AbstractTestModule testModule : project.getTestModules()) {
				// Rebuild all page objects
				if (testModule.getArtifactType() == BuildArtifactType.SELENIUM_TEST)
					guiTestCaseService.rebuildAllPageObjectSourceFiles();

				// Rebuild all tests
				buildObjectsOfNamespace(testModule.getNamespace(), false, monitor);
			}

		if (project.hasClient()) {
			// Rebuild all forms
			for (final Form form : project.getAllFormsOfProject())
				formService.rebuildForm(form);

			// Rebuild all grid panels
			for (final FormPanel panel : project.getAllGridPanelsOfProject())
				formService.rebuildGridPanel(panel);

			// Rebuild the application tree navigator
			formService.rebuildNavigator();
		}

		// Rebuild the persistence.xml file
		rebuildPersistenceUnit();

		// Rebuild all security-related files
		rebuildSecurity();

		// Rebuild the logging service
		rebuildLoggingService();

		// Rebuild the service for saved queries
		rebuildSavedQueryService();

		// Rebuild the configuration files of a Spring Boot application
		rebuildSpringBootConfigurationFiles();

		// Rebuild or remove the package-info files
		packageInfoService.rebuildPackageInfoFiles();

		// Rebuild the service configuration file
		new ServiceConfigurationGenerator(project).createFile();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.IProjectBuildService#rebuildSpringBootConfigurationFiles()
	 */
	@Override
	public void rebuildSpringBootConfigurationFiles() throws Exception {
		if (!project.isSpringBootApplication())
			return;

		final BuildArtifactType artifactType;

		// A project might contain more than one application.properties file! Only the file that contains the properties for Spring
		// Boot should be rebuilt!
		if (project.hasJSFOrVaadinClient())
			artifactType = BuildArtifactType.GUI;
		else
			artifactType = BuildArtifactType.SERVER;

		final String path = project.getConfigFolder(artifactType) + "/application.properties";
		final String content = new ProjectFilesGenerator(project).createPropertiesFile(artifactType);

		EclipseIDEService.createOrUpdateFile(new WorkspaceFile(project, artifactType, path, content));

		if (project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK)
			new EclipseLinkConfigurationGenerator(project).createSourceFile();
	}

	/**
	 * @param project
	 * @param buildArtifact
	 * @return true if the given artifact should be added as a sub-module
	 */
	private boolean createSubModule(Project project, BuildArtifact buildArtifact) {
		if (project.hasEclipseClient() || project.hasAngularClient()) {
			// No sub-modules should be used for Eclipse applications that use Java SE
			if (project.isJavaSEApplication())
				return false;

			// All Eclipse and Angular GUI artifacts should be top-level projects
			if (buildArtifact.getType() == BuildArtifactType.GUI)
				return false;
		}

		return buildArtifact.getType() != BuildArtifactType.MASTER;
	}

	/**
	 * Add standard persistence unit properties depending on the selected platform and the persistence provider
	 */
	private void addPersistenceUnitProperties() {
		if (project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE) {
			addPersistenceUnitProperty("hibernate.show.sql", "true");
			addPersistenceUnitProperty("hibernate.enable_lazy_load_no_trans", "true");
			addPersistenceUnitProperty("hibernate.dialect", project.getDatabase().getHibernateDialect());
		}
		else if (project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK) {
			if (project.isJavaSEApplication()) {
				addPersistenceUnitProperty("jakarta.persistence.jdbc.driver", project.getDataSource().getDriverName());
				addPersistenceUnitProperty("eclipselink.logging.level", "INFO");
				addPersistenceUnitProperty("eclipselink.logging.timestamp", "false");
				addPersistenceUnitProperty("eclipselink.logging.session", "false");
				addPersistenceUnitProperty("eclipselink.logging.thread", "false");
				addPersistenceUnitProperty("eclipselink.target-database", project.getDatabase().getEclipseLinkTargetDBName());
			}
			else if (project.isSpringBootApplication())
				addPersistenceUnitProperty("eclipselink.weaving", "false");

			addPersistenceUnitProperty("eclipselink.id-validation", "NULL");

			// Disable the L2 cache for all kinds of applications by default!
			addPersistenceUnitProperty("eclipselink.cache.shared.default", "false");
		}
	}

	/**
	 * Add a persistence unit property
	 * @param name the persistence unit property name
	 * @param value the persistence unit property value
	 */
	private void addPersistenceUnitProperty(String name, String value) {
		final PersistenceUnitProperty persistenceUnitProperty = ProjectFactory.eINSTANCE.createPersistenceUnitProperty();
		persistenceUnitProperty.setName(name);
		persistenceUnitProperty.setValue(value);

		project.getPersistenceUnitProperties().add(persistenceUnitProperty);
	}

}
