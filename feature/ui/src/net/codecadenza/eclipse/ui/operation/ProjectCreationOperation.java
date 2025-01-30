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
package net.codecadenza.eclipse.ui.operation;

import static net.codecadenza.eclipse.shared.Constants.MODEL_FILE_EXTENSION;
import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.MODEL_ROOT_FILE;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_MATH;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.resource.db.DatabaseTemplate;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.wizard.ApplicationWizardPage;
import net.codecadenza.eclipse.ui.wizard.BuildConfigWizardPage;
import net.codecadenza.eclipse.ui.wizard.DBConnectionWizardPage;
import net.codecadenza.eclipse.ui.wizard.IntegrationWizardPage;
import net.codecadenza.eclipse.ui.wizard.ProjectWizardPage;
import net.codecadenza.eclipse.ui.wizard.TestingWizardPage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * <p>
 * Class for creating new CodeCadenza projects within the Eclipse workspace
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectCreationOperation implements IRunnableWithProgress {
	private static final String MODEL_SUB = "package-";
	private static final String LOCAL_JDBC_LIB_PATH = "/development/lib/jdbc/";

	private final ProjectWizardPage pageProject;
	private final DBConnectionWizardPage pageDBCon;
	private final ApplicationWizardPage pageApplication;
	private final BuildConfigWizardPage pageBuildConfig;
	private final IntegrationWizardPage pageIntegration;
	private final TestingWizardPage pageTesting;
	private Project project;

	/**
	 * Constructor
	 * @param pageProject
	 * @param pageDBCon
	 * @param pageApplication
	 * @param pageBuildConfig
	 * @param pageIntegration
	 * @param pageTesting
	 */
	public ProjectCreationOperation(ProjectWizardPage pageProject, DBConnectionWizardPage pageDBCon,
			ApplicationWizardPage pageApplication, BuildConfigWizardPage pageBuildConfig, IntegrationWizardPage pageIntegration,
			TestingWizardPage pageTesting) {
		this.pageProject = pageProject;
		this.pageDBCon = pageDBCon;
		this.pageApplication = pageApplication;
		this.pageBuildConfig = pageBuildConfig;
		this.pageIntegration = pageIntegration;
		this.pageTesting = pageTesting;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
		if (monitor == null)
			monitor = new NullProgressMonitor();

		try {
			createInitialModel();

			// Create a new project
			ProjectBuildFactory.getBuildService(project).buildProject(pageApplication.getDeployDir(), monitor);

			configureModelResources();

			pageProject.setProject(project);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
		finally {
			monitor.done();
		}
	}

	/**
	 * Create and initialize resource files for the meta-model
	 * @throws Exception if the creation of the meta-model resource files has failed
	 */
	private void configureModelResources() throws Exception {
		// Create a resource set
		final var resourceSet = new ResourceSetImpl();
		var path = MODEL_FOLDER + "/" + MODEL_ROOT_FILE;
		var modelFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, "");

		// Register the default resource factory -- only needed for stand-alone!
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put(Resource.Factory.Registry.DEFAULT_EXTENSION,
				new XMIResourceFactoryImpl());

		// Create the model resource file
		final IFile projectResourceFile = EclipseIDEService.createOrUpdateFile(modelFile);

		// Get the URI of the model file
		final URI fileURI = URI.createFileURI(projectResourceFile.getLocationURI().getPath());

		// Create a resource for this file
		final Resource resource = resourceSet.createResource(fileURI);

		// Add objects to the project resource
		resource.getContents().add(project);

		resource.getContents().add(project.getBoundaryNamespace());
		resource.getContents().add(project.getRepositoryNamespace());
		resource.getContents().add(project.getDTONamespace());
		resource.getContents().add(project.getExchangeNamespace());

		project.getIntegrationModules().forEach(module -> resource.getContents().add(module.getNamespace()));
		project.getTestModules().forEach(module -> resource.getContents().add(module.getNamespace()));
		project.getFormGroups().forEach(formGroup -> resource.getContents().add(formGroup));
		project.getBoundaryNamespace().getChildNamespaces().forEach(namespace -> resource.getContents().add(namespace));
		project.getDTONamespace().getChildNamespaces().forEach(namespace -> resource.getContents().add(namespace));
		project.getExchangeNamespace().getChildNamespaces().forEach(namespace -> resource.getContents().add(namespace));
		project.getRepositoryNamespace().getChildNamespaces().forEach(namespace -> resource.getContents().add(namespace));

		// Make the project page aware of the new project!
		pageProject.setProject(project);

		for (final Namespace n : project.getDomainNamespace().getChildNamespaces()) {
			path = MODEL_FOLDER + "/" + MODEL_SUB + n.getName() + "." + MODEL_FILE_EXTENSION;
			modelFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, "");

			// Create the namespace file
			final IFile namesSpaceFile = EclipseIDEService.createOrUpdateFile(modelFile);

			final URI namespaceURI = URI.createFileURI(namesSpaceFile.getLocationURI().getPath());

			// The project page saves all domain model URIs in order to create diagram files afterwards
			pageProject.getModelURISet().add(namespaceURI);

			// Create a resource for this file
			final Resource namespaceResource = resourceSet.createResource(namespaceURI);

			// Add a namespace to the resource
			namespaceResource.getContents().add(n);

			namespaceResource.save(Collections.emptyMap());
		}

		resource.save(Collections.emptyMap());
	}

	/**
	 * Generate domain objects
	 */
	private void createInitialModel() {
		final DatabaseTemplate template = pageDBCon.getDatabaseTemplate();

		// Create domain objects
		project = ProjectFactory.eINSTANCE.createProject();
		project.setName(pageProject.getName());
		project.setCode(pageProject.getProjectCode());
		project.setProtectManualChanges(pageProject.isProtectManualChanges());
		project.setBuildTool(pageBuildConfig.getBuildTool());
		project.getBuildConfiguration().addAll(pageBuildConfig.getBuildConfiguration());
		project.getIntegrationModules().addAll(pageIntegration.getIntegrationModules());
		project.getTestModules().addAll(pageTesting.getTestModules());
		project.setMappingStrategy(pageIntegration.getMappingStrategy());
		project.setDefaultXMLMappingType(pageIntegration.getXMLMappingType());
		project.setXmlNamespace(pageIntegration.getXmlNamespace());
		project.setXmlNamespacePrefix(pageIntegration.getXmlNamespacePrefix());

		final FormGroup group = ClientFactory.eINSTANCE.createFormGroup();
		group.setGroupOrder(1);
		group.setName("Master data");
		group.setProject(project);

		project.getFormGroups().add(group);

		// Add the root namespace
		final Namespace rootNamespace = JavaFactory.eINSTANCE.createNamespace();
		rootNamespace.setName(pageProject.getRootNamespaceName());
		rootNamespace.setProject(project);

		project.setRootNamespace(rootNamespace);

		// Add the client namespace
		final Namespace clientNamespace = JavaFactory.eINSTANCE.createNamespace();
		clientNamespace.setName(pageApplication.getClientNamespace());
		clientNamespace.setParent(rootNamespace);
		clientNamespace.setProject(project);

		project.setClientNamespace(clientNamespace);

		// Add the repository namespace
		final Namespace repositoryNamespace = JavaFactory.eINSTANCE.createNamespace();
		repositoryNamespace.setName(pageProject.getRepositoryNamespaceName());
		repositoryNamespace.setParent(rootNamespace);
		repositoryNamespace.setProject(project);

		project.setRepositoryNamespace(repositoryNamespace);

		// Add the DTO namespace
		final Namespace dtoNamespace = JavaFactory.eINSTANCE.createNamespace();
		dtoNamespace.setName(pageProject.getDTONamespaceName());
		dtoNamespace.setParent(rootNamespace);
		dtoNamespace.setProject(project);

		project.setDTONamespace(dtoNamespace);

		// Add the exchange namespace
		final Namespace exchangeNamespace = JavaFactory.eINSTANCE.createNamespace();
		exchangeNamespace.setName(pageProject.getExchangeNamespaceName());
		exchangeNamespace.setParent(rootNamespace);
		exchangeNamespace.setProject(project);

		project.setExchangeNamespace(exchangeNamespace);

		// Add the domain namespace
		final DomainNamespace domainObjectNamespace = DomainFactory.eINSTANCE.createDomainNamespace();
		domainObjectNamespace.setName(pageProject.getDomainNamespaceName());
		domainObjectNamespace.setParent(rootNamespace);
		domainObjectNamespace.setProject(project);

		project.setDomainNamespace(domainObjectNamespace);

		// Add the boundary namespace
		final Namespace boundaryNamespace = JavaFactory.eINSTANCE.createNamespace();
		boundaryNamespace.setName(pageProject.getBoundaryNamespaceName());
		boundaryNamespace.setParent(rootNamespace);
		boundaryNamespace.setProject(project);

		project.setBoundaryNamespace(boundaryNamespace);

		// Add all sub-namespaces
		for (final String subNamespaceName : pageProject.getSubNamespaceNames()) {
			final Namespace repositorySubNamespace = JavaFactory.eINSTANCE.createNamespace();
			repositorySubNamespace.setName(subNamespaceName);
			repositorySubNamespace.setParent(project.getRepositoryNamespace());
			repositorySubNamespace.setProject(project);

			project.getRepositoryNamespace().getChildNamespaces().add(repositorySubNamespace);

			final Namespace boundarySubNamespace = JavaFactory.eINSTANCE.createNamespace();
			boundarySubNamespace.setName(subNamespaceName);
			boundarySubNamespace.setParent(project.getBoundaryNamespace());
			boundarySubNamespace.setProject(project);

			project.getBoundaryNamespace().getChildNamespaces().add(boundarySubNamespace);

			final Namespace dtoSubNamespace = JavaFactory.eINSTANCE.createNamespace();
			dtoSubNamespace.setName(subNamespaceName);
			dtoSubNamespace.setParent(project.getDTONamespace());
			dtoSubNamespace.setProject(project);

			project.getDTONamespace().getChildNamespaces().add(dtoSubNamespace);

			final Namespace exchangeSubNamespace = JavaFactory.eINSTANCE.createNamespace();
			exchangeSubNamespace.setName(subNamespaceName);
			exchangeSubNamespace.setParent(project.getExchangeNamespace());
			exchangeSubNamespace.setProject(project);

			project.getExchangeNamespace().getChildNamespaces().add(exchangeSubNamespace);

			final DomainNamespace subDomainNamespace = DomainFactory.eINSTANCE.createDomainNamespace();
			subDomainNamespace.setName(subNamespaceName);
			subDomainNamespace.setParent(project.getDomainNamespace());
			subDomainNamespace.setProject(project);

			project.getDomainNamespace().getChildNamespaces().add(subDomainNamespace);
		}

		// Finish the initialization of the integration namespaces
		project.getIntegrationModules().stream().map(IntegrationModule::getNamespace).forEach(integrationNamespace -> {
			integrationNamespace.setProject(project);
			integrationNamespace.setParent(project.getRootNamespace());
		});

		// Finish the initialization of test module namespaces
		project.getTestModules().stream().map(AbstractTestModule::getNamespace).forEach(testModuleNamespace -> {
			testModuleNamespace.setProject(project);
			testModuleNamespace.setParent(project.getRootNamespace());
		});

		// Add the data source
		final Datasource dataSource = ProjectFactory.eINSTANCE.createDatasource();
		dataSource.setConnectionURL(pageDBCon.getConnectionURL());
		dataSource.setName(pageDBCon.getDSName());
		dataSource.setPassword(pageDBCon.getPassword());
		dataSource.setUserName(pageDBCon.getUserName());
		dataSource.setDriverName(pageDBCon.getDriverName());

		if (pageDBCon.getSelectedLibraries().isEmpty()) {
			final String vendorGroupDirectory = template.getVendorGroup().toString().toLowerCase();
			final var userHomeDirectory = new File(System.getProperty("user.home"));
			final var libDirectory = new File(userHomeDirectory, LOCAL_JDBC_LIB_PATH + vendorGroupDirectory);

			if (libDirectory.exists() && libDirectory.isDirectory())
				for (final var file : libDirectory.listFiles())
					dataSource.getDriverList().add(file.getAbsolutePath());
		}
		else
			dataSource.getDriverList().addAll(pageDBCon.getSelectedLibraries());

		project.setDataSource(dataSource);
		project.setClientPlatform(pageApplication.getClientPlatform());
		project.setServerPlatform(pageApplication.getServerPlatform());
		project.setPersistenceProvider(pageApplication.getPersistenceProvider());
		project.setJpaVersion(pageApplication.getJPAVersion());
		project.setTechnology(pageApplication.getTechnologyPlatform());
		project.setValidationType(pageApplication.getValidationType());
		project.setBoundaryMode(true);

		if (!pageProject.isBoundaryMode() && pageApplication.isFacadeModeSupported())
			project.setBoundaryMode(false);

		// Add default roles
		final Role roleAdmin = ProjectFactory.eINSTANCE.createRole();
		roleAdmin.setName("ADMINISTRATOR");
		roleAdmin.setAdminRole(true);
		roleAdmin.setReadonlyRole(false);

		project.getRoles().add(roleAdmin);

		final Role roleReadonly = ProjectFactory.eINSTANCE.createRole();
		roleReadonly.setName("READONLY");
		roleReadonly.setAdminRole(false);
		roleReadonly.setReadonlyRole(true);

		project.getRoles().add(roleReadonly);

		// Add the administrator role to the default form group
		group.getRoles().add(roleAdmin);

		final Namespace nsUtil = JavaFactory.eINSTANCE.createNamespace();
		nsUtil.setName(PACK_JAVA_UTIL);
		nsUtil.setProject(project);

		project.getSupportedStandardNamespaces().add(nsUtil);

		final Namespace nsMath = JavaFactory.eINSTANCE.createNamespace();
		nsMath.setName(PACK_JAVA_MATH);
		nsMath.setProject(project);

		project.getSupportedStandardNamespaces().add(nsMath);

		final Namespace nsTime = JavaFactory.eINSTANCE.createNamespace();
		nsTime.setName(PACK_JAVA_TIME);
		nsTime.setProject(project);

		project.getSupportedStandardNamespaces().add(nsTime);

		// Add all supported Java types
		final JavaType voidTypePrimitive = JavaFactory.eINSTANCE.createJavaType();
		voidTypePrimitive.setName(JavaType.VOID);
		voidTypePrimitive.setMappable(false);
		voidTypePrimitive.setPrimitive(true);

		project.getAllSupportedTypes().add(voidTypePrimitive);

		final JavaType intTypePrimitive = JavaFactory.eINSTANCE.createJavaType();
		intTypePrimitive.setName(JavaType.INT);
		intTypePrimitive.setMappable(true);
		intTypePrimitive.setPrimitive(true);

		project.getAllSupportedTypes().add(intTypePrimitive);

		final JavaType longTypePrimitive = JavaFactory.eINSTANCE.createJavaType();
		longTypePrimitive.setName(JavaType.LONG);
		longTypePrimitive.setMappable(true);
		longTypePrimitive.setPrimitive(true);

		project.getAllSupportedTypes().add(longTypePrimitive);

		final JavaType doubleTypePrimitive = JavaFactory.eINSTANCE.createJavaType();
		doubleTypePrimitive.setName(JavaType.DOUBLE);
		doubleTypePrimitive.setMappable(true);
		doubleTypePrimitive.setPrimitive(true);

		project.getAllSupportedTypes().add(doubleTypePrimitive);

		final JavaType booleanTypePrimitive = JavaFactory.eINSTANCE.createJavaType();
		booleanTypePrimitive.setName(JavaType.BOOL);
		booleanTypePrimitive.setMappable(true);
		booleanTypePrimitive.setPrimitive(true);

		project.getAllSupportedTypes().add(booleanTypePrimitive);

		final JavaType floatTypePrimitive = JavaFactory.eINSTANCE.createJavaType();
		floatTypePrimitive.setName(JavaType.FLOAT);
		floatTypePrimitive.setMappable(true);
		floatTypePrimitive.setPrimitive(true);

		project.getAllSupportedTypes().add(floatTypePrimitive);

		final JavaType integerType = JavaFactory.eINSTANCE.createJavaType();
		integerType.setName(JavaType.INTEGER);
		integerType.setMappable(true);
		integerType.setPrimitive(false);

		project.getAllSupportedTypes().add(integerType);

		final JavaType longType = JavaFactory.eINSTANCE.createJavaType();
		longType.setName(JavaType.LONG_OBJ);
		longType.setMappable(true);
		longType.setPrimitive(false);

		project.getAllSupportedTypes().add(longType);

		final JavaType doubleType = JavaFactory.eINSTANCE.createJavaType();
		doubleType.setName(JavaType.DOUBLE_OBJ);
		doubleType.setMappable(true);
		doubleType.setPrimitive(false);

		project.getAllSupportedTypes().add(doubleType);

		final JavaType booleanType = JavaFactory.eINSTANCE.createJavaType();
		booleanType.setName(JavaType.BOOL_OBJ);
		booleanType.setMappable(true);
		booleanType.setPrimitive(false);

		project.getAllSupportedTypes().add(booleanType);

		final JavaType floatType = JavaFactory.eINSTANCE.createJavaType();
		floatType.setName(JavaType.FLOAT_OBJ);
		floatType.setMappable(true);
		floatType.setPrimitive(false);

		project.getAllSupportedTypes().add(floatType);

		final JavaType bigDecimalType = JavaFactory.eINSTANCE.createJavaType();
		bigDecimalType.setName(JavaType.BIG_DECIMAL);
		bigDecimalType.setMappable(true);
		bigDecimalType.setPrimitive(false);
		nsMath.getJavaTypes().add(bigDecimalType);

		project.getAllSupportedTypes().add(bigDecimalType);

		final JavaType stringType = JavaFactory.eINSTANCE.createJavaType();
		stringType.setName(JavaType.STRING);
		stringType.setMappable(true);
		stringType.setPrimitive(false);

		project.getAllSupportedTypes().add(stringType);

		final JavaType charType = JavaFactory.eINSTANCE.createJavaType();
		charType.setName(JavaType.CHAR);
		charType.setMappable(true);
		charType.setPrimitive(true);

		project.getAllSupportedTypes().add(charType);

		final JavaType dateType = JavaFactory.eINSTANCE.createJavaType();
		dateType.setName(JavaType.DATE);
		dateType.setMappable(true);
		dateType.setPrimitive(false);
		dateType.setNamespace(nsUtil);
		nsUtil.getJavaTypes().add(dateType);

		project.getAllSupportedTypes().add(dateType);

		final JavaType gregorianCalendarType = JavaFactory.eINSTANCE.createJavaType();
		gregorianCalendarType.setName(JavaType.GREGORIAN_CAL);
		gregorianCalendarType.setMappable(true);
		gregorianCalendarType.setPrimitive(false);
		gregorianCalendarType.setNamespace(nsUtil);
		nsUtil.getJavaTypes().add(gregorianCalendarType);

		project.getAllSupportedTypes().add(gregorianCalendarType);

		final JavaType localDateType = JavaFactory.eINSTANCE.createJavaType();
		localDateType.setName(JavaType.LOCAL_DATE);
		localDateType.setMappable(true);
		localDateType.setPrimitive(false);
		localDateType.setNamespace(nsTime);
		nsTime.getJavaTypes().add(localDateType);

		project.getAllSupportedTypes().add(localDateType);

		final JavaType localDateTimeType = JavaFactory.eINSTANCE.createJavaType();
		localDateTimeType.setName(JavaType.LOCAL_DATE_TIME);
		localDateTimeType.setMappable(true);
		localDateTimeType.setPrimitive(false);
		localDateTimeType.setNamespace(nsTime);
		nsTime.getJavaTypes().add(localDateTimeType);

		project.getAllSupportedTypes().add(localDateTimeType);

		final JavaType hashMapType = JavaFactory.eINSTANCE.createJavaType();
		hashMapType.setName(JavaType.HASH_MAP);
		hashMapType.setMappable(false);
		hashMapType.setPrimitive(false);
		hashMapType.setNamespace(nsUtil);
		nsUtil.getJavaTypes().add(hashMapType);

		project.getAllSupportedTypes().add(hashMapType);

		final JavaType byteArrayType = JavaFactory.eINSTANCE.createJavaType();
		byteArrayType.setName(JavaType.BYTE_ARRAY);
		byteArrayType.setMappable(true);
		byteArrayType.setPrimitive(false);

		project.getAllSupportedTypes().add(byteArrayType);

		final JavaType byteObjArrayType = JavaFactory.eINSTANCE.createJavaType();
		byteObjArrayType.setName(JavaType.BYTE_OBJ_ARRAY);
		byteObjArrayType.setMappable(true);
		byteObjArrayType.setPrimitive(false);

		project.getAllSupportedTypes().add(byteObjArrayType);

		final JavaType uuidType = JavaFactory.eINSTANCE.createJavaType();
		uuidType.setName(JavaType.UUID);
		uuidType.setMappable(true);
		uuidType.setPrimitive(false);
		uuidType.setNamespace(nsUtil);
		nsUtil.getJavaTypes().add(uuidType);

		project.getAllSupportedTypes().add(uuidType);

		// Create the database based on the selected template
		final Database db = DbFactory.eINSTANCE.createDatabase();
		db.setProject(project);
		db.setSchemaName(pageDBCon.getSchemaName());
		db.setCatalogName(pageDBCon.getCatalogName());
		db.setEclipseLinkTargetDBName(template.getEclipseLinkTargetDBName());
		db.setHibernateDialect(template.getHibernateDialect());
		db.setIdentifierRegEx(template.getIdentifierRegEx());
		db.setIdentifierStyle(template.getIdentifierStyle());
		db.setMaxIdentifierLength(template.getMaxIdentifierLength());
		db.setReservedWords(template.getReservedWords());
		db.setSupportsIdentityColumn(template.isIdentityColumnSupport());
		db.setSupportsSequence(template.isSequenceSupport());
		db.setVendorGroup(template.getVendorGroup());

		for (final net.codecadenza.eclipse.resource.db.DBColumnType templateColType : template.getMappings()) {
			final DBColumnType columnType = DbFactory.eINSTANCE.createDBColumnType();
			columnType.setName(templateColType.getName());
			columnType.setOmitSizeInformation(templateColType.isOmitSizeInformation());

			db.getAllSupportedColumnTypes().add(columnType);

			for (final net.codecadenza.eclipse.resource.db.JavaType type : templateColType.getJavaTypes())
				for (final JavaType javaType : project.getAllSupportedTypes())
					if (javaType.getName().equals(type.getName())) {
						columnType.getJavaTypes().add(javaType);
						break;
					}
		}

		project.setDatabase(db);
	}

}
