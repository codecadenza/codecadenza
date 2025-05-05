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
package net.codecadenza.eclipse.tools.jpaeditor.util;

import static net.codecadenza.eclipse.shared.Constants.DRIVER_DERBY;
import static net.codecadenza.eclipse.shared.Constants.DRIVER_DERBY_EMBEDDED;
import static net.codecadenza.eclipse.shared.Constants.DRIVER_MSSQL;
import static net.codecadenza.eclipse.shared.Constants.DRIVER_MYSQL;
import static net.codecadenza.eclipse.shared.Constants.DRIVER_ORACEL;
import static net.codecadenza.eclipse.shared.Constants.DRIVER_POSTGRESQL;
import static net.codecadenza.eclipse.shared.Constants.ECLIPSE_LINK_CONFIG;
import static net.codecadenza.eclipse.shared.Constants.JAVA_RESOURCE_SUFFIX;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import java.io.File;
import java.io.FileInputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.hibernate.cfg.Configuration;
import org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl;
import org.hibernate.jpa.boot.internal.PersistenceUnitInfoDescriptor;

/**
 * <p>
 * Persistence engine for providing an {@link EntityManager} for the JPA query editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PersistenceEngine {
	private static final String CLASS_FILE_SUFFIX = ".class";
	private static final String ECLIPSE_LINK_CONFIG_CLASS_NAME = ECLIPSE_LINK_CONFIG + JAVA_RESOURCE_SUFFIX;
	private static final String LIB_JPA = "codecadenza-jpa";
	private static final String LIB_UTIL = "codecadenza-util";
	private static final String PACKAGE_FILTER_JPA = "net/codecadenza/runtime/jpa/converter";
	private static final String PACKAGE_FILTER_UTIL = "net/codecadenza/runtime/util/conversion";

	private EntityManagerFactory emf;

	/**
	 * Constructor
	 */
	private PersistenceEngine() {

	}

	/**
	 * Initialize the persistence engine
	 * @param project
	 * @return the initialized persistence engine
	 */
	public static synchronized PersistenceEngine initialize(Project project) {
		final var config = new Configuration();
		final var engine = new PersistenceEngine();
		final DBVendorGroupEnumeration vendor = project.getDatabase().getVendorGroup();
		final String schemaName = project.getDatabase().getSchemaName();
		final String catalogName = project.getDatabase().getCatalogName();
		final var props = new Properties();

		if (vendor == DBVendorGroupEnumeration.MYSQL) {
			props.put("hibernate.dialect", "org.hibernate.dialect.MySQLDialect");
			props.put("hibernate.connection.driver_class", DRIVER_MYSQL);
		}
		else if (vendor == DBVendorGroupEnumeration.ORACLE) {
			props.put("hibernate.dialect", "org.hibernate.dialect.Oracle9Dialect");
			props.put("hibernate.connection.driver_class", DRIVER_ORACEL);
		}
		else if (vendor == DBVendorGroupEnumeration.DERBY) {
			props.put("hibernate.dialect", "org.hibernate.dialect.DerbyDialect");
			props.put("hibernate.connection.driver_class", DRIVER_DERBY);
		}
		else if (vendor == DBVendorGroupEnumeration.DERBY_EMBEDDED) {
			props.put("hibernate.dialect", "org.hibernate.dialect.DerbyDialect");
			props.put("hibernate.connection.driver_class", DRIVER_DERBY_EMBEDDED);
		}
		else if (vendor == DBVendorGroupEnumeration.POSTGRESQL) {
			props.put("hibernate.dialect", "org.hibernate.dialect.PostgreSQLDialect");
			props.put("hibernate.connection.driver_class", DRIVER_POSTGRESQL);
		}
		else if (vendor == DBVendorGroupEnumeration.MSSQL) {
			props.put("hibernate.dialect", "org.hibernate.dialect.SQLServerDialect");
			props.put("hibernate.connection.driver_class", DRIVER_MSSQL);
		}

		props.put("hibernate.connection.username", project.getDataSource().getUserName());
		props.put("hibernate.connection.password", project.getDataSource().getPassword());
		props.put("hibernate.connection.url", project.getDataSource().getConnectionURL());
		props.put("hibernate.show_sql", "false");

		if (schemaName != null && !schemaName.isEmpty())
			props.put("hibernate.default_schema", schemaName);

		if (catalogName != null && !catalogName.isEmpty())
			props.put("hibernate.default_catalog", catalogName);

		// Disable validation!
		props.put("jakarta.persistence.validation.mode", "NONE");

		config.setProperties(props);

		try {
			final var allClasses = new ArrayList<>(getClassesOfPersistenceUnit(project, engine, BuildArtifactType.SHARED));
			allClasses.addAll(getClassesOfPersistenceUnit(project, engine, BuildArtifactType.DOMAIN));

			final var persistenceUnitInfo = new InternalPersistenceUnitInfo(allClasses, props);
			final var persistenceUnitInfoDescriptor = new PersistenceUnitInfoDescriptor(persistenceUnitInfo);

			engine.emf = new EntityManagerFactoryBuilderImpl(persistenceUnitInfoDescriptor, Collections.emptyMap()).build();

			return engine;
		}
		catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * @return the entity manager
	 */
	public synchronized EntityManager getEntityManager() {
		try {
			return emf.createEntityManager();
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
		}

		return null;
	}

	/**
	 * Shutdown the persistence engine
	 */
	public synchronized void close() {
		if (emf != null && emf.isOpen())
			emf.close();
	}

	/**
	 * Determine all classes of a persistence unit
	 * @param project
	 * @param engine
	 * @param artifactType
	 * @return a list containing all classes of a persistence unit
	 * @throws Exception if an internal error has occurred
	 */
	private static List<String> getClassesOfPersistenceUnit(Project project, PersistenceEngine engine,
			BuildArtifactType artifactType) throws Exception {
		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject proj = workspaceRoot.getProject(project.getTargetProjectName(artifactType));
		final IFolder folder = proj.getFolder(project.getSourceFolder());
		final IJavaProject javaProject = JavaCore.create(proj);
		final ClassLoader classLoader = engine.getClassLoader(javaProject, project, true);
		final var classList = new ArrayList<Class<?>>();
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final var subPackages = new ArrayList<IPackageFragment>();
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(project.getDomainNamespace().toString());

		Thread.currentThread().setContextClassLoader(classLoader);

		subPackages.add(fragment);
		subPackages.addAll(engine.getSubPackages(fragment, project));

		for (final IPackageFragment packageFragment : subPackages)
			for (final IJavaElement javaElement : packageFragment.getCompilationUnits())
				if (javaElement.getElementType() == IJavaElement.COMPILATION_UNIT) {
					var className = packageFragment.getElementName() + "." + javaElement.getElementName();
					className = className.substring(0, className.lastIndexOf('.'));

					// The configuration class for EclipseLink must not be added!
					if (javaElement.getElementName().equals(ECLIPSE_LINK_CONFIG_CLASS_NAME))
						continue;

					classList.add(Class.forName(className, false, classLoader));
				}

		// Try to add the converter classes to the persistence unit
		addClassesOfRuntimeLibrary(javaProject, classList, classLoader, LIB_JPA, PACKAGE_FILTER_JPA);
		addClassesOfRuntimeLibrary(javaProject, classList, classLoader, LIB_UTIL, PACKAGE_FILTER_UTIL);

		return classList.stream().map(Class::getName).toList();
	}

	/**
	 * Add all necessary classes of the given library to the list of classes that must be added to the persistence context
	 * @param javaProject the Java project
	 * @param classList a list to add the classes to
	 * @param classLoader the class loader
	 * @param libraryName the name of the library
	 * @param packageFilter the filter that controls what classes should be added
	 * @throws Exception if a class could not be found
	 */
	private static void addClassesOfRuntimeLibrary(IJavaProject javaProject, ArrayList<Class<?>> classList, ClassLoader classLoader,
			String libraryName, String packageFilter) throws Exception {
		final URL runtimeLibURL = getRuntimeLibraryURL(javaProject, libraryName);

		if (runtimeLibURL != null) {
			final Path path = Path.of(runtimeLibURL.toURI());

			try (final ZipInputStream zip = new ZipInputStream(new FileInputStream(path.toFile()))) {
				for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
					if (!entry.isDirectory() && entry.getName().contains(packageFilter) && entry.getName().endsWith(CLASS_FILE_SUFFIX)) {
						String className = entry.getName().replace('/', '.');
						className = className.substring(0, className.length() - CLASS_FILE_SUFFIX.length());

						classList.add(Class.forName(className, true, classLoader));
					}
				}
			}
		}
	}

	/**
	 * Get all packages that contain classes of a persistence unit
	 * @param packageFragment
	 * @param project
	 * @return a list containing all packages
	 * @throws Exception if an internal error has occurred
	 */
	private List<IPackageFragment> getSubPackages(IPackageFragment packageFragment, Project project) throws Exception {
		final IJavaElement[] allPackages = ((IPackageFragmentRoot) packageFragment.getParent()).getChildren();
		final var packages = new ArrayList<IPackageFragment>();

		for (final IJavaElement aPackage : allPackages) {
			if (!(aPackage instanceof final IPackageFragment fragment))
				continue;

			// The domain object namespace names of the project can be used in order to determine if the package must be added to the
			// result list!
			for (final Namespace namespace : project.getDomainNamespace().getChildNamespaces())
				if (aPackage.getElementName().contains(namespace.toString()))
					packages.add(fragment);
		}

		return packages;
	}

	/**
	 * Get the classpath URLs of the project
	 * @param javaProject
	 * @param project
	 * @return a list of URLs
	 * @throws Exception if an internal error has occurred
	 * @throws IllegalStateException if the path to the JDBC driver doesn't refer to a file or the file doesn't exist
	 */
	private List<URL> getClassPathURLs(IJavaProject javaProject, Project project) throws Exception {
		final var paths = new ArrayList<URL>();
		final IPath location = getProjectLocation(javaProject.getProject());
		final IPath outputPath = location.append(javaProject.getOutputLocation().removeFirstSegments(1));

		// Add the output path in order to include all classes of this project
		paths.add(outputPath.toFile().toURI().toURL());

		// Add all JDBC drivers of this project
		for (final String path : project.getDataSource().getDriverList()) {
			final var driverFile = new File(path);

			if (!driverFile.exists())
				throw new IllegalStateException("The JDBC driver '" + driverFile.getName() + "' could not be found!");

			if (!driverFile.isFile())
				throw new IllegalStateException("The JDBC driver '" + driverFile.getName() + "' is not a file!");

			paths.add(driverFile.toURI().toURL());
		}

		final URL jpaLibURL = getRuntimeLibraryURL(javaProject, LIB_JPA);
		final URL utilLibURL = getRuntimeLibraryURL(javaProject, LIB_UTIL);

		if (jpaLibURL != null)
			paths.add(jpaLibURL);

		if (utilLibURL != null)
			paths.add(utilLibURL);

		return paths;
	}

	/**
	 * Get the project location
	 * @param project
	 * @return the location
	 */
	private IPath getProjectLocation(IProject project) {
		return (project.getRawLocation() != null ? project.getRawLocation() : project.getLocation());
	}

	/**
	 * Get the class loader of the project by using the parent class loader
	 * @param javaProject
	 * @param project
	 * @param useParentClassLoader
	 * @return the class loader
	 * @throws Exception if an internal error has occurred
	 */
	private ClassLoader getClassLoader(IJavaProject javaProject, Project project, boolean useParentClassLoader) throws Exception {
		final List<URL> paths = getClassPathURLs(javaProject, project);

		if (useParentClassLoader)
			return new URLClassLoader(paths.toArray(new URL[paths.size()]), Thread.currentThread().getContextClassLoader());

		return new URLClassLoader(paths.toArray(new URL[paths.size()]));
	}

	/**
	 * Get the URL of the runtime library of the given project
	 * @param project the project that contains the given library
	 * @param libraryName the name of the library to search for
	 * @return the URL of the given runtime library or null if it could not be found
	 * @throws Exception if the URL could not be created
	 */
	private static URL getRuntimeLibraryURL(IJavaProject project, String libraryName) throws Exception {
		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();

		for (final IClasspathEntry entry : project.getResolvedClasspath(true))
			if (entry.getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
				final String entryPath = entry.getPath().toOSString();

				if (entryPath.contains(libraryName)) {
					final File libFile = new File(entryPath);

					if (libFile.exists())
						return libFile.toURI().toURL();

					// In the case of an Eclipse RCP/RAP application the library path is relative to the workspace location!
					return workspaceRoot.getLocation().append(libFile.getPath()).toFile().toURI().toURL();
				}
			}

		return null;
	}

}
