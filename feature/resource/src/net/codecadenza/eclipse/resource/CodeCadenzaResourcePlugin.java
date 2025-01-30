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
package net.codecadenza.eclipse.resource;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.db.DatabaseTemplate;
import net.codecadenza.eclipse.resource.db.DatabaseTemplateRoot;
import net.codecadenza.eclipse.resource.dependency.ConfigRootMappingType;
import net.codecadenza.eclipse.resource.dependency.DependencyConfigMappingType;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import org.xml.sax.InputSource;

/**
 * <p>
 * The activator class controls the plug-in life cycle and provides access methods for resources
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaResourcePlugin extends AbstractUIPlugin {
	// Relative path to plug-in images
	private static final String IMAGE_DEFAULT_PATH = "icons/obj16/";
	private static final String IMAGE_WIZBAN_PATH = "icons/wizban/";
	private static final String BUILD_PATH_ADDON = "/build/addons/";
	private static final String BUILD_PATH_CONTENT = "/build/content/";
	private static final String DUMMY_JAR = "dummy.jar";
	private static final String CONFIG_FILE_DB = "/config/db_templates.xml";
	private static final String CONFIG_FILE_TAG = "/config/tagmapping.conf";
	private static final String CONFIG_FILE_DEPENDENCY = "/config/dependencies.xml";
	// The plug-in ID
	public static final String PLUGIN_ID = "net.codecadenza.eclipse.resource";

	// The shared instance
	private static CodeCadenzaResourcePlugin plugin;

	private static Properties tagMappingProperties;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/**
	 * @return a list containing predefined database template objects loaded from an internal XML file
	 * @throws Exception if the database template file either could not be found, or if it could not be parsed
	 */
	public static EList<DatabaseTemplate> getDatabaseTemplates() throws Exception {
		final URL fileURL = getInstance().getBundle().getEntry(CONFIG_FILE_DB);
		final var databaseList = new BasicEList<DatabaseTemplate>();
		final Unmarshaller unmarshaller = JAXBContext.newInstance(DatabaseTemplateRoot.class).createUnmarshaller();
		final var is = new InputSource(new InputStreamReader(fileURL.openStream(), StandardCharsets.UTF_8));
		final var templateRoot = (DatabaseTemplateRoot) unmarshaller.unmarshal(is);

		templateRoot.getDatabaseTemplates().forEach(databaseList::add);

		return databaseList;
	}

	/**
	 * @return a list containing dependency configuration objects loaded from an internal XML file
	 * @throws Exception if the dependency configuration file either could not be found, or if it could not be parsed
	 */
	public static List<DependencyConfigMappingType> getDependencyConfiguration() throws Exception {
		final URL fileURL = getInstance().getBundle().getEntry(CONFIG_FILE_DEPENDENCY);
		final Unmarshaller unmarshaller = JAXBContext.newInstance(ConfigRootMappingType.class).createUnmarshaller();
		final var is = new InputSource(new InputStreamReader(fileURL.openStream(), StandardCharsets.UTF_8));
		final var templateRoot = (ConfigRootMappingType) unmarshaller.unmarshal(is);

		return templateRoot.getDependencyConfigurations();
	}

	/**
	 * Get the wizard image from the image registry
	 * @param name the unique name of the image
	 * @return the image object
	 */
	public static Image getWizardImage(String name) {
		if (plugin.getImageRegistry().get(name) == null)
			plugin.getImageRegistry().put(name, getImageDescriptor(IMAGE_WIZBAN_PATH, name).createImage());

		return plugin.getImageRegistry().get(name);
	}

	/**
	 * Get an image descriptor for the image file
	 * @param path
	 * @param name
	 * @return the image descriptor
	 */
	private static ImageDescriptor getImageDescriptor(String path, String name) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path + name);
	}

	/**
	 * Get an image descriptor for the image file
	 * @param name
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String name) {
		return imageDescriptorFromPlugin(PLUGIN_ID, IMAGE_DEFAULT_PATH + name);
	}

	/**
	 * Get an image from the image registry
	 * @param name the unique name of the image
	 * @return the image object
	 */
	public static Image getImage(String name) {
		if (plugin.getImageRegistry().get(name) == null)
			plugin.getImageRegistry().put(name, getImageDescriptor(IMAGE_DEFAULT_PATH, name).createImage());

		return plugin.getImageRegistry().get(name);
	}

	/**
	 * @return the URI of the dummy library
	 * @throws URISyntaxException if the URI is invalid
	 */
	public static URI getDummyLibrary() throws URISyntaxException {
		return getInstance().getBundle().getEntry(BUILD_PATH_ADDON + DUMMY_JAR).toURI();
	}

	/**
	 * @param project
	 * @return the URL to client content resources
	 */
	public static URL getContentResourcePath(Project project) {
		if (project.hasJSFOrVaadinClient() || project.hasAngularClient() || project.hasJavaFXClient()) {
			final var fileName = "content_" + project.getClientPlatform().getName().toLowerCase() + ".zip";

			return getInstance().getBundle().getEntry(BUILD_PATH_CONTENT + fileName);
		}

		return getInstance().getBundle().getEntry(BUILD_PATH_CONTENT + "icons.zip");
	}

	/**
	 * @param project
	 * @param tag
	 * @return a set containing all valid types for this attribute tag
	 */
	public static synchronized Set<JavaType> getValidJavaTypesOfTag(Project project, AttributeTagEnumeration tag) {
		final var validTypesSet = new HashSet<JavaType>();

		// Check if the properties are loaded! Initially, load the properties from the respective file!
		if (tagMappingProperties == null) {
			// We assume that the configuration property file exists, and that it can be accessed!
			try (InputStream in = getInstance().getBundle().getEntry(CONFIG_FILE_TAG).openStream()) {
				tagMappingProperties = new Properties();
				tagMappingProperties.load(in);
			}
			catch (final Exception e) {
				getInstance().logError(e);
				return validTypesSet;
			}
		}

		// Get the allowed data types of this tag
		final String types = tagMappingProperties.getProperty(tag.getName());

		if (types == null || types.isEmpty())
			return validTypesSet;

		// There might be multiple types allowed for a tag
		final String[] typeNames = types.split(",");

		for (final String name : typeNames) {
			final JavaType t = project.getJavaTypeByName(name);

			if (t != null)
				validTypesSet.add(t);
		}

		return validTypesSet;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * @return the shared instance
	 */
	public static CodeCadenzaResourcePlugin getInstance() {
		return plugin;
	}

	/**
	 * @param throwable
	 */
	public void logError(Throwable throwable) {
		logError(null, throwable);
	}

	/**
	 * @param error
	 * @param throwable
	 */
	public void logError(String error, Throwable throwable) {
		if (error == null && throwable != null)
			error = throwable.getMessage();

		getLog().log(new Status(IStatus.ERROR, PLUGIN_ID, IStatus.OK, error, throwable));
		debug(error, throwable);
	}

	/**
	 * @param message
	 */
	public void logInfo(String message) {
		logInfo(message, null);
	}

	/**
	 * @param message
	 * @param throwable
	 */
	public void logInfo(String message, Throwable throwable) {
		if (message == null && throwable != null)
			message = throwable.getMessage();

		getLog().log(new Status(IStatus.INFO, PLUGIN_ID, IStatus.OK, message, throwable));
		debug(message, throwable);
	}

	/**
	 * @param message
	 * @param throwable
	 */
	private void debug(String message, Throwable throwable) {
		if (!isDebugging())
			return;

		if (message != null)
			System.err.println(message);

		if (throwable != null)
			throwable.printStackTrace();
	}

}
