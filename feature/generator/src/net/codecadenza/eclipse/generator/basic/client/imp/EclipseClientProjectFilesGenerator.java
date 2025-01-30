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
package net.codecadenza.eclipse.generator.basic.client.imp;

import static net.codecadenza.eclipse.shared.Constants.APPLICATION_MODEL_NAME;
import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.CHANGE_PASSWORD_HANDLER_NAME;
import static net.codecadenza.eclipse.shared.Constants.CHANGE_PWD_DLG_NAME;
import static net.codecadenza.eclipse.shared.Constants.CONTRIBUTION_XML;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_ACTIVATOR_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_APPLICATION_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_E4_LIFE_CYCLE_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_IMAGE_CACHE_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_LOG_ON_DLG_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_PART_STACK_ID;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_PLUGIN_ID;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_RAP_APPLICATION_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_RAP_CONTEXT_PATH;
import static net.codecadenza.eclipse.shared.Constants.FORMAT_HANDLER_NAME;
import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;
import static net.codecadenza.eclipse.shared.Constants.ICON_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.LIB_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.META_INF_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.NAV_VIEW_NAME;
import static net.codecadenza.eclipse.shared.Constants.OSGI_INF_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.PLUGIN_XML;
import static net.codecadenza.eclipse.shared.Constants.RCP_ICON_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.common.LoggingGenerator;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.IDGenerator;
import net.codecadenza.eclipse.tools.util.eclipse.EclipseRAPPlugins;
import net.codecadenza.eclipse.tools.util.eclipse.EclipseRCPPlugins;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

/**
 * <p>
 * Generator for basic source and configuration files for Eclipse RCP/RAP applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseClientProjectFilesGenerator extends AbstractClientProjectFilesGenerator {
	private static final String RAP_APPLICATION_COMMENT = "Configuration class of this RAP application";
	private static final String E4_LIFE_CYCLE_COMMENT = "Life cycle class that opens the log-on dialog when the application is starting";
	private static final String IMAGE_CACHE_COMMENT = "Global cache for images that are used by this application";
	private static final String FORMAT_HANDLER_COMMENT = "Action handler for opening the dialog for changing the user preferences";
	private static final String PASSWORD_HANDLER_COMMENT = "Action handler for opening the dialog for changing the user password";
	private static final String ACTIVATOR_COMMENT = "The activator of this application";
	private static final int DEFAULT_KEY_LENGTH = 23;

	private RichClientI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public EclipseClientProjectFilesGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final String securityManagerPackage = project.getClientNamespace().toString() + SUB_PACKAGE_UTIL;
		final var sourceFiles = new ArrayList<JavaFile>();
		i18n = new RichClientI18NGenerator(project);

		final JavaFile activator = createJavaSourceFile(DEFAULT_ACTIVATOR_NAME, createActivator(), ACTIVATOR_COMMENT);
		final JavaFile changePasswordDialog = createJavaSourceFile(CHANGE_PWD_DLG_NAME, createChangePasswordDialog(),
				CHANGE_PWD_COMMENT);
		final JavaFile changePasswordHandler = createJavaSourceFile(CHANGE_PASSWORD_HANDLER_NAME, createChangePasswordHandler(),
				PASSWORD_HANDLER_COMMENT);
		final JavaFile formatHandler = createJavaSourceFile(FORMAT_HANDLER_NAME, createFormatHandler(), FORMAT_HANDLER_COMMENT);
		final JavaFile imageCache = createJavaSourceFile(DEFAULT_IMAGE_CACHE_NAME, createImageCache(), IMAGE_CACHE_COMMENT);
		final JavaFile lifeCycleHandler = createJavaSourceFile(DEFAULT_E4_LIFE_CYCLE_NAME, createE4LifeCycle(),
				E4_LIFE_CYCLE_COMMENT);
		final JavaFile logOnDialog = createJavaSourceFile(DEFAULT_LOG_ON_DLG_NAME, createLogOnDialog(), LOG_ON_DLG_COMMENT);
		final JavaFile securityManager = createJavaSourceFile(SECURITY_MANAGER, createInitalSecurityManager(), securityManagerPackage,
				SECURITY_MANAGER_COMMENT);

		sourceFiles.addAll(List.of(activator, changePasswordDialog, changePasswordHandler, formatHandler, imageCache,
				lifeCycleHandler, logOnDialog, securityManager));

		if (project.hasRAPClient()) {
			final JavaFile application = createJavaSourceFile(DEFAULT_RAP_APPLICATION_NAME, createApplication(),
					RAP_APPLICATION_COMMENT);

			sourceFiles.add(application);
		}

		i18n.save();

		return sourceFiles;
	}

	/**
	 * Create the dialog to change the password of the currently logged on user
	 * @return the generated content
	 */
	private String createChangePasswordDialog() {
		final var b = new StringBuilder();
		b.append("import org.eclipse.swt.widgets.*;\n");
		b.append("import org.eclipse.swt.graphics.Point;\n");
		b.append("import net.codecadenza.runtime.richclient.eclipse.dialog.*;\n");

		if (project.isProtectManualChanges())
			b.append("import " + GENERATED_ELEMENT_ANNOTATION + ";\n");

		b.append("\n");
		b.append("public class " + CHANGE_PWD_DLG_NAME + " extends AbstractChangePasswordDialog\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parentShell\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public " + CHANGE_PWD_DLG_NAME + "(Shell parentShell)\n");
		b.append("{\n");
		b.append("super(parentShell);\n\n");
		b.append("this.parentShell = parentShell;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.dialog.AbstractChangePasswordDialog#");
		b.append("saveNewPassword(java.lang.String, java.lang.String, java.lang.String)\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("@Override\n");
		b.append("public boolean saveNewPassword(String oldPassword, String newPassword, String newPasswordConfirm)\n");
		b.append("{\n");
		b.append("// This implementation is empty as long as security features are not applied to this project!\n");
		b.append("return true;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.dialogs.TitleAreaDialog#getInitialSize()\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("@Override\n");
		b.append("protected Point getInitialSize()\n");
		b.append("{\n");
		b.append("return DialogUtility.adaptSizeToSystemDPI(350, 300);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the image cache
	 * @return the generated content
	 */
	private String createImageCache() {
		final var b = new StringBuilder();
		b.append("import java.io.IOException;\n");
		b.append("import java.net.URL;\n");
		b.append("import java.util.Collections;\n");
		b.append("import java.util.HashMap;\n");
		b.append("import java.util.Map;\n");
		b.append("import org.eclipse.core.runtime.FileLocator;\n");
		b.append("import org.eclipse.core.runtime.Path;\n");
		b.append("import org.eclipse.jface.resource.ImageDescriptor;\n");
		b.append("import org.eclipse.swt.graphics.Image;\n");
		b.append("import org.osgi.framework.Bundle;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public class " + DEFAULT_IMAGE_CACHE_NAME + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static Map<String,Image> imageRegistry = new HashMap<>();\n");
		b.append("public static final String IMAGE_DEFAULT_PATH = \"/../" + RCP_ICON_FOLDER + "\";\n\n");
		b.append("/**\n");
		b.append(" * Prevent instantiation\n");
		b.append(" */\n");
		b.append("private " + DEFAULT_IMAGE_CACHE_NAME + "()\n");
		b.append("{\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Get image from cache\n");
		b.append(" * @param name the unique name of the image\n");
		b.append(" * @return the image\n");
		b.append(" */\n");
		b.append("public static synchronized Image getImage(String name)\n");
		b.append("{\n");
		b.append("final Bundle bundle = " + DEFAULT_ACTIVATOR_NAME + ".getContext().getBundle();\n");
		b.append("final var path = new Path(IMAGE_DEFAULT_PATH + name);\n");
		b.append("URL fileUrl = null;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("fileUrl = FileLocator.toFileURL(FileLocator.find(bundle, path, Collections.emptyMap()));\n");
		b.append("}\n");
		b.append("catch (final IOException e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while loading image {}!", "e", "name");

		b.append("\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("imageRegistry.putIfAbsent(name, ImageDescriptor.createFromURL(fileUrl).createImage());\n\n");
		b.append("return imageRegistry.get(name);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Get image descriptor\n");
		b.append(" * @param name the unique name of the image\n");
		b.append(" * @return the image descriptor\n");
		b.append(" */\n");
		b.append("public static synchronized ImageDescriptor getImageDescriptor(String name)\n");
		b.append("{\n");
		b.append("final Image img = getImage(name);\n\n");
		b.append("if(img == null)\n");
		b.append("return null;\n\n");
		b.append("return ImageDescriptor.createFromImage(img);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the application life cycle class
	 * @return the generated content
	 */
	private String createE4LifeCycle() {
		final var b = new StringBuilder();
		boolean usesEmbeddedDerby = false;

		if (project.hasRCPClient() && project.getDatabase().getVendorGroup() == DBVendorGroupEnumeration.DERBY_EMBEDDED) {
			usesEmbeddedDerby = true;
			b.append("import java.sql.*;\n");
		}

		b.append("import static " + project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS + ".*;\n");
		b.append("import java.util.*;\n");
		b.append("import org.eclipse.e4.core.contexts.*;\n");
		b.append("import org.eclipse.e4.ui.workbench.lifecycle.*;\n");
		b.append("import org.eclipse.swt.widgets.*;\n");
		b.append("import org.eclipse.swt.*;\n");
		b.append("import org.eclipse.jface.dialogs.Dialog;\n");
		b.append("import org.eclipse.jface.dialogs.MessageDialog;\n");
		b.append("import net.codecadenza.runtime.richclient.persistence.PersistenceHelper;\n");
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");

		if (project.hasRAPClient())
			b.append("@SuppressWarnings(\"restriction\")\n");

		b.append("public class ");
		b.append(DEFAULT_E4_LIFE_CYCLE_NAME);
		b.append("\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");

		if (!project.isJavaSEApplication()) {
			b.append("private static final String ALIAS_HOST_LOCAL = \"LOCAL\";\n");
			b.append("private static final String ALIAS_HOST_TEST = \"TEST\";\n");
		}
		else
			b.append("private static final String ALIAS_HOST_DB = \"DB\";\n");

		b.append("private static final String DATA_PATH = \"application.data\";\n\n");
		b.append("/**\n");
		b.append(" * @return a map that contains all available connection configurations\n");
		b.append(" */\n");
		b.append("private Map<String, ServiceLocatorDTO> initializeHosts()\n");
		b.append("{\n");
		b.append("final Map<String, ServiceLocatorDTO> hostMap = new HashMap<>();\n\n");

		if (!project.isJavaSEApplication()) {
			b.append("final var hostLocal = new ServiceLocatorDTO(ALIAS_HOST_LOCAL, \"http://localhost:8080/");
			b.append(project.getCode() + "\", false, TransportType.HTTP);\n");
			b.append("final var hostTest = new ServiceLocatorDTO(ALIAS_HOST_TEST, \"http://unknownhost/");
			b.append(project.getCode() + "\", false, TransportType.HTTP);\n\n");
			b.append("hostMap.put(ALIAS_HOST_LOCAL, hostLocal);\n");
			b.append("hostMap.put(ALIAS_HOST_TEST, hostTest);\n");
		}
		else {
			b.append("final var hostDatabase = new ServiceLocatorDTO(ALIAS_HOST_DB, \"\", false, TransportType.LOCAL);\n\n");
			b.append("hostMap.put(ALIAS_HOST_DB, hostDatabase);\n");
		}

		b.append("return hostMap;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param workbenchContext\n");
		b.append(" */\n");
		b.append("@PostContextCreate\n");
		b.append("protected void postContextCreate(IEclipseContext workbenchContext)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param workbenchContext\n");
		b.append(" */\n");
		b.append("@PreSave\n");
		b.append("protected void preSave(IEclipseContext workbenchContext)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param workbenchContext\n");
		b.append(" */\n");
		b.append("@ProcessAdditions\n");
		b.append("protected void processAdditions(IEclipseContext workbenchContext)\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Start application");

		b.append("\n");
		b.append("final var shell = new Shell(SWT.TOOL | SWT.NO_TRIM);\n");
		b.append("Locale.setDefault(Locale.ENGLISH);\n\n");
		b.append("// Initialize local persistence system\n");
		b.append("try\n");
		b.append("{\n");
		b.append("PersistenceHelper.initialize(DATA_PATH);\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while starting local persistence system!", "ex");

		final var msg = "Could not start local persistence system! Maybe it is already used by another application! Message: ";

		b.append("\n");
		b.append("MessageDialog.openError(shell, " + i18n.getI18NMessage("msg_app_title", "Application") + ", ");
		b.append(i18n.getI18NMessage("msg_err_persistence", msg) + " + ex.getMessage());\n");
		b.append("}\n\n");

		if (project.hasRAPClient()) {
			b.append("int dlgReturnCode = Dialog.CANCEL;\n\n");
			b.append("// The log in dialog is opened as long as log in operation is finished successfully!\n");
			b.append("while(dlgReturnCode == Dialog.CANCEL)\n");
			b.append("{\n");
			b.append("// Open dialog to log in\n");
			b.append("final var logOn = new LogOnDialog(shell, initializeHosts());\n");
			b.append("dlgReturnCode = logOn.open();\n");
			b.append("}\n");
		}
		else {
			b.append("// Open dialog to log in\n");
			b.append("final var logOn = new " + DEFAULT_LOG_ON_DLG_NAME + "(shell, initializeHosts());\n");
			b.append("final int dlgReturnCode = logOn.open();\n\n");
			b.append("if(dlgReturnCode == Dialog.CANCEL)\n");

			if (usesEmbeddedDerby) {
				String connectionURL = project.getDataSource().getConnectionURL();

				if (!connectionURL.endsWith(";"))
					connectionURL += ";";

				connectionURL += "shutdown=true";

				b.append("{\n");
				b.append("// Shutdown embedded database instance!\n");
				b.append("try(final Connection con = DriverManager.getConnection(\"" + connectionURL + "\"))\n");
				b.append("{\n\n");
				b.append("}\n");
				b.append("catch (final SQLException e)\n");
				b.append("{\n");
				b.append("// Derby throws an exception even in the case that shutdown was successful!\n");

				LoggingGenerator.addInfoLog(b, "The embedded database has been shut down!");

				b.append("}\n\n");
			}

			b.append("System.exit(-1);\n");

			if (usesEmbeddedDerby)
				b.append("}\n");

			b.append("\n");
			b.append("PersistenceHelper.save();\n");
		}

		b.append("\n");
		b.append("System.setProperty(\"osgi.nl\", Locale.ENGLISH.toString());\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param workbenchContext\n");
		b.append(" */\n");
		b.append("@ProcessRemovals\n");
		b.append("protected void processRemovals(IEclipseContext workbenchContext)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the logon dialog
	 * @return the generated content
	 */
	private String createLogOnDialog() {
		final var b = new StringBuilder();
		b.append("import java.util.Map;\n");
		b.append("import org.eclipse.swt.widgets.Shell;\n");
		b.append("import net.codecadenza.runtime.richclient.eclipse.dialog.AbstractLogOnDialog;\n");
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");
		b.append("import " + project.getClientNamespace().toString() + SUB_PACKAGE_UTIL + "." + SECURITY_MANAGER + ";\n\n");
		b.append("public class " + DEFAULT_LOG_ON_DLG_NAME + " extends AbstractLogOnDialog\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parentShell\n");
		b.append(" * @param hostMap a map that contains all available connection configurations\n");
		b.append(" */\n");
		b.append("public " + DEFAULT_LOG_ON_DLG_NAME + "(Shell parentShell, Map<String, ServiceLocatorDTO> hostMap)\n");
		b.append("{\n");
		b.append("super(parentShell, hostMap);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.dialog.__AbstractLogOnDialog#logOn()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void logOn()\n");
		b.append("{\n");
		b.append(SECURITY_MANAGER + ".logOn(super.getServiceLocatorDTO());\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createFormatHandler() {
		final var b = new StringBuilder();
		b.append("import org.eclipse.e4.core.di.annotations.*;\n");
		b.append("import org.eclipse.swt.widgets.*;\n");
		b.append("import net.codecadenza.runtime.richclient.eclipse.dialog.*;\n");
		b.append("import org.eclipse.e4.ui.workbench.*;\n");
		b.append("\n");
		b.append("public class " + FORMAT_HANDLER_NAME + "\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * @param workbench\n");
		b.append(" * @param shell\n");
		b.append(" */\n");
		b.append("@Execute\n");
		b.append("public void execute(IWorkbench workbench, Shell shell)\n");
		b.append("{\n");
		b.append("final var dlg = new FormatPreferencesDialog(shell);\n");
		b.append("dlg.open();\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createChangePasswordHandler() {
		final var b = new StringBuilder();
		b.append("import org.eclipse.e4.core.di.annotations.*;\n");
		b.append("import org.eclipse.swt.widgets.*;\n");
		b.append("import org.eclipse.e4.ui.workbench.*;\n");
		b.append("\n");
		b.append("public class " + CHANGE_PASSWORD_HANDLER_NAME + "\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * @param workbench\n");
		b.append(" * @param shell\n");
		b.append(" */\n");
		b.append("@Execute\n");
		b.append("public void execute(IWorkbench workbench, Shell shell)\n");
		b.append("{\n");
		b.append("new " + CHANGE_PWD_DLG_NAME + "(shell).open();\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the activator class
	 * @return the generated content
	 */
	private String createActivator() {
		final var b = new StringBuilder();
		boolean usesEmbeddedDerby = false;

		if (project.getDatabase().getVendorGroup() == DBVendorGroupEnumeration.DERBY_EMBEDDED) {
			usesEmbeddedDerby = true;
			b.append("import java.sql.*;\n");
		}

		b.append("import org.osgi.framework.*;\n");

		if (usesEmbeddedDerby)
			new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public class " + DEFAULT_ACTIVATOR_NAME + " implements BundleActivator\n");
		b.append("{\n");

		if (usesEmbeddedDerby)
			b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");

		b.append("private static BundleContext context;\n\n");
		b.append("/**\n");
		b.append(" * @return the context of this bundle\n");
		b.append(" */\n");
		b.append("static BundleContext getContext()\n");
		b.append("{\n");
		b.append("return context;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void start(BundleContext bundleContext) throws Exception\n");
		b.append("{\n");
		b.append("context = bundleContext;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void stop(BundleContext bundleContext) throws Exception\n");
		b.append("{\n");

		if (usesEmbeddedDerby) {
			String connectionURL = project.getDataSource().getConnectionURL();

			if (!connectionURL.endsWith(";"))
				connectionURL += ";";

			connectionURL += "shutdown=true";

			b.append("// Shutdown embedded database instance!\n");
			b.append("try(final Connection con = DriverManager.getConnection(\"" + connectionURL + "\"))\n");
			b.append("{\n\n");
			b.append("}\n");
			b.append("catch (final SQLException e)\n");
			b.append("{\n");
			b.append("// Derby throws an exception even in the case that shutdown was successful!\n");

			LoggingGenerator.addInfoLog(b, "The embedded database has been shut down!");

			b.append("}\n\n");
		}

		b.append("Activator.context = null;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the plugin.xml
	 * @return the generated content
	 */
	public String createPluginXML() {
		final var b = new StringBuilder();
		final var lifeCycleClasse = project.getClientNamespace().toString() + "." + DEFAULT_E4_LIFE_CYCLE_NAME;

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<plugin>\n");
		b.append("\t<extension id=\"product\" point=\"org.eclipse.core.runtime.products\">\n");
		b.append("\t\t<product name=\"" + DEFAULT_APPLICATION_NAME);
		b.append("\" application=\"org.eclipse.e4.ui.workbench.swt.E4Application\">\n");
		b.append("\t\t\t<property name=\"appName\" value=\"" + DEFAULT_APPLICATION_NAME + "\"></property>\n");
		b.append("\t\t\t<property name=\"lifeCycleURI\" value=\"bundleclass://" + DEFAULT_PLUGIN_ID);
		b.append("/" + lifeCycleClasse + "\"></property>\n");
		b.append("\t\t</product>\n");
		b.append("\t</extension>\n\n");
		b.append("</plugin>");

		return b.toString();
	}

	/**
	 * Create the contribution.xml
	 * @return the generated content
	 */
	public String createContributionXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<scr:component xmlns:scr=\"http://www.osgi.org/xmlns/scr/v1.1.0\" name=\"");
		b.append(DEFAULT_RAP_APPLICATION_NAME + "\">\n");
		b.append("\t<implementation class=\"" + project.getClientNamespace().toString() + ".");
		b.append(DEFAULT_RAP_APPLICATION_NAME + "\"/>\n");
		b.append("\t<service>\n");
		b.append("\t\t<provide interface=\"org.eclipse.rap.rwt.application.ApplicationConfiguration\"/>\n");
		b.append("\t</service>\n");
		b.append("</scr:component>\n");

		return b.toString();
	}

	/**
	 * Create the RAP application
	 * @return the generated content
	 */
	public String createApplication() {
		final var b = new StringBuilder();
		final var lifeCycleClass = project.getClientNamespace().toString() + "." + DEFAULT_E4_LIFE_CYCLE_NAME;

		b.append("import java.util.*;\n");
		b.append("import org.eclipse.rap.e4.*;\n");
		b.append("import org.eclipse.rap.rwt.application.*;\n");
		b.append("import org.eclipse.rap.rwt.application.Application.*;\n");
		b.append("import org.eclipse.rap.rwt.client.*;\n\n");
		b.append("public class " + DEFAULT_RAP_APPLICATION_NAME + " implements ApplicationConfiguration\n");
		b.append("{\n");
		b.append("private static final String E4_MODEL = \"platform:/plugin/");
		b.append(DEFAULT_PLUGIN_ID + "/" + APPLICATION_MODEL_NAME + "\";\n");
		b.append("private static final String E4_LIFE_CYCLE = \"bundleclass://" + DEFAULT_PLUGIN_ID + "/" + lifeCycleClass + "\";\n");
		b.append("private static final String APP_TITLE = \"My generated application\";\n");
		b.append("private static final String APP_ENTRY_POINT = \"" + DEFAULT_RAP_CONTEXT_PATH + "\";\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.rap.rwt.application.ApplicationConfiguration");
		b.append("#configure(org.eclipse.rap.rwt.application.Application)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void configure(Application application)\n");
		b.append("{\n");
		b.append("final var properties = new HashMap<String, String>();\n");
		b.append("properties.put(WebClient.PAGE_TITLE, APP_TITLE);\n\n");
		b.append("application.addEntryPoint(APP_ENTRY_POINT, ");
		b.append("new E4EntryPointFactory(E4ApplicationConfig.create(E4_MODEL, E4_LIFE_CYCLE)), properties);\n");
		b.append("application.setOperationMode(OperationMode.SWT_COMPATIBILITY);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the RCP product file
	 * @return the generated content
	 */
	public String createProductFile() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<?pde version=\"3.5\"?>\n");
		b.append("\t<product name=\"My application\" id=\"" + DEFAULT_PLUGIN_ID + ".product\" ");
		b.append("application=\"org.eclipse.e4.ui.workbench.swt.E4Application\" useFeatures=\"false\" includeLaunchers=\"true\">\n");
		b.append("\t<configIni use=\"default\"/>\n\n");
		b.append("\t<launcherArgs>\n");
		b.append("\t\t<programArgs>-clearPersistedState</programArgs>\n");
		b.append("\t\t<vmArgsMac>-XstartOnFirstThread</vmArgsMac>\n");
		b.append("\t</launcherArgs>\n\n");
		b.append("\t<plugins>\n");
		b.append("\t\t<plugin id=\"" + DEFAULT_PLUGIN_ID + "\"/>\n");

		EclipseRCPPlugins.getPlugins().stream().forEach(plugin -> b.append("\t\t<plugin id=\"" + plugin.name() + "\"/>\n"));

		b.append("\t</plugins>\n");
		b.append("</product>");

		return b.toString();
	}

	/**
	 * Create initial file for the application model
	 * @return the generated content
	 */
	public String createApplicationModelFile() {
		final var b = new StringBuilder();
		final String appId = DEFAULT_PLUGIN_ID;
		final var navigatorClass = project.getClientNamespace().toString() + PACK_CLIENT_VIEW + "." + NAV_VIEW_NAME;
		final String handlerPackage = project.getClientNamespace().toString();

		// Initialize an ID generator for standard xmi:id attributes
		final var xmiIDGen = new IDGenerator();
		xmiIDGen.setLength(DEFAULT_KEY_LENGTH);
		xmiIDGen.setPrefix("xmi:id=\"_");
		xmiIDGen.setSuffix("\"");

		// Initialize a generator for IDs that are referenced by other elements
		final var refIdGen = new IDGenerator();
		refIdGen.setLength(DEFAULT_KEY_LENGTH);
		refIdGen.setPrefix("_");

		final String contextId = refIdGen.generateNextID();
		final String changePWCommandId = refIdGen.generateNextID();
		final String formatCommandId = refIdGen.generateNextID();
		final String defaultSashContId = refIdGen.generateNextID();

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<application:Application xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" ");
		b.append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ");
		b.append("xmlns:advanced=\"http://www.eclipse.org/ui/2010/UIModel/application/ui/advanced\" ");
		b.append("xmlns:application=\"http://www.eclipse.org/ui/2010/UIModel/application\" ");
		b.append("xmlns:basic=\"http://www.eclipse.org/ui/2010/UIModel/application/ui/basic\" ");
		b.append("xmlns:menu=\"http://www.eclipse.org/ui/2010/UIModel/application/ui/menu\" ");
		b.append(xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ide.application\" bindingContexts=\"");
		b.append(contextId + "\">\n");
		b.append("<children xsi:type=\"basic:TrimmedWindow\" " + xmiIDGen.generateNextID());
		b.append(" label=\"My application\" width=\"1024\" height=\"768\">\n");

		if (project.hasRAPClient())
			b.append("<tags>shellMaximized</tags>\n");

		b.append("<children xsi:type=\"advanced:PerspectiveStack\" " + xmiIDGen.generateNextID() + ">\n");
		b.append("<children xsi:type=\"advanced:Perspective\" " + xmiIDGen.generateNextID() + ">\n");
		b.append("<children xsi:type=\"basic:PartSashContainer\" " + xmiIDGen.generateNextID());
		b.append(" elementId=\"" + appId + ".partsashcontainer.0\" ");
		b.append("selectedElement=\"" + defaultSashContId + "\" horizontal=\"true\">\n");
		b.append("<children xsi:type=\"basic:PartStack\" " + xmiIDGen.generateNextID() + " containerData=\"4000\">\n");
		b.append("<children xsi:type=\"basic:Part\" " + xmiIDGen.generateNextID() + " contributionURI=\"bundleclass://");
		b.append(appId + "/" + navigatorClass + "\" ");
		b.append("label=\"Main navigator\"/>\n");
		b.append("</children>\n");
		b.append("<children xsi:type=\"basic:PartSashContainer\" xmi:id=\"" + defaultSashContId);
		b.append("\" containerData=\"6000\" horizontal=\"true\">\n");
		b.append("<children xsi:type=\"basic:PartStack\" " + xmiIDGen.generateNextID() + " ");
		b.append("elementId=\"" + DEFAULT_PART_STACK_ID + "\">\n");
		b.append("<tags>NoAutoCollapse</tags>\n");
		b.append("</children>\n");
		b.append("</children>\n");
		b.append("</children>\n");
		b.append("</children>\n");
		b.append("</children>\n");
		b.append("<mainMenu " + xmiIDGen.generateNextID() + " elementId=\"menu:org.eclipse.ui.main.menu\">\n");
		b.append("<children xsi:type=\"menu:Menu\" " + xmiIDGen.generateNextID() + " label=\"File\">\n");
		b.append("<children xsi:type=\"menu:HandledMenuItem\" " + xmiIDGen.generateNextID() + " label=\"Change password\" ");
		b.append("iconURI=\"platform:/plugin/" + appId + "/icons/full/obj16/window_view.png\" ");
		b.append("command=\"" + changePWCommandId + "\"/>\n");
		b.append("<children xsi:type=\"menu:HandledMenuItem\" " + xmiIDGen.generateNextID() + " label=\"Save\" ");
		b.append("iconURI=\"platform:/plugin/" + appId + "/icons/full/obj16/format_preferences.png\" ");
		b.append("command=\"" + formatCommandId + "\"/>\n");
		b.append("</children>\n");
		b.append("<children xsi:type=\"menu:Menu\" " + xmiIDGen.generateNextID() + " label=\"Help\">\n");
		b.append("</children>\n");
		b.append("</mainMenu>\n");
		b.append("<trimBars " + xmiIDGen.generateNextID() + ">\n");
		b.append("<children xsi:type=\"menu:ToolBar\" " + xmiIDGen.generateNextID());
		b.append(" elementId=\"toolbar:org.eclipse.ui.main.toolbar\">\n");
		b.append("<children xsi:type=\"menu:HandledToolItem\" " + xmiIDGen.generateNextID() + " ");
		b.append("iconURI=\"platform:/plugin/" + appId + "/icons/full/obj16/window_view.png\" ");
		b.append("command=\"" + changePWCommandId + "\"/>\n");
		b.append("<children xsi:type=\"menu:HandledToolItem\" " + xmiIDGen.generateNextID() + " ");
		b.append("iconURI=\"platform:/plugin/" + appId + "/icons/full/obj16/format_preferences.png\" ");
		b.append("command=\"" + formatCommandId + "\"/>\n");
		b.append("</children>\n");
		b.append("</trimBars>\n");
		b.append("</children>\n");
		b.append("<handlers " + xmiIDGen.generateNextID() + " elementId=\"" + appId + ".handler.openCommand\" ");
		b.append("contributionURI=\"bundleclass://" + appId + "/" + handlerPackage + "." + CHANGE_PASSWORD_HANDLER_NAME);
		b.append("\" command=\"" + changePWCommandId + "\"/>\n");
		b.append("<handlers " + xmiIDGen.generateNextID() + " elementId=\"" + appId + ".handler.saveCommand\" ");
		b.append("contributionURI=\"bundleclass://" + appId + "/" + handlerPackage + "." + FORMAT_HANDLER_NAME);
		b.append("\" command=\"" + formatCommandId + "\"/>\n");
		b.append("<bindingTables " + xmiIDGen.generateNextID() + " bindingContext=\"" + contextId + "\">\n");
		b.append("<bindings " + xmiIDGen.generateNextID() + " keySequence=\"M1+O\" command=\"" + changePWCommandId + "\"/>\n");
		b.append("<bindings " + xmiIDGen.generateNextID() + " keySequence=\"M1+S\" command=\"" + formatCommandId + "\"/>\n");
		b.append("</bindingTables>\n");
		b.append("<rootContext xmi:id=\"" + contextId + "\" ");
		b.append("elementId=\"org.eclipse.ui.contexts.dialogAndWindow\" name=\"In Dialog and Windows\">\n");
		b.append("<children " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.ui.contexts.window\" name=\"In Windows\"/>\n");
		b.append("<children " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.ui.contexts.dialog\" name=\"In Dialogs\"/>\n");
		b.append("</rootContext>\n");
		b.append("<commands xmi:id=\"" + changePWCommandId + "\" elementId=\"" + appId);
		b.append(".changePassword\" commandName=\"changePasswordCommand\"/>\n");
		b.append("<commands xmi:id=\"" + formatCommandId);
		b.append("\" elementId=\"org.eclipse.ui.window.preferences\" commandName=\"changeFormatSettingsCommand\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.core.commands.service\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.core.commands/");
		b.append("org.eclipse.e4.core.commands.CommandServiceAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.contexts.service\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.services/org.eclipse.e4.ui.services.ContextServiceAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.bindings.service\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.bindings/org.eclipse.e4.ui.bindings.BindingServiceAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.workbench.commands.model\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.workbench/");
		b.append("org.eclipse.e4.ui.internal.workbench.addons.CommandProcessingAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.workbench.handler.model\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.workbench/");
		b.append("org.eclipse.e4.ui.internal.workbench.addons.HandlerProcessingAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.workbench.contexts.model\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.workbench/");
		b.append("org.eclipse.e4.ui.internal.workbench.addons.ContextProcessingAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.workbench.bindings.model\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.workbench.swt/");
		b.append("org.eclipse.e4.ui.workbench.swt.util.BindingProcessingAddon\"/>\n");
		b.append("<addons " + xmiIDGen.generateNextID() + " elementId=\"org.eclipse.e4.ui.workbench.addons.dndaddon.DnDAddon\" ");
		b.append("contributionURI=\"bundleclass://org.eclipse.e4.ui.workbench.addons.swt/");
		b.append("org.eclipse.e4.ui.workbench.addons.dndaddon.DnDAddon\"/>\n");
		b.append("</application:Application>\n");

		return b.toString();
	}

	/**
	 * Create the manifest file
	 * @param wsProject
	 * @param defaultPackage
	 * @return the generated content
	 * @throws Exception if the creation of the manifest file has failed
	 */
	public String createManifest(IProject wsProject, String defaultPackage) throws Exception {
		final var b = new StringBuilder();
		final IFolder libFolder = wsProject.getFolder(LIB_FOLDER);
		final var className = defaultPackage + "." + DEFAULT_ACTIVATOR_NAME;

		b.append("Manifest-Version: 1.0\n");
		b.append("Automatic-Module-Name: " + defaultPackage + "\n");
		b.append("Bundle-ManifestVersion: 2\n");
		b.append("Bundle-Name: " + DEFAULT_APPLICATION_NAME + " Plugin\n");
		b.append("Bundle-SymbolicName: " + DEFAULT_PLUGIN_ID + "; singleton:=true\n");
		b.append("Bundle-Version: 1.0.0\n");
		b.append("Bundle-Activator: " + className + "\n");
		b.append("Bundle-RequiredExecutionEnvironment: JavaSE-21\n");
		b.append("Bundle-Vendor: Vendor name\n");

		// If the application uses JPA locally the plug-in must not be packed into a single jar file. Otherwise EclipseLink cannot
		// find the persistence.xml file!
		if (project.isJavaSEApplication())
			b.append("Eclipse-BundleShape: dir\n");

		b.append("Require-Bundle: \n");
		b.append(" jakarta.annotation-api,\n");
		b.append(" jakarta.inject.jakarta.inject-api,\n");
		b.append(" org.eclipse.e4.ui.model.workbench,\n");
		b.append(" org.eclipse.e4.ui.workbench,\n");
		b.append(" org.eclipse.e4.ui.services,\n");
		b.append(" org.eclipse.e4.core.contexts,\n");
		b.append(" org.eclipse.e4.core.di,\n");
		b.append(" org.eclipse.e4.ui.di,\n");
		b.append(" org.eclipse.core.jobs,\n");
		b.append(" org.eclipse.core.runtime,\n");

		if (project.hasRCPClient()) {
			b.append(" org.eclipse.swt,\n");
			b.append(" org.eclipse.jface,\n");
			b.append(" org.eclipse.jface.databinding,\n");
			b.append(" org.eclipse.core.databinding,\n");
			b.append(" org.eclipse.core.databinding.beans,\n");
			b.append(" org.eclipse.core.databinding.observable,\n");
			b.append(" org.eclipse.core.databinding.property,\n");
			b.append(" org.eclipse.e4.ui.workbench.addons.swt\n");
		}
		else {
			b.append(" org.apache.felix.http.servlet-api,\n");
			b.append(" org.apache.commons.commons-fileupload,\n");
			b.append(" org.apache.commons.commons-io,\n");
			b.append(" org.eclipse.rap.jface,\n");
			b.append(" org.eclipse.rap.filedialog,\n");
			b.append(" org.eclipse.rap.fileupload,\n");
			b.append(" org.eclipse.rap.rwt;bundle-version=\"[3.28.0,4.0.0)\",\n");
			b.append(" org.eclipse.rap.e4\n");
		}

		b.append("Import-Package: org.osgi.framework\n");

		if (project.hasRAPClient())
			b.append("Service-Component: " + OSGI_INF_FOLDER + "/" + CONTRIBUTION_XML + "\n");

		b.append("Bundle-ActivationPolicy: lazy\n");

		// Add all required libraries and folders to the bundle classpath
		b.append("Bundle-ClassPath: ");
		b.append(META_INF_FOLDER + "/,\n");
		b.append(" " + project.getResourceFolder() + "/,\n");

		for (final IResource libFile : libFolder.members()) {
			if (!(libFile instanceof IFile))
				continue;

			b.append(" " + LIB_FOLDER + "/" + libFile.getName() + ",\n");
		}

		b.append(" .\n");

		return b.toString();
	}

	/**
	 * Create the build properties file
	 * @param wsProject
	 * @return the generated content
	 * @throws Exception if the creation of the properties file has failed
	 */
	public String createBuildPropertiesFile(IProject wsProject) throws Exception {
		final var b = new StringBuilder();
		final IFolder libFolder = wsProject.getFolder(LIB_FOLDER);

		b.append("output..=target/classes/\n");
		b.append("bin.includes=");

		if (project.hasRCPClient())
			b.append(PLUGIN_XML);
		else
			b.append(OSGI_INF_FOLDER + "/");

		b.append(",\\\n");
		b.append("\t" + META_INF_FOLDER + "/,\\\n");
		b.append("\t" + ICON_FOLDER + "/,\\\n");
		b.append("\t" + project.getResourceFolder() + "/,\\\n");

		// Add libraries
		for (final IResource libFile : libFolder.members())
			if (libFile instanceof IFile)
				b.append("\t" + LIB_FOLDER + "/" + libFile.getName() + ",\\\n");

		b.append("\t" + APPLICATION_MODEL_NAME + ",\\\n");
		b.append("\t.\n");
		b.append("source..=" + project.getSourceFolder() + "/,\\\n");
		b.append("\t" + project.getResourceFolder() + "/,\\\n");
		b.append("\t" + project.getTestSourceFolder() + "/\n");

		return b.toString();
	}

	/**
	 * Create the RAP application launch file
	 * @return the generated content
	 */
	public String createLaunchFile() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\" standalone=\"no\"?>\n");
		b.append("<launchConfiguration type=\"org.eclipse.rap.ui.launch.RAPLauncher\">\n");
		b.append("\t<booleanAttribute key=\"append.args\" value=\"true\"/>\n");
		b.append("\t<booleanAttribute key=\"askclear\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"automaticAdd\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"automaticValidate\" value=\"true\"/>\n");
		b.append("\t<stringAttribute key=\"bootstrap\" value=\"\"/>\n");
		b.append("\t<stringAttribute key=\"checked\" value=\"[NONE]\"/>\n");
		b.append("\t<booleanAttribute key=\"clearConfig\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"clearws\" value=\"false\"/>\n");
		b.append("\t<stringAttribute key=\"configLocation\" value=\"${workspace_loc}/.metadata/");
		b.append(".plugins/org.eclipse.pde.core/application\"/>\n");
		b.append("\t<booleanAttribute key=\"default\" value=\"true\"/>\n");
		b.append("\t<booleanAttribute key=\"default_auto_start\" value=\"true\"/>\n");
		b.append("\t<intAttribute key=\"default_start_level\" value=\"4\"/>\n");
		b.append("\t<booleanAttribute key=\"includeOptional\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.debug.core.appendEnvironmentVariables\" value=\"true\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.jdt.launching.PROGRAM_ARGUMENTS\" ");
		b.append("value=\"-os ${target.os} -ws ${target.ws} -arch ${target.arch} -nl ${target.nl} -console -consolelog\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.jdt.launching.SOURCE_PATH_PROVIDER\" ");
		b.append("value=\"org.eclipse.pde.ui.workbenchClasspathProvider\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.jdt.launching.VM_ARGUMENTS\" ");
		b.append("value=\"-Declipse.ignoreApp=true -Dosgi.noShutdown=true ");
		b.append("-Dorg.eclipse.equinox.http.jetty.log.stderr.threshold=info\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.rap.launch.browserMode\" value=\"EXTERNAL\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.rap.launch.contextpath\" value=\"/\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.rap.launch.dataLocation\" ");
		b.append("value=\"${workspace_loc}/.metadata/.plugins/org.eclipse.rap.tools.launch/application\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.developmentMode\" value=\"true\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.openBrowser\" value=\"true\"/>\n");
		b.append("\t<intAttribute key=\"org.eclipse.rap.launch.port\" value=\"10080\"/>\n");
		b.append("\t<stringAttribute key=\"org.eclipse.rap.launch.servletPath\" value=\"" + DEFAULT_RAP_CONTEXT_PATH + "\"/>\n");
		b.append("\t<intAttribute key=\"org.eclipse.rap.launch.sessionTimeout\" value=\"0\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.terminatePrevious\" value=\"true\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.useDefaultDataLocation\" value=\"true\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.useManualContextPath\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.useManualPort\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"org.eclipse.rap.launch.useSessionTimeout\" value=\"false\"/>\n");
		b.append("\t<stringAttribute key=\"pde.version\" value=\"3.3\"/>\n");
		b.append("\t<booleanAttribute key=\"show_selected_only\" value=\"true\"/>\n");
		b.append("\t<setAttribute key=\"selected_target_bundles\">\n");

		EclipseRAPPlugins.getPlugins().stream().forEach(plugin -> b.append("\t\t<setEntry value=\"" + plugin.name() + "\"/>\n"));

		b.append("\t</setAttribute>\n");
		b.append("\t<booleanAttribute key=\"tracing\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"useCustomFeatures\" value=\"false\"/>\n");
		b.append("\t<booleanAttribute key=\"useDefaultConfigArea\" value=\"true\"/>\n");
		b.append("\t<stringAttribute key=\"workspace_bundles\" value=\"" + DEFAULT_PLUGIN_ID + "@default:default\"/>\n");
		b.append("</launchConfiguration>\n");

		return b.toString();
	}

}
