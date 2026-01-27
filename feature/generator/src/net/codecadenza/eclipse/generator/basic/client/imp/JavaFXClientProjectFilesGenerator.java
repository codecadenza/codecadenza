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

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.CHANGE_PWD_DLG_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_LOG_ON_DLG_NAME;
import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;
import static net.codecadenza.eclipse.shared.Constants.JAVAFX_LAUNCHER_NAME;
import static net.codecadenza.eclipse.shared.Constants.NAV_VIEW_NAME;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import java.util.List;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.common.LoggingGenerator;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for basic source and configuration files that are necessary for JavaFX applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXClientProjectFilesGenerator extends AbstractClientProjectFilesGenerator {
	private static final String MAIN_WINDOW_NAME = "MainWindow";
	private static final String LAUNCHER_COMMENT = "Class for launching the application";

	private RichClientI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public JavaFXClientProjectFilesGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final String securityManagerPackage = project.getClientNamespace().toString() + SUB_PACKAGE_UTIL;
		i18n = new RichClientI18NGenerator(project);

		final JavaFile mainWindow = createJavaSourceFile(MAIN_WINDOW_NAME, createMainWindow(), APPLICATION_COMMENT);
		final JavaFile changePasswordDialog = createJavaSourceFile(CHANGE_PWD_DLG_NAME, createChangePasswordDialog(),
				CHANGE_PWD_COMMENT);
		final JavaFile launcher = createJavaSourceFile(JAVAFX_LAUNCHER_NAME, createLauncher(), LAUNCHER_COMMENT);
		final JavaFile logOnDialog = createJavaSourceFile(DEFAULT_LOG_ON_DLG_NAME, createLogOnDialog(), LOG_ON_DLG_COMMENT);
		final JavaFile securityManager = createJavaSourceFile(SECURITY_MANAGER, createInitalSecurityManager(), securityManagerPackage,
				SECURITY_MANAGER_COMMENT);

		i18n.save();

		return List.of(mainWindow, changePasswordDialog, launcher, logOnDialog, securityManager);
	}

	/**
	 * Create the dialog to change the password of the currently logged on user
	 * @return the generated content
	 */
	private String createChangePasswordDialog() {
		final var b = new StringBuilder();
		b.append("import javafx.stage.*;\n");
		b.append("import net.codecadenza.runtime.richclient.javafx.dialog.*;\n");

		if (project.isProtectManualChanges())
			b.append("import " + GENERATED_ELEMENT_ANNOTATION + ";\n");

		b.append("\n");
		b.append("public class " + CHANGE_PWD_DLG_NAME + " extends AbstractChangePasswordDialog\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param owner\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public " + CHANGE_PWD_DLG_NAME + "(Window owner)\n");
		b.append("{\n");
		b.append("super(owner);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractChangePasswordDialog#");
		b.append("saveNewPassword(java.lang.String, java.lang.String, java.lang.String)\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("@Override\n");
		b.append("public boolean saveNewPassword(String oldPassword, String newPassword, String newPasswordConfirm)\n");
		b.append("{\n");
		b.append("// This implementation is empty as long as security features are not applied to this project!\n");
		b.append("return true;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the main window
	 * @return the generated content
	 */
	private String createMainWindow() {
		final var b = new StringBuilder();
		final String exitConf = i18n.getI18NMessage("application_exit", "Do you really want to exit?");
		final String appTitle = i18n.getI18NMessage("application_name", "Application name");
		final boolean usesEmbeddedDerby = project.getDatabase().getVendorGroup() == DBVendorGroupEnumeration.DERBY_EMBEDDED;

		b.append("import static " + project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS + ".*;\n");
		b.append("import java.util.*;\n");
		b.append("import javafx.application.*;\n");
		b.append("import javafx.event.*;\n");
		b.append("import javafx.geometry.*;\n");
		b.append("import javafx.scene.*;\n");
		b.append("import javafx.scene.control.*;\n");
		b.append("import javafx.scene.layout.*;\n");
		b.append("import javafx.stage.*;\n");
		b.append("import net.codecadenza.runtime.richclient.javafx.dialog.*;\n");
		b.append("import net.codecadenza.runtime.richclient.javafx.image.*;\n");
		b.append("import net.codecadenza.runtime.richclient.javafx.control.*;\n");
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");
		b.append("import net.codecadenza.runtime.richclient.persistence.*;\n");

		if (usesEmbeddedDerby)
			b.append("import java.sql.*;\n");

		if (project.isJavaSEApplication())
			b.append("import net.codecadenza.runtime.jpa.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public class " + MAIN_WINDOW_NAME + " extends Application\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String DATA_PATH = \"application.data\";\n");

		if (!project.isJavaSEApplication()) {
			b.append("private static final String ALIAS_HOST_LOCAL = \"LOCAL\";\n");
			b.append("private static final String ALIAS_HOST_TEST = \"TEST\";\n");
		}
		else
			b.append("private static final String ALIAS_HOST_DB = \"DB\";\n");

		b.append("private Stage stage;\n");
		b.append("private TabPane panTab;\n\n");
		b.append("/**\n");
		b.append(" * Close tabs\n");
		b.append(" */\n");
		b.append("private void closeTabs()\n");
		b.append("{\n");
		b.append("for(final Tab tab : panTab.getTabs())\n");
		b.append("Event.fireEvent(tab, new Event(Tab.CLOSED_EVENT));\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Shutdown services\n");
		b.append(" */\n");
		b.append("public void shutdown()\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Close the application");

		b.append("\n");

		if (usesEmbeddedDerby)
			b.append("Connection con = null;\n\n");

		b.append("try\n");
		b.append("{\n");
		b.append("closeTabs();\n\n");
		b.append("// Save \"local\" user queries and format settings\n");
		b.append("PersistenceHelper.save();\n");

		if (project.isJavaSEApplication()) {
			b.append("\n// Shutdown JPA engine\n");
			b.append("PersistenceEngine.shutdown();\n");
		}

		if (usesEmbeddedDerby) {
			String connectionURL = project.getDataSource().getConnectionURL();

			if (!connectionURL.endsWith(";"))
				connectionURL += ";";

			connectionURL += "shutdown=true";

			b.append("\n// Shutdown embedded database instance!\n");
			b.append("con = DriverManager.getConnection(\"" + connectionURL + "\");\n");
		}

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		if (usesEmbeddedDerby)
			LoggingGenerator.addInfoLog(b, "The embedded database has been shut down!");
		else
			LoggingGenerator.addErrorLog(b, "Error while closing the application!", "ex");

		b.append("}\n");

		if (usesEmbeddedDerby) {
			b.append("finally\n");
			b.append("{\n");
			b.append("if(con != null)\n");
			b.append("try\n");
			b.append("{\n");
			b.append("con.close();\n");
			b.append("}\n");
			b.append("catch (final SQLException e)\n");
			b.append("{\n");

			LoggingGenerator.addWarningLog(b, "Could not close the database connection!", "e");

			b.append("}\n");
			b.append("}\n");
		}

		b.append("\nstage.close();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Action to exit application\n");
		b.append(" */\n");
		b.append("private class ExitAction extends Action\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public ExitAction()\n");
		b.append("{\n");
		b.append("this.title = " + i18n.getI18NMessage("action_name_exit", "Exit") + ";\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void handle()\n");
		b.append("{\n");
		b.append("if(DialogButtonType.YES != DialogUtil.openConfirmationDialog(stage, " + appTitle + ", " + exitConf + "))\n");
		b.append("return;\n\n");
		b.append("shutdown();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Action to open preferences dialog\n");
		b.append(" */\n");
		b.append("private class PreferencesAction extends Action\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public PreferencesAction()\n");
		b.append("{\n");
		b.append("this.title = " + i18n.getI18NMessage("action_name_pref", "Preferences") + ";\n");
		b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_PREFERENCES);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void handle()\n");
		b.append("{\n");
		b.append("final var dlg = new FormatPreferencesDialog(stage);\n");
		b.append("dlg.open();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Action to open change password dialog\n");
		b.append(" */\n");
		b.append("private class ChangePasswordAction extends Action\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public ChangePasswordAction()\n");
		b.append("{\n");
		b.append("this.title = " + i18n.getI18NMessage("action_name_change_password", "Change password") + ";\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void handle()\n");
		b.append("{\n");
		b.append("final var dlg = new " + CHANGE_PWD_DLG_NAME + "(stage);\n");
		b.append("dlg.open();\n");
		b.append("}\n");
		b.append("}\n\n");
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
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.application.Application#start(javafx.stage.Stage)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void start(Stage stage)\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Start application");

		b.append("\n");
		b.append("final var exitAction = new ExitAction();\n");
		b.append("final var preferencesAction = new PreferencesAction();\n");
		b.append("final var changePasswordAction = new ChangePasswordAction();\n\n");
		b.append("this.stage = stage;\n");
		b.append("this.stage.setTitle(" + appTitle + ");\n\n");
		b.append("// Initialize persistence helper\n");
		b.append("try\n");
		b.append("{\n");
		b.append("PersistenceHelper.initialize(DATA_PATH);\n");
		b.append("}\n");
		b.append("catch (final GeneralPersistenceExeption e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while starting local persistence system!", "e");

		b.append("}\n\n");
		b.append("// Open log-on dialog\n");
		b.append("final var logOnDialog = new " + DEFAULT_LOG_ON_DLG_NAME + "(stage, initializeHosts());\n\n");
		b.append("if(DialogButtonType.OK != logOnDialog.open())\n");
		b.append("return;\n\n");
		b.append("final var panRoot = new VBox();\n");
		b.append("final var mnuMain = new MenuBar();\n");
		b.append("final var toolBar = new ToolBar();\n");
		b.append("final var panSplit = new SplitPane();\n");
		b.append("final var statusBar = new StatusBar();\n");
		b.append("statusBar.setPadding(new Insets(0, 5, 0, 5));\n");
		b.append("statusBar.setProgressBarVisible(false);\n\n");
		b.append("panTab = new TabPane();\n\n");
		b.append("final var navigator = new " + NAV_VIEW_NAME + "(panTab);\n\n");
		b.append("final var mnuFile = new Menu(" + i18n.getI18NMessage("mnu_file", "File") + ");\n\n");
		b.append("mnuMain.getMenus().add(mnuFile);\n");
		b.append("mnuFile.getItems().add(exitAction.createMenuItem());\n");
		b.append("mnuFile.getItems().add(preferencesAction.createMenuItem());\n");
		b.append("mnuFile.getItems().add(changePasswordAction.createMenuItem());\n\n");
		b.append("toolBar.getItems().add(preferencesAction.createToolbarButton());\n\n");
		b.append("panSplit.setOrientation(Orientation.HORIZONTAL);\n");
		b.append("panSplit.setDividerPosition(0, 0.25);\n");
		b.append("panSplit.setFocusTraversable(true);\n");
		b.append("panSplit.getItems().add(navigator);\n");
		b.append("panSplit.getItems().add(panTab);\n\n");
		b.append("panRoot.getChildren().add(mnuMain);\n");
		b.append("panRoot.getChildren().add(toolBar);\n");
		b.append("panRoot.getChildren().add(panSplit);\n");
		b.append("panRoot.getChildren().add(statusBar);\n\n");
		b.append("VBox.setVgrow(mnuMain, Priority.NEVER);\n");
		b.append("VBox.setVgrow(panSplit, Priority.ALWAYS);\n");
		b.append("VBox.setVgrow(statusBar, Priority.NEVER);\n\n");
		b.append("final var scene = new Scene(panRoot, 800, 600);\n");
		b.append("scene.getStylesheets().add(getClass().getResource(\"/css/application.css\").toExternalForm());\n\n");
		b.append("navigator.initialize();\n\n");
		b.append("stage.setScene(scene);\n");
		b.append("stage.show();\n");
		b.append("stage.setOnCloseRequest(_ -> shutdown());\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createLauncher() {
		final var b = new StringBuilder();
		b.append("import java.util.Locale;\n");
		b.append("import javafx.application.Application;\n\n");
		b.append("public class " + JAVAFX_LAUNCHER_NAME + "\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Launch the JavaFX GUI\n");
		b.append(" * @param args\n");
		b.append(" */\n");
		b.append("public static void main(String[] args)\n");
		b.append("{\n");
		b.append("Locale.setDefault(Locale.ENGLISH);\n\n");
		b.append("Application.launch(MainWindow.class, args);\n");
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
		b.append("import javafx.stage.Window;\n");
		b.append("import net.codecadenza.runtime.richclient.javafx.dialog.AbstractLogOnDialog;\n");
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");
		b.append("import " + project.getClientNamespace().toString() + SUB_PACKAGE_UTIL + "." + SECURITY_MANAGER + ";\n\n");
		b.append("public class " + DEFAULT_LOG_ON_DLG_NAME + " extends AbstractLogOnDialog\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param owner\n");
		b.append(" * @param hostMap a map that contains all available connection configurations\n");
		b.append(" */\n");
		b.append("public " + DEFAULT_LOG_ON_DLG_NAME + "(Window owner, Map<String, ServiceLocatorDTO> hostMap)\n");
		b.append("{\n");
		b.append("super(owner, hostMap);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractLogOnDialog#");
		b.append("logOn(net.codecadenza.runtime.richclient.transport.ServiceLocatorDTO)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void logOn(ServiceLocatorDTO settings)\n");
		b.append("{\n");
		b.append(SECURITY_MANAGER + ".logOn(settings);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

}
