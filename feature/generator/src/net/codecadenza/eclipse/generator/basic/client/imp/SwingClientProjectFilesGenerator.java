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
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_APPLICATION_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_LOG_ON_DLG_NAME;
import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;
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
 * Generator for basic source and configuration files that are necessary for Swing applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingClientProjectFilesGenerator extends AbstractClientProjectFilesGenerator {
	private RichClientI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public SwingClientProjectFilesGenerator(Project project) {
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

		final JavaFile application = createJavaSourceFile(DEFAULT_APPLICATION_NAME, createApplication(), APPLICATION_COMMENT);
		final JavaFile changePasswordDialog = createJavaSourceFile(CHANGE_PWD_DLG_NAME, createChangePasswordDialog(),
				CHANGE_PWD_COMMENT);
		final JavaFile logOnDialog = createJavaSourceFile(DEFAULT_LOG_ON_DLG_NAME, createLogOnDialog(), LOG_ON_DLG_COMMENT);
		final JavaFile securityManager = createJavaSourceFile(SECURITY_MANAGER, createInitalSecurityManager(), securityManagerPackage,
				SECURITY_MANAGER_COMMENT);

		i18n.save();

		return List.of(application, changePasswordDialog, logOnDialog, securityManager);
	}

	/**
	 * Create the dialog to change the password of the currently logged on user
	 * @return the generated content
	 */
	private String createChangePasswordDialog() {
		final var b = new StringBuilder();
		b.append("import net.codecadenza.runtime.richclient.swing.dialog.*;\n");

		if (project.isProtectManualChanges())
			b.append("import " + GENERATED_ELEMENT_ANNOTATION + ";\n");

		b.append("\n");
		b.append("public class " + CHANGE_PWD_DLG_NAME + " extends AbstractChangePasswordDialog\n");
		b.append("{\n");
		b.append(annotationForGeneratedElement);
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractChangePasswordDialog#");
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
	 * Create application class
	 * @return the content of the application class
	 */
	private String createApplication() {
		final var b = new StringBuilder();
		final boolean usesEmbeddedDerby = project.isJavaSEApplication()
				&& project.getDatabase().getVendorGroup() == DBVendorGroupEnumeration.DERBY_EMBEDDED;

		b.append("import static " + project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS + ".*;\n");
		b.append("import java.awt.*;\n");
		b.append("import java.awt.event.*;\n");
		b.append("import java.util.*;\n");
		b.append("import javax.swing.*;\n");
		b.append("import javax.swing.border.*;\n");
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");
		b.append("import net.codecadenza.runtime.richclient.persistence.*;\n");
		b.append("import net.codecadenza.runtime.richclient.swing.dialog.*;\n");
		b.append("import net.codecadenza.runtime.richclient.swing.image.*;\n");
		b.append("import javax.swing.plaf.*;\n");

		if (usesEmbeddedDerby)
			b.append("import java.sql.*;\n");

		if (project.isJavaSEApplication())
			b.append("import net.codecadenza.runtime.jpa.*;\n");

		new LoggingGenerator(false).getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("public class " + DEFAULT_APPLICATION_NAME + "\n");
		b.append("{\n");
		b.append("private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());\n");
		b.append("private static final String DATA_PATH = \"application.data\";\n");

		if (!project.isJavaSEApplication()) {
			b.append("private static final String ALIAS_HOST_LOCAL = \"LOCAL\";\n");
			b.append("private static final String ALIAS_HOST_TEST = \"TEST\";\n");
		}
		else
			b.append("private static final String ALIAS_HOST_DB = \"DB\";\n");

		b.append("private static final float FONT_FACTOR = 1.0f;\n\n");
		b.append("private JFrame mainFrame;\n\n");
		b.append("/**\n");
		b.append(" * Shutdown services\n");
		b.append(" */\n");
		b.append("public void shutdown()\n");
		b.append("{\n");

		if (usesEmbeddedDerby)
			b.append("Connection con = null;\n\n");

		b.append("final int result = JOptionPane.showConfirmDialog(mainFrame, ");
		b.append(i18n.getI18NMessage("application_exit", "Do you really want to exit?"));
		b.append(", " + i18n.getI18NMessage("application_name", "Application name") + ", JOptionPane.YES_NO_OPTION);\n\n");
		b.append("if(result == JOptionPane.NO_OPTION)\n");
		b.append("return;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("// Save \"local\" user queries and format settings\n");
		b.append("PersistenceHelper.save();\n");

		if (project.isJavaSEApplication()) {
			b.append("\n// Shutdown JPA engine\n");
			b.append("PersistenceEngine.shutdown();\n");
		}

		if (usesEmbeddedDerby) {
			b.append("\n// Shutdown embedded database instance!\n");

			String connectionURL = project.getDataSource().getConnectionURL();

			if (!connectionURL.endsWith(";"))
				connectionURL += ";";

			connectionURL += "shutdown=true";

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

		b.append("\nmainFrame.dispose();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Method to increase font sizes of all components by given factor!\n");
		b.append(" */\n");
		b.append("private void increaseFontSize()\n");
		b.append("{\n");
		b.append("final UIDefaults defaults = UIManager.getDefaults();\n");
		b.append("final Enumeration<?> keys = defaults.keys();\n\n");
		b.append("while(keys.hasMoreElements())\n");
		b.append("{\n");
		b.append("final Object key = keys.nextElement();\n");
		b.append("final Object value = defaults.get(key);\n\n");
		b.append("if(value instanceof Font)\n");
		b.append("{\n");
		b.append("UIManager.put(key, null);\n\n");
		b.append("final Font font = UIManager.getFont(key);\n\n");
		b.append("if(font != null)\n");
		b.append("{\n");
		b.append("final float size = font.getSize2D() * FONT_FACTOR;\n\n");
		b.append("UIManager.put(key, new FontUIResource(font.deriveFont(size)));\n");
		b.append("}\n");
		b.append("}\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Action to exit application\n");
		b.append(" */\n");
		b.append("private class ExitAction extends AbstractAction\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public ExitAction()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18NMessage("action_name_exit", "Exit") + ");\n\n");
		b.append("putValue(SHORT_DESCRIPTION, " + i18n.getI18NMessage("action_shortdesc_exit", "Exit application") + ");\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void actionPerformed(ActionEvent e)\n");
		b.append("{\n");
		b.append("shutdown();\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Action to open preferences dialog\n");
		b.append(" */\n");
		b.append("private class PreferencesAction extends AbstractAction\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public PreferencesAction()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18NMessage("action_name_pref", "Preferences"));
		b.append(", ImageLoader.getImage(ImageLoader.PREFERENCES));\n\n");
		b.append("putValue(SHORT_DESCRIPTION, " + i18n.getI18NMessage("action_shortdesc_pref", "Open preferences") + ");\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void actionPerformed(ActionEvent e)\n");
		b.append("{\n");
		b.append("final var dlg = new FormatPreferencesDialog();\n");
		b.append("dlg.setVisible(true);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Action to open change password dialog\n");
		b.append(" */\n");
		b.append("private class ChangePasswordAction extends AbstractAction\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public ChangePasswordAction()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18NMessage("action_name_change_password", "Change password") + ");\n\n");
		b.append("putValue(SHORT_DESCRIPTION, ");
		b.append(i18n.getI18NMessage("action_shortdesc_change_password", "Open dialog to change your password") + ");\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void actionPerformed(ActionEvent e)\n");
		b.append("{\n");
		b.append("final var dlg = new " + CHANGE_PWD_DLG_NAME + "();\n");
		b.append("dlg.setVisible(true);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return a map that contains all available connection configurations\n");
		b.append(" */\n");
		b.append("private Map<String, ServiceLocatorDTO> initializeHosts()\n");
		b.append("{\n");
		b.append("final var hostMap = new HashMap<String, ServiceLocatorDTO>();\n\n");

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
		b.append(" * Initialize main frame of application\n");
		b.append(" */\n");
		b.append("private void initApplicationFrame()\n");
		b.append("{\n");
		b.append("// Initialize persistence helper\n");
		b.append("try\n");
		b.append("{\n");
		b.append("PersistenceHelper.initialize(DATA_PATH);\n");
		b.append("}\n");
		b.append("catch (final GeneralPersistenceExeption e1)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while starting local persistence system!", "e1");

		b.append("}\n\n");
		b.append("// Change the look-and-feel here\n");
		b.append("try\n");
		b.append("{\n");
		b.append("UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while setting the application's look and feel!", "e");

		b.append("}\n\n");
		b.append("increaseFontSize();\n\n");
		b.append("// Open logon dialog\n");
		b.append("final var logOnDialog = new " + DEFAULT_LOG_ON_DLG_NAME + "(initializeHosts());\n");
		b.append("logOnDialog.setVisible(true);\n\n");
		b.append("if(logOnDialog.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
		b.append("return;\n\n");
		b.append("mainFrame = new JFrame(" + i18n.getI18NMessage("application_name", "Application name") + ");\n");
		b.append("mainFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);\n");
		b.append("mainFrame.setSize(1000, 700);\n\n");
		b.append("// Center application frame\n");
		b.append("mainFrame.setLocationRelativeTo(null);\n\n");
		b.append("// Create actions\n");
		b.append("final var exitAction = new ExitAction();\n");
		b.append("final var preferencesAction = new PreferencesAction();\n");
		b.append("final var changePasswordAction = new ChangePasswordAction();\n\n");
		b.append("// Create contents of application frame\n");
		b.append("final var menuBar = new JMenuBar();\n");
		b.append("mainFrame.setJMenuBar(menuBar);\n\n");
		b.append("// Define and add two drop down menus to menu bar\n");
		b.append("final var fileMenu = new JMenu(" + i18n.getI18NMessage("mnu_file", "File") + ");\n");
		b.append("fileMenu.add(exitAction);\n\n");
		b.append("final var editMenu = new JMenu(" + i18n.getI18NMessage("mnu_edit", "Edit") + ");\n");
		b.append("editMenu.add(preferencesAction);\n");
		b.append("editMenu.add(changePasswordAction);\n\n");
		b.append("menuBar.add(fileMenu);\n");
		b.append("menuBar.add(editMenu);\n\n");
		b.append("final var splitPane = new JSplitPane();\n");
		b.append("splitPane.setAutoscrolls(true);\n");
		b.append("splitPane.setResizeWeight(0.15);\n");
		b.append("splitPane.setDividerLocation(0.15);\n");
		b.append("splitPane.setOneTouchExpandable(true);\n\n");
		b.append("mainFrame.getContentPane().add(splitPane, BorderLayout.CENTER);\n\n");
		b.append("final var desktop = new JDesktopPane();\n");
		b.append("desktop.setBackground(SystemColor.LIGHT_GRAY);\n\n");
		b.append("splitPane.setRightComponent(desktop);\n\n");
		b.append("final var tabbedPane = new JTabbedPane(JTabbedPane.TOP);\n");
		b.append("tabbedPane.setBorder(new EmptyBorder(2, 2, 0, 0));\n\n");
		b.append("splitPane.setLeftComponent(tabbedPane);\n\n");
		b.append("final var navigatorPanel = new NavigatorPanel(desktop);\n");
		b.append("navigatorPanel.setBorder(new EmptyBorder(0, 0, 0, 0));\n\n");
		b.append("tabbedPane.add(" + i18n.getI18NMessage("label_navigator", "Navigator") + ", navigatorPanel);\n\n");
		b.append("final var toolBar = new JToolBar();\n");
		b.append("toolBar.add(preferencesAction);\n\n");
		b.append("mainFrame.getContentPane().add(toolBar, BorderLayout.NORTH);\n");
		b.append("mainFrame.setVisible(true);\n\n");
		b.append("mainFrame.addWindowListener(new WindowAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void windowClosing(WindowEvent arg0)\n");
		b.append("{\n");
		b.append("shutdown();\n");
		b.append("}\n");
		b.append("});\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Start application\n");
		b.append(" * @param args\n");
		b.append(" */\n");
		b.append("public static void main(String[] args)\n");
		b.append("{\n");
		b.append("Locale.setDefault(Locale.ENGLISH);\n\n");
		b.append("final var app = new " + DEFAULT_APPLICATION_NAME + "();\n");
		b.append("app.initApplicationFrame();\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the logon dialog
	 * @return the content of the logon dialog class
	 */
	private String createLogOnDialog() {
		final var b = new StringBuilder();
		b.append("import java.util.Map;\n");
		b.append("import net.codecadenza.runtime.richclient.swing.dialog.AbstractLogOnDialog;\n");
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");
		b.append("import " + project.getClientNamespace().toString() + SUB_PACKAGE_UTIL + "." + SECURITY_MANAGER + ";\n\n");
		b.append("public class " + DEFAULT_LOG_ON_DLG_NAME + " extends AbstractLogOnDialog\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param hostMap a map that contains all available connection configurations\n");
		b.append(" */\n");
		b.append("public " + DEFAULT_LOG_ON_DLG_NAME + "(Map<String, ServiceLocatorDTO> hostMap)\n");
		b.append("{\n");
		b.append("super(hostMap);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLogOnDialog#");
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
