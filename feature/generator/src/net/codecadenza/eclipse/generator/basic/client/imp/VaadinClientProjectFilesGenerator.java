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

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.HOME_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.PREFERENCES_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_APPLICATION_NAME;
import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;

/**
 * <p>
 * Generator for basic source and configuration files that are necessary for Vaadin applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinClientProjectFilesGenerator extends AbstractClientProjectFilesGenerator {
	public static final String APPLICATION_COMMENT = "Class to configure application features";
	public static final String HOME_VIEW_COMMENT = "The landing page";
	public static final String MAIN_VIEW_COMMENT = "Main view that determines the application layout";
	public static final String PREFERENCES_VIEW_COMMENT = "View for changing user preferences";

	private VaadinI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinClientProjectFilesGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		i18n = new VaadinI18NGenerator(project);

		final JavaFile homeView = createJavaSourceFile(HOME_VIEW, createHomeView(), HOME_VIEW_COMMENT);
		final JavaFile mainView = createJavaSourceFile(MAIN_VIEW, createMainView(), MAIN_VIEW_COMMENT);
		final JavaFile preferencesView = createJavaSourceFile(PREFERENCES_VIEW, createPreferencesView(), PREFERENCES_VIEW_COMMENT);
		final var javaFiles = new ArrayList<>(List.of(homeView, mainView, preferencesView));

		if (project.isJakartaEEApplication()) {
			final JavaFile application = createJavaSourceFile(DEFAULT_APPLICATION_NAME, createApplication(), APPLICATION_COMMENT);

			javaFiles.add(application);
		}

		i18n.save();

		return javaFiles;
	}

	/**
	 * @return the generated content
	 */
	private String createApplication() {
		final var b = new StringBuilder();
		b.append("import com.vaadin.flow.component.dependency.*;\n");
		b.append("import com.vaadin.flow.component.page.*;\n");
		b.append("import com.vaadin.flow.theme.lumo.*;\n\n");
		b.append("@StyleSheet(Lumo.STYLESHEET)\n");
		b.append("@StyleSheet(Lumo.COMPACT_STYLESHEET)\n");
		b.append("@StyleSheet(\"styles.css\")\n");
		b.append("public class " + DEFAULT_APPLICATION_NAME + " implements AppShellConfigurator\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createHomeView() {
		var homeViewText1 = "The generated application provides all features of your model. Beside of ";
		homeViewText1 += "standard CRUD (Create, Read, Update and Delete) functionality you can also invoke your data exchange operations, ";
		homeViewText1 += "use tree views and other great stuff!";

		var homeViewText2 = "All generated form groups and view forms are available in the tree navigator. ";
		homeViewText2 += "By opening a view form you can use further functions and dialogs upon a selected domain object!";

		var homeViewText3 = "Most of the generated artifacts can be used without modification. However, if you ";
		homeViewText3 += "like to adapt the generated sources make sure that you replace respective @Generated by @Customized annotations in order ";
		homeViewText3 += "to avoid changes being lost by source code rebuild operations!";

		final var b = new StringBuilder();
		final String transText1 = i18n.getI18NMessage("homeview_text1", homeViewText1);
		final String transText2 = i18n.getI18NMessage("homeview_text2", homeViewText2);
		final String transText3 = i18n.getI18NMessage("homeview_text3", homeViewText3);

		b.append("import static " + project.getClientNamespace().toString() + "." + TRANSLATION_KEYS + ".*;\n");

		if (project.isJakartaEEApplication())
			b.append("import jakarta.inject.*;\n");

		b.append("import com.vaadin.flow.component.*;\n");
		b.append("import com.vaadin.flow.component.html.*;\n");
		b.append("import com.vaadin.flow.component.orderedlayout.*;\n");
		b.append("import com.vaadin.flow.router.*;\n");
		b.append("import net.codecadenza.runtime.webclient.vaadin.i18n.*;\n\n");
		b.append("@Route(value = " + HOME_VIEW + ".ROUTE, layout = " + MAIN_VIEW + ".class)\n");
		b.append("@RouteAlias(value = \"\", layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + HOME_VIEW + " extends VerticalLayout\n");
		b.append("{\n");
		b.append("public static final String ROUTE = \"home\";\n");
		b.append("private static final long serialVersionUID = 1L;\n");
		b.append("private final I18NService i18n;\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append("public " + HOME_VIEW + "()\n");
			b.append("{\n");
			b.append("this.i18n = null;\n");
			b.append("}\n\n");
		}

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param i18n\n");
		b.append(" */\n");

		if (project.isJakartaEEApplication())
			b.append("@Inject\n");

		b.append("public " + HOME_VIEW + "(I18NService i18n)\n");
		b.append("{\n");
		b.append("this.i18n = i18n;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void onAttach(AttachEvent attachEvent)\n");
		b.append("{\n");
		b.append("final var title = new H1(" + i18n.getI18NMessage("homeview_title", "Guide to the generated application") + ");\n");
		b.append("final var header1 = new H2(" + i18n.getI18NMessage("homeview_header_1", "A Complete Application") + ");\n");
		b.append("final var text1 = new NativeLabel(" + transText1 + ");\n");
		b.append("final var header2 = new H2(" + i18n.getI18NMessage("homeview_header_2", "Everything You Need Is Here") + ");\n");
		b.append("final var text2 = new NativeLabel(" + transText2 + ");\n");
		b.append("final var header3 = new H2(" + i18n.getI18NMessage("homeview_header_3", "The Basis for Real World") + ");\n");
		b.append("final var text3 = new NativeLabel(" + transText3 + ");\n\n");
		b.append("add(title, new Paragraph(header1, text1), new Paragraph(header2, text2), new Paragraph(header3, text3));\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createPreferencesView() {
		final var b = new StringBuilder();
		final var locale = i18n.getLocaleFragment();
		final var illegalDateFormatMessage = i18n.getI18NMessage("msg_illegal_date_format", "Please insert a valid date format!");
		final var illegalDateTimeFormatMessage = i18n.getI18NMessage("msg_illegal_date_time_format",
				"Please insert a valid date time format!");
		final var illegalNumberFormatMessage = i18n.getI18NMessage("msg_illegal_number_format",
				"Please insert a valid number format!");

		b.append("import static " + project.getClientNamespace().toString() + "." + TRANSLATION_KEYS + ".*;\n");
		b.append("import net.codecadenza.runtime.webclient.vaadin.util.*;\n");
		b.append("import net.codecadenza.runtime.webclient.vaadin.dialog.*;\n");
		b.append("import net.codecadenza.runtime.webclient.vaadin.i18n.*;\n");

		if (project.isJakartaEEApplication())
			b.append("import jakarta.inject.*;\n");

		b.append("import java.text.*;\n");
		b.append("import com.vaadin.flow.component.*;\n");
		b.append("import com.vaadin.flow.component.button.*;\n");
		b.append("import com.vaadin.flow.component.orderedlayout.*;\n");
		b.append("import com.vaadin.flow.component.textfield.*;\n");
		b.append("import com.vaadin.flow.router.*;\n\n");
		b.append("@Route(value = " + PREFERENCES_VIEW + ".ROUTE, layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + PREFERENCES_VIEW + " extends VerticalLayout implements HasDynamicTitle\n");
		b.append("{\n");
		b.append("public static final String ROUTE = \"user/" + PREFERENCES_VIEW + "\";\n");
		b.append("private static final long serialVersionUID = 1L;\n");
		b.append("private static final int FIELD_WIDTH = 300;\n\n");
		b.append("private final PreferencesStore preferences;\n");
		b.append("private final I18NService i18n;\n");
		b.append("private final TextField txtDateFormat = new TextField();\n");
		b.append("private final TextField txtDateTimeFormat = new TextField();\n");
		b.append("private final TextField txtNumberFormat = new TextField();\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append("public " + PREFERENCES_VIEW + "()\n");
			b.append("{\n");
			b.append("this.preferences = null;\n");
			b.append("this.i18n = null;\n");
			b.append("}\n\n");
		}

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param preferences\n");
		b.append(" * @param i18n\n");
		b.append(" */\n");

		if (project.isJakartaEEApplication())
			b.append("@Inject\n");

		b.append("public " + PREFERENCES_VIEW + "(PreferencesStore preferences, I18NService i18n)\n");
		b.append("{\n");
		b.append("this.preferences = preferences;\n");
		b.append("this.i18n = i18n;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void onAttach(AttachEvent attachEvent)\n");
		b.append("{\n");
		b.append("txtDateFormat.setValue(preferences.getDateFormat());\n");
		b.append("txtDateFormat.setWidth(FIELD_WIDTH, Unit.PIXELS);\n");
		b.append("txtDateFormat.setLabel(");
		b.append(i18n.getI18NMessage("field_editpreferencesdialog_txtdateformat", "Date format", true) + ");\n\n");
		b.append("txtDateTimeFormat.setValue(preferences.getDateTimeFormat());\n");
		b.append("txtDateTimeFormat.setWidth(FIELD_WIDTH, Unit.PIXELS);\n");
		b.append("txtDateTimeFormat.setLabel(");
		b.append(i18n.getI18NMessage("field_editpreferencesdialog_txtdatetimeformat", "Date time format", true) + ");\n\n");
		b.append("txtNumberFormat.setValue(preferences.getNumberFormat());\n");
		b.append("txtNumberFormat.setWidth(FIELD_WIDTH, Unit.PIXELS);\n");
		b.append("txtNumberFormat.setLabel(");
		b.append(i18n.getI18NMessage("field_editpreferencesdialog_txtnumberformat", "Number format", true) + ");\n\n");
		b.append("final var cmdOK = new Button(" + i18n.getI18NMessage("cmd_save", "Save") + ");\n");
		b.append("cmdOK.addClickListener(_ -> savePreferences());\n\n");
		b.append("final var cmdCancel = new Button(" + i18n.getI18NMessage("cmd_cancel", "Cancel") + ");\n");
		b.append("cmdCancel.addClickListener(_ -> getUI().ifPresent(ui -> ui.getPage().getHistory().back()));\n\n");
		b.append("final var hlButtons = new HorizontalLayout();\n");
		b.append("hlButtons.add(cmdOK, cmdCancel);\n\n");
		b.append("add(txtDateFormat, txtDateTimeFormat, txtNumberFormat, hlButtons);\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.HasDynamicTitle#getPageTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getPageTitle()\n");
		b.append("{\n");
		b.append("return " + i18n.getI18NMessage("form_editpreferencesdialog_title", "Change preferences") + ";\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Save preferences\n");
		b.append(" */\n");
		b.append("private void savePreferences()\n");
		b.append("{\n");
		b.append("final var dialogTitle =" + i18n.getI18NMessage("form_editpreferencesdialog_title", "Change preferences") + ";\n");
		b.append("final var dateFormat = new SimpleDateFormat();\n");
		b.append("final var decimalFormat = new DecimalFormat();\n\n");
		b.append("// Validate user input\n");
		b.append("try\n");
		b.append("{\n");
		b.append("// The method applyPattern() accepts empty Strings!\n");
		b.append("if(txtDateFormat.getValue() == null || txtDateFormat.getValue().isEmpty())\n");
		b.append("throw new IllegalStateException();\n\n");
		b.append("dateFormat.applyPattern(txtDateFormat.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("new InfoMessageDialog(dialogTitle, " + illegalDateFormatMessage + ", " + locale + ").open();\n\n");
		b.append("txtDateFormat.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(txtDateTimeFormat.getValue() == null || txtDateTimeFormat.getValue().isEmpty())\n");
		b.append("throw new IllegalStateException();\n\n");
		b.append("dateFormat.applyPattern(txtDateTimeFormat.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("new InfoMessageDialog(dialogTitle, " + illegalDateTimeFormatMessage + ", " + locale + ").open();\n\n");
		b.append("txtDateTimeFormat.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(txtNumberFormat.getValue() == null || txtNumberFormat.getValue().isEmpty())\n");
		b.append("throw new IllegalStateException();\n\n");
		b.append("decimalFormat.applyPattern(txtNumberFormat.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("new InfoMessageDialog(dialogTitle, " + illegalNumberFormatMessage + ", " + locale + ").open();\n\n");
		b.append("txtNumberFormat.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("preferences.setDateFormat(txtDateFormat.getValue());\n");
		b.append("preferences.setDateTimeFormat(txtDateTimeFormat.getValue());\n");
		b.append("preferences.setNumberFormat(txtNumberFormat.getValue());\n");
		b.append("preferences.save();\n\n");
		b.append("getUI().ifPresent(ui -> ui.getPage().getHistory().back());\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createMainView() {
		final var b = new StringBuilder();
		b.append("import static " + project.getClientNamespace().toString() + "." + TRANSLATION_KEYS + ".*;\n");

		if (project.isJakartaEEApplication())
			b.append("import jakarta.inject.*;\n");

		b.append("import com.vaadin.flow.component.*;\n");
		b.append("import com.vaadin.flow.component.applayout.*;\n");
		b.append("import com.vaadin.flow.component.avatar.*;\n");
		b.append("import com.vaadin.flow.component.html.*;\n");
		b.append("import com.vaadin.flow.component.orderedlayout.*;\n");
		b.append("import com.vaadin.flow.router.*;\n");
		b.append("import net.codecadenza.runtime.webclient.vaadin.i18n.*;\n");

		if (project.isProtectManualChanges())
			b.append("import " + GENERATED_ELEMENT_ANNOTATION + ";\n");

		b.append("\n");
		b.append("public class " + MAIN_VIEW + " extends AppLayout implements AfterNavigationObserver\n");
		b.append("{\n");
		b.append(annotationForGeneratedElement);
		b.append("private static final long serialVersionUID = 1;\n");
		b.append(annotationForGeneratedElement);
		b.append("private final H1 viewTitle = new H1();\n\n");
		b.append(annotationForGeneratedElement);
		b.append("private final I18NService i18n;\n");
		b.append(annotationForGeneratedElement);
		b.append("private TreeNavigator treeNavigator;\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("/**\n");
			b.append(" * Default constructor\n");
			b.append(" */\n");
			b.append(annotationForGeneratedElement);
			b.append("public " + MAIN_VIEW + "()\n");
			b.append("{\n");
			b.append("this.i18n = null;\n");
			b.append("}\n\n");
		}

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param i18n\n");
		b.append(" */\n");

		if (project.isJakartaEEApplication())
			b.append("@Inject\n");

		b.append(annotationForGeneratedElement);
		b.append("public " + MAIN_VIEW + "(I18NService i18n)\n");
		b.append("{\n");
		b.append("this.i18n = i18n;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("@Override\n");
		b.append("protected void onAttach(AttachEvent attachEvent)\n");
		b.append("{\n");
		b.append("treeNavigator = new TreeNavigator(i18n);\n\n");
		b.append("setPrimarySection(Section.DRAWER);\n");
		b.append("addToNavbar(true, createHeader());\n");
		b.append("addToDrawer(createSideMenu());\n\n");
		b.append("treeNavigator.buildTree();\n");
		b.append("}\n\n");
		b.append("/*\n");
		b.append(" * (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.AfterNavigationObserver#");
		b.append("afterNavigation(com.vaadin.flow.router.AfterNavigationEvent)\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("@Override\n");
		b.append("public void afterNavigation(AfterNavigationEvent event)\n");
		b.append("{\n");
		b.append("viewTitle.setText(getCurrentPageTitle());\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the generated page header\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("private Component createHeader()\n");
		b.append("{\n");
		b.append("final var hlHeader = new HorizontalLayout();\n");
		b.append("hlHeader.setClassName(\"sidemenu-header\");\n");
		b.append("hlHeader.getThemeList().set(\"dark\", true);\n");
		b.append("hlHeader.setWidthFull();\n");
		b.append("hlHeader.setSpacing(false);\n");
		b.append("hlHeader.setAlignItems(FlexComponent.Alignment.CENTER);\n");
		b.append("hlHeader.add(new DrawerToggle(), viewTitle, new Avatar());\n\n");
		b.append("return hlHeader;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the generated side menu with a logo and the tree navigator\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("private Component createSideMenu()\n");
		b.append("{\n");
		b.append("final var hlLogo = new HorizontalLayout();\n");
		b.append("hlLogo.setId(\"logo\");\n");
		b.append("hlLogo.setAlignItems(FlexComponent.Alignment.CENTER);\n");
		b.append("hlLogo.add(new Image(\"images/logo.png\", \"My App logo\"));\n");
		b.append("hlLogo.add(new H1(" + i18n.getI18NMessage("application_title", "My generated application") + "));\n\n");
		b.append("final var vlSideMenu = new VerticalLayout();\n");
		b.append("vlSideMenu.setClassName(\"sidemenu-menu\");\n");
		b.append("vlSideMenu.setSizeFull();\n");
		b.append("vlSideMenu.setPadding(false);\n");
		b.append("vlSideMenu.setSpacing(false);\n");
		b.append("vlSideMenu.setAlignItems(FlexComponent.Alignment.STRETCH);\n");
		b.append("vlSideMenu.add(hlLogo, treeNavigator);\n\n");
		b.append("return vlSideMenu;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the title of the currently selected page\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("private String getCurrentPageTitle()\n");
		b.append("{\n");
		b.append("if(getContent() instanceof final HasDynamicTitle content)\n");
		b.append("return content.getPageTitle();\n\n");
		b.append("final PageTitle title = getContent().getClass().getAnnotation(PageTitle.class);\n");
		b.append("return title == null ? \"\" : title.value();\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createWebXML() {
		final var b = new StringBuilder();
		final boolean secure = project.getServerPlatform() != ServerPlatformEnumeration.GLASSFISH
				&& project.getApplicationLogOnDTO() != null && project.getLogOnBoundary() != null;

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<web-app xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/jakartaee ");
		b.append("https://jakarta.ee/xml/ns/jakartaee/web-app_6_1.xsd\"\n");
		b.append("\tid=\"WebApp_ID\" version=\"6.1\">\n\n");

		if (secure) {
			project.getRoles().forEach(role -> {
				b.append("\t<security-role>\n");
				b.append("\t\t<role-name>" + role.getName() + "</role-name>\n");
				b.append("\t</security-role>\n");
			});

			b.append("\n\t<security-constraint>\n");
			b.append("\t\t<web-resource-collection>\n");
			b.append("\t\t\t<web-resource-name>Restricted areas</web-resource-name>\n");
			b.append("\t\t\t<url-pattern>/dialog/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/tree/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/user/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/view/*</url-pattern>\n");
			b.append("\t\t</web-resource-collection>\n");
			b.append("\t\t<auth-constraint>\n");

			project.getRoles().forEach(role -> b.append("\t\t\t<role-name>" + role.getName() + "</role-name>\n"));

			b.append("\t\t</auth-constraint>\n");
			b.append("\t</security-constraint>\n\n");
			b.append("\t<security-constraint>\n");
			b.append("\t\t<web-resource-collection>\n");
			b.append("\t\t\t<web-resource-name>Unrestricted areas</web-resource-name>\n");
			b.append("\t\t\t<url-pattern>/VAADIN/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/favicon.ico</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/manifest.webmanifest</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/sw.js</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/offline.html</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/icons/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/images/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/styles/*</url-pattern>\n");
			b.append("\t\t\t<url-pattern>/login</url-pattern>\n");
			b.append("\t\t</web-resource-collection>\n");
			b.append("\t</security-constraint>\n\n");
		}

		b.append("\t<session-config>\n");
		b.append("\t\t<session-timeout>30</session-timeout>\n");
		b.append("\t</session-config>\n\n");
		b.append("</web-app>\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createStylesheet() {
		final StringBuilder b = new StringBuilder();
		b.append("/* Width of the side-menu */\n");
		b.append("vaadin-app-layout::part(drawer) {\n");
		b.append("  width: 350px;\n");
		b.append("}\n\n");
		b.append("vaadin-app-layout[dir='rtl'] .sidemenu-header vaadin-avatar {\n");
		b.append("  margin-left: var(--lumo-space-m);\n");
		b.append("  margin-right: auto;\n");
		b.append("  width: 600px;\n");
		b.append("}\n\n");
		b.append(".sidemenu-header {\n");
		b.append("  height: var(--lumo-size-xl);\n");
		b.append("  box-shadow: var(--lumo-box-shadow-s);\n");
		b.append("  padding-right: var(--lumo-space-m);\n");
		b.append("  width: 600px;\n");
		b.append("}\n\n");
		b.append(".sidemenu-header vaadin-avatar {\n");
		b.append("  margin-left: auto;\n");
		b.append("  margin-right: var(--lumo-space-m);\n");
		b.append("}\n\n");
		b.append(".sidemenu-header h1 {\n");
		b.append("  font-size: var(--lumo-font-size-l);\n");
		b.append("  margin: 0;\n");
		b.append("}\n\n");
		b.append(".sidemenu-menu #logo {\n");
		b.append("  box-sizing: border-box;\n");
		b.append("  box-shadow: inset 0 -1px var(--lumo-contrast-10pct);\n");
		b.append("  padding: var(--lumo-space-s) var(--lumo-space-m);\n");
		b.append("}\n\n");
		b.append(".sidemenu-menu #logo img {\n");
		b.append("  height: calc(var(--lumo-size-l) * 1.5);\n");
		b.append("}\n\n");
		b.append(".sidemenu-menu #logo h1 {\n");
		b.append("  font-size: var(--lumo-font-size-xl);\n");
		b.append("  font-weight: 600;\n");
		b.append("  margin: 0 var(--lumo-space-s);\n");
		b.append("}\n\n");
		b.append("legend {\n");
		b.append("  font-size: var(--lumo-font-size-m);\n");
		b.append("  font-weight: bold;\n");
		b.append("  margin-inline-start: 10px;\n");
		b.append("}\n\n");
		b.append("fieldset {\n");
		b.append("  padding: 0px;\n");
		b.append("  margin: 10px 0px 10px 0px;\n");
		b.append("  border: 1px solid #CDCDCD;\n");
		b.append("}\n\n");

		return b.toString();
	}

}
