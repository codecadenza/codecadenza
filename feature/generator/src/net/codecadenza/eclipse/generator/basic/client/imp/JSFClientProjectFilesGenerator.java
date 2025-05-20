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

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.WELCOME_FILE;
import static net.codecadenza.eclipse.shared.Constants.BUNDLE_NAME;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;
import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;
import static net.codecadenza.eclipse.shared.Constants.UI_DIALOG_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_LOV_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_TREE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_VIEW_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.generator.common.LoggingGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for basic source and configuration files that are necessary for JSF applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFClientProjectFilesGenerator extends AbstractClientProjectFilesGenerator {
	public static final String INDEX_CONTROLLER_COMMENT = "Controller that forwards all respective requests to the index page";
	public static final String INDEX_CONTROLLER_NAME = "IndexController";
	public static final String USER_SESSION_COMMENT = "Class that holds user-related session data";
	public static final String USER_SESSION_NAME = USER_SESSION_BEAN.substring(0, 1).toUpperCase() + USER_SESSION_BEAN.substring(1);

	/**
	 * Constructor
	 * @param project
	 */
	public JSFClientProjectFilesGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		final var sourceFiles = new ArrayList<JavaFile>();

		sourceFiles.add(createJavaSourceFile(USER_SESSION_NAME, createUserSession(), USER_SESSION_COMMENT));

		if (project.isSpringBootApplication())
			sourceFiles.add(createJavaSourceFile(INDEX_CONTROLLER_NAME, createIndexController(), INDEX_CONTROLLER_COMMENT));

		return sourceFiles;
	}

	/**
	 * @return the generated content
	 */
	private String createUserSession() {
		final var b = new StringBuilder();
		final var bundleInit = "ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, locale)";
		final boolean protectManualChanges = project.isProtectManualChanges();
		final var loggingGenerator = new LoggingGenerator(protectManualChanges);

		b.append("import static " + project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS + ".*;\n");
		b.append("import java.util.*;\n");
		b.append("import jakarta.annotation.PostConstruct;\n");
		b.append("import jakarta.faces.application.*;\n");
		b.append("import jakarta.faces.context.*;\n");
		b.append("import jakarta.faces.event.*;\n");
		b.append("import jakarta.inject.*;\n");
		b.append("import jakarta.servlet.http.*;\n");
		b.append("import java.io.*;\n");
		b.append("import java.net.*;\n");
		b.append("import java.nio.charset.*;\n");
		b.append("import net.codecadenza.runtime.webclient.primefaces.util.*;\n");

		if (protectManualChanges)
			b.append("import " + GENERATED_ELEMENT_ANNOTATION + ";\n");

		if (project.isSpringBootApplication())
			b.append("import org.springframework.web.context.annotation.SessionScope;\n");
		else
			b.append("import jakarta.enterprise.context.*;\n");

		loggingGenerator.getImports().forEach(imp -> b.append(imp + "\n"));

		b.append("\n");
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(USER_SESSION_NAME) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@SessionScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class " + USER_SESSION_NAME + " implements Serializable\n");
		b.append("{\n");

		loggingGenerator.addField(b);

		new JavaFieldGenerator(JavaType.LONG, "serialVersionUID", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.asConstant("1L").create();
		new JavaFieldGenerator(JavaType.STRING, "DEFAULT_BUNDLE_NAME", JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges)
				.asConstant("\"" + BUNDLE_NAME + "\"").create();
		new JavaFieldGenerator(JavaType.STRING, "DATE_FORMAT_COOKIE", JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges)
				.asConstant("\"dateFormat\"").create();
		new JavaFieldGenerator(JavaType.STRING, "DATE_TIME_FORMAT_COOKIE", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges).asConstant("\"dateTimeFormat\"").create();
		new JavaFieldGenerator(JavaType.STRING, "NUMBER_FORMAT_COOKIE", JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges)
				.asConstant("\"numberFormat\"").create();
		new JavaFieldGenerator(JavaType.STRING, "TIMEZONE_COOKIE", JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges)
				.asConstant("\"timeZone\"").create();
		new JavaFieldGenerator(JavaType.STRING, "START_PAGE", JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges)
				.asConstant("\"" + UI_VIEW_FOLDER + "/index.jsf?faces-redirect=true\"").create();
		new JavaFieldGenerator("Map<String, Locale>", "supportedLocales", JavaFieldGenerator.VISIBILITY_PRIVATE, b,
				protectManualChanges).asConstant(null).create();
		new JavaFieldGenerator(JavaType.STRING, "dateStyle", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"medium\"").create();
		new JavaFieldGenerator(JavaType.STRING, "dateFormat", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"dd.MM.yyyy\"").create();
		new JavaFieldGenerator(JavaType.STRING, "dateTimeFormat", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"dd.MM.yyyy HH:mm:ss\"").create();
		new JavaFieldGenerator(JavaType.STRING, "numberFormat", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"###,###,##0.0000\"").create();
		new JavaFieldGenerator(JavaType.STRING, "localeCode", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.create();
		new JavaFieldGenerator(JavaType.STRING, "oldPassword", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"\"").create();
		new JavaFieldGenerator(JavaType.STRING, "newPassword", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"\"").create();
		new JavaFieldGenerator(JavaType.STRING, "confirmedPassword", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("\"\"").create();
		new JavaFieldGenerator(JavaType.STRING, "timeZone", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("TimeZone.getDefault().getID()").create();
		new JavaFieldGenerator("Locale", "locale", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("Locale.ENGLISH").create();
		new JavaFieldGenerator("LinkedList<String>", "history", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue("new LinkedList<>()").withFinalModifier().create();
		new JavaFieldGenerator("ResourceBundle", "bundle", JavaFieldGenerator.VISIBILITY_PRIVATE, b, protectManualChanges)
				.withDefaultValue(bundleInit).withTransientModifier().withFinalModifier().create();

		b.append("\nstatic\n");
		b.append("{\n");
		b.append("supportedLocales = new LinkedHashMap<>();\n");
		b.append("supportedLocales.put(\"EN\", Locale.ENGLISH);\n");
		b.append("}\n\n");

		final var fieldLocaleCode = new JavaFieldGenerator(JavaType.STRING, "localeCode", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldLocaleCode.createGetter("the locale code");
		fieldLocaleCode.createSetter();

		final var fieldLocale = new JavaFieldGenerator("Locale", "locale", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldLocale.createGetter("the locale");
		fieldLocale.createSetter();

		final var fieldDateStyle = new JavaFieldGenerator(JavaType.STRING, "dateStyle", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldDateStyle.createGetter("the date style");
		fieldDateStyle.createSetter();

		final var fieldDateFormat = new JavaFieldGenerator(JavaType.STRING, "dateFormat", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldDateFormat.createGetter("the date format");
		fieldDateFormat.createSetter();

		final var fieldDateTimeFormat = new JavaFieldGenerator(JavaType.STRING, "dateTimeFormat",
				JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges);
		fieldDateTimeFormat.createGetter("the date time format");
		fieldDateTimeFormat.createSetter();

		final var fieldNumberFormat = new JavaFieldGenerator(JavaType.STRING, "numberFormat", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldNumberFormat.createGetter("the number format");
		fieldNumberFormat.createSetter();

		final var fieldTimeZone = new JavaFieldGenerator(JavaType.STRING, "timeZone", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldTimeZone.createGetter("the time zone");
		fieldTimeZone.createSetter();

		final var fieldOldPassword = new JavaFieldGenerator(JavaType.STRING, "oldPassword", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldOldPassword.createGetter("the current password");
		fieldOldPassword.createSetter();

		final var fieldNewPassword = new JavaFieldGenerator(JavaType.STRING, "newPassword", JavaFieldGenerator.VISIBILITY_PUBLIC, b,
				protectManualChanges);
		fieldNewPassword.createGetter("the new password");
		fieldNewPassword.createSetter();

		final var fieldConfPassword = new JavaFieldGenerator(JavaType.STRING, "confirmedPassword",
				JavaFieldGenerator.VISIBILITY_PUBLIC, b, protectManualChanges);
		fieldConfPassword.createGetter("the re-entered password");
		fieldConfPassword.createSetter();

		b.append("/**\n");
		b.append(" * @return all supported locales\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public Map<String, Locale> getSupportedLocales()\n");
		b.append("{\n");
		b.append("return supportedLocales;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return all available time zones\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public String[] getTimeZones()\n");
		b.append("{\n");
		b.append("return TimeZone.getAvailableIDs();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Initialize session and try to read user settings from cookies\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("@PostConstruct\n");
		b.append("public void init()\n");
		b.append("{\n");

		LoggingGenerator.addDebugLog(b, "Initialize user session");

		b.append("\n");
		b.append("final var req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();\n");
		b.append("final Cookie[] cookies = req.getCookies();\n\n");
		b.append("if(cookies == null)\n");
		b.append("return;\n\n");
		b.append("for(final Cookie cookie : cookies)\n");
		b.append("{\n");
		b.append("String cookieValue = null;\n\n");
		b.append("if(cookie.getValue() == null || cookie.getValue().isEmpty())\n");
		b.append("continue;\n\n");
		b.append("cookieValue = URLDecoder.decode(cookie.getValue(), StandardCharsets.UTF_8);\n\n");
		b.append("if(cookie.getName().equals(DATE_FORMAT_COOKIE))\n");
		b.append("dateFormat = cookieValue;\n");
		b.append("else if(cookie.getName().equals(DATE_TIME_FORMAT_COOKIE))\n");
		b.append("dateTimeFormat = cookieValue;\n");
		b.append("else if(cookie.getName().equals(NUMBER_FORMAT_COOKIE))\n");
		b.append("numberFormat = cookieValue;\n");
		b.append("else if(cookie.getName().equals(TIMEZONE_COOKIE))\n");
		b.append("timeZone = cookieValue;\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Change event if user selects different locale\n");
		b.append(" * @param e\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public void onLocaleChanged(ValueChangeEvent e)\n");
		b.append("{\n");
		b.append("final String newLocaleValue = e.getNewValue().toString().toUpperCase();\n\n");
		b.append("if(supportedLocales.containsKey(newLocaleValue))\n");
		b.append("{\n");
		b.append("locale = supportedLocales.get(newLocaleValue);\n");
		b.append("localeCode = newLocaleValue;\n");
		b.append("FacesContext.getCurrentInstance().getViewRoot().setLocale(locale);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the navigation target\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public String logout()\n");
		b.append("{\n");
		b.append("return START_PAGE;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Method to change user password. This method will be regenerated ");
		b.append("as soon as security features are applied for this project!\n");
		b.append(" * @return the navigation target!\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public String changePassword()\n");
		b.append("{\n\n");
		b.append("return START_PAGE;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Save last visited URL in history\n");
		b.append(" * @param navigationCase\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public void setLastPage(String navigationCase)\n");
		b.append("{\n");
		b.append("history.push(navigationCase);\n\n");
		b.append("if(history.size() > 20)\n");
		b.append("history.pollLast();\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return the last entry from history list. If list is empty an empty String will be returned.\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public String getLastPage()\n");
		b.append("{\n");
		b.append("if(!history.isEmpty())\n");
		b.append("return history.pop();\n\n");
		b.append("return \"\";\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param currentPageURL the relative URL of the current page\n");
		b.append(" * @param targetURL the relative URL of the page to redirect to\n");
		b.append(" * @return true if the method has redirected the request to the given target URL\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public boolean redirectTo(String currentPageURL, String targetURL)\n");
		b.append("{\n");
		b.append("final FacesContext context = FacesContext.getCurrentInstance();\n");
		b.append("final String contextPath = context.getExternalContext().getRequestContextPath();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(targetURL != null && !targetURL.isEmpty())\n");
		b.append("{\n");
		b.append("setLastPage(currentPageURL);\n\n");
		b.append("context.getExternalContext().redirect(contextPath + targetURL);\n\n");

		LoggingGenerator.addDebugLog(b, "Redirect to page '{}'", "targetURL");

		b.append("\n");
		b.append("return true;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Redirect to target URL '{}' failed!", "e", "targetURL");

		b.append("}\n\n");
		b.append("return false;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Save user settings in respective cookies\n");
		b.append(" * @return the navigation target\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public String saveSettings()\n");
		b.append("{\n");
		b.append("final var resp = (HttpServletResponse) FacesContext.getCurrentInstance().getExternalContext().getResponse();\n");
		b.append("final var req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var dateFormatCookie = new Cookie(DATE_FORMAT_COOKIE, ");
		b.append("URLEncoder.encode(dateFormat, StandardCharsets.UTF_8));\n");
		b.append("dateFormatCookie.setPath(req.getContextPath());\n\n");
		b.append("final var dateTimeFormatCookie = new Cookie(DATE_TIME_FORMAT_COOKIE, ");
		b.append("URLEncoder.encode(dateTimeFormat, StandardCharsets.UTF_8));\n");
		b.append("dateTimeFormatCookie.setPath(req.getContextPath());\n\n");
		b.append("final var numberFormatCookie = new Cookie(NUMBER_FORMAT_COOKIE, ");
		b.append("URLEncoder.encode(numberFormat, StandardCharsets.UTF_8));\n");
		b.append("numberFormatCookie.setPath(req.getContextPath());\n\n");
		b.append("final var timezoneCookie = new Cookie(TIMEZONE_COOKIE, URLEncoder.encode(timeZone, StandardCharsets.UTF_8));\n");
		b.append("timezoneCookie.setPath(req.getContextPath());\n\n");
		b.append("resp.addCookie(dateFormatCookie);\n");
		b.append("resp.addCookie(dateTimeFormatCookie);\n");
		b.append("resp.addCookie(numberFormatCookie);\n");
		b.append("resp.addCookie(timezoneCookie);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		LoggingGenerator.addErrorLog(b, "Error while saving user settings in respective cookies!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_SAVE_FAIL, e);\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("return START_PAGE;\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the index controller for a JSF Spring Boot application
	 * @return the generated content
	 */
	private String createIndexController() {
		final var b = new StringBuilder();
		b.append("import org.springframework.stereotype.Controller;\n");
		b.append("import org.springframework.web.bind.annotation.RequestMapping;\n\n");
		b.append("@Controller\n");
		b.append("public class " + INDEX_CONTROLLER_NAME + "\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * @return the relative path of the index page if no path has been provided\n");
		b.append(" */\n");
		b.append("@RequestMapping(\"/\")\n");
		b.append("public String navigateToIndexPage()\n");
		b.append("{\n");
		b.append("return \"/view/index.jsf\";\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createI18NDefaultResourceFile() {
		final var b = new StringBuilder();
		b.append("application_title=My generated application\n");
		b.append("navigator_home=Home\n");
		b.append("navigator_logout=Logout\n");
		b.append("navigator_settings=Preferences\n");
		b.append("navigator_title=Main menu\n");
		b.append("navigator_password=Password\n");
		b.append("action_delete=Delete item\n");
		b.append("action_copy=Create copy\n");
		b.append("action_continue=Click here to continue...\n");
		b.append("mnu_export=Export\n");
		b.append("mnu_file=File\n");
		b.append("mnu_view=View\n");
		b.append("action_refresh=Refresh\n");
		b.append("action_create=Create new\n");
		b.append("action_add=Add\n");
		b.append("action_view=View\n");
		b.append("action_export=Export\n");
		b.append("action_import=Import\n");
		b.append("action_edit=Edit\n");
		b.append("action_export_xlsx=as XLSX\n");
		b.append("action_export_pdf=as PDF\n");
		b.append("action_fetch_data=Fetching data...\n");
		b.append("result_total_number_records=Total number of records: \n");
		b.append("command_add=Add\n");
		b.append("command_save=Save\n");
		b.append("command_cancel=Cancel\n");
		b.append("command_back=Go back\n");
		b.append("command_filter=Filter\n");
		b.append("label_list_enter_filter=Enter filter criterion: \n");
		b.append("dialog_delete_question=Are you sure to delete selected item?\n");
		b.append("dialog_delete_title=Confirm delete\n");
		b.append("dialog_copy_question=Do you really want to create a copy of selected item?\n");
		b.append("dialog_copy_title=Confirm copy operation\n");
		b.append("command_yes=Yes\n");
		b.append("command_no=No\n");
		b.append("command_browse=Browse\n");
		b.append("command_download=Download\n");
		b.append("errorpage_title=An error has occurred!\n");
		b.append("dialog_upload_title=Upload file\n");
		b.append("action_download=Download\n");
		b.append("label_lookup=Enter filter criterion:\n");
		b.append("command_select=Select\n");
		b.append("login_title=Login\n");
		b.append("login_username=User name\n");
		b.append("login_password=Password\n");
		b.append("login_command=Login\n");
		b.append("login_language=Language\n");
		b.append("form_editusersettingsdialog_title=Edit user settings\n");
		b.append("form_changepassworddialog_title=Change password\n");
		b.append("field_changepassworddialog_txtoldpassword=Current password\n");
		b.append("field_changepassworddialog_txtnewpassword=New password\n");
		b.append("field_changepassworddialog_txtnewpasswordconfirm=Re-enter new password\n");
		b.append("field_editusersettingsdialog_cbolocale=Language\n");
		b.append("field_editusersettingsdialog_txtdateformat=Date format\n");
		b.append("field_editusersettingsdialog_txtdatetimeformat=Date time format\n");
		b.append("field_editusersettingsdialog_txtnumberformat=Number format\n");
		b.append("field_editusersettingsdialog_cbotimezone=Time zone\n");
		b.append("field_errorpage_code=Error code\n");
		b.append("field_errorpage_description=Error description\n");
		b.append("field_errorpage_type=Error type\n");
		b.append("field_errorpage_class=Error class\n");
		b.append("field_errorpage_uri=Request URI\n");
		b.append("feedback_dialog_header=Application feedback\n");
		b.append("pick_list_add=Add\n");
		b.append("pick_list_add_all=Add all\n");
		b.append("pick_list_remove=Remove\n");
		b.append("pick_list_remove_all=Remove all\n");
		b.append("search_input_filter_fetch_size=Max. fetch size\n");
		b.append("search_input_case=Case sensitive\n");
		b.append("search_input_sort=Sort\n");
		b.append("search_input_filter_input=Filter (between)\n");
		b.append("search_input_and=and\n");
		b.append("search_input_visible_fields=Visible fields\n");
		b.append("search_input_all_fields=Available fields\n");
		b.append("search_input_filter=Filter settings\n");
		b.append("search_input_filter_match=Exact filter match\n");
		b.append("search_input_dlg=Search input\n");
		b.append("search_input_visible_fields_label=Select fields to display\n");
		b.append("search_input_adv=Advanced\n");
		b.append("search_input_count=Count records\n");
		b.append("search_input_perform_search=Search\n");
		b.append("search_input_operator=Operator\n");
		b.append("search_input_column=Column\n");
		b.append("search_input_perform_count=Count\n");
		b.append("search_input_count_of=of \n");
		b.append("operation_count_fail=Count operation failed!\n");
		b.append("operation_count_result=Query would return {0} record(s)\n");
		b.append("command_reset=Reset\n");
		b.append("command_logout=Logout\n");
		b.append("action_search_input=Search\n");
		b.append("search_input_saved_query_title=Saved queries\n");
		b.append("search_input_lbl_saved_query=Select one of available saved queries:\n");
		b.append("search_input_run_saved_query=Run selected\n");
		b.append("search_input_delete_saved_query=Delete selected\n");
		b.append("search_input_dlg_save_query=Save query\n");
		b.append("search_input_lbl_save_query=Enter name\n");
		b.append("search_input_validation=Validation of field \"{0}\" has failed!\n");
		b.append("operation_save_ok=Save operation finished successfully!\n");
		b.append("operation_save_fail=Save operation failed!\n");
		b.append("operation_delete_fail=Delete operation failed!\n");
		b.append("operation_copy_fail=Copy operation failed!\n");
		b.append("operation_drag_invalid=Invalid drag and drop operation!\n");
		b.append("operation_fetch_fail=Data fetch operation failed!\n");
		b.append("operation_fetch_no_data=No records found!\n");
		b.append("dialog_init_fail=Error while dialog initialization!\n");
		b.append("dialog_missing_param=Could't initialize dialog. Parameter is missing!\n");
		b.append("operation_upload_ok=Upload finished successfully!\n");
		b.append("operation_upload_fail=Error while uploading file!\n");
		b.append("operation_download_fail=Error while downloading file!\n");
		b.append("operation_export_fail=Error while performing data export operation!\n");
		b.append("operation_export_ok=Data export operation finished successfully!\n");
		b.append("operation_import_fail=Error while performing data import operation!\n");
		b.append("operation_import_ok=Data import operation finished successfully!\n");
		b.append("dialog_illegal_reference=Illegal reference on non-existing object!\n");
		b.append("log_on_failed=Logon failed!\n");
		b.append("illegal_item_selected=No or illegal item selected!\n");
		b.append("search_input_missing=Selected search operator requires input for field {0}!\n");
		b.append("saved_query_empty_name=Please enter a name for new query!\n");
		b.append("saved_query_illegal_name=Please enter a different name for new query!\n");
		b.append("saved_query_new_success=Query \"{0}\" saved successfully!\n");
		b.append("saved_query_delete_success=Query \"{0}\" deleted successfully!\n");
		b.append("msg_passwords_not_equal=New password and re-entered password are not equal!\n");
		b.append("msg_session_exp_dlg=Your session has expired!\n");
		b.append("msg_session_exp_title=The application has been left inactive for a while!\n");
		b.append("msg_session_exp_desc=Due to security reasons it is necessary that you login again!\n");
		b.append("element_collection_editor_lbl_add=Value for a new element\n");
		b.append("element_collection_editor_lbl_no_of_elements=Number of elements:\n");
		b.append("element_collection_editor_col_header_element=Element\n");
		b.append("element_collection_editor_action_delete=Delete\n");
		b.append("element_collection_editor_action_delete_all=Delete all\n");
		b.append("msg_add_element_failed=The new value could not be added to the collection!\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createFacesConfigXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<faces-config xmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/jakartaee ");
		b.append("https://jakarta.ee/xml/ns/jakartaee/web-facesconfig_4_0.xsd\"\n");
		b.append("\tversion=\"4.0\">\n\n");
		b.append("<application>\n");

		if (project.isSpringBootApplication())
			b.append("\t<el-resolver>org.springframework.web.jsf.el.SpringBeanFacesELResolver</el-resolver>\n\n");

		b.append("\t<locale-config>\n");
		b.append("\t\t<default-locale>en</default-locale>\n");
		b.append("\t\t<supported-locale>en</supported-locale>\n");
		b.append("\t</locale-config>\n");
		b.append("\t<resource-bundle>\n");
		b.append("\t\t<base-name>\n");
		b.append("\t\t\t" + BUNDLE_NAME + "\n");
		b.append("\t\t</base-name>\n");
		b.append("\t\t<var>" + EL_I18N_VAR + "</var>\n");
		b.append("\t</resource-bundle>\n");
		b.append("\t<converter>\n");
		b.append("\t\t<converter-for-class>java.util.UUID</converter-for-class>\n");
		b.append("\t\t<converter-class>net.codecadenza.runtime.webclient.");
		b.append("primefaces.converter.UUIDConverter</converter-class>\n");
		b.append("\t</converter>\n");
		b.append("</application>\n\n");
		b.append("</faces-config>\n");

		return b.toString();
	}

	/**
	 * Create the content of the web.xml file
	 * @return the generated content
	 */
	public String createWebXMLFile() {
		final var b = new StringBuilder();
		final boolean enableSecurity = project.getApplicationLogOnDTO() != null;

		b.append("<?xml version=\"1.0\"?>\n");
		b.append("<web-app id=\"WebApp_ID\" version=\"6.0\"\n");
		b.append("\txmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/jakartaee ");
		b.append("https://jakarta.ee/xml/ns/jakartaee/web-app_6_0.xsd\">\n\n");
		b.append("<filter>\n");
		b.append("\t<filter-name>Character Encoding Filter</filter-name>\n");
		b.append("\t<filter-class>net.codecadenza.runtime.webclient.");
		b.append("primefaces.filter.CharacterEncodingFilter</filter-class>\n");
		b.append("</filter>\n");
		b.append("<filter-mapping>\n");
		b.append("\t<filter-name>Character Encoding Filter</filter-name>\n");
		b.append("\t<servlet-name>Faces Servlet</servlet-name>\n");
		b.append("</filter-mapping>\n\n");
		b.append("<context-param>\n");
		b.append("\t<param-name>jakarta.faces.DEFAULT_SUFFIX</param-name>\n");
		b.append("\t<param-value>.xhtml</param-value>\n");
		b.append("</context-param>\n\n");
		b.append("<context-param>\n");
		b.append("\t<param-name>jakarta.faces.PROJECT_STAGE</param-name>\n");
		b.append("\t<param-value>Production</param-value>\n");
		b.append("</context-param>\n\n");
		b.append("<context-param>\n");
		b.append("\t<param-name>jakarta.faces.STATE_SAVING_METHOD</param-name>\n");
		b.append("\t<param-value>server</param-value>\n");
		b.append("</context-param>\n\n");
		b.append("<context-param>\n");
		b.append("\t<param-name>jakarta.faces.FACELETS_SKIP_COMMENTS</param-name>\n");
		b.append("\t<param-value>true</param-value>\n");
		b.append("</context-param>\n\n");
		b.append("<context-param>\n");
		b.append("\t<param-name>primefaces.THEME</param-name>\n");
		b.append("\t<param-value>nova-light</param-value>\n");
		b.append("</context-param>\n\n");
		b.append("<servlet>\n");
		b.append("\t<servlet-name>Faces Servlet</servlet-name>\n");
		b.append("\t<servlet-class>jakarta.faces.webapp.FacesServlet</servlet-class>\n");
		b.append("\t<load-on-startup>1</load-on-startup>\n");
		b.append("</servlet>\n\n");
		b.append("<servlet-mapping>\n");
		b.append("\t<servlet-name>Faces Servlet</servlet-name>\n");
		b.append("\t<url-pattern>*.jsf</url-pattern>\n");
		b.append("</servlet-mapping>\n\n");
		b.append("<error-page>\n");
		b.append("\t<error-code>403</error-code>\n");
		b.append("\t<location>/error.jsf</location>\n");
		b.append("</error-page>\n");
		b.append("<error-page>\n");
		b.append("\t<error-code>404</error-code>\n");
		b.append("\t<location>/error.jsf</location>\n");
		b.append("</error-page>\n");
		b.append("<error-page>\n");
		b.append("\t<error-code>500</error-code>\n");
		b.append("\t<location>/error.jsf</location>\n");
		b.append("</error-page>\n");
		b.append("<error-page>\n");
		b.append("\t<exception-type>java.lang.Exception</exception-type>\n");
		b.append("\t<location>/error.jsf</location>\n");
		b.append("</error-page>\n\n");

		if (enableSecurity) {
			project.getRoles().forEach(role -> {
				b.append("<security-role>\n");
				b.append("\t<role-name>" + role.getName() + "</role-name>\n");
				b.append("</security-role>\n");
			});

			b.append("\n<security-constraint>\n");
			b.append("\t<web-resource-collection>\n");
			b.append("\t\t<web-resource-name>Restricted areas</web-resource-name>\n");
			b.append("\t\t<url-pattern>" + UI_DIALOG_FOLDER + "/*</url-pattern>\n");
			b.append("\t\t<url-pattern>" + UI_VIEW_FOLDER + "/*</url-pattern>\n");
			b.append("\t\t<url-pattern>" + UI_TREE_FOLDER + "/*</url-pattern>\n");
			b.append("\t\t<url-pattern>" + UI_LOV_FOLDER + "/*</url-pattern>\n");
			b.append("\t</web-resource-collection>\n");
			b.append("\t<auth-constraint>\n");

			project.getRoles().forEach(role -> b.append("\t\t<role-name>" + role.getName() + "</role-name>\n"));

			b.append("\t</auth-constraint>\n");
			b.append("</security-constraint>\n\n");
			b.append("<security-constraint>\n");
			b.append("\t<web-resource-collection>\n");
			b.append("\t\t<web-resource-name>Unrestricted areas</web-resource-name>\n");
			b.append("\t\t<url-pattern>/css/*</url-pattern>\n");
			b.append("\t\t<url-pattern>/script/*</url-pattern>\n");
			b.append("\t\t<url-pattern>/themes/*</url-pattern>\n");
			b.append("\t\t<url-pattern>/images/*</url-pattern>\n");
			b.append("\t\t<url-pattern>/jakarta.faces.resource/*</url-pattern>\n");
			b.append("\t</web-resource-collection>\n");
			b.append("</security-constraint>\n\n");
		}

		b.append("<session-config>\n");
		b.append("\t<session-timeout>30</session-timeout>\n");
		b.append("</session-config>\n\n");
		b.append("<welcome-file-list>\n");
		b.append("\t<welcome-file>" + WELCOME_FILE + "</welcome-file>\n");
		b.append("</welcome-file-list>\n\n");
		b.append("</web-app>\n");

		return b.toString();
	}

}
