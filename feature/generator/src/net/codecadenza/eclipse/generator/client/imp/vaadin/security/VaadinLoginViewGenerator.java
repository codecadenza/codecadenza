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
package net.codecadenza.eclipse.generator.client.imp.vaadin.security;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.HOME_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.LOGIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the Vaadin login dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinLoginViewGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final VaadinI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinLoginViewGenerator(Project project) {
		this.project = project;
		this.i18n = new VaadinI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, LOGIN_VIEW, project.getClientNamespace().toString());
		javaFile.setComment("The login page");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("java.nio.charset");
		importPackage("jakarta.servlet.http");
		importPackage("com.vaadin.flow.component");
		importPackage("com.vaadin.flow.component.button");
		importPackage("com.vaadin.flow.component.html");
		importPackage("com.vaadin.flow.component.orderedlayout");
		importPackage("com.vaadin.flow.component.textfield");
		importPackage("com.vaadin.flow.router");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");
		importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");

		addImports(new VaadinSecurityHelper(project).getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Route(" + LOGIN_VIEW + ".ROUTE" + ")\n");
		b.append("public class " + LOGIN_VIEW + " extends VerticalLayout implements BeforeEnterObserver");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPublicConstant(JavaType.STRING, "ROUTE", "\"login\"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateConstant(JavaType.LONG, "COMPONENT_WIDTH", "300").create();
		addPrivateConstant(JavaType.STRING, "COOKIE_LAST_LOGGED_ON", "\"lastloggedon\"").create();
		addPrivateField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();
		addPrivateField("CookieManager", "cookieManager").inject().create();
		addPrivateField("I18NService", "i18n").inject().create();
		addPrivateField("TextField", "txtUserName").withDefaultValue("new TextField()").withFinalModifier().create();
		addPrivateField("PasswordField", "txtPassword").withDefaultValue("new PasswordField()").withFinalModifier().create();
		addPrivateField("Navigator", "navigator").withDefaultValue("new Navigator(this)").withFinalModifier().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var illegalInputMessage = i18n.getI18NMessage("msg_err_login", "User name or password are not valid!");

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void beforeEnter(BeforeEnterEvent event)\n");
		b.append("{\n");

		addDebugLog(b, "Initialize login dialog");

		b.append("\n");
		b.append("final Cookie cookie = cookieManager.getCookieByName(COOKIE_LAST_LOGGED_ON);\n\n");
		b.append("txtUserName.setId(\"txtUserName\");\n");
		b.append("txtUserName.setLabel(" + i18n.getI18NMessage("loginview_lblusername", "Username", true) + ");\n");
		b.append("txtUserName.setWidth(COMPONENT_WIDTH, Unit.PIXELS);\n\n");
		b.append("if(cookie != null)\n");
		b.append("txtUserName.setValue(java.net.URLDecoder.decode(cookie.getValue(), StandardCharsets.UTF_8));\n\n");
		b.append("txtPassword.setValue(\"\");\n");
		b.append("txtPassword.setLabel(" + i18n.getI18NMessage("loginview_lblpassword", "Password", true) + ");\n");
		b.append("txtPassword.setId(\"txtPassword\");\n");
		b.append("txtPassword.setWidth(COMPONENT_WIDTH, Unit.PIXELS);\n\n");
		b.append("final var cmdLogin = new Button(" + i18n.getI18NMessage("loginview_cmdlogin", "Login") + ");\n");
		b.append("cmdLogin.setId(\"cmdLogin\");\n");
		b.append("cmdLogin.setWidth(COMPONENT_WIDTH, Unit.PIXELS);\n");
		b.append("cmdLogin.addClickListener(clickEvent -> logIn());\n\n");
		b.append("add(new H1(" + i18n.getI18NMessage("application_title", "My generated application"));
		b.append("), txtUserName, txtPassword, cmdLogin);\n\n");
		b.append("setAlignItems(Alignment.CENTER);\n");
		b.append("setJustifyContentMode(JustifyContentMode.CENTER);\n");
		b.append("setSizeFull();\n\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n\n");

		addMethod("void beforeEnter(BeforeEnterEvent event)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Perform log in\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void logIn()\n");
		b.append("{\n");

		addDebugLog(b, "Login user '{}'", "txtUserName.getValue()");

		b.append("\n");
		b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_login", "Login") + ";\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append(MANAGED_SECURITY_MANAGER + ".logOn(txtUserName.getValue(), txtPassword.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while performing login of user '{}'", "e", "txtUserName.getValue()");

		b.append("\n");
		b.append("new InfoMessageDialog(dialogTitle, " + illegalInputMessage + ", " + i18n.getLocaleFragment() + ").open();\n\n");
		b.append("txtPassword.setValue(null);\n");
		b.append("txtPassword.focus();\n\n");
		b.append("return;\n");
		b.append("}\n\n");

		addDebugLog(b, "User '{}' logged in successfully!", "txtUserName.getValue()");

		b.append("\n");
		b.append("try\n");
		b.append("{\n");
		b.append("cookieManager.saveCookie(COOKIE_LAST_LOGGED_ON, txtUserName.getValue());\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Saving of cookie for user '{}' failed!", "e", "txtUserName.getValue()");

		b.append("}\n\n");
		b.append("navigator.navigateTo(" + HOME_VIEW + ".class);\n");
		b.append("}\n\n");

		addMethod("private void logIn()", b.toString());

		i18n.save();
	}

}
