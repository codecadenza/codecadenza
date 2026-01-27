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

import static net.codecadenza.eclipse.generator.basic.client.imp.AbstractClientProjectFilesGenerator.CHANGE_PWD_COMMENT;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.CHANGE_PASSWORD_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.ArrayList;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the dialog to change the password of the currently logged on user
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinChangePasswordViewGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final VaadinI18NGenerator i18n;
	private final BoundaryBean logOnBoundary;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinChangePasswordViewGenerator(Project project) {
		this.project = project;
		this.i18n = new VaadinI18NGenerator(project);
		this.logOnBoundary = project.getLogOnBoundary();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, CHANGE_PASSWORD_VIEW,
				project.getClientNamespace().toString());
		javaFile.setComment(CHANGE_PWD_COMMENT);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("com.vaadin.flow.component");
		importPackage("com.vaadin.flow.component.button");
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
		b.append("@Route(value = " + CHANGE_PASSWORD_VIEW + ".ROUTE, layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + CHANGE_PASSWORD_VIEW);
		b.append(" extends VerticalLayout implements BeforeEnterObserver, HasDynamicTitle");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPublicConstant(JavaType.STRING, "ROUTE", "\"user/" + CHANGE_PASSWORD_VIEW + "\"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateConstant(JavaType.INT, "MIN_PASSWORD_LENGTH", "4").create();
		addPrivateConstant(JavaType.INT, "MAX_PASSWORD_LENGTH", "10").create();
		addPrivateConstant(JavaType.INT, "FIELD_WIDTH", "300").create();
		addPrivateField("Navigator", "navigator").withDefaultValue("new Navigator(this)").withFinalModifier().create();
		addPrivateField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();
		addPrivateField("I18NService", "i18n").inject().create();
		addPrivateField("PasswordField", "txtOldPassword").withFinalModifier().withDefaultValue("new PasswordField()").create();
		addPrivateField("PasswordField", "txtNewPassword").withFinalModifier().withDefaultValue("new PasswordField()").create();
		addPrivateField("PasswordField", "txtConfPassword").withFinalModifier().withDefaultValue("new PasswordField()").create();

		new ServiceDeclarationGenerator(this, logOnBoundary).addField();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final BoundaryMethod method = logOnBoundary.getBoundaryMethodByReturnType(project.getJavaTypeByName(JavaType.VOID),
				BoundaryMethodTypeEnumeration.CHANGE_PASSWORD);
		final var locale = i18n.getLocaleFragment();
		final var msgPasswordTooShort = "Length of password must be greater or equal than {0}!";
		final var msgPasswordTooLong = "Length of password must be less or equal than {0}!";
		final var msgPasswordsNotMatch = "New passwords do not match!";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void beforeEnter(BeforeEnterEvent event)\n");
		b.append("{\n");

		addDebugLog(b, "Initialize dialog");

		b.append("\n");
		b.append("txtOldPassword.setValue(\"\");\n");
		b.append("txtOldPassword.setLabel(");
		b.append(i18n.getI18NMessage("field_changepassworddialog_txtoldpassword", "Old password", true) + ");\n");
		b.append("txtOldPassword.setId(\"txtOldPassword\");\n");
		b.append("txtOldPassword.setWidth(FIELD_WIDTH, Unit.PIXELS);\n\n");
		b.append("txtNewPassword.setValue(\"\");\n");
		b.append("txtNewPassword.setLabel(");
		b.append(i18n.getI18NMessage("field_changepassworddialog_txtnewpassword", "New password", true) + ");\n");
		b.append("txtNewPassword.setId(\"txtNewPassword\");\n");
		b.append("txtNewPassword.setWidth(FIELD_WIDTH, Unit.PIXELS);\n\n");
		b.append("txtConfPassword.setValue(\"\");\n");
		b.append("txtConfPassword.setLabel(");
		b.append(i18n.getI18NMessage("field_changepassworddialog_txtconfpassword", "Repeat password", true) + ");\n");
		b.append("txtConfPassword.setId(\"txtConfPassword\");\n");
		b.append("txtConfPassword.setWidth(FIELD_WIDTH, Unit.PIXELS);\n\n");
		b.append("final var cmdOK = new Button(" + i18n.getI18NMessage("cmd_save", "Save") + ");\n");
		b.append("cmdOK.addClickListener(_ -> changePassword());\n\n");
		b.append("final var cmdCancel = new Button(" + i18n.getI18NMessage("cmd_cancel", "Cancel") + ");\n");
		b.append("cmdCancel.addClickListener(_ -> navigator.navigateBack());\n\n");
		b.append("final var hlButtons = new HorizontalLayout();\n");
		b.append("hlButtons.add(cmdOK, cmdCancel);\n\n");
		b.append("add(txtOldPassword, txtNewPassword, txtConfPassword, hlButtons);\n\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n\n");

		addMethod("void beforeEnter(BeforeEnterEvent event)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.HasDynamicTitle#getPageTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getPageTitle()\n");
		b.append("{\n");
		b.append("return " + i18n.getI18NMessage("form_changepassworddialog_title", "Change password") + ";\n");
		b.append("}\n\n");

		addMethod("String getPageTitle()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Change password\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void changePassword()\n");
		b.append("{\n");
		b.append("final String oldPassword = txtOldPassword.getValue();\n");
		b.append("final String newPassword = txtNewPassword.getValue();\n");
		b.append("final String confPassword = txtConfPassword.getValue();\n");
		b.append("final String dialogTitle = " + i18n.getI18NMessage("form_changepassworddialog_title", "Change password") + ";\n\n");
		b.append("// Validate user input\n");
		b.append("if(oldPassword.isEmpty())\n");
		b.append("{\n");
		b.append("final String errorMessage = " + i18n.getI18NMessage("msg_err_empty_field", "Field must not be empty!") + ";\n\n");
		b.append("new InfoMessageDialog(dialogTitle, errorMessage, " + locale + ").open();\n");
		b.append("txtOldPassword.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(newPassword.isEmpty())\n");
		b.append("{\n");
		b.append("final String errorMessage = " + i18n.getI18NMessage("msg_err_empty_field", "Field must not be empty!") + ";\n\n");
		b.append("new InfoMessageDialog(dialogTitle, errorMessage, " + locale + ").open();\n");
		b.append("txtNewPassword.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(newPassword.length() < MIN_PASSWORD_LENGTH)\n");
		b.append("{\n");
		b.append("final String errorMessage = ");
		b.append(i18n.getI18NMessage("msg_err_min_password_length", msgPasswordTooShort, "MIN_PASSWORD_LENGTH"));
		b.append(";\n\n");
		b.append("new InfoMessageDialog(dialogTitle, errorMessage, " + locale + ").open();\n");
		b.append("txtNewPassword.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(newPassword.length() > MAX_PASSWORD_LENGTH)\n");
		b.append("{\n");
		b.append("final String errorMessage = ");
		b.append(i18n.getI18NMessage("msg_err_max_password_length", msgPasswordTooLong, "MAX_PASSWORD_LENGTH"));
		b.append(";\n\n");
		b.append("new InfoMessageDialog(dialogTitle, errorMessage, " + locale + ").open();\n");
		b.append("txtNewPassword.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(!newPassword.equals(confPassword))\n");
		b.append("{\n");
		b.append("final String errorMessage = " + i18n.getI18NMessage("msg_err_password_match", msgPasswordsNotMatch) + ";\n\n");
		b.append("new InfoMessageDialog(dialogTitle, errorMessage, " + locale + ").open();\n");
		b.append("txtConfPassword.focus();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");

		final var params = new ArrayList<String>();
		params.add(
				MANAGED_SECURITY_MANAGER + ".getLogOnDTO()." + project.getApplicationLogOnDTO().getPKAttribute().getModelGetterName());
		params.add("oldPassword");
		params.add("newPassword");
		params.add("confPassword");

		new ServiceInvocationGenerator(method, b).addInvocation(params.stream().toArray(String[]::new));

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while saving new password!", "e");

		final var errorMessage = i18n.getI18NMessage("msg_err_save", "Save operation failed!");

		b.append("\n");
		b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("navigator.navigateBack();\n");
		b.append("}\n\n");

		addMethod("void changePassword()", b.toString());

		i18n.save();
	}

}
