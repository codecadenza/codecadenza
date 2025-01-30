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
package net.codecadenza.eclipse.generator.client.imp.swing.security;

import static net.codecadenza.eclipse.generator.basic.client.imp.AbstractClientProjectFilesGenerator.CHANGE_PWD_COMMENT;
import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.CHANGE_PWD_DLG_NAME;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.ArrayList;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
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
public class SwingChangePasswordDialogGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final BoundaryBean logOnBoundary;

	/**
	 * Constructor
	 * @param project
	 */
	public SwingChangePasswordDialogGenerator(Project project) {
		this.project = project;
		this.i18n = new RichClientI18NGenerator(project);
		this.logOnBoundary = project.getLogOnBoundary();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString();

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, CHANGE_PWD_DLG_NAME, packageName);
		javaFile.setComment(CHANGE_PWD_COMMENT);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("net.codecadenza.runtime.richclient.swing.dialog");
		importPackage("javax.swing");

		addImports(new SwingSecurityHelper(project).getSecurityManagerImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + CHANGE_PWD_DLG_NAME + " extends AbstractChangePasswordDialog");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final String msgText = i18n.getI18NMessage("msg_err_save", "Error while performing save operation! Message: ");
		final String msgTitle = i18n.getI18NMessage("msg_title_save", "Save operation");
		final var declarationGenerator = new ServiceDeclarationGenerator(this, logOnBoundary, b);
		final BoundaryMethod method = logOnBoundary.getBoundaryMethodByReturnType(project.getJavaTypeByName(JavaType.VOID),
				BoundaryMethodTypeEnumeration.CHANGE_PASSWORD);

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractChangePasswordDialog#");
		b.append("saveNewPassword(java.lang.String, java.lang.String, java.lang.String)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public boolean saveNewPassword(String oldPassword, String newPassword, String newPasswordConfirm)\n");
		b.append("{\n");

		declarationGenerator.addLocalVariable();

		b.append("\n");
		b.append("try\n");
		b.append("{\n");

		final var params = new ArrayList<String>();
		params.add(SECURITY_MANAGER + ".getLogOnDTO()." + project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
		params.add("oldPassword");
		params.add("newPassword");
		params.add("newPasswordConfirm");

		new ServiceInvocationGenerator(method, b).addInvocation(params.stream().toArray(String[]::new));

		b.append("\n");

		if (!project.isJavaSEApplication()) {
			b.append("// Service locator must be reinitialized properly!\n");
			b.append("// Otherwise, subsequent requests will fail if the back-end application uses JAAS!\n");
			b.append("final ServiceLocatorDTO serviceLocatorSettings = ServiceLocator.getServiceLocatorSettings();\n");
			b.append("serviceLocatorSettings.setPassword(newPassword);\n\n");
			b.append("ServiceLocator.initialize(serviceLocatorSettings);\n\n");
		}

		b.append("return true;\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("JOptionPane.showMessageDialog(" + CHANGE_PWD_DLG_NAME + ".this, " + msgText + " + ex.getMessage(), ");
		b.append(msgTitle + ", JOptionPane.WARNING_MESSAGE);\n");
		b.append("return false;\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		b.append("}\n\n");

		addMethod("boolean saveNewPassword(String oldPassword, String newPassword, String newPasswordConfirm)", b.toString());

		i18n.save();
	}

}
