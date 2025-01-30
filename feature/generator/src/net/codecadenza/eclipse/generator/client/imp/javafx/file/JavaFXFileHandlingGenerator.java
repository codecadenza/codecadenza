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
package net.codecadenza.eclipse.generator.client.imp.javafx.file;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;

/**
 * <p>
 * Generator for handling files in a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXFileHandlingGenerator {
	private final AbstractJavaSourceGenerator formGenerator;
	private final BoundaryMethod method;
	private final RichClientI18NGenerator i18n;
	private final Project project;
	private FormAction action;

	/**
	 * Constructor
	 * @param formGenerator
	 * @param method
	 * @param i18n
	 */
	public JavaFXFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, BoundaryMethod method,
			RichClientI18NGenerator i18n) {
		this.formGenerator = formGenerator;
		this.method = method;
		this.i18n = i18n;
		this.project = method.getBoundaryBean().getNamespace().getProject();
	}

	/**
	 * Constructor
	 * @param formGenerator
	 * @param action
	 * @param i18n
	 */
	public JavaFXFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, FormAction action, RichClientI18NGenerator i18n) {
		this(formGenerator, action.getBoundaryMethod(), i18n);

		this.action = action;
	}

	/**
	 * Add all necessary imports for the given action
	 */
	public void addImports() {
		if (action.getType() == ActionType.DOWNLOAD) {
			formGenerator.importPackage("javafx.stage");
			formGenerator.importClass("java.io.File");

			if (project.isJavaSEApplication())
				formGenerator.importPackage("net.codecadenza.runtime.file");
			else
				formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.dialog");
		}
		else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.returnsContent()) {
				formGenerator.importPackage("javafx.application");
				formGenerator.importPackage("javafx.concurrent");
				formGenerator.importPackage("net.codecadenza.runtime.richclient.util");
			}

			if (exchangeMethod.returnsPath()) {
				formGenerator.importPackage("javafx.stage");
				formGenerator.importClass("java.io.File");
				formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

				if (project.isJavaSEApplication())
					formGenerator.importPackage("net.codecadenza.runtime.file");
			}

			if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath()) {
				formGenerator.importPackage("javafx.application");
				formGenerator.importPackage("javafx.concurrent");
			}
		}
		else if (action.getType() == ActionType.UPLOAD_IMPORT) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			formGenerator.importPackage("javafx.application");
			formGenerator.importPackage("javafx.concurrent");

			if (action.getBoundaryMethod().getMethodParameters().size() == 1) {
				formGenerator.importClass("java.io.File");
				formGenerator.importPackage("javafx.stage");
				formGenerator.importPackage("javafx.stage.FileChooser");

				if (!exchangeMethod.hasPathParameter())
					formGenerator.importPackage("java.nio.file");
			}

			if (!project.isJavaSEApplication() && exchangeMethod.hasPathParameter())
				formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.dialog");
		}
	}

	/**
	 * @param processSpecificObject
	 * @param ownerName
	 * @return the generated content
	 */
	public String createDownloadMethodBody(boolean processSpecificObject, String ownerName) {
		final var b = new StringBuilder();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");
		b.append("final var fc = new FileChooser();\n");
		b.append("fc.setTitle(" + i18n.getI18NMessage("file_download_dialog", "Save as") + ");\n\n");
		b.append("final File targetFile = fc.showSaveDialog(" + ownerName + ");\n\n");
		b.append("if(targetFile == null)\n");
		b.append("return;\n\n");

		formGenerator.addDebugLog(b, "Download file");

		b.append("\n");
		b.append("try\n");
		b.append("{\n");

		if (!project.isJavaSEApplication()) {
			b.append("// Prepare download operation on server\n");
			b.append("final String pathOnServer = ");

			if (processSpecificObject)
				invocationGenerator.addInvocation("id");
			else
				invocationGenerator.addInvocation();

			b.append("\n");
			b.append("final var dlg = new FileDownloadDialog(" + ownerName + ", pathOnServer, targetFile);\n");
			b.append("dlg.open();\n\n");
			b.append("if(dlg.getException() != null)\n");
			b.append("throw dlg.getException();\n");
		}
		else {
			b.append("final String tempPath = ");

			if (processSpecificObject)
				invocationGenerator.addInvocation("id");
			else
				invocationGenerator.addInvocation();

			b.append("\n");
			b.append("if(tempPath == null || tempPath.isEmpty())\n");
			b.append("return;\n\n");
			b.append("FileUtil.copyFile(new File(tempPath), targetFile);\n");
		}

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing download operation!", "e");

		b.append("\n");
		b.append("DialogUtil.openErrorDialog(" + ownerName + ", ");
		b.append(i18n.getI18NMessage("msg_err_download", "Could not download selected file!") + ", e);\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createExportInvocationFragment() {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Start data export");

		b.append("\n");
		b.append("setEnabled(false);\n");
		b.append("getStatusBar().showProgress();\n\n");
		b.append("final var task = new Task<Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#call()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Void call() throws Exception\n");
		b.append("{\n");
		b.append("// Invoke data export operation!\n");

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocationGenerator.addInvocation("id");
		else
			invocationGenerator.addInvocation();

		b.append("\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#succeeded()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void succeeded()\n");
		b.append("{\n");

		final String msgTitle = i18n.getI18NMessage("msg_title_export", "Data export");
		final String msgInfo = i18n.getI18NMessage("msg_export_finished", "Data export operation finished successfully!");

		b.append("DialogUtil.openInformationDialog(null, " + msgTitle + ", " + msgInfo + ");\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.util.concurrent.FutureTask#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void done()\n");
		b.append("{\n");
		b.append("Platform.runLater(() ->\n");
		b.append("{\n");
		b.append("setEnabled(true);\n");
		b.append("getStatusBar().stopProgress();\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("});\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#failed()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void failed()\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing data export operation!", "getException()");

		b.append("\n");
		b.append("DialogUtil.openErrorDialog(null, ");
		b.append(i18n.getI18NMessage("msg_err_export", "Error while performing data export operation!") + ", getException());\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("new Thread(task).start();\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createDownloadFragmentForExport() {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Start data export");

		b.append("\n");
		b.append("setEnabled(false);\n");
		b.append("getStatusBar().showProgress();\n\n");
		b.append("final var task = new Task<Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#call()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Void call() throws Exception\n");
		b.append("{\n");
		b.append("// Invoke data export operation!\n");
		b.append("final String exportContent = ");

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocationGenerator.addInvocation("id");
		else
			invocationGenerator.addInvocation();

		b.append("\n");
		b.append("// Create temporary file and open it in respective system editor!\n");
		b.append("new DesktopHelper(\"ExportFile\", \".");
		b.append(exchangeMethod.getDefaultFileExtension());
		b.append("\").openFile(exportContent);\n\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#succeeded()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void succeeded()\n");
		b.append("{\n");

		final String msgTitle = i18n.getI18NMessage("msg_title_export", "Data export");
		final String msgInfo = i18n.getI18NMessage("msg_export_finished", "Data export operation finished successfully!");

		b.append("DialogUtil.openInformationDialog(null, " + msgTitle + ", " + msgInfo + ");\n");
		b.append("}\n\n");

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.util.concurrent.FutureTask#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void done()\n");
		b.append("{\n");
		b.append("Platform.runLater(() ->\n");
		b.append("{\n");
		b.append("setEnabled(true);\n");
		b.append("getStatusBar().stopProgress();\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("});\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#failed()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void failed()\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing data export operation!", "getException()");

		b.append("\n");
		b.append("DialogUtil.openErrorDialog(null, ");
		b.append(i18n.getI18NMessage("msg_err_export", "Error while performing data export operation!") + ", getException());\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("new Thread(task).start();\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createUploadFragmentForImport() {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		if (method.getMethodParameters().size() == 1) {
			final var translationKey = "file_description_" + exchangeMethod.getDefaultFileExtension();
			final String fileDesc = i18n.getI18NMessage(translationKey, exchangeMethod.getContentType().getDefaultFileDescription());
			final var msgMaxFileSize = "Selected file exceeds size limit of {0}!";

			b.append("// Open file selection dialog\n");
			b.append("final var fc = new FileChooser();\n");
			b.append("fc.getExtensionFilters().add(new ExtensionFilter(" + fileDesc);
			b.append(", \"*." + exchangeMethod.getDefaultFileExtension() + "\"));\n");
			b.append("fc.setTitle(" + i18n.getI18NMessage("file_upload_dialog", "Select file to upload") + ");\n\n");
			b.append("final File sourceFile = fc.showOpenDialog(null);\n\n");
			b.append("if(sourceFile == null)\n");
			b.append("return;\n\n");
			b.append("// Prevent upload of files that are too large!\n");
			b.append("if(new File(sourceFile.getAbsolutePath()).length() > Integer.MAX_VALUE)\n");
			b.append("{\n");
			b.append("final String msgTitle = " + i18n.getI18NMessage("msg_title_import", "Data import") + ";\n");
			b.append("final String msgInfo = ");
			b.append(i18n.getI18NMessage("msg_err_upload_size", msgMaxFileSize, "Integer.MAX_VALUE") + ";\n\n");
			b.append("DialogUtil.openWarningDialog(null, msgTitle, msgInfo);\n");
			b.append("return;\n");
			b.append("}\n\n");
		}

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Perform data import");

		b.append("\n");

		if (!project.isJavaSEApplication() && method.getMethodParameters().size() == 1 && exchangeMethod.hasPathParameter()) {
			b.append("// Upload file to remote server\n");
			b.append("final var uploadDialog = new FileUploadDialog(null, sourceFile);\n");
			b.append("uploadDialog.open();\n\n");
			b.append("if(uploadDialog.getException() != null)\n");
			b.append("{\n");
			b.append("DialogUtil.openErrorDialog(null, ");
			b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file!") + ", uploadDialog.getException());\n");
			b.append("return;\n");
			b.append("}\n\n");
			b.append("final String serverPath = uploadDialog.getPath();\n\n");
		}

		b.append("setEnabled(false);\n");
		b.append("getStatusBar().showProgress();\n\n");
		b.append("final var task = new Task<Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#call()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Void call() throws Exception\n");
		b.append("{\n");

		if (method.getMethodParameters().size() == 1) {
			final String invocationParam;

			if (exchangeMethod.hasPathParameter())
				if (project.isJavaSEApplication())
					invocationParam = "sourceFile.getAbsolutePath()";
				else
					invocationParam = "serverPath";
			else {
				formGenerator.importClass("java.nio.charset.StandardCharsets");

				invocationParam = "new String(Files.readAllBytes(sourceFile.toPath()), " + exchangeMethod.getStandardCharset() + ")";
			}

			invocationGenerator.addInvocation(invocationParam);
		}
		else
			invocationGenerator.addInvocation();

		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#succeeded()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void succeeded()\n");
		b.append("{\n");

		final String msgTitle = i18n.getI18NMessage("msg_title_import", "Data import");
		final String msgInfo = i18n.getI18NMessage("msg_import_finished", "Data import operation finished successfully!");

		b.append("DialogUtil.openInformationDialog(null, " + msgTitle + ", " + msgInfo + ");\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.util.concurrent.FutureTask#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void done()\n");
		b.append("{\n");
		b.append("Platform.runLater(() ->\n");
		b.append("{\n");
		b.append("setEnabled(true);\n");
		b.append("getStatusBar().stopProgress();\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		// It doesn't make sense to perform a refresh operation if the import method is invoked asynchronously!
		if (!(exchangeMethod.getMethodInvocation() instanceof AsynchronousInvocation)) {
			b.append("\n");
			b.append("refreshView();\n");
		}

		b.append("});\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javafx.concurrent.Task#failed()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void failed()\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing data import operation!", "getException()");

		b.append("\n");
		b.append("DialogUtil.openErrorDialog(null, ");
		b.append(i18n.getI18NMessage("msg_err_import", "Error while performing data import operation!") + ", getException());\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("new Thread(task).start();\n");

		return b.toString();
	}

}
