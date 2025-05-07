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
package net.codecadenza.eclipse.generator.client.imp.eclipse.file.rcp;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.IEclipseFileHandlingGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;

/**
 * <p>
 * RCP implementation of a generator for handling file content
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RCPFileHandlingGenerator implements IEclipseFileHandlingGenerator {
	private final AbstractJavaSourceGenerator formGenerator;
	private final BoundaryMethod method;
	private final RichClientI18NGenerator i18n;
	private final Project project;
	private final boolean performDownload;
	private final boolean performUpload;
	private final boolean performDataImport;
	private final boolean performDataExport;
	private FormAction action;

	/**
	 * Constructor
	 * @param formGenerator
	 * @param method
	 * @param i18n
	 */
	public RCPFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, BoundaryMethod method,
			RichClientI18NGenerator i18n) {
		this.formGenerator = formGenerator;
		this.method = method;
		this.i18n = i18n;
		this.project = method.getBoundaryBean().getNamespace().getProject();
		this.performDownload = method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD;
		this.performUpload = method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD;
		this.performDataImport = method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT;
		this.performDataExport = method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT;
	}

	/**
	 * Constructor
	 * @param formGenerator
	 * @param action
	 * @param i18n
	 */
	public RCPFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, FormAction action, RichClientI18NGenerator i18n) {
		this(formGenerator, action.getBoundaryMethod(), i18n);

		this.action = action;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.file.IEclipseFileHandlingGenerator#addImports()
	 */
	@Override
	public void addImports() {
		formGenerator.importClass("org.eclipse.jface.dialogs.MessageDialog");

		if (performDownload) {
			if (project.isJavaSEApplication()) {
				formGenerator.importPackage("java.io");
				formGenerator.importPackage("org.eclipse.core.runtime");
				formGenerator.importPackage("org.eclipse.core.runtime.jobs");
				formGenerator.importPackage("net.codecadenza.runtime.file");
			}
			else {
				formGenerator.importClass("org.eclipse.jface.dialogs.ProgressMonitorDialog");
				formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.action");
			}
		}
		else if (performDataExport) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.returnsContent()) {
				formGenerator.importPackage("net.codecadenza.runtime.richclient.util");
				formGenerator.importPackage("org.eclipse.core.runtime");
				formGenerator.importPackage("org.eclipse.core.runtime.jobs");
			}

			if (exchangeMethod.returnsPath()) {
				if (project.isJavaSEApplication()) {
					formGenerator.importPackage("org.eclipse.core.runtime");
					formGenerator.importPackage("org.eclipse.core.runtime.jobs");
					formGenerator.importPackage("java.io");
					formGenerator.importPackage("net.codecadenza.runtime.file");
				}
				else {
					formGenerator.importClass("org.eclipse.jface.dialogs.ProgressMonitorDialog");
					formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.action");
				}
			}

			if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath()) {
				formGenerator.importPackage("org.eclipse.core.runtime");
				formGenerator.importPackage("org.eclipse.core.runtime.jobs");
			}
		}
		else if (performDataImport) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			formGenerator.importPackage("org.eclipse.core.runtime");
			formGenerator.importPackage("org.eclipse.core.runtime.jobs");

			if (action.getBoundaryMethod().getMethodParameters().size() == 1) {
				formGenerator.importPackage("java.io");

				if (!exchangeMethod.hasPathParameter())
					formGenerator.importPackage("java.nio.file");
			}

			if (!project.isJavaSEApplication() && exchangeMethod.hasPathParameter()) {
				formGenerator.importClass("org.eclipse.jface.dialogs.ProgressMonitorDialog");
				formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.action");
			}
		}
		else if (performUpload) {
			formGenerator.importPackage("java.io");
			formGenerator.importPackage("net.codecadenza.runtime.file");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.file.IEclipseFileHandlingGenerator#createDownloadFragment(String)
	 */
	@Override
	public String createDownloadFragment(String invocationParameter) {
		final var b = new StringBuilder();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (!project.isJavaSEApplication()) {
			b.append("try\n");
			b.append("{\n");
			b.append("final var dlg = new FileDialog(parentShell, SWT.SAVE);\n");
			b.append("dlg.setText(" + i18n.getI18NMessage("file_download_dialog", "Save as") + ");\n\n");
			b.append("final String path = dlg.open();\n\n");
			b.append("if(path == null)\n");
			b.append("return;\n\n");

			formGenerator.addDebugLog(b, "Download file");

			b.append("\n");
			b.append("// Create download operation\n");
			b.append("final var downloadOperation = new AbstractFileDownloadOperation()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.action.");
			b.append("__AbstractFileDownloadOperation#prepareDownload()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public String prepareDownload() throws Exception\n");
			b.append("{\n");
			b.append("return ");

			if (invocationParameter != null)
				invocationGenerator.addInvocation(invocationParameter);
			else
				invocationGenerator.addInvocation();

			b.append("}\n");
			b.append("};\n\n");
			b.append("downloadOperation.setClientPath(path);\n\n");
			b.append("// Perform download\n");
			b.append("new ProgressMonitorDialog(parentShell).run(true, true, downloadOperation);\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");
			b.append("if(ex instanceof InterruptedException)\n");
			b.append("Thread.currentThread().interrupt();\n\n");

			formGenerator.addErrorLog(b, "Error while performing download operation!", "ex");

			b.append("\n");
			b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_download", "Download file") + ", ");
			b.append(i18n.getI18NMessage("msg_err_download", "Could not download selected file! Message: ") + " + ex.getMessage());\n");
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}
		else {
			b.append("final var dlg = new FileDialog(parentShell, SWT.SAVE);\n");
			b.append("dlg.setText(" + i18n.getI18NMessage("file_download_dialog", "Save as") + ");\n\n");
			b.append("final String path = dlg.open();\n\n");
			b.append("if(path == null)\n");
			b.append("return;\n\n");

			formGenerator.addDebugLog(b, "Download file");

			b.append("\n");
			b.append("final var job = new Job(" + i18n.getI18NMessage("job_name_export", "Perform data export operation") + ")\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public IStatus run(IProgressMonitor monitor)\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");
			b.append("final String tempPath = ");

			if (invocationParameter != null)
				invocationGenerator.addInvocation(invocationParameter);
			else
				invocationGenerator.addInvocation();

			b.append("\n");
			b.append("if(tempPath == null || tempPath.isEmpty())\n");
			b.append("return Status.CANCEL_STATUS;\n\n");
			b.append("FileUtil.copyFile(new File(tempPath), new File(path));\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while performing download operation! ", "ex");

			b.append("\n");
			b.append("parentShell.getDisplay().syncExec(() -> ");
			b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_download", "Download file") + ", ");
			b.append(i18n.getI18NMessage("msg_err_download", "Could not download selected file! Message: "));
			b.append(" + ex.getMessage()));\n");
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();

			b.append("\n");
			b.append("return Status.OK_STATUS;\n");
			b.append("}\n");
			b.append("};\n\n");
			b.append("job.schedule();\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.file.IEclipseFileHandlingGenerator#createExportInvocationFragment()
	 */
	@Override
	public String createExportInvocationFragment() {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Start data export");

		b.append("\n");
		b.append("final var job = new Job(" + i18n.getI18NMessage("job_name_export", "Perform data export operation") + ")\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public IStatus run(IProgressMonitor monitor)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("// Invoke data export operation!\n");

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocationGenerator.addInvocation("id");
		else
			invocationGenerator.addInvocation();

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing data export operation!", "ex");

		b.append("\n");
		b.append("parentShell.getDisplay().syncExec(() -> ");
		b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_export", "Data export") + ", ");
		b.append(i18n.getI18NMessage("msg_err_export", "Error while performing data export operation! Message: "));
		b.append(" + ex.getMessage()));\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		b.append("\n");
		b.append("return Status.OK_STATUS;\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("job.schedule();\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.file.IEclipseFileHandlingGenerator#
	 * createDownloadFragmentForExport()
	 */
	@Override
	public String createDownloadFragmentForExport() {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Download data export file");

		b.append("\n");
		b.append("final var job = new Job(" + i18n.getI18NMessage("job_name_export", "Perform data export operation") + ")\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public IStatus run(IProgressMonitor monitor)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final String exportContent = ");

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocationGenerator.addInvocation("id");
		else
			invocationGenerator.addInvocation();

		b.append("\n");
		b.append("// Create temporary file and open it in respective system editor!\n");
		b.append("new DesktopHelper(\"ExportFile\", \".");
		b.append(exchangeMethod.getDefaultFileExtension());
		b.append("\").openFile(exportContent);\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing data export operation!", "ex");

		b.append("\n");
		b.append("parentShell.getDisplay().syncExec(() -> ");
		b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_export", "Data export") + ", ");
		b.append(i18n.getI18NMessage("msg_err_export", "Error while performing data export operation! Message: "));
		b.append(" + ex.getMessage()));\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		b.append("\n");
		b.append("return Status.OK_STATUS;\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("job.schedule();\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.file.IEclipseFileHandlingGenerator#
	 * createUploadFragmentForImport(boolean)
	 */
	@Override
	public String createUploadFragmentForImport(boolean addFragmentToForm) {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		if (method.getMethodParameters().size() == 1) {
			final var messageText = "Selected file exceeds size limit of {0}!";
			final var maxFileSize = "Integer.MAX_VALUE";

			b.append("final var dlg = new FileDialog(parentShell);\n");
			b.append("dlg.setFilterExtensions(new String[] {\"*.");
			b.append(exchangeMethod.getDefaultFileExtension());
			b.append("\"});\n");
			b.append("dlg.setText(" + i18n.getI18NMessage("msg_title_import_sel", "Select file to be imported") + ");\n\n");
			b.append("final String path = dlg.open();\n\n");
			b.append("if(path == null)\n");
			b.append("return;\n\n");
			b.append("// Prevent upload of files that are too large!\n");
			b.append("if(new File(path).length() > " + maxFileSize + ")\n");
			b.append("{\n");
			b.append("MessageDialog.openInformation(parentShell, " + i18n.getI18NMessage("msg_title_import", "Import file") + ", ");
			b.append(i18n.getI18NMessage("msg_err_upload_size", messageText, maxFileSize) + ");\n");
			b.append("return;\n");
			b.append("}\n\n");
		}

		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (!project.isJavaSEApplication() && method.getMethodParameters().size() == 1 && exchangeMethod.hasPathParameter()) {
			formGenerator.addDebugLog(b, "Upload data import file");

			b.append("\n");
			b.append("// Upload file to remote server\n");
			b.append("final var uploadOperation = new FileUploadOperation(path);\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("new ProgressMonitorDialog(parentShell).run(true, false, uploadOperation);\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");
			b.append("if(ex instanceof InterruptedException)\n");
			b.append("Thread.currentThread().interrupt();\n\n");

			formGenerator.addErrorLog(b, "Error while performing file upload operation!", "ex");

			b.append("\n");
			b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_upload", "Upload file") + ", ");
			b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file! Message: ") + " + ex.getMessage());\n");
			b.append("return;\n");
			b.append("}\n\n");
			b.append("final String serverPath = uploadOperation.getServerPath();\n");
		}
		else
			formGenerator.addDebugLog(b, "Start data import");

		b.append("\n");

		if (addFragmentToForm) {
			b.append("// Disable action in order to avoid running two imports concurrently!\n");
			b.append("setEnabled(false);\n\n");
		}

		b.append("final var job = new Job(" + i18n.getI18NMessage("job_name_import", "Perform data import operation") + ")\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public IStatus run(IProgressMonitor monitor)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");

		if (method.getMethodParameters().size() == 1) {
			final String invocationParam;

			if (exchangeMethod.hasPathParameter())
				if (project.isJavaSEApplication())
					invocationParam = "path";
				else
					invocationParam = "serverPath";
			else {
				formGenerator.importClass("java.nio.charset.StandardCharsets");

				invocationParam = "new String(Files.readAllBytes(new File(path).toPath()), " + exchangeMethod.getStandardCharset() + ")";
			}

			invocationGenerator.addInvocation(invocationParam);
		}
		else
			invocationGenerator.addInvocation();

		// It doesn't make sense to perform a refresh operation if the import method is invoked asynchronously!
		if (!(exchangeMethod.getMethodInvocation() instanceof AsynchronousInvocation)) {
			b.append("\nparentShell.getDisplay().syncExec(() -> ");

			if (addFragmentToForm)
				b.append("refreshView()");
			else
				b.append("refreshData()");

			b.append(");\n");
		}

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while performing data import operation!", "ex");

		b.append("\n");
		b.append("parentShell.getDisplay().syncExec(() -> ");
		b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_import", "Data import") + ", ");
		b.append(i18n.getI18NMessage("msg_err_import", "Error while performing data import operation! Message: "));
		b.append(" + ex.getMessage()));\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");

		if (addFragmentToForm)
			b.append("parentShell.getDisplay().syncExec(() -> setEnabled(true));\n");

		if (declarationGenerator.needsCloseStatement()) {
			if (addFragmentToForm)
				b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("}\n\n");
		b.append("return Status.OK_STATUS;\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("job.schedule();\n");

		return b.toString();
	}

}
