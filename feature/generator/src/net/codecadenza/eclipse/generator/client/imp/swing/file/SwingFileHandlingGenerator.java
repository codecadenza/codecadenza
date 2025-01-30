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
package net.codecadenza.eclipse.generator.client.imp.swing.file;

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
 * Generator for handling files in a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingFileHandlingGenerator {
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
	public SwingFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, BoundaryMethod method,
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
	public SwingFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, FormAction action, RichClientI18NGenerator i18n) {
		this(formGenerator, action.getBoundaryMethod(), i18n);

		this.action = action;
	}

	/**
	 * Add all necessary imports for the given action
	 */
	public void addImports() {
		if (action.getType() == ActionType.DOWNLOAD) {
			if (project.isJavaSEApplication()) {
				formGenerator.importClass("java.io.File");
				formGenerator.importPackage("net.codecadenza.runtime.file");
			}
			else
				formGenerator.importPackage("net.codecadenza.runtime.richclient.swing.file");
		}
		else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.returnsContent())
				formGenerator.importPackage("net.codecadenza.runtime.richclient.util");

			if (exchangeMethod.returnsPath()) {
				if (project.isJavaSEApplication()) {
					formGenerator.importClass("java.io.File");
					formGenerator.importPackage("net.codecadenza.runtime.file");
				}
				else
					formGenerator.importPackage("net.codecadenza.runtime.richclient.swing.file");
			}
		}
		else if (action.getType() == ActionType.UPLOAD_IMPORT) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (action.getBoundaryMethod().getMethodParameters().size() == 1) {
				formGenerator.importClass("java.io.File");

				if (!exchangeMethod.hasPathParameter())
					formGenerator.importPackage("java.nio.file");
			}

			if (!project.isJavaSEApplication() && exchangeMethod.hasPathParameter())
				formGenerator.importPackage("net.codecadenza.runtime.richclient.swing.file");
		}
	}

	/**
	 * @param processSpecificObject
	 * @param parentCompName
	 * @return the generated content
	 */
	public String createDownloadMethodBody(boolean processSpecificObject, String parentCompName) {
		final var b = new StringBuilder();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (!project.isJavaSEApplication()) {
			b.append("try\n");
			b.append("{\n");
			b.append("// Open file selection dialog\n");
			b.append("final var fc = new JFileChooser();\n");
			b.append("fc.setFileSelectionMode(JFileChooser.FILES_ONLY);\n");
			b.append("fc.setDialogTitle(" + i18n.getI18NMessage("file_download_dialog", "Save as") + ");\n\n");
			b.append("if(JFileChooser.APPROVE_OPTION != fc.showSaveDialog(" + parentCompName + ".this))\n");
			b.append("return;\n\n");
			b.append("final String path = fc.getSelectedFile().getAbsolutePath();\n\n");

			formGenerator.addDebugLog(b, "Download file");

			b.append("\n");
			b.append("// Create download operation\n");
			b.append("final var downloadOperation = new AbstractFileDownloadOperation(" + parentCompName + ".this)\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.file.AbstractFileDownloadOperation#prepareDownload()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public String prepareDownload() throws Exception\n");
			b.append("{\n");
			b.append("return ");

			if (processSpecificObject)
				invocationGenerator.addInvocation("id");
			else
				invocationGenerator.addInvocation();

			b.append("}\n");
			b.append("};\n\n");
			b.append("// Perform download\n");
			b.append("downloadOperation.startDownload(path);\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while performing download operation!", "ex");

			b.append("\n");
			b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
			b.append(i18n.getI18NMessage("msg_err_download", "Could not download selected file! Message: "));
			b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_download", "Download file"));
			b.append(", JOptionPane.WARNING_MESSAGE);\n");
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();

			b.append("\n");
		}
		else {
			b.append("// Open file selection dialog\n");
			b.append("final var fc = new JFileChooser();\n");
			b.append("fc.setFileSelectionMode(JFileChooser.FILES_ONLY);\n");
			b.append("fc.setDialogTitle(" + i18n.getI18NMessage("file_download_dialog", "Save as") + ");\n\n");
			b.append("if(JFileChooser.APPROVE_OPTION != fc.showSaveDialog(" + parentCompName + ".this))\n");
			b.append("return;\n\n");

			formGenerator.addDebugLog(b, "Download file");

			b.append("\n");
			b.append("setBusy(true);\n\n");
			b.append("new SwingWorker<Void, Void>()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("protected Void doInBackground() throws Exception\n");
			b.append("{\n");
			b.append("final String tempPath = ");

			if (processSpecificObject)
				invocationGenerator.addInvocation("id");
			else
				invocationGenerator.addInvocation();

			b.append("\n");
			b.append("if(tempPath == null || tempPath.isEmpty())\n");
			b.append("return null;\n\n");
			b.append("final String path = fc.getSelectedFile().getAbsolutePath();\n");
			b.append("FileUtil.copyFile(new File(tempPath), new File(path));\n\n");
			b.append("return null;\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see javax.swing.SwingWorker#done()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("protected void done()\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");
			b.append("get();\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");
			b.append("if(ex instanceof InterruptedException)\n");
			b.append("Thread.currentThread().interrupt();\n\n");

			formGenerator.addErrorLog(b, "Error while performing download operation!", "ex");

			b.append("\n");
			b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
			b.append(i18n.getI18NMessage("msg_err_download", "Could not download selected file! Message: "));
			b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_download", "Download file"));
			b.append(", JOptionPane.WARNING_MESSAGE);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("setBusy(false);\n");

			if (declarationGenerator.needsCloseStatement()) {
				b.append("\n");

				declarationGenerator.addCloseStatement();
			}

			b.append("}\n");
			b.append("}\n");
			b.append("}.execute();\n");
		}

		return b.toString();
	}

	/**
	 * @param parentCompName
	 * @return the generated content
	 */
	public String createExportInvocationFragment(String parentCompName) {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Start data export");

		b.append("\n");
		b.append("setBusy(true);\n\n");
		b.append("new SwingWorker<Void, Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected Void doInBackground() throws Exception\n");
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
		b.append(" * @see javax.swing.SwingWorker#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void done()\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("get();\n\n");
		b.append("setStatusInfoMessage(");
		b.append(i18n.getI18NMessage("msg_export_finished", "Data export operation finished successfully!") + ");\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("if(ex instanceof InterruptedException)\n");
		b.append("Thread.currentThread().interrupt();\n\n");

		formGenerator.addErrorLog(b, "Error while performing data export operation!", "ex");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_export", "Error while performing data export operation! Message: "));
		b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_export", "Data export"));
		b.append(", JOptionPane.WARNING_MESSAGE);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("setBusy(false);\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("}\n");
		b.append("}\n");
		b.append("}.execute();\n");

		return b.toString();
	}

	/**
	 * @param parentCompName
	 * @return the generated content
	 */
	public String createDownloadFragmentForExport(String parentCompName) {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		declarationGenerator.addLocalVariable();

		b.append("\n");

		formGenerator.addDebugLog(b, "Download data export file");

		b.append("\n");
		b.append("setBusy(true);\n\n");
		b.append("new SwingWorker<String, Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected String doInBackground() throws Exception\n");
		b.append("{\n");
		b.append("return ");

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocationGenerator.addInvocation("id");
		else
			invocationGenerator.addInvocation();

		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void done()\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final String exportContent = get();\n\n");
		b.append("// Create temporary file and open it in respective system editor!\n");
		b.append("new DesktopHelper(\"ExportFile\", \".");
		b.append(exchangeMethod.getDefaultFileExtension());
		b.append("\").openFile(exportContent);\n\n");
		b.append("setStatusInfoMessage(");
		b.append(i18n.getI18NMessage("msg_export_finished", "Data export operation finished successfully!") + ");\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("if(ex instanceof InterruptedException)\n");
		b.append("Thread.currentThread().interrupt();\n\n");

		formGenerator.addErrorLog(b, "Error while downloading data export file!", "ex");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_export", "Error while performing data export operation! Message: "));
		b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_export", "Data export"));
		b.append(", JOptionPane.WARNING_MESSAGE);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("setBusy(false);\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("}\n");
		b.append("}\n");
		b.append("}.execute();\n");

		return b.toString();
	}

	/**
	 * @param parentCompName
	 * @return the generated content
	 */
	public String createUploadFragmentForImport(String parentCompName) {
		final var b = new StringBuilder();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, method, b);
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);
		final var translationKey = "file_description_" + exchangeMethod.getDefaultFileExtension();
		final String fileDesc = i18n.getI18NMessage(translationKey, exchangeMethod.getContentType().getDefaultFileDescription());

		if (method.getMethodParameters().size() == 1) {
			final var msg = "Selected file exceeds size limit of {0}!";

			b.append("final var fc = new JFileChooser();\n");
			b.append("fc.setFileSelectionMode(JFileChooser.FILES_ONLY);\n");
			b.append("fc.setDialogTitle(" + i18n.getI18NMessage("file_upload_dialog", "Select file to upload!") + ");\n\n");
			b.append("fc.setFileFilter(new javax.swing.filechooser.FileFilter(){\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see javax.swing.filechooser.FileFilter#getDescription()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public String getDescription()\n");
			b.append("{\n");
			b.append("return " + fileDesc + ";\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see javax.swing.filechooser.FileFilter#accept(java.io.File)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public boolean accept(File f)\n");
			b.append("{\n");
			b.append("if(f.isDirectory())\n");
			b.append("return true;\n\n");
			b.append("return f.getName().toLowerCase().endsWith(\".");
			b.append(exchangeMethod.getDefaultFileExtension());
			b.append("\");\n");
			b.append("}\n");
			b.append("});\n\n");
			b.append("if(JFileChooser.APPROVE_OPTION != fc.showOpenDialog(" + parentCompName + ".this))\n");
			b.append("return;\n\n");
			b.append("// Prevent upload of files that are too large!\n");
			b.append("if(new File(fc.getSelectedFile().getAbsolutePath()).length() > Integer.MAX_VALUE)\n");
			b.append("{\n");
			b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
			b.append(i18n.getI18NMessage("msg_err_upload_size", msg, "Integer.MAX_VALUE"));
			b.append(", " + i18n.getI18NMessage("msg_title_import", "Import file") + ", JOptionPane.WARNING_MESSAGE);\n");
			b.append("return;\n");
			b.append("}\n\n");

			formGenerator.addDebugLog(b, "Upload data import file");
		}
		else
			formGenerator.addDebugLog(b, "Start data import");

		b.append("\n");

		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (!project.isJavaSEApplication() && method.getMethodParameters().size() == 1 && exchangeMethod.hasPathParameter()) {
			b.append("// Upload file to remote server\n");
			b.append("final var uploadOperation = new FileUploadOperation(" + parentCompName + ".this);\n");
			b.append("final String serverPath;\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("serverPath = uploadOperation.startUpload(fc.getSelectedFile().getAbsolutePath());\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while uploading data import file!", "ex");

			b.append("\n");
			b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
			b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file! Message: "));
			b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_upload", "Upload file"));
			b.append(", JOptionPane.WARNING_MESSAGE);\n");
			b.append("return;\n");
			b.append("}\n\n");
		}

		b.append("setEnabled(false);\n");
		b.append("setBusy(true);\n\n");
		b.append("new SwingWorker<Void, Void>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected Void doInBackground() throws Exception\n");
		b.append("{\n");

		if (method.getMethodParameters().size() == 1) {
			final String invocationParam;

			if (exchangeMethod.hasPathParameter())
				if (project.isJavaSEApplication())
					invocationParam = "fc.getSelectedFile().getAbsolutePath()";
				else
					invocationParam = "serverPath";
			else {
				formGenerator.importClass("java.nio.charset.StandardCharsets");

				invocationParam = "new String(Files.readAllBytes(fc.getSelectedFile().toPath()), " + exchangeMethod.getStandardCharset()
						+ ")";
			}

			invocationGenerator.addInvocation(invocationParam);
		}
		else
			invocationGenerator.addInvocation();

		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected void done()\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("get();\n");

		// It doesn't make sense to perform a refresh operation if the import method is invoked asynchronously!
		if (!(exchangeMethod.getMethodInvocation() instanceof AsynchronousInvocation)) {
			b.append("\n// Refresh view\n");
			b.append("performFetch();\n");
		}

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("if(ex instanceof InterruptedException)\n");
		b.append("Thread.currentThread().interrupt();\n\n");

		formGenerator.addErrorLog(b, "Error while performing data import operation!", "ex");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(" + parentCompName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_import", "Error while performing data import operation! Message: "));
		b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_import", "Data import"));
		b.append(", JOptionPane.WARNING_MESSAGE);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("setBusy(false);\n");
		b.append("setEnabled(true);\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("}\n");
		b.append("}\n");
		b.append("}.execute();\n");

		return b.toString();
	}

}
