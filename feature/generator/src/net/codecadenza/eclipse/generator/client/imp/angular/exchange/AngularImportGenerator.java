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
package net.codecadenza.eclipse.generator.client.imp.angular.exchange;

import java.util.List;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;

/**
 * <p>
 * Utility class for data import operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularImportGenerator {
	private final AbstractTypeScriptSourceGenerator generator;
	private final List<FormAction> actions;
	private final AngularI18NGenerator i18n;
	private final FormAction actionWithFileSelection;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param i18n
	 */
	public AngularImportGenerator(AbstractTypeScriptSourceGenerator generator, Form form, AngularI18NGenerator i18n) {
		this(generator, i18n, form.getActions());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 * @param i18n
	 */
	public AngularImportGenerator(AbstractTypeScriptSourceGenerator generator, FormPanel panel, AngularI18NGenerator i18n) {
		this(generator, i18n, panel.getActions());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param i18n
	 * @param actions
	 */
	private AngularImportGenerator(AbstractTypeScriptSourceGenerator generator, AngularI18NGenerator i18n,
			List<FormAction> actions) {
		this.generator = generator;
		this.i18n = i18n;
		this.actions = actions.stream().filter(e -> e.getType() == ActionType.UPLOAD_IMPORT).toList();
		this.actionWithFileSelection = this.actions.stream().filter(a -> a.getBoundaryMethod().getMethodParameters().size() == 1)
				.findFirst().orElse(null);
	}

	/**
	 * @return true if this generator creates source code that requires the service for uploading a file
	 */
	public boolean addFileService() {
		if (actionWithFileSelection != null) {
			final var exchangeMethod = (DataExchangeMethod) actionWithFileSelection.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.hasPathParameter())
				return true;
		}

		return false;
	}

	/**
	 * @return true if the 'Import' button is required
	 */
	public boolean isImportButtonRequired() {
		return actionWithFileSelection != null;
	}

	/**
	 * Add all data import methods
	 */
	public void addImportMethods() {
		actions.forEach(action -> {
			// We may only create one method that needs to upload a file as only one respective button exists!
			final boolean processContent = action.getBoundaryMethod().getMethodParameters().size() == 1;

			if (!processContent || action.equals(actionWithFileSelection))
				addImportMethod(action, processContent);
		});
	}

	/**
	 * Add a data import method based on the given form action
	 * @param action
	 * @param processContent
	 */
	private void addImportMethod(FormAction action, boolean processContent) {
		final AngularContentFormatter formatter = generator.getContentFormatter();
		final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
		final String errorMsg = i18n.getI18NMessage("msg_errordataimport", "Error while performing data import operation!");
		final String infoMsg = i18n.getI18NMessage("msg_infodataimport", "Data import operation finished successfully!");

		formatter.addBlockComment(action.getDescription());

		if (processContent) {
			generator.importTypes(Stream.of("FileUploadHandlerEvent", "FileUpload"), "primeng/fileupload");

			formatter.addLine("override onImportButtonPressed($event: FileUploadHandlerEvent, fileUpload: FileUpload) {");
			formatter.increaseIndent();
			formatter.addLine("for (const file of $event.files) {");
			formatter.increaseIndent();
			formatter.addLine("const importFile: File = file;");
			formatter.addBlankLine();

			if (exchangeMethod.hasPathParameter()) {
				final String invocation = new AngularServiceInvocationGenerator(action.getBoundaryMethod()).createInvocation("path");

				generator.importType("mergeMap", "rxjs/operators");

				formatter.addLineComment("Upload file to back-end");
				formatter.addLine("this.fileService.uploadFile(importFile.name, file).pipe(");
				formatter.increaseIndent();
				formatter.addLine("mergeMap((path: string) => " + invocation + "))");
				formatter.decreaseIndent();
				formatter.addLine(".subscribe({");
			}
			else {
				final String invocation = new AngularServiceInvocationGenerator(action.getBoundaryMethod())
						.createInvocation("fileContent");

				formatter.addLine("const fileReader = new FileReader();");
				formatter.addLine("fileReader.onload = () => {");
				formatter.increaseIndent();
				formatter.addLine("const fileContent = fileReader.result as string;");
				formatter.addBlankLine();
				formatter.addLine(invocation + ".subscribe({");
			}

			formatter.increaseIndent();
			formatter.addLine("error: error => this.displayError(error, " + errorMsg + "),");
			formatter.addLine("complete: () => this.messageService.add({ severity: 'info', summary: " + infoMsg + " })");
			formatter.decreaseIndent();
			formatter.addLine("});");

			if (!exchangeMethod.hasPathParameter()) {
				formatter.decreaseIndent();
				formatter.addLine("};");
				formatter.addBlankLine();
				formatter.addLine("fileReader.readAsText(importFile);");
			}
			else
				formatter.addBlankLine();

			formatter.addLine("fileUpload.clear();");
			formatter.decreaseIndent();
			formatter.addLine("}");
		}
		else {
			formatter.addLine(action.getBoundaryMethod().getName() + "() {");
			formatter.increaseIndent();
			formatter.addLine(new AngularServiceInvocationGenerator(action.getBoundaryMethod()).createInvocation() + ".subscribe({");
			formatter.increaseIndent();
			formatter.addLine("error: error => this.displayError(error, " + errorMsg + "),");
			formatter.addLine("complete: () => this.messageService.add({ severity: 'info', summary: " + infoMsg + " })");
			formatter.decreaseIndent();
			formatter.addLine("});");
		}

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
