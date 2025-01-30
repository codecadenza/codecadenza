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
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;

/**
 * <p>
 * Utility class for data export operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularExportGenerator {
	private final AbstractTypeScriptSourceGenerator generator;
	private final List<FormAction> actions;
	private final AngularI18NGenerator i18n;
	private final DTOBean dto;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param i18n
	 */
	public AngularExportGenerator(AbstractTypeScriptSourceGenerator generator, Form form, AngularI18NGenerator i18n) {
		this(generator, i18n, form.getActions(), form.getDTO());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 * @param i18n
	 */
	public AngularExportGenerator(AbstractTypeScriptSourceGenerator generator, FormPanel panel, AngularI18NGenerator i18n) {
		this(generator, i18n, panel.getActions(), panel.getDTO());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param i18n
	 * @param actions
	 * @param dto
	 */
	private AngularExportGenerator(AbstractTypeScriptSourceGenerator generator, AngularI18NGenerator i18n, List<FormAction> actions,
			DTOBean dto) {
		this.generator = generator;
		this.i18n = i18n;
		this.actions = actions.stream().filter(e -> e.getType() == ActionType.DOWNLOAD_EXPORT).toList();
		this.dto = dto;
	}

	/**
	 * @return true if this generator creates source code that requires the service for downloading a file
	 */
	public boolean addFileService() {
		for (final FormAction action : actions) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.returnsPath() || exchangeMethod.returnsContent())
				return true;
		}

		return false;
	}

	/**
	 * Add all data export methods
	 */
	public void addExportMethods() {
		actions.forEach(this::addDataExportMethod);
	}

	/**
	 * Add a data export method based on the given form action
	 * @param action
	 */
	private void addDataExportMethod(FormAction action) {
		final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
		final boolean returnsContent = exchangeMethod.returnsContent();
		final boolean returnsPath = exchangeMethod.returnsPath();
		final AngularContentFormatter formatter = generator.getContentFormatter();
		final DTOBeanAttribute attr = dto.getPKAttribute();
		final String errorMsg = i18n.getI18NMessage("msg_errordataexport", "Error while performing data export operation!");
		final var exportFileName = "export." + exchangeMethod.getDefaultFileExtension();
		var param = "this.selectedItem." + attr.getName();

		if (attr.getDomainAttribute().getJavaType().isIntegerOrLong())
			param += ".toString()";

		formatter.addBlockComment(action.getDescription());
		formatter.addLine(action.getBoundaryMethod().getName() + "() {");
		formatter.increaseIndent();

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			formatter.addIfStatement("!this.selectedItem", "return;", true);

		formatter.addLine("console.log('Start data export');");
		formatter.addBlankLine();

		final var invocation = new StringBuilder();

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocation.append(new AngularServiceInvocationGenerator(action.getBoundaryMethod()).createInvocation(param));
		else
			invocation.append(new AngularServiceInvocationGenerator(action.getBoundaryMethod()).createInvocation());

		if (returnsContent) {
			invocation.append(".subscribe({");

			formatter.addLine(invocation.toString());
			formatter.increaseIndent();
			formatter.addLine("next: data => this.fileService.openFile(data, '" + exportFileName + "'),");
		}
		else if (returnsPath) {
			generator.importType("mergeMap", "rxjs/operators");

			invocation.append(".pipe(");

			formatter.addLine(invocation.toString());
			formatter.increaseIndent();
			formatter.addLine("mergeMap((path: string) => this.fileService.downloadFile(path)))");
			formatter.decreaseIndent();
			formatter.addLine(".subscribe({");
			formatter.increaseIndent();
			formatter.addLine("next: data => this.fileService.openFile(data, '" + exportFileName + "'),");
		}
		else {
			invocation.append(".subscribe({");

			formatter.addLine(invocation.toString());
			formatter.increaseIndent();
		}

		formatter.addLine("error: error => this.displayError(error, " + errorMsg + ")");
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
