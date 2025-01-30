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
package net.codecadenza.eclipse.generator.client.imp.angular.file;

import java.util.List;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Utility class for download operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularDownloadGenerator {
	private final AbstractTypeScriptSourceGenerator generator;
	private final List<FormAction> actions;
	private final AngularI18NGenerator i18n;
	private final DTOBean dto;
	private FormTypeEnumeration formType;
	private boolean addToPanel;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param i18n
	 */
	public AngularDownloadGenerator(AbstractTypeScriptSourceGenerator generator, Form form, AngularI18NGenerator i18n) {
		this(generator, i18n, form.getActions(), form.getDTO());

		this.formType = form.getFormType();
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 * @param i18n
	 */
	public AngularDownloadGenerator(AbstractTypeScriptSourceGenerator generator, FormPanel panel, AngularI18NGenerator i18n) {
		this(generator, i18n, panel.getActions(), panel.getDTO());

		this.addToPanel = true;
	}

	/**
	 * Constructor
	 * @param generator
	 * @param i18n
	 * @param actions
	 * @param dto
	 */
	private AngularDownloadGenerator(AbstractTypeScriptSourceGenerator generator, AngularI18NGenerator i18n,
			List<FormAction> actions, DTOBean dto) {
		this.generator = generator;
		this.i18n = i18n;
		this.actions = actions.stream().filter(e -> e.getType() == ActionType.DOWNLOAD).toList();
		this.dto = dto;
	}

	/**
	 * @return true if this generator creates source code for downloading files
	 */
	public boolean isDownloadFragmentAdded() {
		return !actions.isEmpty();
	}

	/**
	 * Add buttons for all download operations
	 * @param formatter
	 */
	public void addButtons(AngularContentFormatter formatter) {
		actions.forEach(action -> {
			final var button = new StringBuilder();
			final var buttonId = "cmd" + action.getBoundaryMethod().getDomainAttribute().getUpperCaseName() + "Download";

			button.append("<button pButton type=\"button\" icon=\"pi pi-file\" i18n-label=\"@@button_download\" ");
			button.append("[style]=\"{'margin': '0px 0px 0.5em 0.5em'}\" class=\"col xl:col-1 md:col-3 sm:col-12\" ");
			button.append("label=\"Download\" (click)=\"" + action.getBoundaryMethod().getName() + "()\" ");
			button.append("id=\"" + buttonId + "\"></button>");

			formatter.addLine(button.toString());
		});
	}

	/**
	 * Add all file download methods
	 */
	public void addDownloadMethods() {
		actions.forEach(this::addDownloadMethod);
	}

	/**
	 * Add a file download method based on the given form action
	 * @param action
	 */
	private void addDownloadMethod(FormAction action) {
		final AngularContentFormatter formatter = generator.getContentFormatter();
		final DTOBeanAttribute pkAttr = dto.getPKAttribute();
		final String errorMsg = i18n.getI18NMessage("msg_errordownload", "Error while performing file download operation!");
		var param = "this.object." + pkAttr.getName();
		var fileNameParam = "";

		generator.importType("mergeMap", "rxjs/operators");

		for (final DTOBeanAttribute attr : dto.getAttributes())
			if (attr.getDomainAttribute() != null && attr.getDomainAttribute().getTag() == AttributeTagEnumeration.DOCUMENT_NAME) {
				if (addToPanel || formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW)
					fileNameParam = ", selectedItem." + attr.getName();
				else
					fileNameParam = ", this.object." + attr.getName();

				break;
			}

		formatter.addBlockComment("Download file");
		formatter.addLine(action.getBoundaryMethod().getName() + "() {");
		formatter.increaseIndent();

		if (addToPanel || formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW) {
			param = "selectedItem." + pkAttr.getName();

			formatter.addLine("const selectedItem = this.selectedItem;");
			formatter.addBlankLine();
			formatter.addIfStatement("!selectedItem", "return;", true);
		}

		if (pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong())
			param += ".toString()";

		formatter.addLineComment("Determine the real path of the file in the back-end");
		formatter.addLine(new AngularServiceInvocationGenerator(action.getBoundaryMethod()).createInvocation(param) + ".pipe(");
		formatter.increaseIndent();
		formatter.addLine("mergeMap((path: string) => this.fileService.downloadFile(path)))");
		formatter.decreaseIndent();
		formatter.addLine(".subscribe({");
		formatter.increaseIndent();
		formatter.addLine("next: data => this.fileService.openFile(data" + fileNameParam + "),");

		if (addToPanel || formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW)
			formatter.addLine("error: error => this.displayError(error, " + errorMsg + ")");
		else
			formatter.addLine("error: error => this.openErrorDialog(error, false)");

		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
