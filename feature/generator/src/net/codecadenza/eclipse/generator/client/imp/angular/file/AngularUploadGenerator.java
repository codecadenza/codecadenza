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
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Utility class for upload operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularUploadGenerator {
	private final AbstractTypeScriptSourceGenerator generator;
	private final List<FormAction> actions;
	private final AngularI18NGenerator i18n;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param i18n
	 */
	public AngularUploadGenerator(AbstractTypeScriptSourceGenerator generator, Form form, AngularI18NGenerator i18n) {
		this.generator = generator;
		this.i18n = i18n;
		this.actions = form.getActions().stream()
				.filter(a -> a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD).toList();
	}

	/**
	 * @return true if this generator creates source code for uploading files
	 */
	public boolean isUploadFragmentAdded() {
		return !actions.isEmpty();
	}

	/**
	 * Add binding fragments for all attributes that contain either the file content or the file path
	 */
	public void addBindingFragments() {
		if (actions.isEmpty())
			return;

		final AngularContentFormatter formatter = generator.getContentFormatter();

		for (final FormAction formAction : actions) {
			final DomainAttribute uploadAttr = formAction.getBoundaryMethod().getDomainAttribute();

			for (final DTOBeanAttribute attr : formAction.getForm().getDTO().getAttributes()) {
				final Form form = formAction.getForm();

				// Don't add the binding if a form field is already mapped to this DTO attribute!
				if (form.getAllFormFields().stream().anyMatch(f -> f.getDTOAttribute().equals(attr)))
					continue;

				if (attr.getDomainAttribute() == null)
					continue;

				final AttributeTagEnumeration tag = attr.getDomainAttribute().getTag();
				boolean addBinding = tag == AttributeTagEnumeration.DOCUMENT_REF || tag == AttributeTagEnumeration.DOCUMENT_DATA;

				if (!addBinding)
					addBinding = tag == AttributeTagEnumeration.NONE && uploadAttr.equals(attr.getDomainAttribute());

				if (addBinding) {
					final boolean nullable = attr.getDomainAttribute().getDomainAttributeValidator().isNullable();

					if (!nullable || attr.getDomainAttribute().getMinFieldLength().isPresent()) {
						formatter.addLine("this.addControl('" + attr.getName() + "', [Validators.required]);");

						generator.importType("Validators", "@angular/forms");
					}
					else
						formatter.addLine("this.addControl('" + attr.getName() + "');");
				}
			}
		}
	}

	/**
	 * Add buttons for all upload operations
	 * @param formatter
	 */
	public void addButtons(AngularContentFormatter formatter) {
		actions.forEach(action -> {
			final DomainAttribute uploadAttr = action.getBoundaryMethod().getDomainAttribute();
			final Integer maxSize = uploadAttr.getDomainAttributeValidator().getMaxLength();
			final var buttonId = "cmd" + uploadAttr.getUpperCaseName() + "Upload";
			String maxFileSize = Integer.toString(Integer.MAX_VALUE);

			if (uploadAttr.isLob() && maxSize != null)
				maxFileSize = Integer.toString(maxSize);

			final var button = new StringBuilder();
			button.append("<p-fileUpload #fileUpload mode=\"basic\" [customUpload]=\"true\" ");
			button.append("[auto]=\"true\" [maxFileSize]=\"" + maxFileSize + "\" ");
			button.append("[style]=\"{'margin': '0px 0px 0.5em 0.5em'}\" class=\"ui-g-2 ui-xl-1 ui-md-3 ui-sm-12\" ");
			button.append("i18n-chooseLabel=\"@@button_upload\" chooseLabel=\"Upload\" id=\"" + buttonId + "\" ");
			button.append("(uploadHandler)=\"" + action.getBoundaryMethod().getName() + "($event, fileUpload)\"></p-fileUpload>");

			formatter.addLine(button.toString());
		});
	}

	/**
	 * Add all file upload methods
	 */
	public void addUploadMethods() {
		actions.forEach(this::addUploadMethod);
	}

	/**
	 * Add a file upload method based on the given form action
	 * @param action
	 */
	private void addUploadMethod(FormAction action) {
		final AngularContentFormatter formatter = generator.getContentFormatter();
		final DomainAttribute uploadAttr = action.getBoundaryMethod().getDomainAttribute();
		final String infoMsg = i18n.getI18NMessage("msg_infoupload", "File uploaded successfully!");
		boolean attributeAdded = false;

		generator.importTypes(Stream.of("FileUploadHandlerEvent", "FileUpload"), "primeng/fileupload");

		formatter.addBlockComment("Upload file");
		formatter.addLine(action.getBoundaryMethod().getName() + "($event: FileUploadHandlerEvent, fileUpload: FileUpload) {");
		formatter.increaseIndent();
		formatter.addLine("for (const file of $event.files) {");
		formatter.increaseIndent();
		formatter.addLine("const uploadFile: File = file;");
		formatter.addBlankLine();
		formatter.addLineComment("Upload file to back-end");

		if (action.getType() == ActionType.INDIRECT_UPLOAD) {
			final DTOBeanAttribute pkAttr = action.getForm().getDTO().getPKAttribute();
			var param = "this.object." + pkAttr.getName();

			generator.importType("mergeMap", "rxjs/operators");

			if (pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong())
				param += ".toString()";

			param += ", path";

			final String invocation = new AngularServiceInvocationGenerator(action.getBoundaryMethod()).createInvocation(param);

			formatter.addLine("this.fileService.uploadFile(uploadFile.name, file).pipe(");
			formatter.increaseIndent();
			formatter.addLine("mergeMap((path: string) => " + invocation + "))");
			formatter.decreaseIndent();
			formatter.addLine(".subscribe({");
			formatter.increaseIndent();
		}
		else {
			formatter.addLine("this.fileService.uploadFile(uploadFile.name, file).subscribe({");
			formatter.increaseIndent();
			formatter.addLine("next: path => {");
			formatter.increaseIndent();

			for (final DTOBeanAttribute attr : action.getForm().getDTO().getAttributes()) {
				if (attr.getDomainAttribute() == null)
					continue;

				final AttributeTagEnumeration tag = attr.getDomainAttribute().getTag();

				if (tag == AttributeTagEnumeration.DOCUMENT_NAME) {
					formatter.addLine("this.object." + attr.getName() + " = uploadFile.name;");
					attributeAdded = true;
				}
				else if (tag == AttributeTagEnumeration.DOCUMENT_SIZE) {
					formatter.addLine("this.object." + attr.getName() + " = uploadFile.size;");
					attributeAdded = true;
				}
				else if (tag == AttributeTagEnumeration.DOCUMENT_REF || tag == AttributeTagEnumeration.DOCUMENT_DATA
						|| (tag == AttributeTagEnumeration.NONE && uploadAttr.equals(attr.getDomainAttribute()))) {
					formatter.addLine("this.object." + attr.getName() + " = path;");
					attributeAdded = true;
				}
			}

			formatter.decreaseIndent();
			formatter.addLine("},");
		}

		formatter.addLine("error: error => this.openErrorDialog(error, false),");
		formatter.addLine("complete: () => {");
		formatter.increaseIndent();

		if (action.getType() == ActionType.INDIRECT_UPLOAD)
			for (final DTOBeanAttribute attr : action.getForm().getDTO().getAttributes()) {
				if (attr.getDomainAttribute() == null || !attr.getDomainAttribute().isTrackVersion())
					continue;

				formatter.addLine("this.object." + attr.getName() + " = this.object." + attr.getName() + " + 1;");
				attributeAdded = true;
			}

		if (attributeAdded) {
			formatter.addLine("this.formGroup.patchValue(this.object);");
			formatter.addBlankLine();
		}

		formatter.addLine("fileUpload.clear();");
		formatter.addBlankLine();
		formatter.addLine("this.messageService.add({ severity: 'info', summary: " + infoMsg + " });");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
