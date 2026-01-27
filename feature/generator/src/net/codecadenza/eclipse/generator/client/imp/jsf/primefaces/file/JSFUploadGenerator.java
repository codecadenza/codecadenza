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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.file;

import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;

import java.util.Optional;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for file upload operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFUploadGenerator {
	private final AbstractJavaSourceGenerator generator;
	private final Form form;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 */
	public JSFUploadGenerator(AbstractJavaSourceGenerator generator, Form form) {
		this.generator = generator;
		this.form = form;
	}

	/**
	 * Add all necessary imports
	 */
	public void addImports() {
		final Project project = form.getDTO().getNamespace().getProject();

		for (final FormAction a : form.getActions()) {
			if (a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD) {
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();

				generator.importPackage("org.primefaces.event");
				generator.importPackage("net.codecadenza.runtime.webclient.primefaces.util");
				generator.importClass("jakarta.faces.application.FacesMessage");

				if (a.getType() == ActionType.INDIRECT_UPLOAD || project.isBoundaryMode()
						|| uploadAttr.getTag() == AttributeTagEnumeration.DOCUMENT_REF)
					generator.importPackage("java.io");
			}

			// We have to import the file utility package in case of a direct upload into a Byte[]
			if (a.getType() == ActionType.DIRECT_UPLOAD && !project.isBoundaryMode()) {
				for (final DTOBeanAttribute attr : form.getDTO().getAttributes()) {
					if (attr.getDomainAttribute() == null)
						continue;

					if (attr.getDomainAttribute().getJavaType().isType(JavaType.BYTE_OBJ_ARRAY)) {
						generator.importPackage("net.codecadenza.runtime.file");
						break;
					}
				}
			}
		}
	}

	/**
	 * @return the generated content
	 */
	public String createDialogFragment() {
		final var b = new StringBuilder();

		for (final FormAction a : form.getActions()) {
			if (a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD) {
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final var widgetVar = "dialog" + uploadAttr.getUpperCaseName() + "Upload";
				final var dialogId = "dlg" + uploadAttr.getUpperCaseName() + "Upload";
				final var controller = JSFGeneratorUtil.createManagedBeanName(form.getName()) + ".on" + uploadAttr.getUpperCaseName()
						+ "Upload";
				final var msgVar = "upload" + uploadAttr.getUpperCaseName() + "Messages";
				final Optional<Integer> maxFieldLength = uploadAttr.getMaxFieldLenght();
				int maxFileSize = Integer.MAX_VALUE;

				if (uploadAttr.isLob() && maxFieldLength.isPresent())
					maxFileSize = maxFieldLength.get();

				b.append("\t<p:dialog id=\"" + dialogId + "\" modal=\"true\" width=\"350\" height=\"250\" ");
				b.append("header=\"#{" + EL_I18N_VAR + ".dialog_upload_title}\" widgetVar=\"" + widgetVar + "\">\n");
				b.append("\t\t<p:messages id=\"" + msgVar + "\" showDetail=\"true\" showSummary=\"true\"/>\n");
				b.append("\t\t<p:fileUpload auto=\"true\" mode=\"advanced\" listener=\"#{");
				b.append(controller + "}\" update=\"" + msgVar);

				int panelsOfFirstRow = 0;
				int panelsOfSecondRow = 0;

				for (final FormPanel panel : form.getFormPanels())
					if (panel.getRowIndex() == 1)
						panelsOfFirstRow++;
					else
						panelsOfSecondRow++;

				for (final FormField f : form.getAllFormFields()) {
					if (f.getDTOAttribute().getDomainAttribute() == null)
						continue;

					// We have to update all fields supplied with appropriate tags as we cannot figure out what fields belong to a specific
					// document!
					final AttributeTagEnumeration tag = f.getDTOAttribute().getDomainAttribute().getTag();

					if (tag == AttributeTagEnumeration.DOCUMENT_NAME || tag == AttributeTagEnumeration.DOCUMENT_SIZE) {
						if (f.getPanel().getRowIndex() == 1) {
							if (panelsOfFirstRow > 1)
								b.append(",:form:tabview1:" + f.getName());
							else
								b.append("," + f.getName());
						}

						if (f.getPanel().getRowIndex() == 2) {
							if (panelsOfSecondRow > 1)
								b.append(",:form:tabview2:" + f.getName());
							else
								b.append("," + f.getName());
						}
					}
				}

				b.append("\">\n");
				b.append("\t\t\t<p:validateFile sizeLimit=\"" + maxFileSize + "\"/>\n");
				b.append("\t\t</p:fileUpload>\n");
				b.append("\t</p:dialog>\n\n");
			}
		}

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createCommandFragment() {
		final var b = new StringBuilder();

		form.getActions().stream().filter(a -> a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD)
				.forEach(action -> {
					final DomainAttribute uploadAttr = action.getBoundaryMethod().getDomainAttribute();
					final var widgetVar = "dialog" + uploadAttr.getUpperCaseName() + "Upload";
					final var buttonId = "cmd" + uploadAttr.getUpperCaseName() + "Upload";

					b.append("\t\t<p:commandButton id=\"" + buttonId + "\" value=\"#{i18n.command_browse}\" ");
					b.append("onclick=\"PF('" + widgetVar + "').show()\" type=\"button\"/>\n");
				});

		return b.toString();
	}

	/**
	 * Add upload listener methods
	 */
	public void addUploadListenerMethods() {
		final String modelObjectName = form.getDTO().getDomainObject().getLowerCaseName();
		final Project project = form.getDTO().getNamespace().getProject();
		final var uploadLogic = new StringBuilder();
		var addTryCatchBlock = false;

		for (final FormAction a : form.getActions()) {
			if (a.getType() != ActionType.INDIRECT_UPLOAD && a.getType() != ActionType.DIRECT_UPLOAD)
				continue;

			final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
			final var methodName = "on" + uploadAttr.getUpperCaseName() + "Upload";
			final var methodSignature = "void " + methodName + "(FileUploadEvent event)";
			final String pkGetter = form.getDTO().getPKAttribute().getModelGetterName();

			final var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Listener for upload events\n");
			b.append(" * @param event\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			generator.addDebugLog(b, "Upload file");

			b.append("\n");

			if (a.getType() == ActionType.INDIRECT_UPLOAD || project.isBoundaryMode()
					|| uploadAttr.getTag() == AttributeTagEnumeration.DOCUMENT_REF) {
				b.append("final File tempFile;\n");
				b.append("final String fileName = event.getFile().getFileName();\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("tempFile = File.createTempFile(fileName, Long.toString(System.currentTimeMillis()));\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				generator.addErrorLog(b, "Error while creating temporary file '{}'!", "e", "fileName");

				b.append("\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_UPLOAD_FAIL, e);\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append("try(final var fout = new FileOutputStream(tempFile))\n");
				b.append("{\n");
				b.append("event.getFile().getInputStream().transferTo(fout);\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				generator.addErrorLog(b, "Error while saving file '{}'!", "e", "fileName");

				b.append("\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_UPLOAD_FAIL, e);\n");
				b.append("return;\n");
				b.append("}\n");
			}

			if (a.getType() == ActionType.INDIRECT_UPLOAD) {
				boolean isFirstEntry = true;
				addTryCatchBlock = true;

				new ServiceInvocationGenerator(a.getBoundaryMethod(), uploadLogic).addInvocation(modelObjectName + "." + pkGetter,
						"tempFile.getAbsolutePath()");

				for (final DTOBeanAttribute dtoAttr : form.getDTO().getAttributes()) {
					if (dtoAttr.getDomainAttribute() == null || !dtoAttr.getDomainAttribute().isTrackVersion())
						continue;

					if (isFirstEntry) {
						uploadLogic.append("\n// We must increment the version field(s) manually in order to avoid optimistic\n");
						uploadLogic.append("// locking exceptions if user performs save operations upon same object afterwards!\n");

						isFirstEntry = false;
					}

					uploadLogic.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "(" + modelObjectName);
					uploadLogic.append("." + dtoAttr.getModelGetterName() + " + 1);\n");
				}
			}
			else {
				for (final DTOBeanAttribute attr : form.getDTO().getAttributes()) {
					if (attr.getDomainAttribute() == null)
						continue;

					final AttributeTagEnumeration tag = attr.getDomainAttribute().getTag();

					if (tag == AttributeTagEnumeration.DOCUMENT_NAME)
						uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(event.getFile().getFileName());\n");
					else if (tag == AttributeTagEnumeration.DOCUMENT_REF || tag == AttributeTagEnumeration.DOCUMENT_DATA) {
						if (!project.isBoundaryMode()) {
							if (tag == AttributeTagEnumeration.DOCUMENT_DATA) {
								addTryCatchBlock = true;
								uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(");

								if (attr.getDomainAttribute().getJavaType().isType(JavaType.BYTE_OBJ_ARRAY))
									uploadLogic.append("FileUtil.convertToByteArray(");

								uploadLogic.append("org.apache.commons.io.IOUtils.toByteArray(event.getFile().getInputStream())");

								if (attr.getDomainAttribute().getJavaType().isType(JavaType.BYTE_OBJ_ARRAY))
									uploadLogic.append(")");

								uploadLogic.append(");\n");
							}
							else {
								uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(");
								uploadLogic.append("tempFile.getAbsolutePath());\n");
							}
						}
						else
							uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(tempFile.getAbsolutePath());\n");
					}
					else if (tag == AttributeTagEnumeration.DOCUMENT_SIZE) {
						final JavaType type = attr.getDomainAttribute().getJavaType();

						if (type.isInteger())
							uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "((int) event.getFile().getSize());\n");
						else
							uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(event.getFile().getSize());\n");
					}
					else if (tag == AttributeTagEnumeration.NONE && uploadAttr.equals(attr.getDomainAttribute())) {
						if (!project.isBoundaryMode()) {
							addTryCatchBlock = true;

							uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(");

							if (attr.getDomainAttribute().getJavaType().isType(JavaType.BYTE_OBJ_ARRAY))
								uploadLogic.append("FileUtil.convertToByteArray(");

							uploadLogic.append("org.apache.commons.io.IOUtils.toByteArray(event.getFile().getInputStream())");

							if (attr.getDomainAttribute().getJavaType().isType(JavaType.BYTE_OBJ_ARRAY))
								uploadLogic.append(")");

							uploadLogic.append(");\n");
						}
						else {
							uploadLogic.append(modelObjectName + "." + attr.getModelSetterName() + "(");
							uploadLogic.append("tempFile.getAbsolutePath());\n");
						}
					}
				}
			}

			b.append("\n");

			if (addTryCatchBlock) {
				b.append("try\n");
				b.append("{\n");
				b.append(uploadLogic.toString());
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				generator.addErrorLog(b, "Error while performing upload of file '{}'!", "e", "event.getFile().getFileName()");

				b.append("\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_UPLOAD_FAIL, e);\n");
				b.append("return;\n");
				b.append("}\n");
			}
			else
				b.append(uploadLogic.toString());

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_UPLOAD_OK);\n");
			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());
		}
	}

}
