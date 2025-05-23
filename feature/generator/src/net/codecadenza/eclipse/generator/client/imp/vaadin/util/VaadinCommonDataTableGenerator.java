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
package net.codecadenza.eclipse.generator.client.imp.vaadin.util;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.SAVED_QUERIES_VIEW;
import static net.codecadenza.eclipse.shared.Constants.BUTTON_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;

import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for common parts of views and grid panels of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinCommonDataTableGenerator {
	private final AbstractJavaSourceGenerator generator;
	private final FormAction updateAction;
	private final FormAction readonlyAction;
	private final DTOBean dto;
	private final Project project;
	private final VaadinSecurityHelper securityHelper;
	private final VaadinI18NGenerator i18n;
	private final EList<FormAction> actionList;
	private FormTypeEnumeration formType;
	private final String idFieldName;
	private boolean addToPanel = true;
	private final String locale;
	private final EList<TableColumnField> columns;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param i18n
	 */
	public VaadinCommonDataTableGenerator(AbstractJavaSourceGenerator generator, Form form, VaadinI18NGenerator i18n) {
		this.generator = generator;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.securityHelper = new VaadinSecurityHelper(project);
		this.updateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityAdded());
		this.readonlyAction = ActionHelper.getDefaultReadOnlyAction(form, securityHelper.isSecurityAdded());
		this.i18n = i18n;
		this.actionList = form.getActions();
		this.formType = form.getFormType();
		this.idFieldName = "id";
		this.addToPanel = false;
		this.locale = i18n.getLocaleFragment();
		this.columns = form.getViewFormPanel().getFormTable().getFields();
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 * @param i18n
	 */
	public VaadinCommonDataTableGenerator(AbstractJavaSourceGenerator generator, FormPanel panel, VaadinI18NGenerator i18n) {
		this.generator = generator;
		this.dto = panel.getDTO();
		this.project = dto.getNamespace().getProject();
		this.securityHelper = new VaadinSecurityHelper(project);
		this.updateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityAdded());
		this.readonlyAction = ActionHelper.getDefaultReadOnlyAction(panel, securityHelper.isSecurityAdded());
		this.i18n = i18n;
		this.actionList = panel.getActions();
		this.idFieldName = "parentId";
		this.locale = i18n.getLocaleFragment();
		this.columns = panel.getFormTable().getFields();
	}

	/**
	 * Create the grid columns for list-of-values dialogs and grid panels
	 * @return the generated content
	 */
	public String createGridColumns() {
		final var b = new StringBuilder();
		boolean firstColumn = true;

		ECollections.sort(columns, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		for (final TableColumnField col : columns) {
			if (!col.isVisible())
				continue;

			final var columnName = "column" + col.getColIndex();

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				if (!firstColumn)
					b.append("\n");
				else
					firstColumn = false;

				b.append("final var translationMap" + col.getColIndex() + " = new HashMap<String, String>();\n");

				for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
					b.append("translationMap" + col.getColIndex() + ".put(\"" + value.getName() + "\", i18n.getTranslation");
					b.append("(" + javaEnum.getName().toUpperCase() + "_" + value.getName().toUpperCase() + "));\n");
				}

				b.append("\n");
			}
			else if (firstColumn)
				firstColumn = false;
			else
				b.append("\n");

			b.append("final Column<" + dto.getName() + "> " + columnName + " = ");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.DATE) {
				if (col.hasDateFormat())
					b.append("addDateColumn");
				else
					b.append("addDateTimeColumn");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE)
				b.append("addLocalDateColumn");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME)
				b.append("addLocalDateTimeColumn");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR)
				b.append("addGregorianCalendarColumn");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.DOUBLE)
				b.append("addDoubleColumn");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.FLOAT)
				b.append("addFloatColumn");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
				b.append("addEnumColumn");
			else
				b.append("addColumn");

			b.append("(\"" + col.getDTOAttribute().getName() + "\", ");
			b.append(dto.getName() + col.getDTOAttribute().getGetterReference());

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
				b.append(", translationMap" + col.getColIndex());

			b.append(");\n");
			b.append(columnName + ".setWidth(\"" + col.getWidth() + "px\");\n");
			b.append(columnName + ".setHeader(" + i18n.getI18N(col) + ");\n");
		}

		return b.toString();
	}

	/**
	 * Add the double-click method
	 */
	public void createDoubleClick() {
		final var b = new StringBuilder();
		final var methodSignature = "void onDoubleClick(" + dto.getName() + " element)";

		if (updateAction == null && readonlyAction == null)
			return;

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.component.");
		b.append("AbstractDataGrid#onDoubleClick(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		generator.addDebugLog(b, "Handle double-click event");

		b.append("\n");

		if (updateAction != null) {
			final var dblClick = new StringBuilder();

			if (addToPanel) {
				b.append("if(!readOnly)\n");
				b.append("{\n");
			}

			dblClick.append("open" + updateAction.getTargetForm().getName() + "();\n");

			if (readonlyAction != null)
				dblClick.append("\nreturn;\n");

			b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), dblClick.toString()));

			if (addToPanel)
				b.append("}\n");
		}

		if (readonlyAction != null) {
			if (updateAction != null)
				b.append("\n");

			final var dblClick = new StringBuilder();
			dblClick.append("open" + readonlyAction.getTargetForm().getName() + "();\n");

			b.append(securityHelper.wrapSecurityCode(readonlyAction.getRoles(), dblClick.toString()));
		}

		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * Add action methods
	 */
	public void addActionMethods() {
		for (final var action : actionList) {
			boolean returnsFile = false;
			String methodName;

			if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
				final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

				if (exchangeMethod.isProcessSingleObject())
					continue;

				if (exchangeMethod.returnsContent() || exchangeMethod.returnsPath())
					returnsFile = true;
			}
			else if (action.getType() == ActionType.DOWNLOAD)
				continue;

			if (action.getTargetForm() != null)
				methodName = "open" + action.getTargetForm().getName();
			else
				methodName = action.getBoundaryMethod().getName();

			var methodSignature = "void " + methodName + "()";

			if (returnsFile)
				methodSignature = "File " + methodName + "()";

			final var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * " + action.getDescription() + "\n");

			if (returnsFile)
				b.append(" * @return the generated file\n");

			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");

			final DTOBeanAttribute pkAttribute = dto.getPKAttribute();
			final String getter = pkAttribute.getModelGetterName();
			final String typeName = pkAttribute.getDomainAttribute().getJavaType().getName();
			final var noObjectSelectedMessage = i18n.getI18NMessage("msg_no_object_selected", "No object selected!");

			if (action.getBoundaryMethod() != null) {
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();

				if (action.getType() == ActionType.DELETE) {
					final var errorMessage = i18n.getI18NMessage("msg_err_delete", "Could not delete selected object!");

					b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ";\n");
					b.append("final String dialogMsg = ");
					b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete the selected object?") + ";\n\n");
					b.append("if(getSelectedItem() == null)\n");
					b.append("{\n");
					b.append("new InfoMessageDialog(dialogTitle, " + noObjectSelectedMessage + ", " + locale + ").open();\n");
					b.append("return;\n");
					b.append("}\n\n");
					b.append("final var dlg = new ConfirmationMessageDialog(dialogTitle, dialogMsg, " + locale + ");\n");
					b.append("dlg.open();\n\n");
					b.append("dlg.setButtonClickListener(type ->\n");
					b.append("{\n");
					b.append("if(type != ButtonType.YES)\n");
					b.append("return;\n\n");
					b.append("try\n");
					b.append("{\n");
					b.append("final " + typeName + " id = getSelectedItem()." + getter + ";\n\n");

					generator.addDebugLog(b, "Delete selected object with id '{}'", "id");

					b.append("\n");

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("id");

					b.append("\n");
					b.append("// Remove selected object from grid\n");
					b.append("removeItem(getSelectedItem());\n");
					b.append("}\n");
					b.append("catch (final Exception e)\n");
					b.append("{\n");

					generator.addErrorLog(b, "Error while deleting selected object!", "e");

					b.append("\n");
					b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
					b.append("}\n");
					b.append("});\n");
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					final var invocationGenerator = new ServiceInvocationGenerator(boundaryMethod, b);

					generator.addDebugLog(b, "Start data export");

					b.append("\n");
					b.append("try\n");
					b.append("{\n");

					if (exchangeMethod.returnsPath()) {
						b.append("final String path = ");

						invocationGenerator.addInvocation();

						b.append("\n");
						b.append("return new File(path);\n");
					}

					if (exchangeMethod.returnsContent()) {
						b.append("final String content = ");

						invocationGenerator.addInvocation();

						b.append("\n");
						b.append("// Create temporary file with generated content!\n");
						b.append("final File tempFile = File.createTempFile(\"DataExport\", \".");
						b.append(exchangeMethod.getDefaultFileExtension());
						b.append("\");\n\n");
						b.append("try(final var fileWriter = new FileWriter(tempFile))\n");
						b.append("{\n");
						b.append("fileWriter.write(content);\n");
						b.append("}\n\n");
						b.append("return tempFile;\n");
					}

					if (!returnsFile)
						invocationGenerator.addInvocation();

					b.append("}\n");
					b.append("catch (final Exception e)\n");
					b.append("{\n");

					generator.addErrorLog(b, "Error while performing data export operation!", "e");

					if (returnsFile) {
						b.append("\n");
						b.append("return null;\n");
					}

					b.append("}\n");
				}
				else if (action.getType() == ActionType.COPY) {
					final var errorMessage = i18n.getI18NMessage("msg_err_copy", "Could not create copy of selected object!");
					var userIdParam = "";

					b.append("final String dialogTitle = ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ";\n");
					b.append("final String dialogMsg = ");
					b.append(i18n.getI18NMessage("msg_confirm_copy", "Do you really want to create a copy of the selected object?"));
					b.append(";\n\n");
					b.append("if(getSelectedItem() == null)\n");
					b.append("{\n");
					b.append("new InfoMessageDialog(dialogTitle, " + noObjectSelectedMessage + ", " + locale + ").open();\n");
					b.append("return;\n");
					b.append("}\n\n");
					b.append("final var dlg = new ConfirmationMessageDialog(dialogTitle, dialogMsg, " + locale + ");\n");
					b.append("dlg.open();\n\n");
					b.append("dlg.setButtonClickListener(type ->\n");
					b.append("{\n");
					b.append("if(type != ButtonType.YES)\n");
					b.append("return;\n\n");
					b.append("try\n");
					b.append("{\n");
					b.append("final " + typeName + " id = getSelectedItem()." + getter + ";\n\n");

					generator.addDebugLog(b, "Create a copy of the selected object with id '{}'", "id");

					b.append("\n");

					if (updateAction != null)
						b.append("final " + typeName + " newId = ");

					if (securityHelper.isSecurityAdded())
						userIdParam = MANAGED_SECURITY_MANAGER + ".getLogOnDTO()."
								+ project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("id", userIdParam);

					b.append("\n");

					if (updateAction != null) {
						final var targetFormClass = updateAction.getTargetForm().getName() + ".class";
						final var sb = "navigateTo(" + targetFormClass + ", newId);\n";

						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), sb));
					}
					else {
						final var copyMessage = i18n.getI18NMessage("msg_copy_finished", "Copy operation finished successfully!");

						b.append("new InfoMessageDialog(dialogTitle, " + copyMessage + ", " + locale + ").open();\n");
					}

					b.append("}\n");
					b.append("catch (final Exception e)\n");
					b.append("{\n");

					generator.addErrorLog(b, "Error while creating a copy of the selected object!", "e");

					b.append("\n");
					b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
					b.append("}\n");
					b.append("});\n");
				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					b.append(createImportFragment(action));
			}
			else {
				final Form targetForm = action.getTargetForm();
				final var errorMessage = i18n.getI18NMessage("msg_err_open_dialog", "The selected dialog could not be opened!");

				generator.addDebugLog(b, "Open '" + targetForm.getName() + "'");

				b.append("\n");
				b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_open_dialog", "Open dialog") + ";\n\n");

				if (targetForm.getFormType() == FormTypeEnumeration.READONLY || targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
					b.append("if(getSelectedItem() == null)\n");
					b.append("return;\n\n");
				}

				b.append("try\n");
				b.append("{\n");

				if (targetForm.getFormType() == FormTypeEnumeration.READONLY || targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
					b.append("final " + typeName + " id = getSelectedItem()." + getter + ";\n\n");
					b.append("navigateTo(" + targetForm.getName() + ".class, id);\n");
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.ADD)
					b.append("navigateTo(" + targetForm.getName() + ".class, " + idFieldName + ");\n");
				else
					b.append("navigateTo(" + targetForm.getName() + ".class);\n");

				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				generator.addErrorLog(b, "Error while opening '" + targetForm.getName() + "'!", "e");

				b.append("\n");
				b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
				b.append("}\n");
			}

			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());
		}
	}

	/**
	 * Add the method for the initialization of context-menu items
	 */
	public void addContextMenuItems() {
		final var b = new StringBuilder();
		final var methodSignature = "void addContextMenuItems()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#addContextMenuItems()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.addContextMenuItems();\n");

		for (final var action : actionList) {
			boolean addReadOnlyTest = true;
			final var itemContent = new StringBuilder();
			String methodName;

			if (action.getTargetForm() != null)
				methodName = "open" + action.getTargetForm().getName();
			else
				methodName = action.getBoundaryMethod().getName();

			if (action.getTargetForm() != null) {
				if (action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE) {
					itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_create", "Create") + ", ");
					itemContent.append("event -> " + methodName + "())");
					itemContent.append(".setId(getId().orElseThrow() + \"_mniCreate\");\n");
				}
				else if (action.getTargetForm().getFormType() == FormTypeEnumeration.ADD) {
					itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_add", "Add") + ", ");
					itemContent.append("event -> " + methodName + "())");
					itemContent.append(".setId(getId().orElseThrow() + \"_mniAdd\");\n");
				}
				else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY) {
					addReadOnlyTest = false;

					itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_view", "View") + ", ");
					itemContent.append("event -> " + methodName + "())");
					itemContent.append(".setId(getId().orElseThrow() + \"_mniView\");\n");
				}
				else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE) {
					itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_edit", "Edit") + ", ");
					itemContent.append("event -> " + methodName + "())");
					itemContent.append(".setId(getId().orElseThrow() + \"_mniEdit\");\n");
				}
			}
			else if (action.getType() == ActionType.DELETE) {
				itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_delete", "Delete") + ", ");
				itemContent.append("event -> " + methodName + "())");
				itemContent.append(".setId(getId().orElseThrow() + \"_mniDelete\");\n");
			}
			else if (action.getType() == ActionType.COPY) {
				itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_copy", "Create copy") + ", ");
				itemContent.append("event -> " + methodName + "())");
				itemContent.append(".setId(getId().orElseThrow() + \"_mniCopy\");\n");
			}
			else if (action.getType() == ActionType.UPLOAD_IMPORT) {
				itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_import", "Import data") + ", ");
				itemContent.append("event -> " + methodName + "())");
				itemContent.append(".setId(getId().orElseThrow() + \"_mniImport\");\n");
			}
			else if (action.getType() == ActionType.DOWNLOAD) {
				addReadOnlyTest = false;

				// File download operations cannot be triggered by a context menu item! It is added without a handler as this
				// functionality might be supported again in future releases!
				itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_download", "Download") + ", null)");
				itemContent.append(".setId(getId().orElseThrow() + \"_mniDownload\");\n");
			}
			else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
				final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
				addReadOnlyTest = false;

				itemContent.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_export", "Export data") + ", ");

				// Export operations that either need a selected item or provide a file to be downloaded cannot be triggered by a
				// context menu item!
				if (exchangeMethod.isProcessSingleObject() || exchangeMethod.returnsContent() || exchangeMethod.returnsPath())
					itemContent.append("null");
				else
					itemContent.append("event -> " + methodName + "()");

				itemContent.append(").setId(getId().orElseThrow() + \"_mniExport\");\n");
			}

			final var itemCheck = new StringBuilder();

			if (addToPanel && addReadOnlyTest)
				itemCheck.append("if(!readOnly)\n");

			itemCheck.append(itemContent);

			b.append("\n");
			b.append(securityHelper.wrapSecurityCode(action.getRoles(), itemCheck.toString()));
		}

		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the method for adding further buttons to the button bar
	 */
	public void createButtonBar() {
		final var methodBody = new StringBuilder();
		final var methodSignature = "void addButtonsToButtonBar(HorizontalLayout hlButtonBar)";

		for (final FormAction action : actionList) {
			final boolean addCreateButton = action.getTargetForm() != null
					&& ((!addToPanel && action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE)
							|| (addToPanel && action.getTargetForm().getFormType() == FormTypeEnumeration.ADD));

			if (addCreateButton) {
				final var sb = new StringBuilder();
				sb.append("final var cmdCreateNew = new Button(" + i18n.getI18NMessage("cmd_create", "Create new") + ");\n");
				sb.append("cmdCreateNew.setIcon(new Icon(VaadinIcon.FILE_ADD));\n");
				sb.append("cmdCreateNew.setId(getId().orElseThrow() + \"_cmdCreate\");\n");
				sb.append("cmdCreateNew.addClickListener(event -> open" + action.getTargetForm().getName() + "());\n\n");
				sb.append("hlButtonBar.add(cmdCreateNew);\n");

				methodBody.append("\n");
				methodBody.append(securityHelper.wrapSecurityCode(action.getRoles(), sb.toString()));
				break;
			}
		}

		for (final FormAction action : actionList) {
			if (action.getType() != ActionType.DOWNLOAD_EXPORT)
				continue;

			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.isProcessSingleObject())
				continue;

			if (exchangeMethod.returnsContent() || exchangeMethod.returnsPath()) {
				final var buttonName = BUTTON_PREFIX + action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);
				final var methodName = action.getBoundaryMethod().getName();
				final var sb = new StringBuilder();

				sb.append("\n");
				sb.append("final var " + buttonName + " = new FileDownloadAnchor(this::" + methodName + ");\n");
				sb.append(buttonName + ".setId(\"" + buttonName + "\");\n");
				sb.append(buttonName + ".add(new Button(" + i18n.getI18NMessage("cmd_download_export", "Download export") + "));\n\n");

				sb.append("hlButtonBar.add(" + buttonName + ");\n");

				methodBody.append("\n");
				methodBody.append(securityHelper.wrapSecurityCode(action.getRoles(), sb.toString()));
			}
		}

		if (formType != null && formType == FormTypeEnumeration.SEARCHABLE_VIEW && securityHelper.isSecurityAdded()
				&& project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null) {
			final String logOnPKGetter = project.getApplicationLogOnDTO().getPKAttribute().getModelGetterName();
			final var infoMessage = i18n.getI18NMessage("msg_query_saved", "Query saved successfully!");
			final var errorMessage = i18n.getI18NMessage("msg_err_save_query", "Could not save selected query!");

			methodBody.append("\nfinal var cmdCreateSavedQuery = new Button(");
			methodBody.append(i18n.getI18NMessage("cmd_save_query", "Save query") + ");\n");
			methodBody.append("cmdCreateSavedQuery.setIcon(new Icon(VaadinIcon.PLUS_CIRCLE));\n\n");
			methodBody.append("cmdCreateSavedQuery.addClickListener(clickEvent ->\n");
			methodBody.append("{\n");
			methodBody.append("final String dialogTitle = ");
			methodBody.append(i18n.getI18NMessage("msg_title_create_saved_query", "Save query") + ";\n");
			methodBody.append("final String inputPrompt = ");
			methodBody.append(i18n.getI18NMessage("msg_saved_query_input_prompt", "Enter name for saved query") + ";\n\n");
			methodBody.append("// Open dialog that requests a name for the new query\n");
			methodBody.append("final var dlg = new InputDialog(dialogTitle, inputPrompt, " + locale + ");\n\n");
			methodBody.append("dlg.setButtonClickListener(type ->\n");
			methodBody.append("{\n");
			methodBody.append("final String value = dlg.getValue();\n\n");
			methodBody.append("if(type == ButtonType.OK && value != null && !value.isEmpty())\n");
			methodBody.append("{\n");
			methodBody.append("try\n");
			methodBody.append("{\n");
			methodBody.append("queryManager.saveQuery(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO().");
			methodBody.append(logOnPKGetter + ", ID, value, searchObj);\n\n");
			methodBody.append("new InfoMessageDialog(dialogTitle, " + infoMessage + ", " + locale + ").open();\n");
			methodBody.append("}\n");
			methodBody.append("catch (final Exception e)\n");
			methodBody.append("{\n");

			generator.addErrorLog(methodBody, "Error while saving query!", "e");

			methodBody.append("\n");
			methodBody.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
			methodBody.append("}\n");
			methodBody.append("}\n");
			methodBody.append("});\n\n");
			methodBody.append("dlg.open();\n");
			methodBody.append("});\n\n");
			methodBody.append("hlButtonBar.add(cmdCreateSavedQuery);\n\n");
			methodBody.append("final var cmdSavedQueries = new Button(");
			methodBody.append(i18n.getI18NMessage("cmd_open_saved_queries", "Saved queries") + ");\n");
			methodBody.append("cmdSavedQueries.setIcon(new Icon(VaadinIcon.PLUS_CIRCLE));\n\n");
			methodBody.append("cmdSavedQueries.addClickListener(event ->\n");
			methodBody.append("{\n");
			methodBody.append("try\n");
			methodBody.append("{\n");
			methodBody.append("navigateTo(" + SAVED_QUERIES_VIEW + ".class, ID);\n");
			methodBody.append("}\n");
			methodBody.append("catch (final Exception e)\n");
			methodBody.append("{\n");

			generator.addErrorLog(methodBody, "Navigation to target view failed!", "e");

			methodBody.append("}\n");
			methodBody.append("});\n\n");
			methodBody.append("hlButtonBar.add(cmdSavedQueries);\n");
		}

		if (methodBody.toString().isEmpty())
			return;

		final var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.");

		if (formType == null || formType == FormTypeEnumeration.SIMPLE_VIEW)
			b.append("component.AbstractDataGrid");
		else
			b.append("search.AbstractDataGridSearchView");

		b.append("#addButtonsToButtonBar(com.vaadin.flow.component.orderedlayout.HorizontalLayout)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.addButtonsToButtonBar(hlButtonBar);\n");
		b.append(methodBody);
		b.append("}\n\n");

		generator.importPackage("com.vaadin.flow.component.button");
		generator.importPackage("com.vaadin.flow.component.orderedlayout");
		generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * Add all necessary imports for all available actions
	 */
	public void addActionImports() {
		actionList.forEach(action -> {
			generator.importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");

			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

					if (!exchangeMethod.isProcessSingleObject() && (exchangeMethod.returnsContent() || exchangeMethod.returnsPath())) {
						generator.importPackage("net.codecadenza.runtime.webclient.vaadin.component");
						generator.importPackage("java.io");
					}
				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT && !action.getBoundaryMethod().getMethodParameters().isEmpty()) {
					final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

					generator.importPackage("java.io");

					if (!exchangeMethod.hasPathParameter())
						generator.importPackage("java.nio.file");
				}
			}
			else {
				// Import the target form
				generator.importClass(project.getClientNamespace().toString() + PACK_CLIENT_DLG + "." + action.getTargetForm().getName());

				if (formType != null && formType == FormTypeEnumeration.SEARCHABLE_VIEW && securityHelper.isSecurityAdded()
						&& project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null)
					generator.importPackage("com.vaadin.flow.component.icon");

				if (action.getTargetForm() != null && (!addToPanel && action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE)
						|| (addToPanel && action.getTargetForm().getFormType() == FormTypeEnumeration.ADD))
					generator.importPackage("com.vaadin.flow.component.icon");
			}
		});
	}

	/**
	 * @param actionImport
	 * @return the generated content
	 */
	private String createImportFragment(FormAction actionImport) {
		final var b = new StringBuilder();
		final BoundaryMethod method = actionImport.getBoundaryMethod();
		final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);
		final var finishMessage = i18n.getI18NMessage("msg_import_finished", "Import operation finished successfully!");
		final var importErrorMessage = i18n.getI18NMessage("msg_err_import", "Error while performing data import operation!");

		if (method.getMethodParameters().size() == 1) {
			b.append("final var dlg = new FileUploadDialog(" + locale + ");\n\n");
			b.append("dlg.setUploadFinishedListener((File uploadFile, String originalFileName) ->\n");
			b.append("{\n");
			b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_data_import", "Data import") + ";\n\n");

			generator.addDebugLog(b, "Upload data import file");

			b.append("\n");

			if (exchangeMethod.hasPathParameter())
				invocationGenerator.addInvocation("uploadFile.getPath()");
			else {
				generator.importClass("java.nio.charset.StandardCharsets");

				final var contentParam = "new String(Files.readAllBytes(uploadFile.toPath()), ";
				invocationGenerator.addInvocation(contentParam + exchangeMethod.getStandardCharset() + ")");
			}

			b.append("\n");
			b.append("new InfoMessageDialog(dialogTitle, " + finishMessage + ", " + locale + ").open();\n");
			b.append("});\n\n");
			b.append("dlg.open();\n");
		}
		else {
			b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_data_import", "Data import") + ";\n\n");

			generator.addDebugLog(b, "Start data import");

			b.append("\n");
			b.append("try\n");
			b.append("{\n");

			invocationGenerator.addInvocation();

			b.append("\n");
			b.append("new InfoMessageDialog(dialogTitle, " + finishMessage + ", " + locale + ").open();\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while performing data import operation!", "ex");

			b.append("\n");
			b.append("new ErrorMessageDialog(dialogTitle, " + importErrorMessage + ", ex, " + locale + ").open();\n");
			b.append("}\n");
		}

		return b.toString();
	}

}
