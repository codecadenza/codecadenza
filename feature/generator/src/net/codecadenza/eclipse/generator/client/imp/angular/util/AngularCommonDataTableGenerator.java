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
package net.codecadenza.eclipse.generator.client.imp.angular.util;

import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.exchange.AngularExportGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.exchange.AngularImportGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.file.AngularDownloadGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.security.AngularSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for common parts of views and grid panels of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularCommonDataTableGenerator {
	private final AbstractTypeScriptSourceGenerator generator;
	private final Form form;
	private final FormPanel gridPanel;
	private final FormAction updateAction;
	private final FormAction readonlyAction;
	private final DTOBean dto;
	private final AngularSecurityHelper securityHelper;
	private final AngularI18NGenerator i18n;
	private final AngularDownloadGenerator downloadGenerator;
	private final AngularExportGenerator dataExportGenerator;
	private final AngularImportGenerator dataImportGenerator;
	private final EList<FormAction> actionList;
	private final AngularContentFormatter formatter;
	private boolean addToPanel = true;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param i18n
	 */
	public AngularCommonDataTableGenerator(AbstractTypeScriptSourceGenerator generator, Form form, AngularI18NGenerator i18n) {
		this.generator = generator;
		this.form = form;
		this.i18n = i18n;
		this.gridPanel = null;
		this.dto = form.getDTO();
		this.formatter = generator.getContentFormatter();
		this.securityHelper = new AngularSecurityHelper(dto.getNamespace().getProject());
		this.downloadGenerator = new AngularDownloadGenerator(generator, form, i18n);
		this.dataExportGenerator = new AngularExportGenerator(generator, form, i18n);
		this.dataImportGenerator = new AngularImportGenerator(generator, form, i18n);
		this.updateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityEnabled());
		this.readonlyAction = ActionHelper.getDefaultReadOnlyAction(form, securityHelper.isSecurityEnabled());
		this.actionList = form.getActions();
		this.addToPanel = false;
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 * @param i18n
	 */
	public AngularCommonDataTableGenerator(AbstractTypeScriptSourceGenerator generator, FormPanel panel,
			AngularI18NGenerator i18n) {
		this.generator = generator;
		this.gridPanel = panel;
		this.dto = panel.getDTO();
		this.form = null;
		this.formatter = generator.getContentFormatter();
		this.securityHelper = new AngularSecurityHelper(dto.getNamespace().getProject());
		this.downloadGenerator = new AngularDownloadGenerator(generator, panel, i18n);
		this.dataExportGenerator = new AngularExportGenerator(generator, panel, i18n);
		this.dataImportGenerator = new AngularImportGenerator(generator, panel, i18n);
		this.readonlyAction = ActionHelper.getDefaultReadOnlyAction(panel, securityHelper.isSecurityEnabled());
		this.i18n = i18n;
		this.actionList = panel.getActions();

		if (gridPanel.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
			this.updateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityEnabled());
		else
			this.updateAction = null;
	}

	/**
	 * Add all necessary imports
	 */
	public void addImports() {
		generator.importType("FieldTypeEnum", "../../common/model/field-type.enum");
	}

	/**
	 * Add fields
	 */
	public void addFields() {
		boolean addService = !addToPanel;

		if (addToPanel) {
			// Check if the respective service for this grid panel must be added!
			final DomainObject domainObject = gridPanel.getBoundaryMethod().getBoundaryBean().getDomainObject();

			if (!dto.getDomainObject().equals(domainObject)) {
				// Check if at least one action exists that requires this service
				for (final ActionType type : actionList.stream().map(FormAction::getType).toList())
					if (type == ActionType.COPY || type == ActionType.DELETE || type == ActionType.UPLOAD_IMPORT
							|| type == ActionType.DOWNLOAD || type == ActionType.DOWNLOAD_EXPORT) {
						addService = true;
						break;
					}
			}
		}

		if (addService)
			generator.addService(dto);
		else
			generator.importType(dto.getName(), "../../domain/" + dto.getName().toLowerCase() + ".interface");

		if (!actionList.isEmpty() && securityHelper.isSecurityEnabled()) {
			generator.addService("AuthService", "authService", "../../common/services/auth.service");

			generator.importType("RoleEnum ", "../../common/model/role.enum");
		}

		if (actionList.stream().anyMatch(action -> action.getTargetForm() != null))
			generator.addService("Router", "router", "@angular/router");

		if (downloadGenerator.isDownloadFragmentAdded() || dataExportGenerator.addFileService()
				|| dataImportGenerator.addFileService())
			generator.addService("FileService", "fileService", "../../common/services/file.service");
	}

	/**
	 * @return true if the 'New' button should be visible
	 */
	public boolean showButtonForCreatingObject() {
		return actionList.stream().anyMatch(a -> a.getType() == ActionType.CREATE);
	}

	/**
	 * @return true if the 'Import' button should be visible
	 */
	public boolean showButtonForDataImport() {
		return dataImportGenerator.isImportButtonRequired();
	}

	/**
	 * Create the double-click handler
	 */
	public void createDoubleClickHandler() {
		if (updateAction == null && readonlyAction == null)
			return;

		final String paramName = dto.getDomainObject().getLowerCaseName();
		final var selectedObjectId = paramName + "." + dto.getPKAttribute().getName();

		formatter.addBlockComment("Handle double-click events");
		formatter.addLine("override onRowDoubleClicked(" + paramName + ": " + dto.getName() + ") {");
		formatter.increaseIndent();

		if (updateAction != null) {
			final var dblClick = new StringBuilder();
			final String targetFormURL = AngularURLGenerator.createURL(updateAction.getTargetForm(), false);

			if (addToPanel) {
				formatter.addLine("if (!this.readonly) {");
				formatter.increaseIndent();
			}

			dblClick.append("this.router.navigate(['" + targetFormURL + "/' + " + selectedObjectId + "]);\n");

			if (readonlyAction != null)
				dblClick.append("return;\n");

			securityHelper.wrapSecurityCode(formatter, updateAction.getRoles(), dblClick.toString());

			if (addToPanel) {
				formatter.decreaseIndent();
				formatter.addLine("}");
			}
		}

		if (readonlyAction != null) {
			final String targetFormURL = AngularURLGenerator.createURL(readonlyAction.getTargetForm(), false);
			final var dblClick = "this.router.navigate(['" + targetFormURL + "/' + " + selectedObjectId + "]);";

			if (updateAction != null)
				formatter.addBlankLine();

			securityHelper.wrapSecurityCode(formatter, readonlyAction.getRoles(), dblClick);
		}

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Create the action methods
	 */
	public void createActionMethods() {
		actionList.stream().filter(a -> a.getType() == ActionType.DELETE || a.getType() == ActionType.COPY).forEach(action -> {
			final String methodName = action.getBoundaryMethod().getName();

			if (action.getType() == ActionType.DELETE) {
				formatter.addBlockComment("Open a confirmation dialog before deleting the selected item");
				formatter.addLine("confirmDelete() {");
				formatter.increaseIndent();
				formatter.addLine("this.openConfirmDeleteDialog(this." + methodName + ".bind(this));");
			}
			else {
				formatter.addBlockComment("Open a confirmation dialog before creating a copy of the selected item");
				formatter.addLine("confirmCopy() {");
				formatter.increaseIndent();
				formatter.addLine("this.openConfirmCopyDialog(this." + methodName + ".bind(this));");
			}

			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		});

		FormAction createAction = null;

		for (final FormAction action : actionList) {
			final Form targetForm = action.getTargetForm();

			if (targetForm == null)
				continue;

			// The preferred form type for creating new objects in a grid panel is 'ADD'!
			if (addToPanel && targetForm.getFormType() == FormTypeEnumeration.ADD)
				createAction = action;

			// In case of a grid panel we only use a form of type 'CREATE' if the preferred type is not available!
			if (addToPanel && createAction != null && targetForm.getFormType() == FormTypeEnumeration.CREATE)
				createAction = action;

			if (!addToPanel && targetForm.getFormType() == FormTypeEnumeration.CREATE)
				createAction = action;
		}

		if (createAction != null) {
			final var invocation = "this.open" + createAction.getTargetForm().getName() + "();\n";

			formatter.addBlockComment("Open the page for creating a new object when pressing the 'New' button");
			formatter.addLine("override onNewButtonClicked() {");
			formatter.increaseIndent();

			securityHelper.wrapSecurityCode(formatter, createAction.getRoles(), invocation);

			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}

		actionList.forEach(action -> {
			var selectedItemId = "this.selectedItem." + dto.getPKAttribute().getName();

			if (dto.getPKAttribute().getDomainAttribute().getJavaType().isIntegerOrLong())
				selectedItemId += ".toString()";

			if (action.getBoundaryMethod() != null && (action.getType() == ActionType.DELETE || action.getType() == ActionType.COPY)) {
				final String methodName = action.getBoundaryMethod().getName();
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
				final DTOBean logOnDTO = dto.getNamespace().getProject().getApplicationLogOnDTO();
				final String errorMsg;
				String param = selectedItemId;

				if (action.getType() == ActionType.COPY && logOnDTO != null) {
					param += ", this.authService.getLoggedOnUser()." + logOnDTO.getPKAttribute().getName();

					if (logOnDTO.getPKAttribute().getDomainAttribute().getJavaType().isIntegerOrLong())
						param += ".toString()";
				}

				if (action.getType() == ActionType.COPY)
					errorMsg = i18n.getI18NMessage("msg_errorcopy", "Error while creating a copy of the selected object!");
				else
					errorMsg = i18n.getI18NMessage("msg_errordelete", "Error while deleting object!");

				formatter.addBlockComment(action.getDescription());
				formatter.addLine(methodName + "() {");
				formatter.increaseIndent();
				formatter.addIfStatement("!this.selectedItem", "return;", true);
				formatter.addLine(new AngularServiceInvocationGenerator(boundaryMethod).createInvocation(param) + ".subscribe({");
				formatter.increaseIndent();
				formatter.addLine("error: error => this.displayError(error, " + errorMsg + "),");
				formatter.addLine("complete: () => this.refreshView()");
				formatter.decreaseIndent();
				formatter.addLine("});");
				formatter.decreaseIndent();
				formatter.addLine("}");
				formatter.addBlankLine();
			}

			if (action.getTargetForm() != null) {
				final Form targetForm = action.getTargetForm();
				final String targetFormURL = AngularURLGenerator.createURL(targetForm, false);
				final var methodName = "open" + action.getTargetForm().getName();

				formatter.addBlockComment(action.getDescription());
				formatter.addLine(methodName + "() {");
				formatter.increaseIndent();

				if (targetForm.getFormType() == FormTypeEnumeration.READONLY || targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
					formatter.addIfStatement("!this.selectedItem", "return;", true);
					formatter.addLine("const selectedItemId = " + selectedItemId + ";");
					formatter.addLine("this.router.navigate(['" + targetFormURL + "/' + selectedItemId]);");
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.ADD)
					formatter.addLine("this.router.navigate(['" + targetFormURL + "/' + this.parentObjectId]);");
				else
					formatter.addLine("this.router.navigate(['" + targetFormURL + "']);");

				formatter.decreaseIndent();
				formatter.addLine("}");
				formatter.addBlankLine();
			}
		});

		// Add download methods
		downloadGenerator.addDownloadMethods();

		// Add methods for data export operations
		dataExportGenerator.addExportMethods();

		// Add methods for data import operations
		dataImportGenerator.addImportMethods();
	}

	/**
	 * Add context-menu items
	 */
	public void addContextMenuItems() {
		actionList.forEach(action -> {
			boolean addReadOnlyTest = true;
			final var itemFormatter = new AngularContentFormatter();
			final var itemContent = new StringBuilder();
			String methodName;

			if (action.getTargetForm() != null)
				methodName = "open" + action.getTargetForm().getName();
			else
				methodName = action.getBoundaryMethod().getName();

			if (action.getTargetForm() != null) {
				if (action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE) {
					itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_create", "Create"));
					itemContent.append(", 'pi pi-plus', 'mniCreate', ");
					itemContent.append("() => this." + methodName + "());");
				}
				else if (action.getTargetForm().getFormType() == FormTypeEnumeration.ADD) {
					itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_add", "Add"));
					itemContent.append(", 'pi pi-plus', 'mniAdd', ");
					itemContent.append("() => this." + methodName + "());");
				}
				else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY) {
					addReadOnlyTest = false;

					itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_view", "View"));
					itemContent.append(", 'pi pi-search', 'mniView', ");
					itemContent.append("() => this." + methodName + "());");
				}
				else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE) {
					itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_edit", "Edit"));
					itemContent.append(", 'pi pi-pencil', 'mniUpdate', ");
					itemContent.append("() => this." + methodName + "());");
				}
			}
			else if (action.getType() == ActionType.DELETE) {
				itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_delete", "Delete"));
				itemContent.append(", 'pi pi-times', 'mniDelete', ");
				itemContent.append("() => this.confirmDelete());");
			}
			else if (action.getType() == ActionType.COPY) {
				itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_copy", "Create copy"));
				itemContent.append(", 'pi pi-copy', 'mniCopy', ");
				itemContent.append("() => this.confirmCopy());");
			}
			else if (action.getType() == ActionType.UPLOAD_IMPORT && action.getBoundaryMethod().getMethodParameters().isEmpty()) {
				// Add the item if no file selection is required!
				itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_import", "Import data"));
				itemContent.append(", 'pi pi-upload', 'mniImport', ");
				itemContent.append("() => this." + methodName + "());");
			}
			else if (action.getType() == ActionType.DOWNLOAD) {
				addReadOnlyTest = false;

				itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_download", "Download"));
				itemContent.append(", 'pi pi-download', 'mniDownload', ");
				itemContent.append("() => this." + methodName + "());");
			}
			else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
				addReadOnlyTest = false;

				itemContent.append("this.addContextMenuItem(" + i18n.getI18NMessage("action_export", "Export data"));
				itemContent.append(", 'pi pi-download', 'mniExport', ");
				itemContent.append("() => this." + methodName + "());");
			}

			if (addToPanel && addReadOnlyTest)
				itemFormatter.addIfStatement("!this.readonly", itemContent.toString(), false);
			else
				itemFormatter.addLine(itemContent.toString());

			if (!itemContent.isEmpty()) {
				formatter.addBlankLine();

				securityHelper.wrapSecurityCode(formatter, action.getRoles(), itemFormatter.getContent());
			}
		});
	}

	/**
	 * Create the method to initialize the component's data table
	 */
	public void createTableSetup() {
		final EList<TableColumnField> cols;

		if (addToPanel)
			cols = gridPanel.getFormTable().getFields();
		else
			cols = form.getViewFormPanel().getFormTable().getFields();

		// Sort the columns
		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		formatter.addBlockComment("Define the columns");
		formatter.addLine("initTableDefinition(): TableDefinition {");
		formatter.increaseIndent();
		formatter.addLine("const tableDefinition = new TableDefinition();");

		for (final TableColumnField col : cols) {
			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
				generator.addDependentEnum((JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType());

			if (!col.isVisible())
				continue;

			final JavaType type = col.getDTOAttribute().getDomainAttribute().getJavaType();
			final boolean resetDateTimeFormatFlag = type.isTemporalType() && col.hasDateFormat();

			final var columnDef = new StringBuilder();
			columnDef.append(formatter.getIndent() + "tableDefinition.addColumn(");
			columnDef.append("'" + col.getDTOAttribute().getName() + "', ");
			columnDef.append(i18n.getI18N(col) + ", FieldTypeEnum.");

			if (type.isTemporalType())
				columnDef.append("DATE");
			else if (type.isBoolean())
				columnDef.append("BOOLEAN");
			else if (type.isIntegerOrLong())
				columnDef.append("INTEGER");
			else if (type.isDecimalNumber())
				columnDef.append("DECIMAL");
			else if (type.isString() || type.isChar() || type.isUUID())
				columnDef.append("STRING");
			else
				columnDef.append("ENUM");

			columnDef.append(", ");
			columnDef.append(col.getWidth());
			columnDef.append(")");

			if (resetDateTimeFormatFlag)
				columnDef.append(".dateTimeFormat = false");

			columnDef.append(";\n");

			formatter.addBlankLine();
			formatter.addContent(columnDef.toString());
		}

		formatter.addBlankLine();
		formatter.addLine("return tableDefinition;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
