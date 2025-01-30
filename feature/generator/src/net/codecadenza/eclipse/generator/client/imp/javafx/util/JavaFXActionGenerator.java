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
package net.codecadenza.eclipse.generator.client.imp.javafx.util;

import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.file.JavaFXFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for actions of views and grid panels of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXActionGenerator {
	private final AbstractJavaSourceGenerator generator;
	private final DTOBean dto;
	private final Project project;
	private final FormAction updateAction;
	private final FormAction readonlyAction;
	private final String owner;
	private final JavaFXSecurityHelper securityHelper;
	private final EList<FormAction> actions;
	private FormAction deleteAction;
	private Form form;
	private boolean addReadonlyCheck;
	private String baseClassName = "AbstractDataGridView";

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 * @param securityHelper
	 */
	public JavaFXActionGenerator(AbstractJavaSourceGenerator generator, Form form, JavaFXSecurityHelper securityHelper) {
		this.generator = generator;
		this.form = form;
		this.securityHelper = securityHelper;
		this.updateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityAdded());
		this.readonlyAction = ActionHelper.getDefaultReadOnlyAction(form, securityHelper.isSecurityAdded());
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.actions = form.getActions();
		this.owner = "null";
	}

	/**
	 * Constructor
	 * @param generator
	 * @param gridPanel
	 * @param securityHelper
	 */
	public JavaFXActionGenerator(AbstractJavaSourceGenerator generator, FormPanel gridPanel, JavaFXSecurityHelper securityHelper) {
		this.generator = generator;
		this.securityHelper = securityHelper;
		this.updateAction = ActionHelper.getDefaultUpdateAction(gridPanel, securityHelper.isSecurityAdded());
		this.readonlyAction = ActionHelper.getDefaultReadOnlyAction(gridPanel, securityHelper.isSecurityAdded());
		this.dto = gridPanel.getDTO();
		this.project = dto.getNamespace().getProject();
		this.actions = gridPanel.getActions();
		this.owner = "getOwner()";
		this.baseClassName = "AbstractDataGridPanel";
		this.addReadonlyCheck = gridPanel.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional();
	}

	/**
	 * Add fields
	 */
	public void addFields() {
		actions.forEach(action -> {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			generator.addPrivateField(actionClassName, action.getName()).create();
		});
	}

	/**
	 * Add necessary imports
	 */
	public void addImports() {
		if (actions.isEmpty())
			return;

		generator.importPackage("net.codecadenza.runtime.richclient.javafx.image");
		generator.importPackage("net.codecadenza.runtime.richclient.javafx.control");
		generator.addImports(securityHelper.getSecurityImports());

		actions.forEach(action -> {
			if (action.getBoundaryMethod() != null) {
				generator.importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

				if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT
						|| action.getType() == ActionType.UPLOAD_IMPORT)
					new JavaFXFileHandlingGenerator(generator, action, null).addImports();
			}
			else {
				// Import the target form
				generator.importClass(project.getClientNamespace().toString() + PACK_CLIENT_DLG + "." + action.getTargetForm().getName());
			}
		});
	}

	/**
	 * Add actions
	 * @param i18n
	 */
	public void addActions(RichClientI18NGenerator i18n) {
		StringBuilder b;

		for (final FormAction action : actions) {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			b = new StringBuilder();
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("private class " + actionClassName + " extends Action\n");
			b.append("{\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + actionClassName + "()\n");
			b.append("{\n");

			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DELETE) {
					b.append("this.title = " + i18n.getI18NMessage("action_name_delete", "Delete") + ";\n");
					b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_DELETE);\n");

					deleteAction = action;
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("this.title = " + i18n.getI18NMessage("action_name_download", "Download") + ";\n");
					b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_DOWNLOAD);\n");
				}
				else if (action.getType() == ActionType.COPY) {
					b.append("this.title = " + i18n.getI18NMessage("action_name_copy", "Create copy") + ";\n");
					b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_COPY);\n");
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					b.append("this.title = " + i18n.getI18NMessage("action_name_export", "Data export") + ";\n");
					b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_DATA_EXPORT);\n");
				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT) {
					b.append("this.title = " + i18n.getI18NMessage("action_name_import", "Data import") + ";\n");
					b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_DATA_IMPORT);\n");
				}
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE) {
				b.append("this.title = " + i18n.getI18NMessage("action_name_create", "Create") + ";\n");
				b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_NEW_DATA);\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.ADD) {
				b.append("this.title = " + i18n.getI18NMessage("action_name_add", "Add") + ";\n");
				b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_NEW_DATA);\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY) {
				b.append("this.title = " + i18n.getI18NMessage("action_name_view", "View") + ";\n");
				b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_VIEW_DATA);\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE) {
				b.append("this.title = " + i18n.getI18NMessage("action_name_edit", "Edit") + ";\n");
				b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_EDIT_DATA);\n");
			}

			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void handle()\n");
			b.append("{\n");

			if (securityHelper.isSecurityAdded()) {
				b.append("if(!isEnabled())\n");
				b.append("return;\n\n");
			}

			if (action.getBoundaryMethod() != null) {
				final DomainAttribute pkAttribute;

				if (action.getForm() != null)
					pkAttribute = action.getForm().getDomainObject().getPKAttribute();
				else
					pkAttribute = action.getPanel().getDTO().getDomainObject().getPKAttribute();

				final String getter = dto.getPKAttribute().getGetterName();
				final String typeName = pkAttribute.getJavaType().getName();
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
				final var declarationGenerator = new ServiceDeclarationGenerator(generator, boundaryMethod, b);

				if (action.getType() == ActionType.DELETE) {
					b.append("final " + dto.getName() + " selectedItem = getSelectedItem();\n\n");
					b.append("if(selectedItem == null)\n");
					b.append("return;\n\n");
					b.append("final String msg = ");
					b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete selected object?") + ";\n");
					b.append("final String title = " + i18n.getI18NMessage("msg_title_delete", "Delete object?") + ";\n\n");
					b.append("if(DialogButtonType.YES != DialogUtil.openConfirmationDialog(" + owner + ", title, msg))\n");
					b.append("return;\n\n");

					declarationGenerator.addLocalVariable();

					b.append("\ntry\n");
					b.append("{\n");

					generator.addDebugLog(b, "Delete selected object with id '{}'", "selectedItem." + getter);

					b.append("\n");

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedItem." + getter);

					b.append("getTableView().getItems().remove(selectedItem);\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					generator.addErrorLog(b, "Error while deleting selected object!", "ex");

					b.append("\n");
					b.append("DialogUtil.openErrorDialog(" + owner + ", title, ex);\n");
					b.append("}\n");

					declarationGenerator.addCloseStatementInFinallyBlock();
				}
				else if (action.getType() == ActionType.COPY) {
					var userIdParam = "";

					b.append("final " + dto.getName() + " selectedItem = getSelectedItem();\n\n");
					b.append("if(selectedItem == null)\n");
					b.append("return;\n\n");
					b.append("final String msg = ");
					b.append(i18n.getI18NMessage("msg_confirm_copy", "Do you really want to create a copy of selected object?") + ";\n");
					b.append("final String title = " + i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ";\n\n");
					b.append("if(DialogButtonType.YES != DialogUtil.openConfirmationDialog(" + owner + ", title, msg))\n");
					b.append("return;\n\n");

					declarationGenerator.addLocalVariable();

					b.append("\ntry\n");
					b.append("{\n");

					generator.addDebugLog(b, "Create a copy of the selected object with id '{}'", "selectedItem." + getter);

					b.append("\n");

					if (updateAction != null)
						b.append("final " + typeName + " newId = ");

					if (securityHelper.isSecurityAdded())
						userIdParam = SECURITY_MANAGER + ".getLogOnDTO()."
								+ project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedItem." + getter, userIdParam);

					b.append("\n");

					if (updateAction != null) {
						final String targetFormName = updateAction.getTargetForm().getName();
						final var fb = new StringBuilder();

						fb.append("final var dlg = new " + targetFormName + "(" + owner + ", newId);\n");
						fb.append("dlg.open();\n");

						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), fb.toString()));
						b.append("\n");
					}

					b.append("refreshView();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					generator.addErrorLog(b, "Error while creating a copy of the selected object!", "ex");

					b.append("\n");
					b.append("DialogUtil.openErrorDialog(" + owner + ", title, ex);\n");
					b.append("}\n");

					declarationGenerator.addCloseStatementInFinallyBlock();
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("final " + dto.getName() + " selectedItem = getSelectedItem();\n\n");
					b.append("if(selectedItem == null)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " id = selectedItem." + getter + ";\n");
					b.append(new JavaFXFileHandlingGenerator(generator, action, i18n).createDownloadMethodBody(true, owner));
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					boolean processSpecificObject = false;

					if (exchangeMethod.getSingleObjectFilterParam() != null) {
						b.append("final " + dto.getName() + " selectedItem = getSelectedItem();\n\n");
						b.append("if(selectedItem == null)\n");
						b.append("return;\n\n");
						b.append("final " + typeName + " id = selectedItem." + getter + ";\n");

						processSpecificObject = true;
					}

					if (exchangeMethod.returnsPath())
						b.append(
								new JavaFXFileHandlingGenerator(generator, action, i18n).createDownloadMethodBody(processSpecificObject, owner));

					if (exchangeMethod.returnsContent())
						b.append(new JavaFXFileHandlingGenerator(generator, action, i18n).createDownloadFragmentForExport());

					if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath())
						b.append(new JavaFXFileHandlingGenerator(generator, action, i18n).createExportInvocationFragment());
				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					b.append(new JavaFXFileHandlingGenerator(generator, action, i18n).createUploadFragmentForImport());
			}
			else {
				final String targetFormName = action.getTargetForm().getName();
				final Form targetForm = action.getTargetForm();

				if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
					b.append("final var dlg = new " + targetFormName + "(" + owner + ");\n\n");
					b.append("if(DialogButtonType.OK != dlg.open())\n");
					b.append("return;\n\n");
					b.append("refreshView();\n");
				}
				else {
					if (targetForm.getFormType() == FormTypeEnumeration.ADD && form == null)
						b.append("final var dlg = new " + targetFormName + "(" + owner + ", id);\n");
					else {
						b.append("final " + dto.getName() + " selectedItem = getSelectedItem();\n\n");
						b.append("if(selectedItem == null)\n");
						b.append("return;\n\n");
						b.append("final var dlg = new " + targetFormName + "(" + owner + ", selectedItem.");
						b.append(dto.getPKAttribute().getGetterName() + ");\n");
					}

					if (targetForm.getFormType() != FormTypeEnumeration.READONLY) {
						b.append("\n");
						b.append("if(DialogButtonType.OK != dlg.open())\n");
						b.append("return;\n\n");
						b.append("refreshView();\n");
					}
					else
						b.append("dlg.open();\n");
				}
			}

			b.append("}\n");

			if (securityHelper.isSecurityAdded()) {
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.Action#isEnabled()\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public boolean isEnabled()\n");
				b.append("{\n");
				b.append(securityHelper.checkAuthorization(action.getRoles()));
				b.append("}\n");
			}

			b.append("}\n\n");

			generator.addSubClass(actionClassName, b.toString());
		}

		var methodSignature = "void onDeletePressed(" + dto.getName() + " element)";

		if (deleteAction != null) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.");
			b.append(baseClassName + "#onDeletePressed(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public void onDeletePressed(" + dto.getName() + " element)\n");
			b.append("{\n");

			if (addReadonlyCheck)
				b.append("if(!readonly)\n");

			b.append(deleteAction.getName() + ".handle();\n");
			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());
		}

		methodSignature = "void onEnterPressed(" + dto.getName() + " element)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.");
		b.append(baseClassName + "#onEnterPressed(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (updateAction != null) {
			if (addReadonlyCheck)
				b.append("if(!readonly)\n");

			b.append(updateAction.getName() + ".handle();\n");

			if (addReadonlyCheck && readonlyAction != null) {
				b.append("else\n");
				b.append(readonlyAction.getName() + ".handle();\n");
			}
		}
		else if (readonlyAction != null)
			b.append(readonlyAction.getName() + ".handle();\n");

		if (updateAction == null && readonlyAction == null)
			b.append("// No implementation required!\n");

		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());

		methodSignature = "void onDoubleClick(" + dto.getName() + " element)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.");
		b.append(baseClassName + "#onDoubleClick(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (updateAction != null) {
			if (addReadonlyCheck)
				b.append("if(!readonly)\n");

			b.append(updateAction.getName() + ".handle();\n");

			if (addReadonlyCheck && readonlyAction != null) {
				b.append("else\n");
				b.append(readonlyAction.getName() + ".handle();\n");
			}
		}
		else if (readonlyAction != null)
			b.append(readonlyAction.getName() + ".handle();\n");

		if (updateAction == null && readonlyAction == null)
			b.append("// No implementation required!\n");

		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());

		methodSignature = "void initActions()";

		if (!actions.isEmpty()) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");

			if (form != null && form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
				b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.AbstractSearchGridView#initActions()\n");
			else
				b.append(" * @see net.codecadenza.runtime.richclient.javafx.search." + baseClassName + "#initActions()\n");

			b.append(" */\n");
			b.append("@Override\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("super.initActions();\n");

			for (final FormAction action : actions) {
				final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

				b.append("\n");
				b.append(action.getName() + " = new " + actionClassName + "();\n\n");

				if (addReadonlyCheck && action.getType() != ActionType.DOWNLOAD && action.getType() != ActionType.READ
						&& action.getType() != ActionType.DOWNLOAD_EXPORT) {
					b.append("if(!readonly)\n");
					b.append("{\n");
				}

				b.append("addActionToToolBar(" + action.getName() + ");\n");
				b.append("addActionToContextMenu(" + action.getName() + ");\n");

				if (addReadonlyCheck && action.getType() != ActionType.DOWNLOAD && action.getType() != ActionType.READ
						&& action.getType() != ActionType.DOWNLOAD_EXPORT)
					b.append("}\n");
			}

			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());
		}
	}

}
