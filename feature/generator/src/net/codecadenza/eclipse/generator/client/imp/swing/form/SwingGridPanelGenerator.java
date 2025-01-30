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
package net.codecadenza.eclipse.generator.client.imp.swing.form;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.file.SwingFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.swing.util.SwingCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for grid panels of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingGridPanelGenerator extends AbstractJavaSourceGenerator {
	private final FormPanel panel;
	private final DTOBean dto;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final SwingSecurityHelper securityHelper;
	private final String pkTypeName;
	private final boolean addReadonlyParameter;

	/**
	 * Constructor
	 * @param panel
	 */
	public SwingGridPanelGenerator(FormPanel panel) {
		super(panel.getSourceFile());

		this.panel = panel;
		this.dto = panel.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.securityHelper = new SwingSecurityHelper(project);
		this.pkTypeName = panel.getAssociation().getDomainObject().getPKAttribute().getJavaType().getName();
		this.addReadonlyParameter = panel.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		// Add import of the data transfer object that is mapped to this form
		importPackage(panel.getDTO().getNamespace().toString());

		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("net.codecadenza.runtime.richclient.swing.widget");
		importPackage("java.util");
		importPackage("javax.swing");

		// Add imports due to field types
		for (final TableColumnField field : panel.getFormTable().getFields())
			if (field.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				importPackage("net.codecadenza.runtime.richclient.swing.image");
				break;
			}

		// Add security-related imports
		if (!panel.getActions().isEmpty())
			addImports(securityHelper.getSecurityImports());

		// Add imports due to actions
		panel.getActions().forEach(action -> {
			importPackage("java.awt.event");
			importPackage("net.codecadenza.runtime.richclient.swing.image");

			if (action.getBoundaryMethod() != null && (action.getType() == ActionType.DOWNLOAD
					|| action.getType() == ActionType.DOWNLOAD_EXPORT || action.getType() == ActionType.UPLOAD_IMPORT))
				new SwingFileHandlingGenerator(this, action, i18n).addImports();

			if (action.getTargetForm() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

				if (action.getTargetForm().getFormType() != FormTypeEnumeration.READONLY)
					importPackage("net.codecadenza.runtime.richclient.swing.dialog");
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(panel.getName());
		b.append(" extends AbstractDataTablePanel<" + panel.getDTO().getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateField(pkTypeName, "id").withFinalModifier().create();

		panel.getActions().forEach(action -> {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			addPrivateField(actionClassName, action.getName()).create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		var identifier = "";

		if (addReadonlyParameter)
			identifier = panel.getName() + "(" + pkTypeName + " id, final boolean readonly)";
		else
			identifier = panel.getName() + "(" + pkTypeName + " id)";

		// Create the constructor
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param id\n");

		if (addReadonlyParameter)
			b.append(" * @param readonly\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");

		if (addReadonlyParameter)
			b.append("super(false, readonly);\n\n");
		else
			b.append("super(false, true);\n\n");

		b.append("this.id = id;\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		// Add actions and further action-related methods
		addActions();

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();
		final var tableGenerator = new SwingCommonDataTableGenerator(this, panel, i18n);

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		var methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellText());

		methodSignature = "List<ColumnInfo> getColumnDefinition()";

		addMethod(methodSignature, tableGenerator.createColumnDefinition());

		methodSignature = "Icon getCellImage(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellImage());

		methodSignature = "Comparable<?> getCellValue(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellValue());

		if (tableGenerator.addCellExportTextMethod()) {
			methodSignature = "String getCellExportText(" + dto.getName() + " element, int columnIndex)";

			addMethod(methodSignature, tableGenerator.createCellExportText());
		}

		methodSignature = "Collection<" + dto.getName() + "> fetchData()";

		addMethod(methodSignature, tableGenerator.createFetchMethod());

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel");

		i18n.save();
	}

	/**
	 * Add actions
	 */
	private void addActions() {
		final FormAction updateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityAdded());
		final FormAction readonlyAction = ActionHelper.getDefaultReadOnlyAction(panel, securityHelper.isSecurityAdded());
		StringBuilder b;
		String methodSignature;
		FormAction deleteAction = null;

		for (final FormAction action : panel.getActions()) {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			b = new StringBuilder();
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + actionClassName + " extends AbstractAction\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + actionClassName + "()\n");
			b.append("{\n");

			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DELETE) {
					b.append("super(" + i18n.getI18NMessage("action_name_delete", "Delete"));
					b.append(", ImageLoader.getImage(ImageLoader.DELETE));\n\n");

					deleteAction = action;
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("super(" + i18n.getI18NMessage("action_name_download", "Download"));
					b.append(", ImageLoader.getImage(ImageLoader.DOWNLOAD));\n\n");
				}
				else if (action.getType() == ActionType.COPY) {
					b.append("super(" + i18n.getI18NMessage("action_name_copy", "Copy"));
					b.append(", ImageLoader.getImage(ImageLoader.COPY));\n\n");
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					b.append("super(" + i18n.getI18NMessage("action_name_export", "Export"));
					b.append(", ImageLoader.getImage(ImageLoader.DATA_EXPORT));\n\n");
				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT) {
					b.append("super(" + i18n.getI18NMessage("action_name_import", "Import"));
					b.append(", ImageLoader.getImage(ImageLoader.DATA_IMPORT));\n\n");
				}
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE) {
				b.append("super(" + i18n.getI18NMessage("action_name_create", "Create"));
				b.append(", ImageLoader.getImage(ImageLoader.NEW_DATA));\n\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.ADD) {
				b.append("super(" + i18n.getI18NMessage("action_name_add", "Add"));
				b.append(", ImageLoader.getImage(ImageLoader.NEW_DATA));\n\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY) {
				b.append("super(" + i18n.getI18NMessage("action_name_view", "View"));
				b.append(", ImageLoader.getImage(ImageLoader.VIEW_DATA));\n\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE) {
				b.append("super(" + i18n.getI18NMessage("action_name_edit", "Edit"));
				b.append(", ImageLoader.getImage(ImageLoader.EDIT_DATA));\n\n");
			}

			b.append("putValue(SHORT_DESCRIPTION, " + i18n.getI18N(panel, action) + ");\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void actionPerformed(ActionEvent e)\n");
			b.append("{\n");

			if (action.getBoundaryMethod() != null) {
				final DomainAttribute pkAttribute = action.getPanel().getDTO().getDomainObject().getPKAttribute();
				final String getter = action.getPanel().getDTO().getPKAttribute().getGetterName();
				final String typeName = pkAttribute.getJavaType().getName();
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
				final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryMethod, b);

				if (action.getType() == ActionType.DELETE) {
					b.append("final " + dto.getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final int result = JOptionPane.showConfirmDialog(" + panel.getName() + ".this, ");
					b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete selected object?"));
					b.append(", " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ", JOptionPane.YES_NO_OPTION);\n\n");
					b.append("if(result == JOptionPane.NO_OPTION)\n");
					b.append("return;\n\n");

					declarationGenerator.addLocalVariable();

					b.append("\n");
					b.append("try\n");
					b.append("{\n");

					addDebugLog(b, "Delete selected object with id '{}'", "selectedElement." + getter);

					b.append("\n");

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedElement." + getter);

					b.append("removeElementFromModel(selectedElement);\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while deleting selected object!", "ex");

					b.append("\n");
					b.append("JOptionPane.showMessageDialog(" + panel.getName() + ".this, ");
					b.append(i18n.getI18NMessage("msg_err_delete", "Could not delete selected object! Message: "));
					b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_delete", "Delete object"));
					b.append(", JOptionPane.ERROR_MESSAGE);\n");
					b.append("}\n");

					declarationGenerator.addCloseStatementInFinallyBlock();
				}
				else if (action.getType() == ActionType.COPY) {
					var userIdParam = "";

					b.append("final " + dto.getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final int result = JOptionPane.showConfirmDialog(" + panel.getName() + ".this, ");
					b.append(i18n.getI18NMessage("msg_confirm_copy", "Do you really want to create a copy of selected object?"));
					b.append(", " + i18n.getI18NMessage("msg_title_copy", "Create copy of selected object"));
					b.append(", JOptionPane.YES_NO_OPTION);\n\n");
					b.append("if(result == JOptionPane.NO_OPTION)\n");
					b.append("return;\n\n");

					declarationGenerator.addLocalVariable();

					b.append("\n");
					b.append("try\n");
					b.append("{\n");

					addDebugLog(b, "Create a copy of the selected object with id '{}'", "selectedElement." + getter);

					b.append("\n");

					if (updateAction != null)
						b.append("final " + typeName + " newId = ");

					if (securityHelper.isSecurityAdded())
						userIdParam = SECURITY_MANAGER + ".getLogOnDTO()."
								+ project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedElement." + getter, userIdParam);

					b.append("\n");

					if (updateAction != null) {
						final String targetFormName = updateAction.getTargetForm().getName();
						final var fb = new StringBuilder();

						fb.append("final var dlg = new " + targetFormName + "(" + panel.getName() + ".this, newId);\n");
						fb.append("dlg.setVisible(true);\n");

						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), fb.toString()));
						b.append("\n");
					}

					b.append("performFetch();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while creating a copy of the selected object!", "ex");

					b.append("\n");
					b.append("JOptionPane.showMessageDialog(" + panel.getName() + ".this, ");
					b.append(i18n.getI18NMessage("msg_err_copy", "Could not create copy of selected object! Message: "));
					b.append(" + ex.getMessage(), ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object"));
					b.append(", JOptionPane.ERROR_MESSAGE);\n");
					b.append("}\n");

					declarationGenerator.addCloseStatementInFinallyBlock();
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("final " + dto.getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " id = selectedElement." + getter + ";\n");
					b.append(new SwingFileHandlingGenerator(this, action, i18n).createDownloadMethodBody(true, panel.getName()));
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					boolean processSpecificObject = false;

					if (exchangeMethod.getSingleObjectFilterParam() != null) {
						b.append("final " + dto.getName() + " selectedElement = getSelectedElement();\n\n");
						b.append("if(selectedElement == null)\n");
						b.append("return;\n\n");
						b.append("final " + typeName + " id = selectedElement." + getter + ";\n");

						processSpecificObject = true;
					}

					if (exchangeMethod.returnsPath())
						b.append(new SwingFileHandlingGenerator(this, action, i18n).createDownloadMethodBody(processSpecificObject,
								panel.getName()));

					if (exchangeMethod.returnsContent())
						b.append(new SwingFileHandlingGenerator(this, action, i18n).createDownloadFragmentForExport(panel.getName()));

					if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath())
						b.append(new SwingFileHandlingGenerator(this, action, i18n).createExportInvocationFragment(panel.getName()));

				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					b.append(new SwingFileHandlingGenerator(this, action, i18n).createUploadFragmentForImport(panel.getName()));
			}
			else {
				final String targetFormName = action.getTargetForm().getName();
				final Form targetForm = action.getTargetForm();

				if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
					b.append("final var dlg = new " + targetFormName + "(" + panel.getName() + ".this);\n");
					b.append("dlg.setVisible(true);\n\n");
					b.append("if(dlg.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
					b.append("return;\n\n");
					b.append("performFetch();\n");
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.ADD) {
					b.append("final var dlg = new " + targetFormName + "(" + panel.getName() + ".this, id);\n");
					b.append("dlg.setVisible(true);\n\n");
					b.append("if(dlg.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
					b.append("return;\n\n");
					b.append("performFetch();\n");
				}
				else {
					b.append("final " + dto.getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final var dlg = new " + targetFormName + "(" + panel.getName());
					b.append(".this, selectedElement." + dto.getPKAttribute().getGetterName() + ");\n");
					b.append("dlg.setVisible(true);\n");

					if (targetForm.getFormType() != FormTypeEnumeration.READONLY) {
						b.append("\nif(dlg.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
						b.append("return;\n\n");
						b.append("performFetch();\n");
					}
				}
			}

			b.append("}\n");
			b.append("}\n\n");

			addSubClass(actionClassName, b.toString());
		}

		if (deleteAction != null) {
			final var actionCall = "if(!readonly)\n" + deleteAction.getName() + ".actionPerformed(null);\n";
			methodSignature = "void onDeleteKeyPressed(" + dto.getName() + " element)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#");
			b.append("onDeleteKeyPressed(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append(securityHelper.wrapSecurityCode(deleteAction.getRoles(), actionCall));
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		if (updateAction != null || readonlyAction != null) {
			methodSignature = "void onEnterKeyPressed(" + dto.getName() + " element)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#");
			b.append("onEnterKeyPressed(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("openDetailDialog();\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			methodSignature = "void onDblClick(" + dto.getName() + " element)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#");
			b.append("onDblClick(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("openDetailDialog();\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			methodSignature = "void openDetailDialog()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Open a detail dialog for the selected table element\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");

			if (updateAction != null) {
				if (readonlyAction != null) {
					final var codeToWrap = "if(!readonly)\n{\n" + updateAction.getName() + ".actionPerformed(null);\nreturn;\n}\n";

					b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), codeToWrap));
				}
				else {
					final var codeToWrap = "if(!readonly)\n" + updateAction.getName() + ".actionPerformed(null);\n";

					b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), codeToWrap));
				}
			}

			if (readonlyAction != null) {
				if (updateAction != null)
					b.append("\n");

				final var actionCall = readonlyAction.getName() + ".actionPerformed(null);\n";

				b.append(securityHelper.wrapSecurityCode(readonlyAction.getRoles(), actionCall));
			}

			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		methodSignature = "void initActions()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#initActions()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.initActions();\n");

		for (final FormAction action : panel.getActions()) {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			b.append("\n");

			if (action.getType() != ActionType.DOWNLOAD && action.getType() != ActionType.READ
					&& action.getType() != ActionType.DOWNLOAD_EXPORT)
				b.append("if(!readonly)\n");

			b.append(action.getName() + " = new " + actionClassName + "();\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void initToolBar()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#initToolBar()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.initToolBar();\n");

		for (final FormAction action : panel.getActions()) {
			var toolBarAdd = "";

			b.append("\n");

			if (action.getType() != ActionType.DOWNLOAD && action.getType() != ActionType.READ
					&& action.getType() != ActionType.DOWNLOAD_EXPORT)
				toolBarAdd += "if(!readonly)\n";

			toolBarAdd += "toolBar.add(" + action.getName() + ");\n";

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), toolBarAdd));
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void initPopUpMenu()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#initPopUpMenu()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.initPopUpMenu();\n");

		for (final FormAction action : panel.getActions()) {
			var initPopUp = "";

			b.append("\n");

			if (action.getType() != ActionType.DOWNLOAD && action.getType() != ActionType.READ
					&& action.getType() != ActionType.DOWNLOAD_EXPORT)
				initPopUp += "if(!readonly)\n";

			initPopUp += "popupMenu.add(" + action.getName() + ");\n";

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), initPopUp));
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
