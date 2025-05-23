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
package net.codecadenza.eclipse.generator.client.imp.swing.view;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;
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
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for views of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingViewGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final DTOBean dto;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final SwingSecurityHelper securityHelper;
	private final FormPanel panel;
	private boolean hasActions;
	private boolean importLov;

	/**
	 * Constructor
	 * @param form
	 */
	public SwingViewGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.securityHelper = new SwingSecurityHelper(project);
		this.panel = form.getViewFormPanel();

		if (!form.getActions().isEmpty())
			this.hasActions = true;

		// Check if the form uses one list-of-values field at least
		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			for (final TableColumnField col : panel.getFormTable().getFields())
				if (col.getLovForm() != null) {
					this.importLov = true;
					break;
				}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage(form.getDTO().getNamespace().toString());
		importPackage("javax.swing");
		importPackage("net.codecadenza.runtime.search.dto");
		importPackage("net.codecadenza.runtime.richclient.swing.search");
		importPackage("java.util");
		addImports(securityHelper.getSecurityImports());

		if (importLov)
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_LOV);

		if (hasActions) {
			importPackage("java.awt.event");
			importPackage("net.codecadenza.runtime.richclient.swing.image");
		}

		// Add imports due to field types
		for (final TableColumnField f : panel.getFormTable().getFields())
			if (f.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				importPackage("net.codecadenza.runtime.richclient.swing.image");
				break;
			}

		form.getActions().forEach(action -> {
			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT
						|| action.getType() == ActionType.UPLOAD_IMPORT)
					new SwingFileHandlingGenerator(this, action, i18n).addImports();
			}
			else {
				// Import the target form
				importClass(project.getClientNamespace().toString() + PACK_CLIENT_DLG + "." + action.getTargetForm().getName());

				if (action.getType() == ActionType.CREATE || action.getType() == ActionType.UPDATE)
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
		b.append(form.getName());

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			b.append(" extends AbstractResultView<" + form.getDTO().getName() + ">");
		else
			b.append(" extends AbstractSearchResultView<" + form.getDTO().getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		form.getActions().forEach(action -> {
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
		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param title\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "(String title)\n");
		b.append("{\n");

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			b.append("super(title, null);\n");
		else
			b.append("super(title);\n");

		b.append("}\n\n");

		addConstructor(form.getName() + "(String title)", b.toString());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" * @param title\n");
			b.append(" * @param savedSearchId\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + form.getName() + "(String title, Integer savedSearchId)\n");
			b.append("{\n");
			b.append("super(title, savedSearchId);\n");
			b.append("}\n\n");

			addConstructor(form.getName() + "(String title, Integer savedSearchId)", b.toString());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		addActions();

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView#getViewID()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getViewID()\n");
		b.append("{\n");
		b.append("return this.getClass().getName();\n");
		b.append("}\n\n");

		addMethod("String getViewID()", b.toString());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.Countable#");
			b.append("countData(net.codecadenza.runtime.search.dto.SearchDTO)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public long countData(SearchDTO searchDTO)\n");
			b.append("{\n");

			final var declarationGenerator = new ServiceDeclarationGenerator(this, form.getBoundaryMethod(), b);
			declarationGenerator.addLocalVariable();

			b.append("\n");
			b.append("try\n");
			b.append("{\n");

			for (final FormPanel formPanel : form.getFormPanels()) {
				b.append("return ");

				new ServiceInvocationGenerator(formPanel.getBoundaryMethod(), b).addInvocation("searchDTO");
			}

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");
			b.append("JOptionPane.showMessageDialog(dataPanel, ");
			b.append(i18n.getI18NMessage("msg_err_count", "Error while counting records! Message: "));
			b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_count", "Count records"));
			b.append(", JOptionPane.ERROR_MESSAGE);\n");
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();

			b.append("\n");
			b.append("return -1;\n");
			b.append("}\n\n");

			addMethod("long countData(SearchDTO searchDTO)", b.toString());
		}

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView#initSearch()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public SearchDTO initSearch()\n");
		b.append("{\n");
		b.append("final var searchObj = new SearchDTO();\n");
		b.append("int colOrderId = -1;\n\n");
		b.append("// Initialize search object\n");
		b.append("searchObj.setMaxResult(1000);\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(false);\n");
		b.append("searchObj.setCount(true);\n");
		b.append("\n");

		// Add all search input fields
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			boolean addLine = false;

			b.append("final var field" + col.getColIndex() + " = new SearchFieldDTO(++colOrderId, ");
			b.append(col.getDTOAttribute().getSelectTokenConstant());
			b.append(", ");
			b.append(i18n.getI18N(col));
			b.append(", SearchFieldDataTypeEnum.");
			b.append(col.getFieldType().name());
			b.append(", ");
			b.append(col.getWidth());
			b.append(");\n");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				b.append("\nfinal var enumListValues" + col.getColIndex() + " = new HashMap<String, String>();\n");

				for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
					b.append("enumListValues" + col.getColIndex() + ".put(\"" + value.getName() + "\", getTranslation");
					b.append("(" + javaEnum.getName().toUpperCase() + "_" + value.getName().toUpperCase() + "));\n");
				}

				b.append("\n");
				b.append("field" + col.getColIndex() + ".setEnumListValues(enumListValues" + col.getColIndex() + ");\n");

				addLine = true;
			}

			if (col.hasDateFormat()) {
				b.append("field" + col.getColIndex() + ".setDateTimeFormat(false);\n");
				addLine = true;
			}

			if (col.getLovForm() != null) {
				b.append("field" + col.getColIndex() + ".setListOfValues(" + col.getLovForm().getName() + ".class.getName());\n");
				addLine = true;
			}

			if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW && !col.isSearchable()) {
				b.append("field" + col.getColIndex() + ".setType(SearchFieldTypeEnum.NOT_SEARCHABLE);\n");
				addLine = true;
			}

			if (addLine)
				b.append("\n");

			b.append("searchObj.getSearchFields().add(field" + col.getColIndex() + ");\n\n");
		}

		b.append("return searchObj;\n}\n\n");

		addMethod("SearchDTO initSearch()", b.toString());

		final var tableGenerator = new SwingCommonDataTableGenerator(this, form, i18n);
		var methodSignature = "Collection<" + form.getDTO().getName() + "> fetchData()";

		addMethod(methodSignature, tableGenerator.createFetchMethod());

		methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellText());

		methodSignature = "Icon getCellImage(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellImage());

		methodSignature = "Comparable<?> getCellValue(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellValue());

		if (tableGenerator.addCellExportTextMethod()) {
			methodSignature = "String getCellExportText(" + dto.getName() + " element, int columnIndex)";

			addMethod(methodSignature, tableGenerator.createCellExportText());
		}

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.swing.search.AbstractResultView");

		i18n.save();
	}

	/**
	 * Add actions
	 */
	private void addActions() {
		final FormAction updateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityAdded());
		final FormAction readonlyAction = ActionHelper.getDefaultReadOnlyAction(form, securityHelper.isSecurityAdded());
		StringBuilder b;
		String methodSignature;
		FormAction deleteAction = null;

		for (final FormAction action : form.getActions()) {
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
					b.append("super(" + i18n.getI18NMessage("action_name_copy", "Create copy"));
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
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY) {
				b.append("super(" + i18n.getI18NMessage("action_name_view", "View"));
				b.append(", ImageLoader.getImage(ImageLoader.VIEW_DATA));\n\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE) {
				b.append("super(" + i18n.getI18NMessage("action_name_edit", "Edit"));
				b.append(", ImageLoader.getImage(ImageLoader.EDIT_DATA));\n\n");
			}

			b.append("putValue(SHORT_DESCRIPTION, " + i18n.getI18N(action) + ");\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void actionPerformed(ActionEvent e)\n");
			b.append("{\n");

			if (action.getBoundaryMethod() != null) {
				final DomainAttribute pkAttribute = action.getForm().getDomainObject().getPKAttribute();
				final String getter = form.getDTO().getPKAttribute().getGetterName();
				final JavaType type = pkAttribute.getJavaType();
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
				final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryMethod, b);

				if (action.getType() == ActionType.DELETE) {
					b.append("final " + form.getDTO().getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final int result = JOptionPane.showConfirmDialog(dataPanel, ");
					b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete selected object?"));
					b.append(", " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ", JOptionPane.YES_NO_OPTION);\n\n");
					b.append("if(result == JOptionPane.NO_OPTION)\n");
					b.append("return;\n\n");

					declarationGenerator.addLocalVariable();

					b.append("\ntry\n");
					b.append("{\n");

					addDebugLog(b, "Delete selected object with id '{}'", "selectedElement." + getter);

					b.append("\n");

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedElement." + getter);

					b.append(form.getName() + ".this.removeElement(selectedElement);\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while deleting selected object!", "ex");

					b.append("\n");
					b.append("JOptionPane.showMessageDialog(dataPanel, ");
					b.append(i18n.getI18NMessage("msg_err_delete", "Could not delete selected object! Message: "));
					b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_delete", "Delete object"));
					b.append(", JOptionPane.ERROR_MESSAGE);\n");
					b.append("}\n");

					declarationGenerator.addCloseStatementInFinallyBlock();
				}
				else if (action.getType() == ActionType.COPY) {
					var paramUserId = "";

					b.append("final " + form.getDTO().getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final int result = JOptionPane.showConfirmDialog(dataPanel, ");
					b.append(i18n.getI18NMessage("msg_confirm_copy", "Do you really want to create a copy of selected object?"));
					b.append(", " + i18n.getI18NMessage("msg_title_copy", "Create copy of selected object"));
					b.append(", JOptionPane.YES_NO_OPTION);\n\n");
					b.append("if(result == JOptionPane.NO_OPTION)\n");
					b.append("return;\n\n");

					declarationGenerator.addLocalVariable();

					b.append("\ntry\n");
					b.append("{\n");

					addDebugLog(b, "Create a copy of the selected object with id '{}'", "selectedElement." + getter);

					b.append("\n");

					if (updateAction != null)
						b.append("final " + type.getName() + " newId = ");

					if (securityHelper.isSecurityAdded())
						paramUserId = SECURITY_MANAGER + ".getLogOnDTO()."
								+ project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedElement." + getter, paramUserId);

					b.append("\n");

					if (updateAction != null) {
						final String targetFormName = updateAction.getTargetForm().getName();
						final var fb = new StringBuilder();

						fb.append("final var dlg = new " + targetFormName + "(" + form.getName() + ".this, newId);\n");
						fb.append("dlg.setVisible(true);\n");

						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), fb.toString()));
						b.append("\n");
					}

					b.append(form.getName() + ".this.performFetch();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while creating a copy of the selected object!", "ex");

					b.append("\n");
					b.append("JOptionPane.showMessageDialog(dataPanel, ");
					b.append(i18n.getI18NMessage("msg_err_copy", "Could not create copy of selected object! Message: "));
					b.append(" + ex.getMessage(), ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ", JOptionPane.ERROR_MESSAGE);\n");
					b.append("}\n");

					declarationGenerator.addCloseStatementInFinallyBlock();
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("final " + form.getDTO().getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final " + type.getName() + " id = selectedElement." + getter + ";\n");
					b.append(new SwingFileHandlingGenerator(this, action, i18n).createDownloadMethodBody(true, form.getName()));
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					boolean processSpecificObject = false;

					if (exchangeMethod.getSingleObjectFilterParam() != null) {
						b.append("final " + form.getDTO().getName() + " selectedElement = getSelectedElement();\n\n");
						b.append("if(selectedElement == null)\n");
						b.append("return;\n\n");
						b.append("final " + type.getName() + " id = selectedElement." + getter + ";\n");

						processSpecificObject = true;
					}

					if (exchangeMethod.returnsPath())
						b.append(new SwingFileHandlingGenerator(this, action, i18n).createDownloadMethodBody(processSpecificObject,
								form.getName()));

					if (exchangeMethod.returnsContent())
						b.append(new SwingFileHandlingGenerator(this, action, i18n).createDownloadFragmentForExport(form.getName()));

					if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath())
						b.append(new SwingFileHandlingGenerator(this, action, i18n).createExportInvocationFragment(form.getName()));

				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					b.append(new SwingFileHandlingGenerator(this, action, i18n).createUploadFragmentForImport(form.getName()));
			}
			else {
				final String targetFormName = action.getTargetForm().getName();
				final Form targetForm = action.getTargetForm();

				if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
					b.append("final var dlg = new " + targetFormName + "(" + form.getName() + ".this);\n");
					b.append("dlg.setVisible(true);\n\n");
					b.append("if(dlg.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
					b.append("return;\n\n");
					b.append(form.getName() + ".this.performFetch();\n");
				}
				else {
					b.append("final " + form.getDTO().getName() + " selectedElement = getSelectedElement();\n\n");
					b.append("if(selectedElement == null)\n");
					b.append("return;\n\n");
					b.append("final var dlg = new " + targetFormName + "(" + form.getName() + ".this, selectedElement.");
					b.append(form.getDTO().getPKAttribute().getGetterName() + ");\n");
					b.append("dlg.setVisible(true);\n");

					if (targetForm.getFormType() != FormTypeEnumeration.READONLY) {
						b.append("\nif(dlg.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
						b.append("return;\n\n");
						b.append(form.getName() + ".this.performFetch();\n");
					}
				}
			}

			b.append("}\n");
			b.append("}\n\n");

			addSubClass(actionClassName, b.toString());
		}

		if (deleteAction != null) {
			final var actionCall = deleteAction.getName() + ".actionPerformed(null);\n";
			methodSignature = "void onDeletePressed(" + dto.getName() + " element)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.");
			b.append("AbstractResultView#onDeletePressed(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void onDeletePressed(" + dto.getName() + " element)\n");
			b.append("{\n");
			b.append(securityHelper.wrapSecurityCode(deleteAction.getRoles(), actionCall));
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		if (updateAction != null || readonlyAction != null) {
			methodSignature = "void onEnterPressed(" + dto.getName() + " element)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.");
			b.append("AbstractResultView#onEnterPressed(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("openDetailDialog();\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			methodSignature = "void onDoubleClick(" + dto.getName() + " element)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.");
			b.append("AbstractResultView#onDoubleClick(java.lang.Object)\n");
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
			b.append(" * Open a detail dialog for the selected view element\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");

			if (updateAction != null) {
				if (readonlyAction != null) {
					final var actionCall = updateAction.getName() + ".actionPerformed(null);\nreturn;\n";

					b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), actionCall));
				}
				else {
					final var actionCall = updateAction.getName() + ".actionPerformed(null);\n";

					b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), actionCall));
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
		b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView#initActions()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		for (final FormAction action : form.getActions()) {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			b.append(action.getName() + " = new " + actionClassName + "();\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		boolean firstItem = true;
		methodSignature = "void initToolBar(JToolBar toolBar)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.search.");
		b.append("AbstractResultView#initToolBar(javax.swing.JToolBar)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		for (final FormAction action : form.getActions()) {
			final var toolBarAdd = "toolBar.add(" + action.getName() + ");\n";

			if (firstItem)
				firstItem = false;
			else
				b.append("\n");

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), toolBarAdd));
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		boolean firstAction = true;
		methodSignature = "void initPopUpMenu(JPopupMenu menu)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.search.");
		b.append("AbstractResultView#initPopUpMenu(javax.swing.JPopupMenu)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		for (final FormAction action : form.getActions()) {
			final var toolBarAdd = "menu.add(" + action.getName() + ");\n";

			if (firstAction)
				firstAction = false;
			else
				b.append("\n");

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), toolBarAdd));
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
