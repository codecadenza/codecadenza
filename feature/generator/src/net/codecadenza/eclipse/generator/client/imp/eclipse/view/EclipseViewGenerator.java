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
package net.codecadenza.eclipse.generator.client.imp.eclipse.view;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.EclipseFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.eclipse.util.EclipseClientFieldHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.CommonGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for views of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseViewGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final FormPanel panel;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final EclipseSecurityHelper securityHelper;
	private boolean importLov;
	private boolean hasActions;

	/**
	 * Constructor
	 * @param form
	 */
	public EclipseViewGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.project = form.getDomainObject().getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.securityHelper = new EclipseSecurityHelper(project);
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

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			importClass("net.codecadenza.runtime.richclient.eclipse.search.AbstractResultView");
		else
			importClass("net.codecadenza.runtime.richclient.eclipse.search.AbstractSearchResultView");

		importPackage(form.getDTO().getNamespace().toString());
		importPackage("java.util");
		importClass("jakarta.annotation.PostConstruct");
		importClass("jakarta.annotation.PreDestroy");
		importPackage("net.codecadenza.runtime.search.dto");
		importPackage("org.eclipse.swt.widgets");
		importClass("org.eclipse.jface.dialogs.MessageDialog");

		addImports(new EclipseSecurityHelper(project).getSecurityImports());

		if (importLov)
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_LOV);

		if (hasActions) {
			importPackage("org.eclipse.jface.action");
			importPackage("org.eclipse.swt");
			importPackage("org.eclipse.swt.events");
			importClass("net.codecadenza.runtime.richclient.eclipse.image.ImageCache");
		}

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			importPackage("org.eclipse.e4.ui.model.application.ui.basic");

		form.getActions().forEach(action -> {
			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT
						|| action.getType() == ActionType.UPLOAD_IMPORT)
					new EclipseFileHandlingGenerator(this, action, i18n).addImports();
			}
			else {
				if (action.getType() == ActionType.CREATE || action.getType() == ActionType.UPDATE)
					importClass("org.eclipse.jface.dialogs.Dialog");

				// Import the target form
				importClass(project.getClientNamespace().toString() + PACK_CLIENT_DLG + "." + action.getTargetForm().getName());
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
		// Add the unique view identifier
		addPublicConstant(JavaType.STRING, "ID",
				"\"" + project.getClientNamespace().toString() + PACK_CLIENT_VIEW + "." + form.getName() + "\"").create();

		new ServiceDeclarationGenerator(this, form.getBoundaryMethod().getBoundaryBean()).addField(false);

		form.getActions().forEach(action -> {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			addPrivateField(actionClassName, action.getName()).create();
		});

		new EclipseClientFieldHelper(form, this).addClientField();

		addActions();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> {
			final int value1 = col1.getColIndex();
			final int value2 = col2.getColIndex();

			if (value1 == value2)
				return 0;

			if (value1 > value2)
				return 1;

			return -1;
		});

		b.append("/* (non-Javadoc)\n");

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#fetchData()\n");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#fetchData()\n");

		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public Collection<" + form.getDTO().getName() + "> fetchData()\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(form.getBoundaryMethod(), b).addInvocation("searchObj");

		b.append("}\n\n");

		addMethod("Collection<" + form.getDTO().getName() + "> fetchData()", b.toString());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#getViewId()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public String getViewId()\n");
			b.append("{\n");
			b.append("return ID;\n");
			b.append("}\n\n");

			addMethod("String getViewId()", b.toString());
		}

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.Countable#countData()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public long countData()\n");
			b.append("{\n");

			for (final FormPanel formPanel : form.getFormPanels()) {
				b.append("return ");

				new ServiceInvocationGenerator(formPanel.getBoundaryMethod(), b).addInvocation("searchObj");
			}

			b.append("}\n\n");

			addMethod("long countData()", b.toString());
		}

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#initSearch()\n");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#initSearch()\n");

		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void initSearch()\n");
		b.append("{\n");

		new ServiceDeclarationGenerator(this, form.getBoundaryMethod(), b).initField();

		b.append("\n");
		b.append("searchObj = new SearchDTO();\n");
		b.append("int colOrderId = -1;\n\n");
		b.append("// Initialize search object\n");
		b.append("searchObj.setMaxResult(1000);\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(false);\n");
		b.append("searchObj.setCount(true);\n");

		// Add all search input fields
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			boolean addLine = false;

			b.append("\n");
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

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b.append("\nfinal var enumListValues" + col.getColIndex() + " = new HashMap<String, String>();\n");

				// Add an empty element in order to enable deselection!
				b.append("enumListValues" + col.getColIndex() + ".put(\"\", \"\");\n");

				for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
					b.append("enumListValues" + col.getColIndex() + ".put(\"" + value.getName() + "\", getTranslation");
					b.append("(" + javaEnum.getName().toUpperCase() + "_" + value.getName().toUpperCase() + "));\n");
				}

				b.append("\n");
				b.append("field" + col.getColIndex() + ".setEnumListValues(enumListValues" + col.getColIndex() + ");\n");

				addLine = true;
			}

			// It is not supported that we query the database in a generic way if we run in an unmanaged environment!
			if (!project.isJavaSEApplication() && col.getDTOAttribute().getDomainAttribute().isWildcardFilteringSupported()
					&& col.getLovForm() != null) {
				var command = "";
				final DTOBeanAttribute attr = col.getDTOAttribute();
				final DomainAttribute domainAttr = attr.getDomainAttribute();
				DomainObject lovDomainObj = null;

				// Find the proper domain object for a list-of-values command!
				if (attr.getAssociation() == null) {
					if (!attr.getDTOBean().getDomainObject().isMappedSuperClass())
						lovDomainObj = attr.getDTOBean().getDomainObject();
				}
				else
					lovDomainObj = attr.getAssociation().getTarget();

				if (lovDomainObj != null) {
					command = "select distinct a." + domainAttr.getName() + " from " + lovDomainObj.getName() + " a where a."
							+ domainAttr.getName() + " like :paramPrefix";

					if (domainAttr.getDomainObject().isMandated()
							&& form.getBoundaryMethod().getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
						final DTOBeanAttribute clientPKAttr = project.getApplicationLogOnDTO().getClientPKAttribute();
						final JavaType pkType = clientPKAttr.getDomainAttribute().getJavaType();

						command += " and " + CommonGenerator.getClientAccessFragment(domainAttr.getDomainObject(), "a") + " = ";

						if (pkType.isString() || pkType.isUUID())
							command += "'";

						b.append("field" + col.getColIndex() + ".setLovCommand(\"" + command + "\" + ");
						b.append(SECURITY_MANAGER + ".getLogOnDTO()." + clientPKAttr.getGetterName());

						if (pkType.isString() || pkType.isUUID())
							b.append(" + \"'\"");

						b.append(");\n");
					}
					else
						b.append("field" + col.getColIndex() + ".setLovCommand(\"" + command + "\");\n");

					addLine = true;
				}
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

			b.append("searchObj.getSearchFields().add(field" + col.getColIndex() + ");\n");
		}

		b.append("}\n\n");

		addMethod("void initSearch()", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			b.append("__AbstractResultView#getColText(java.lang.Object, int)\n");
		else
			b.append("__AbstractSearchResultView#getColText(java.lang.Object, int)\n");

		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getColText(" + form.getDTO().getName() + " object, int columnIndex)\n");
		b.append("{\n");
		b.append("switch(columnIndex)\n");
		b.append("{\n");

		int colIndex = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			final String getter = "object." + col.getDTOAttribute().getGetterName();
			final DomainAttribute domainAttribute = col.getDTOAttribute().getDomainAttribute();
			final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

			b.append("case " + colIndex++ + ":\n");

			if (addNullCheck)
				b.append("if(" + getter + " != null)\n");

			b.append("return " + domainAttribute.convertToString(getter) + ";\n");

			if (addNullCheck)
				b.append("else\nreturn \"\";\n");
		}

		b.append("default:\n");
		b.append("return \"\";\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod("String getColText(" + form.getDTO().getName() + " object, int columnIndex)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.AbstractSearchResultView#dispose()\n");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.AbstractResultView#dispose()\n");

		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@PreDestroy\n");
		b.append("public void dispose()\n");
		b.append("{\n");
		b.append("super.dispose();\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, form.getBoundaryMethod(), b);

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");
			b.append("if(" + declarationGenerator.getServiceName() + " != null)\n");

			declarationGenerator.addCloseStatement();
		}

		b.append("}\n\n");

		addMethod("void dispose()", b.toString());

		i18n.save();
	}

	/**
	 * Add actions
	 */
	private void addActions() {
		final FormAction updateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityAdded());
		final FormAction readonlyAction = ActionHelper.getDefaultReadOnlyAction(form, securityHelper.isSecurityAdded());
		FormAction deleteAction = null;
		StringBuilder b;

		for (final FormAction action : form.getActions())
			if (action.getType() == ActionType.DELETE)
				deleteAction = action;

		for (final FormAction action : form.getActions()) {
			final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

			b = new StringBuilder();
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + actionClassName + " extends Action\n");
			b.append("{\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + actionClassName + "()\n");
			b.append("{\n");
			b.append("this.setToolTipText(" + i18n.getI18N(action) + ");\n");

			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DELETE)
					b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_DELETE));\n");
				else if (action.getType() == ActionType.COPY)
					b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_COPY));\n");
				else if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT)
					b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(\"download.png\"));\n");
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(\"upload.png\"));\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE)
				b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(\"new_data.png\"));\n");
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY)
				b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(\"view_data.png\"));\n");
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE)
				b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(\"edit_data.png\"));\n");

			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.jface.action.Action#run()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void run()\n");
			b.append("{\n");

			if (action.getBoundaryMethod() != null) {
				final DomainAttribute pkAttribute = action.getForm().getDomainObject().getPKAttribute();
				final String getter = form.getDTO().getPKAttribute().getGetterName();
				final String typeName = pkAttribute.getJavaType().getName();
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();

				if (action.getType() == ActionType.DELETE) {
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " id = getSelection()." + getter + ";\n\n");
					b.append("final boolean ok = MessageDialog.openQuestion(parentShell, ");
					b.append(i18n.getI18NMessage("msg_title_delete", "Delete object") + ", ");
					b.append(i18n.getI18NMessage("msg_conf_delete", "Do you really want to delete selected object?") + ");\n\n");
					b.append("if(!ok)\n");
					b.append("return;\n\n");
					b.append("try\n");
					b.append("{\n");
					b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

					addDebugLog(b, "Delete selected object with id '{}'", "id");

					b.append("\n");

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("id");

					b.append("refreshView();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while deleting selected object!", "ex");

					b.append("\n");
					b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ", ");
					b.append(
							i18n.getI18NMessage("msg_err_delete", "Could not delete selected object! Message: ") + " + ex.getMessage());\n");
					b.append("}\n");
					b.append("finally\n");
					b.append("{\n");
					b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");
					b.append("}\n");
				}
				else if (action.getType() == ActionType.COPY) {
					var paramUserId = "";

					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");

					if (updateAction == null)
						b.append("final ");

					b.append(typeName + " id = getSelection()." + getter + ";\n\n");
					b.append("final boolean ok = MessageDialog.openQuestion(parentShell, ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ", ");
					b.append(i18n.getI18NMessage("msg_conf_copy", "Do you really want to create a copy of selected object?") + ");\n\n");
					b.append("if(!ok)\n");
					b.append("return;\n\n");
					b.append("try\n");
					b.append("{\n");
					b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

					addDebugLog(b, "Create a copy of the selected object with id '{}'", "id");

					b.append("\n");

					if (updateAction != null)
						b.append("id = ");

					if (securityHelper.isSecurityAdded())
						paramUserId = SECURITY_MANAGER + ".getLogOnDTO()."
								+ project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("id", paramUserId);

					b.append("\n");

					if (updateAction != null) {
						final var fb = new StringBuilder();
						final String targetFormName = updateAction.getTargetForm().getName();

						fb.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n\n");
						fb.append("final var dlg = new " + targetFormName + "(parentShell, id);\n");
						fb.append("dlg.open();\n");

						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), fb.toString()));
					}

					b.append("\nrefreshView();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while creating a copy of the selected object!", "ex");

					b.append("\n");
					b.append("MessageDialog.openError(parentShell, ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ", ");
					b.append(i18n.getI18NMessage("msg_err_copy", "Could not create copy of selected object! Message: "));
					b.append(" + ex.getMessage());\n");
					b.append("}\n");
					b.append("finally\n");
					b.append("{\n");
					b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");
					b.append("}\n");
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " id = getSelection()." + getter + ";\n");
					b.append(new EclipseFileHandlingGenerator(this, action, i18n).createDownloadFragment("id"));
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					String invocationParameter = null;

					if (exchangeMethod.getSingleObjectFilterParam() != null) {
						b.append("if(getSelection() == null)\n");
						b.append("return;\n\n");
						b.append("final " + typeName + " id = getSelection()." + getter + ";\n");

						invocationParameter = "id";
					}

					if (exchangeMethod.returnsPath())
						b.append(new EclipseFileHandlingGenerator(this, action, i18n).createDownloadFragment(invocationParameter));

					if (exchangeMethod.returnsContent())
						b.append(new EclipseFileHandlingGenerator(this, action, i18n).createDownloadFragmentForExport());

					if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath())
						b.append(new EclipseFileHandlingGenerator(this, action, i18n).createExportInvocationFragment());

				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					b.append(new EclipseFileHandlingGenerator(this, action, i18n).createUploadFragmentForImport(true));
			}
			else {
				final String targetFormName = action.getTargetForm().getName();
				final Form targetForm = action.getTargetForm();

				if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
					b.append("final var dlg = new " + targetFormName + "(parentShell);\n");
					b.append("final int returnCode = dlg.open();\n\n");
					b.append("if(returnCode == Dialog.CANCEL)\n");
					b.append("return;\n\n");
					b.append("refreshView();\n");
				}
				else {
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");

					final DomainAttribute pkAttribute = targetForm.getDomainObject().getPKAttribute();
					final String typeName = pkAttribute.getJavaType().getName();

					b.append("final " + typeName + " id = getSelection()." + form.getDTO().getPKAttribute().getGetterName() + ";\n\n");
					b.append("final var dlg = new " + targetFormName + "(parentShell, id);\n");

					if (targetForm.getFormType() != FormTypeEnumeration.READONLY) {
						b.append("final int returnCode = dlg.open();\n\n");
						b.append("if(returnCode == Dialog.CANCEL)\n");
						b.append("return;\n\n");
						b.append("refreshView();\n");
					}
					else
						b.append("dlg.open();\n");
				}
			}

			b.append("}\n");
			b.append("}\n\n");

			addSubClass(actionClassName, b.toString());
		}

		if (!form.getActions().isEmpty()) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Create the actions\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private void createActions()\n");
			b.append("{\n");

			for (final FormAction action : form.getActions()) {
				final String actionClassName = action.getName().substring(0, 1).toUpperCase() + action.getName().substring(1);

				b.append(action.getName() + " = new " + actionClassName + "();\n");
			}

			b.append("}\n\n");

			addMethod("void createActions()", b.toString());

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Initialize the toolbar\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void initializeToolBar()\n");
			b.append("{\n");

			for (final FormAction action : form.getActions()) {
				final var actionContent = "toolBarManager.add(" + action.getName() + ");\n\n";

				b.append(securityHelper.wrapSecurityCode(action.getRoles(), actionContent));
			}

			b.append("toolBarManager.update(true);\n");
			b.append("}\n\n");

			addMethod("void initializeToolBar()", b.toString());
		}

		var declaration = "void createPartControl(Composite parent, Shell parentShell)";

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			declaration = "void createPartControl(Composite parent, MPart part, Shell parentShell)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize view\n");
		b.append(" * @param parent\n");

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
			b.append(" * @param part\n");

		b.append(" * @param parentShell\n");
		b.append(" */\n");
		b.append("@PostConstruct\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + declaration + "\n");
		b.append("{\n");

		addDebugLog(b, "Initialize view");

		b.append("\n");
		b.append(new EclipseClientFieldHelper(form, this).initClientField());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b.append("Integer savedSearchId = null;\n\n");
			b.append("if(part.getTransientData().containsKey(SAVED_QUERY_ID_KEY))\n");
			b.append("savedSearchId = (Integer) part.getTransientData().get(SAVED_QUERY_ID_KEY);\n\n");
			b.append("super.init(parent, savedSearchId, parentShell);\n");
		}
		else
			b.append("super.init(parent, parentShell);\n");

		b.append("\n");

		if (!form.getActions().isEmpty()) {
			b.append("createActions();\n");
			b.append("initializeToolBar();\n\n");
		}

		if (readonlyAction != null || updateAction != null || deleteAction != null) {
			b.append("// Add key listener\n");
			b.append("super.getTableViewer().getTable().addKeyListener(new KeyAdapter()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void keyReleased(KeyEvent e)\n");
			b.append("{\n");

			if (updateAction != null) {
				final var fb = new StringBuilder();
				fb.append("if(e.character == SWT.CR)\n");

				if (readonlyAction != null)
					fb.append("{\n");

				fb.append(updateAction.getName() + ".run();\n");

				if (readonlyAction != null) {
					fb.append("return;\n");
					fb.append("}\n");
				}

				b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), fb.toString()));
			}

			if (readonlyAction != null) {
				if (updateAction != null)
					b.append("\n");

				final var fb = new StringBuilder();
				fb.append("if(e.character == SWT.CR)\n");
				fb.append(readonlyAction.getName() + ".run();\n");

				b.append(securityHelper.wrapSecurityCode(readonlyAction.getRoles(), fb.toString()));
			}

			b.append("}\n");
			b.append("});\n\n");
		}

		if (readonlyAction != null || updateAction != null) {
			b.append("// Add double-click listener\n");
			b.append("super.getTableViewer().addDoubleClickListener(event ->");

			if (securityHelper.isSecurityAdded()) {
				b.append("\n");
				b.append("{\n");

				if (updateAction != null)
					if (readonlyAction != null)
						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), updateAction.getName() + ".run();\nreturn;\n"));
					else
						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), updateAction.getName() + ".run();\n"));

				if (readonlyAction != null) {
					if (updateAction != null)
						b.append("\n");

					b.append(securityHelper.wrapSecurityCode(readonlyAction.getRoles(), readonlyAction.getName() + ".run();\n"));
				}

				b.append("});\n\n");
			}
			else
				b.append(" " + (updateAction != null ? updateAction.getName() : readonlyAction.getName()) + ".run());");
		}

		if (!form.getActions().isEmpty())
			b.append("final Menu popUpMenu = getTable().getMenu();\n\n");

		// Add all context menu items
		for (final FormAction action : form.getActions()) {
			final var fb = new StringBuilder();
			fb.append("final var " + action.getName() + "MenuItem = new MenuItem(popUpMenu, SWT.NONE);\n");
			fb.append(action.getName() + "MenuItem.setText(" + i18n.getI18N(action) + ");\n");

			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DELETE)
					fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(ImageCache.IMG_DELETE));\n");
				else if (action.getType() == ActionType.COPY)
					fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(ImageCache.IMG_COPY));\n");
				else if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT)
					fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"download.png\"));\n");
				else if (action.getType() == ActionType.UPLOAD_IMPORT)
					fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"upload.png\"));\n");
			}
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.CREATE)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"new_data.png\"));\n");
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.READONLY)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"view_data.png\"));\n");
			else if (action.getTargetForm().getFormType() == FormTypeEnumeration.UPDATE)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"edit_data.png\"));\n");

			fb.append("\n");
			fb.append(action.getName() + "MenuItem.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append(action.getName() + ".run();\n");
			fb.append("}\n");
			fb.append("});\n");

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), fb.toString()));
			b.append("\n");
		}

		addDebugLog(b, "View initialization finished");

		b.append("}\n\n");

		addMethod(declaration, b.toString());
	}

}
