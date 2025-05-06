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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.FORM_TITLE;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.SEL_OBJ_ID;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.shared.Constants.ACTION_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.UI_VIEW_FOLDER;

import java.util.HashSet;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.exchange.JSFExportGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.exchange.JSFImportGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.file.JSFDownloadGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for common parts of views and grid panels of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFCommonDataTableGenerator {
	private static final String SAVED_QUERY_MANAGER = "queryManager";
	private static final String SAVED_QUERY_NAME = "savedQueryName";
	private static final String SEL_SAVED_QUERY = "selectedSavedQuery";
	private static final String VIEW_ID = "VIEW_ID";
	private static final String PERFORM_COUNT_METHOD_NAME = "countRecords";
	private static final Pattern ACTION_PREFIX_PATTERN = Pattern.compile(ACTION_PREFIX);

	private final AbstractJavaSourceGenerator generator;
	private final DTOBean dto;
	private final FormPanel panel;
	private final Project project;
	private AbstractDomainAssociation assoc;
	private final String listName;
	private final String selItemName;
	private final String doubleClickListener;
	private final String managedBeanName;
	private final JSFI18NGenerator i18n;
	private final String fetchMethodName;
	private final String panelId;
	private final JSFSecurityGenerator securityHelper;
	private final EList<FormAction> actions;
	private Form form;
	private FormAction deleteAction;
	private FormAction downloadAction;
	private FormAction copyAction;
	private boolean readOnly;
	private boolean dataGrid;
	private String deleteConfirmWidgetVar = "deleteConfirm";
	private String copyConfirmWidgetVar = "copyConfirm";
	private boolean isSearchable;
	private boolean saveQueries;
	private String readOnlyCheck;

	/**
	 * Constructor for grid panels
	 * @param generator
	 * @param panel
	 * @param i18n
	 * @param securityHelper
	 */
	public JSFCommonDataTableGenerator(AbstractJavaSourceGenerator generator, FormPanel panel, JSFI18NGenerator i18n,
			JSFSecurityGenerator securityHelper) {
		this.generator = generator;
		this.panel = panel;
		this.dto = panel.getDTO();
		this.assoc = panel.getAssociation();
		this.listName = assoc.getName() + "List";
		this.selItemName = "selItemOf" + assoc.getUpperCaseName();
		this.doubleClickListener = "on" + assoc.getUpperCaseName() + "GridDoubleClick";
		this.managedBeanName = JSFGeneratorUtil.createManagedBeanName(panel.getName());
		this.i18n = i18n;
		this.fetchMethodName = "fetch" + assoc.getUpperCaseName();
		this.panelId = "grid" + panel.getName();
		this.securityHelper = securityHelper;
		this.dataGrid = true;
		this.actions = panel.getActions();
		this.deleteConfirmWidgetVar += assoc.getUpperCaseName();
		this.copyConfirmWidgetVar += assoc.getUpperCaseName();
		this.project = dto.getNamespace().getProject();
		this.readOnly = assoc instanceof ManyToManyAssociation
				|| assoc instanceof final OneToManyAssociation otm && !otm.isBidirectional();
		this.readOnlyCheck = !readOnly ? managedBeanName + ".readOnly" : null;

		// Search for delete, copy and download actions
		panel.getActions().forEach(action -> {
			if (!readOnly && action.getType() == ActionType.DELETE)
				this.deleteAction = action;

			if (!readOnly && action.getType() == ActionType.COPY)
				this.copyAction = action;

			if (action.getType() == ActionType.DOWNLOAD)
				this.downloadAction = action;
		});
	}

	/**
	 * Constructor for views
	 * @param generator
	 * @param form
	 * @param i18n
	 * @param securityHelper
	 */
	public JSFCommonDataTableGenerator(AbstractJavaSourceGenerator generator, Form form, JSFI18NGenerator i18n,
			JSFSecurityGenerator securityHelper) {
		this.generator = generator;
		this.form = form;
		this.dto = form.getDTO();
		this.listName = dto.getDomainObject().getNamePlural().substring(0, 1).toLowerCase()
				+ dto.getDomainObject().getNamePlural().substring(1) + "List";
		this.selItemName = "selectedObject";
		this.doubleClickListener = "onDoubleClick";
		this.managedBeanName = JSFGeneratorUtil.createManagedBeanName(form.getName());
		this.i18n = i18n;
		this.fetchMethodName = "fetch" + dto.getDomainObject().getNamePlural();
		this.panelId = "panData";
		this.securityHelper = securityHelper;
		this.actions = form.getActions();
		this.project = dto.getNamespace().getProject();
		this.panel = form.getViewFormPanel();

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			this.isSearchable = true;

			if (securityHelper.isSecurityAdded() && project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null)
				this.saveQueries = true;
		}

		form.getActions().forEach(action -> {
			if (action.getType() == ActionType.DELETE)
				this.deleteAction = action;
			else if (action.getType() == ActionType.COPY)
				this.copyAction = action;
		});
	}

	/**
	 * Add all necessary imports
	 */
	public void addImports() {
		generator.importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS);
		generator.importStatic(project.getClientNamespace().toString() + "." + USER_SESSION_BEAN_TYPE);
		generator.importPackage(project.getClientNamespace().toString());
		generator.importPackage(dto.getNamespace().toString());
		generator.importPackage("java.util");
		generator.importClass("jakarta.faces.application.FacesMessage");
		generator.importPackage("jakarta.inject");
		generator.importPackage("net.codecadenza.runtime.webclient.primefaces.util");
		generator.importPackage("java.io");

		if (project.isJakartaEEApplication()) {
			if (dataGrid || !isSearchable || saveQueries)
				generator.importPackage("jakarta.faces.view");
			else
				generator.importPackage("jakarta.enterprise.context");
		}
		else
			generator.importClass("org.springframework.web.context.annotation.SessionScope");

		if (isSearchable) {
			generator.importClass("org.primefaces.model.DualListModel");
			generator.importPackage("net.codecadenza.runtime.webclient.primefaces.search");

			// Add further imports for auto-complete search input fields
			for (final TableColumnField col : panel.getFormTable().getFields()) {
				final BoundaryMethod m = getAutoCompleteMethod(col);

				if (m == null)
					continue;

				final var listDTO = (DTOBean) m.getReturnType();

				if (project.isBoundaryMode())
					generator.importPackage(listDTO.getNamespace().toString());
				else
					generator.importPackage(listDTO.getDomainObject().getNamespace().toString());
			}

			if (saveQueries) {
				generator.importPackage(project.getRootNamespace().toString() + PACK_SERVICE);
				generator.importPackage("jakarta.faces.model");
			}
		}

		// Add imports for all enumerations
		for (final TableColumnField col : panel.getFormTable().getFields()) {
			if (!col.isSearchable() || col.getFieldType() != TableColumnFieldTypeEnumeration.ENUM)
				continue;

			final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

			generator.importPackage(javaEnum.getNamespace().toString());
		}

		if (!dataGrid) {
			// Add all necessary imports for download operations
			new JSFDownloadGenerator(generator, form).addImports();

			// Add imports for data import operations
			new JSFImportGenerator(generator, form).addImports();

			// Add imports for data export operations
			new JSFExportGenerator(generator, form).addImports();

			generator.importPackage("net.codecadenza.runtime.search.dto");
			generator.importPackage("java.text");

			for (final FormAction action : form.getActions())
				if (action.getTargetForm() != null) {
					generator.importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
					break;
				}
		}
		else {
			// Add all necessary imports for download operations
			new JSFDownloadGenerator(generator, panel).addImports();

			// Add imports for data import operations
			if (!readOnly)
				new JSFImportGenerator(generator, panel).addImports();

			// Add imports for data export operations
			new JSFExportGenerator(generator, panel).addImports();

			for (final FormAction action : panel.getActions())
				if (action.getTargetForm() != null) {
					generator.importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
					break;
				}
		}
	}

	/**
	 * Add all common fields
	 */
	public void addFields() {
		final var injectedServices = new HashSet<String>();

		generator.addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		generator.addPrivateField("List<" + dto.getName() + ">", listName).withDefaultValue("new ArrayList<>()").create();
		generator.addPrivateField(dto.getName(), selItemName).create();
		generator.addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();
		generator.addPrivateField("ResourceBundle", "bundle").withTransientModifier().create();

		if (dataGrid) {
			generator.addPrivateField(assoc.getDomainObject().getPKAttribute().getJavaType().getName(), SEL_OBJ_ID).create();
			generator.addPrivateField(JavaType.STRING, "currentPageURL").create();

			if (!readOnly)
				generator.addPrivateField(JavaType.BOOL, "readOnly").create();

			String typeName = panel.getBoundaryMethod().getBoundaryBean().getInterfaceName();

			if (!injectedServices.contains(typeName)) {
				new ServiceDeclarationGenerator(generator, panel.getBoundaryMethod().getBoundaryBean()).addField();

				injectedServices.add(typeName);
			}

			if (deleteAction != null) {
				typeName = deleteAction.getBoundaryMethod().getBoundaryBean().getInterfaceName();

				if (!injectedServices.contains(typeName)) {
					new ServiceDeclarationGenerator(generator, deleteAction.getBoundaryMethod().getBoundaryBean()).addField();

					injectedServices.add(typeName);
				}
			}

			if (copyAction != null) {
				typeName = copyAction.getBoundaryMethod().getBoundaryBean().getInterfaceName();

				if (!injectedServices.contains(typeName)) {
					new ServiceDeclarationGenerator(generator, copyAction.getBoundaryMethod().getBoundaryBean()).addField();

					injectedServices.add(typeName);
				}
			}

			if (downloadAction != null) {
				typeName = downloadAction.getBoundaryMethod().getBoundaryBean().getInterfaceName();

				if (!injectedServices.contains(typeName)) {
					new ServiceDeclarationGenerator(generator, downloadAction.getBoundaryMethod().getBoundaryBean()).addField();

					injectedServices.add(typeName);
				}
			}
		}
		else {
			final String typeName = form.getBoundaryMethod().getBoundaryBean().getInterfaceName();
			final var pageURL = "\"" + UI_VIEW_FOLDER + "/" + form.getName() + ".jsf?faces-redirect=true\"";

			if (!injectedServices.contains(typeName)) {
				new ServiceDeclarationGenerator(generator, form.getBoundaryMethod().getBoundaryBean()).addField();

				injectedServices.add(typeName);
			}

			generator.addPublicConstant(JavaType.STRING, "PAGE_URL", pageURL).create();
			generator.addPrivateField(JavaType.STRING, FORM_TITLE).withDefaultValue("\"\"").create();
		}

		// Add services for data exchange operations
		for (final FormAction action : actions) {
			if (action.getType() != ActionType.DOWNLOAD_EXPORT && action.getType() != ActionType.UPLOAD_IMPORT)
				continue;

			if (readOnly && action.getType() == ActionType.UPLOAD_IMPORT)
				continue;

			final String typeName;

			if (project.isBoundaryMode())
				typeName = action.getBoundaryMethod().getBoundaryBean().getInterfaceName();
			else {
				final var dataExchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
				typeName = dataExchangeMethod.getDataExchangeServiceBean().getName();
			}

			if (!injectedServices.contains(typeName)) {
				new ServiceDeclarationGenerator(generator, action.getBoundaryMethod(), null).addField();
				injectedServices.add(typeName);
			}
		}

		if (isSearchable) {
			generator.addPrivateField(JavaType.LONG, "countResult").create();

			final var methodSet = new HashSet<BoundaryMethod>();

			// Add all necessary declarations for auto-complete search input fields
			for (final TableColumnField col : panel.getFormTable().getFields()) {
				final BoundaryMethod m = getAutoCompleteMethod(col);

				if (m == null || methodSet.contains(m))
					continue;

				methodSet.add(m);

				final BoundaryBean listBoundary = m.getBoundaryBean();

				if (!injectedServices.contains(listBoundary.getInterfaceName())) {
					new ServiceDeclarationGenerator(generator, listBoundary).addField();

					injectedServices.add(listBoundary.getInterfaceName());
				}
			}

			if (saveQueries) {
				final var viewId = project.getClientNamespace().toString() + PACK_CLIENT_VIEW + "." + form.getName();

				generator.addPublicConstant(JavaType.STRING, VIEW_ID, "\"" + viewId + "\"").create();
				generator.addPrivateField(SAVED_QUERY_SERVICE, SAVED_QUERY_MANAGER).withTransientModifier().inject().create();
				generator.addPrivateField(JavaType.STRING, SAVED_QUERY_NAME).create();
				generator.addPrivateField(JavaType.STRING, SEL_SAVED_QUERY).create();
			}
		}
	}

	/**
	 * Determine the boundary method to be used for auto-complete functionality for the respective table column
	 * @param col
	 * @return the corresponding boundary method
	 */
	private BoundaryMethod getAutoCompleteMethod(TableColumnField col) {
		if (!col.isVisible() || !col.isSearchable())
			return null;

		if (!col.getDTOAttribute().getDomainAttribute().isWildcardFilteringSupported())
			return null;

		return JSFGeneratorUtil.getAutoCompleteMethod(col.getDTOAttribute().getDomainAttribute());
	}

	/**
	 * Add view methods
	 */
	private void addViewMethods() {
		var methodSignature = "String getCurrentPageURL()";
		var b = new StringBuilder();

		generator.addGetterAndSetter(JavaType.STRING, FORM_TITLE, "the form title");

		if (isSearchable) {
			generator.addGetter(JavaType.LONG, "countResult", "the result of the count operation");

			if (saveQueries) {
				generator.addGetterAndSetter(JavaType.STRING, SAVED_QUERY_NAME, "the name of the query");
				generator.addGetterAndSetter(JavaType.STRING, SEL_SAVED_QUERY, "the name of the selected saved query");
			}
		}

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		b.append("/**\n");
		b.append(" * @return the URL of the current page\n");
		b.append(" */\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return PAGE_URL;\n");
		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());

		methodSignature = "void refreshFormatSettings()";

		if (isSearchable) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Refresh format settings from user session\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("searchObj.setDateFormat(" + USER_SESSION_BEAN + ".getDateFormat());\n");
			b.append("searchObj.setDateTimeFormat(" + USER_SESSION_BEAN + ".getDateTimeFormat());\n");
			b.append("searchObj.setNumberFormat(" + USER_SESSION_BEAN + ".getNumberFormat());\n");
			b.append("searchObj.setDecimalSeparator(DecimalFormatSymbols.getInstance(");
			b.append(USER_SESSION_BEAN + ".getLocale()).getDecimalSeparator());\n");
			b.append("searchObj.setGroupingSeparator(DecimalFormatSymbols.getInstance(");
			b.append(USER_SESSION_BEAN + ".getLocale()).getGroupingSeparator());\n");
			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());
		}

		methodSignature = "void initSearchObject()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize search object\n");
		b.append(" */\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (isSearchable)
			b.append("searchObj = new SearchDTO();\n");
		else
			b.append("final var searchObj = new SearchDTO();\n");

		b.append("int colOrderId = -1;\n\n");
		b.append("// Initialize search object\n");
		b.append("searchObj.setMaxResult(1000);\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(false);\n");
		b.append("searchObj.setCount(false);\n");

		if (!isSearchable) {
			b.append("searchObj.setDateFormat(" + USER_SESSION_BEAN + ".getDateFormat());\n");
			b.append("searchObj.setDateTimeFormat(" + USER_SESSION_BEAN + ".getDateTimeFormat());\n");
			b.append("searchObj.setNumberFormat(" + USER_SESSION_BEAN + ".getNumberFormat());\n");
			b.append("searchObj.setDecimalSeparator(DecimalFormatSymbols.getInstance(");
			b.append(USER_SESSION_BEAN + ".getLocale()).getDecimalSeparator());\n");
			b.append("searchObj.setGroupingSeparator(DecimalFormatSymbols.getInstance(");
			b.append(USER_SESSION_BEAN + ".getLocale()).getGroupingSeparator());\n\n");
		}
		else
			b.append("\nrefreshFormatSettings();\n\n");

		int colCount = 0;
		boolean limitVisibleCols = false;

		// Add all search input fields
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			colCount++;

			if (colCount > 10)
				limitVisibleCols = true;

			if (isSearchable) {
				if (limitVisibleCols) {
					b.append("\n");
					b.append("final var f" + colCount + " = ");
				}

				b.append("new JSFSearchFieldDTO(searchObj, ++colOrderId, ");
			}
			else {
				b.append("\n");
				b.append("final var field" + col.getColIndex() + " = new SearchFieldDTO(++colOrderId, ");
			}

			b.append(col.getDTOAttribute().getSelectTokenConstant());
			b.append(", ");
			b.append(i18n.getI18NBundleFragment(col, form.getName()));
			b.append(", SearchFieldDataTypeEnum.");
			b.append(col.getFieldType().name());
			b.append(", ");
			b.append(col.getWidth());

			if (isSearchable && col.hasDateFormat())
				b.append(", false");

			b.append(");\n");

			if (!isSearchable)
				b.append("searchObj.getSearchFields().add(field" + col.getColIndex() + ");\n\n");
			else if (limitVisibleCols)
				b.append("f" + colCount + ".setVisible(false);\n\n");
		}

		b.append("\n");

		if (!isSearchable) {
			generator.addDebugLog(b, "Perform data fetch operation");

			b.append("\n");
			b.append("try\n");
			b.append("{\n");
			b.append(listName + " = ");

			new ServiceInvocationGenerator(form.getBoundaryMethod(), b).addInvocation("searchObj");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while fetching data!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_FETCH_FAIL, e);\n");
			b.append("}\n");
		}
		else {
			b.append("visibleFields = new DualListModel<>();\n");
			b.append("visibleFields.setSource(new ArrayList<>());\n");

			if (!limitVisibleCols)
				b.append("visibleFields.setTarget(searchObj.getSearchFields());\n");
			else {
				b.append("visibleFields.setTarget(new ArrayList<>());\n\n");
				b.append("for(final SearchFieldDTO d : searchObj.getSearchFields())\n");
				b.append("if(!d.isVisible())\n");
				b.append("visibleFields.getSource().add(d);\n");
				b.append("else\n");
				b.append("visibleFields.getTarget().add(d);\n");
			}
		}

		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());

		if (isSearchable)
			methodSignature = "void initView()";
		else
			methodSignature = "void " + fetchMethodName + "()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize view\n");
		b.append(" */\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		generator.addDebugLog(b, "Initialize view");

		b.append("\n");
		b.append("bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, " + USER_SESSION_BEAN + ".getLocale());\n");
		b.append(securityHelper.addSecurityCheck(form.getRoles()));
		b.append("\n");
		b.append(FORM_TITLE + " = " + i18n.getI18NBundleFragment(form) + ";\n\n");

		if (saveQueries) {
			b.append("// Check if previous search exists!\n");
			b.append("final SearchDTO lastSearch = " + SAVED_QUERY_MANAGER + ".getLastQuery(" + USER_SESSION_BEAN + ".getPrincipal().");
			b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName() + ", " + VIEW_ID + ");\n\n");
			b.append("if(lastSearch != null)\n");
			b.append("{\n");
			b.append("searchObj = lastSearch;\n\n");
			b.append("prepareAfterLoad();\n");
			b.append("}\n");
			b.append("else\n");
		}
		else if (isSearchable)
			b.append("if(searchObj == null)\n");

		b.append("initSearchObject();\n\n");

		if (isSearchable)
			b.append(fetchMethodName + "();\n\n");

		generator.addDebugLog(b, "View initialization finished");

		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());

		if (isSearchable) {
			methodSignature = "void " + fetchMethodName + "()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform data fetch operation\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			generator.addDebugLog(b, "Perform data fetch operation");

			b.append("\n");
			b.append("try\n");
			b.append("{\n");
			b.append("preSearch();\n");
			b.append("}\n");
			b.append("catch (final SearchInputFieldValidationException e)\n");
			b.append("{\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, ");
			b.append("SEARCH_INPUT_VALIDATION, \"\", e.getSearchFieldName());\n");
			b.append("return;\n");
			b.append("}\n\n");
			b.append("refreshFormatSettings();\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append(listName + " = ");

			new ServiceInvocationGenerator(form.getBoundaryMethod(), b).addInvocation("searchObj");

			b.append("\n");
			b.append("if(searchObj.isCount())\n");
			b.append("countResult = ");

			for (final FormPanel formPanel : form.getFormPanels())
				new ServiceInvocationGenerator(formPanel.getBoundaryMethod(), b).addInvocation("searchObj");

			if (saveQueries) {
				b.append("\n");
				b.append(SAVED_QUERY_MANAGER + ".saveQuery(" + USER_SESSION_BEAN + ".getPrincipal().");
				b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
				b.append(", " + VIEW_ID + ", null, searchObj);\n");
			}

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while fetching data!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_FETCH_FAIL, e);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("postSearch();\n");
			b.append("}\n");
			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());

			methodSignature = "void " + PERFORM_COUNT_METHOD_NAME + "()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform count operation\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			generator.addDebugLog(b, "Perform count operation");

			b.append("\n");
			b.append("try\n");
			b.append("{\n");
			b.append("preSearch();\n");
			b.append("}\n");
			b.append("catch (final SearchInputFieldValidationException e)\n");
			b.append("{\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, ");
			b.append("SEARCH_INPUT_VALIDATION, \"\", e.getSearchFieldName());\n");
			b.append("return;\n");
			b.append("}\n\n");
			b.append("refreshFormatSettings();\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("countResult = ");

			for (final FormPanel formPanel : form.getFormPanels())
				new ServiceInvocationGenerator(formPanel.getBoundaryMethod(), b).addInvocation("searchObj");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_COUNT_RESULT, \"\", countResult);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while performing count operation!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_COUNT_FAIL, e);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("postSearch();\n");
			b.append("}\n");
			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());

			final var methodSet = new HashSet<BoundaryMethod>();

			// Add auto-complete callback methods for all respective fields
			for (final TableColumnField col : cols) {
				final BoundaryMethod m = getAutoCompleteMethod(col);

				if (m == null || methodSet.contains(m))
					continue;

				methodSet.add(m);

				final DomainAttribute attr = col.getDTOAttribute().getDomainAttribute();
				final var completeMethodName = "onComplete" + attr.getDomainObject().getName() + attr.getUpperCaseName();
				final var listDTO = (DTOBean) m.getReturnType();
				DTOBeanAttribute dtoAttribute = listDTO.getDisplayAttribute();
				methodSignature = "List<String> " + completeMethodName + "(String query)";

				if (dtoAttribute == null)
					dtoAttribute = listDTO.getPKAttribute();

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Callback method for auto-complete field\n");
				b.append(" * @param query the filter criterion inserted by the user\n");
				b.append(" * @return a list containing all proposals\n");
				b.append(" */\n");
				b.append(generator.getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("final var results = new ArrayList<String>();\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final Collection<" + listDTO.getModelClassName() + "> items = ");

				new ServiceInvocationGenerator(m, listDTO, b).addInvocation("query + \"%\"");

				b.append("\n");
				b.append("for(final " + listDTO.getModelClassName() + " item : items)\n");
				b.append("results.add(");
				b.append(dtoAttribute.getDomainAttribute().convertToString("item." + dtoAttribute.getModelGetterName()) + ");\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				generator.addErrorLog(b, "Error while searching for auto-complete items by using the entered text '{}'!", "e", "query");

				b.append("}\n\n");
				b.append("return results;\n");
				b.append("}\n\n");

				generator.addMethod(methodSignature, b.toString());
			}

			if (saveQueries) {
				methodSignature = "void saveNewQuery()";

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Save new query\n");
				b.append(" */\n");
				b.append(generator.getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(" + SAVED_QUERY_NAME + ".isEmpty())\n");
				b.append("{\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, SAVED_QUERY_EMPTY_NAME);\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append("if(" + SAVED_QUERY_NAME + ".equals(" + SAVED_QUERY_SERVICE + ".LAST_QUERY_TITLE))\n");
				b.append("{\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, SAVED_QUERY_ILLEGAL_NAME);\n");
				b.append("return;\n");
				b.append("}\n\n");

				generator.addDebugLog(b, "Save new query");

				b.append("\n");
				b.append(SAVED_QUERY_MANAGER + ".saveQuery(" + USER_SESSION_BEAN + ".getPrincipal().");
				b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
				b.append(", " + VIEW_ID + ", " + SAVED_QUERY_NAME + ", searchObj);\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, SAVED_QUERY_NEW_SUCCESS, \"\", ");
				b.append(SAVED_QUERY_NAME + ");\n");
				b.append("}\n\n");

				generator.addMethod(methodSignature, b.toString());

				methodSignature = "SelectItem[] getSavedQueries()";

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * @return an array containing all saved queries\n");
				b.append(" */\n");
				b.append(generator.getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");

				generator.addDebugLog(b, "Load saved queries");

				b.append("\n");
				b.append("final Collection<String> savedQueries = " + SAVED_QUERY_MANAGER + ".getSavedQueries(");
				b.append(USER_SESSION_BEAN + ".getPrincipal().");
				b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName() + ", " + VIEW_ID + ");\n");
				b.append("final var items = new SelectItem[savedQueries.size()];\n");
				b.append("int i = 0;\n\n");
				b.append("for(final String item : savedQueries)\n");
				b.append("items[i++] = new SelectItem(item, item);\n\n");
				b.append("return items;\n");
				b.append("}\n\n");

				generator.addMethod(methodSignature, b.toString());

				methodSignature = "void deleteSavedQuery()";

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Delete saved query\n");
				b.append(" */\n");
				b.append(generator.getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(" + SEL_SAVED_QUERY + " == null)\n");
				b.append("return;\n\n");

				generator.addDebugLog(b, "Delete saved query");

				b.append("\n");
				b.append(SAVED_QUERY_MANAGER + ".deleteSavedQuery(" + USER_SESSION_BEAN + ".getPrincipal().");
				b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
				b.append(", " + VIEW_ID + ", " + SEL_SAVED_QUERY + ");\n\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, SAVED_QUERY_DELETE_SUCCESS, \"\", ");
				b.append(SEL_SAVED_QUERY + ");\n");
				b.append(SEL_SAVED_QUERY + " = null;\n");
				b.append("}\n\n");

				generator.addMethod(methodSignature, b.toString());

				methodSignature = "void runSavedQuery()";

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Run selected saved query\n");
				b.append(" */\n");
				b.append(generator.getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(" + SEL_SAVED_QUERY + " == null)\n");
				b.append("return;\n\n");

				generator.addDebugLog(b, "Run saved query");

				b.append("\n");
				b.append("searchObj = " + SAVED_QUERY_MANAGER + ".getSavedQuery(" + USER_SESSION_BEAN + ".getPrincipal().");
				b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
				b.append(", " + VIEW_ID + ", " + SEL_SAVED_QUERY + ");\n\n");
				b.append("prepareAfterLoad();\n");
				b.append(fetchMethodName + "();\n");
				b.append("}\n\n");

				generator.addMethod(methodSignature, b.toString());
			}
		}
	}

	/**
	 * @param targetForm
	 * @param newSelItemName
	 * @return the generated content
	 */
	private String getNavigationTargetForForm(Form targetForm, String newSelItemName) {
		final var b = new StringBuilder();
		final FormTypeEnumeration targetFormType = targetForm.getFormType();
		JavaType pkType = targetForm.getDTO().getPKAttribute().getDomainAttribute().getJavaType();

		if (targetFormType != FormTypeEnumeration.CREATE) {
			if (targetFormType == FormTypeEnumeration.ADD)
				for (final FormField field : targetForm.getAllFormFields())
					if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM) {
						pkType = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute().getDomainAttribute().getJavaType();
						break;
					}

			if (pkType.isString()) {
				b.append("url = " + targetForm.getName() + ".PAGE_INIT_URL + java.net.URLEncoder.encode(");

				if (newSelItemName == null || newSelItemName.isEmpty()) {
					if (targetFormType == FormTypeEnumeration.ADD)
						b.append(SEL_OBJ_ID);
					else
						b.append(selItemName + "." + dto.getPKAttribute().getGetterName());
				}
				else
					b.append(newSelItemName);

				b.append(", java.nio.charset.StandardCharsets.UTF_8);\n");
			}
			else {
				b.append("url = " + targetForm.getName() + ".PAGE_INIT_URL + ");

				if (newSelItemName == null || newSelItemName.isEmpty()) {
					if (targetFormType == FormTypeEnumeration.ADD)
						b.append(SEL_OBJ_ID);
					else
						b.append(selItemName + "." + dto.getPKAttribute().getGetterName());
				}
				else
					b.append(newSelItemName);

				b.append(";\n");
			}
		}
		else
			b.append("url = " + targetForm.getName() + ".PAGE_INIT_URL;\n");

		return securityHelper.wrapSecurityCode(targetForm.getRoles(), b.toString());
	}

	/**
	 * Create the method for double-click events
	 */
	private void addDoubleClickMethod() {
		final var b = new StringBuilder();
		FormAction defaultReadOnlyAction = null;
		FormAction defaultUpdateAction = null;

		b.append("/**\n");
		b.append(" * Event that will be fired if user performs a double-click on a grid row\n");
		b.append(" */\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public void " + doubleClickListener + "()\n");
		b.append("{\n");

		if (dataGrid) {
			if (!readOnly)
				defaultUpdateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityAdded());

			defaultReadOnlyAction = ActionHelper.getDefaultReadOnlyAction(panel, securityHelper.isSecurityAdded());
		}
		else {
			defaultUpdateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityAdded());
			defaultReadOnlyAction = ActionHelper.getDefaultReadOnlyAction(form, securityHelper.isSecurityAdded());
		}

		if (defaultUpdateAction != null && defaultReadOnlyAction != null) {
			if (securityHelper.isSecurityAdded()) {
				// If both actions share the same roles it doesn't make sense to use the read-only action!
				if (defaultReadOnlyAction.getRoles().containsAll(defaultUpdateAction.getRoles())
						&& defaultUpdateAction.getRoles().containsAll(defaultReadOnlyAction.getRoles()))
					defaultReadOnlyAction = null;
			}
			else
				defaultReadOnlyAction = null;
		}

		if (defaultUpdateAction != null || defaultReadOnlyAction != null) {
			generator.addDebugLog(b, "Handle double-click event");

			b.append("\n");
		}
		else
			b.append("// No appropriate form found!\n");

		if (defaultUpdateAction != null) {
			if (dataGrid)
				b.append("if(!readOnly)\n");

			if (defaultReadOnlyAction != null)
				b.append("if(\n");

			b.append(USER_SESSION_BEAN + ".redirectTo(getCurrentPageURL(), open");
			b.append(defaultUpdateAction.getTargetForm().getName() + "())");

			if (defaultReadOnlyAction != null) {
				b.append(")\n");
				b.append("return;\n\n");
			}
			else
				b.append(";\n");
		}

		if (defaultReadOnlyAction != null) {
			b.append(USER_SESSION_BEAN + ".redirectTo(getCurrentPageURL(), open");
			b.append(defaultReadOnlyAction.getTargetForm().getName() + "());\n");
		}

		b.append("}\n\n");

		generator.addMethod("void " + doubleClickListener + "()", b.toString());
	}

	/**
	 * Add methods for this component or view
	 */
	public void addMethods() {
		StringBuilder b;

		generator.addGetter("Collection<" + dto.getName() + ">", listName, "the list of elements");
		generator.addGetterAndSetter(dto.getName(), selItemName, "the selected item");

		if (dataGrid) {
			final var commentCurrentPageURL = "the URL of the page this grid panel is included in";
			final var commentSelectedObjectId = "the ID of the selected parent object";
			final var commentReadOnly = "true if the panel is in read-only mode";

			generator.addGetterAndSetter(JavaType.STRING, "currentPageURL", commentCurrentPageURL);
			generator.addGetterAndSetter(assoc.getDomainObject().getPKAttribute().getJavaType().getName(), SEL_OBJ_ID,
					commentSelectedObjectId);

			if (!readOnly)
				generator.addGetterAndSetter(JavaType.BOOL, "readOnly", commentReadOnly);
		}

		// Add translation methods for enumeration fields
		for (final TableColumnField col : panel.getFormTable().getFields()) {
			if (!col.isSearchable() || col.getFieldType() != TableColumnFieldTypeEnumeration.ENUM)
				continue;

			final var transMethodName = "translate" + col.getDTOAttribute().getUpperCaseName();
			final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

			// Generate translations for all literals
			javaEnum.getEnumerationValues().forEach(i18n::getI18N);

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform translation of given enumeration literal\n");
			b.append(" * @param item\n");
			b.append(" * @return the translation based on the user's locale\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public String " + transMethodName + "(" + javaEnum.getName() + " item)\n");
			b.append("{\n");
			b.append("return bundle.getString(\"" + javaEnum.getName().toLowerCase() + "_\" + item.name().toLowerCase());\n");
			b.append("}\n\n");

			generator.addMethod("String " + transMethodName + "(" + javaEnum.getName() + " item)", b.toString());
		}

		// Add a double-click listener method
		addDoubleClickMethod();

		if (deleteAction != null) {
			final var param = selItemName + "." + dto.getPKAttribute().getGetterName();

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Delete selected element\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public void " + deleteAction.getBoundaryMethod().getName() + "()\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");

			generator.addDebugLog(b, "Delete selected object with id '{}'", param);

			b.append("\n");

			new ServiceInvocationGenerator(deleteAction.getBoundaryMethod(), b).addInvocation(param);

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while deleting selected object!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_DELETE_FAIL, e);\n");
			b.append("}\n\n");
			b.append(fetchMethodName + "();\n");
			b.append("}\n\n");

			generator.addMethod("void " + deleteAction.getBoundaryMethod().getName() + "()", b.toString());
		}

		if (copyAction != null) {
			final String pkTypeName = copyAction.getBoundaryMethod().getBoundaryBean().getDomainObject().getPKAttribute().getJavaType()
					.getName();
			final var idParam = selItemName + "." + dto.getPKAttribute().getGetterName();
			var userIdParam = "";
			FormAction defaultUpdateAction = null;

			if (dataGrid)
				defaultUpdateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityAdded());
			else
				defaultUpdateAction = ActionHelper.getDefaultUpdateAction(form, securityHelper.isSecurityAdded());

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Create copy of selected element\n");
			b.append(" * @return the navigation target\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public String " + copyAction.getBoundaryMethod().getName() + "()\n");
			b.append("{\n");

			if (defaultUpdateAction != null) {
				b.append("var url = \"\";\n");
				b.append(pkTypeName + " newId;\n\n");
			}

			b.append("try\n");
			b.append("{\n");

			generator.addDebugLog(b, "Create a copy of the selected object with id '{}'", idParam);

			b.append("\n");

			if (defaultUpdateAction != null)
				b.append("newId = ");

			if (securityHelper.isSecurityAdded())
				userIdParam = USER_SESSION_BEAN + ".getPrincipal()." + project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

			new ServiceInvocationGenerator(copyAction.getBoundaryMethod(), b).addInvocation(idParam, userIdParam);

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while creating a copy of the selected object!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_COPY_FAIL, e);\n");
			b.append("return \"\";\n");
			b.append("}\n\n");

			if (defaultUpdateAction == null) {
				b.append(fetchMethodName + "();\n");
				b.append("return \"\";\n");
			}
			else {
				b.append(getNavigationTargetForForm(defaultUpdateAction.getTargetForm(), "newId"));
				b.append("\n");
				b.append(USER_SESSION_BEAN + ".setLastPage(getCurrentPageURL());\n");
				b.append("return url;\n");
			}

			b.append("}\n\n");

			generator.addMethod("String " + copyAction.getBoundaryMethod().getName() + "()", b.toString());
		}

		// Add navigation methods for this view form
		final var methodSet = new HashSet<String>();

		for (final FormAction action : actions) {
			if (action.getBoundaryMethod() != null || action.getTargetForm() == null)
				continue;

			final Form targetForm = action.getTargetForm();
			final var actionMethod = "open" + targetForm.getName();
			final var signature = "String " + actionMethod + "()";
			boolean addMethod = true;

			// Avoid duplicate method declarations!
			if (methodSet.contains(signature))
				continue;

			if (readOnly && targetForm.getFormType() != FormTypeEnumeration.READONLY)
				continue;

			// Test if a method for opening a dialog of type 'ADD' is allowed to be added!
			if (targetForm.getFormType() == FormTypeEnumeration.ADD)
				addMethod = addMethodOfTypeAdd(targetForm);

			if (addMethod) {
				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Open dialog\n");
				b.append(" * @return the navigation target\n");
				b.append(" */\n");
				b.append(generator.getAnnotationForGeneratedElement());
				b.append("public " + signature + "\n");
				b.append("{\n");
				b.append("var url = \"\";\n\n");
				b.append(getNavigationTargetForForm(targetForm, null));
				b.append("\nreturn url;\n");
				b.append("}\n\n");

				methodSet.add(signature);

				generator.addMethod(signature, b.toString());
			}
		}

		if (dataGrid) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Initialize view\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public void initView()\n");
			b.append("{\n");

			generator.addDebugLog(b, "Initialize grid panel");

			b.append("\n");
			b.append("bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, " + USER_SESSION_BEAN + ".getLocale());\n\n");
			b.append(fetchMethodName + "();\n\n");

			generator.addDebugLog(b, "Grid panel initialization finished");

			b.append("}\n\n");

			generator.addMethod("void initView()", b.toString());

			// Create the data fetch method
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Fetch data for grid panel\n");
			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public void " + fetchMethodName + "()\n");
			b.append("{\n");
			b.append("// Get data from server\n");
			b.append("try\n");
			b.append("{\n");
			b.append(listName + " = new ArrayList<>(");

			new ServiceInvocationGenerator(panel.getBoundaryMethod(), b).addInvocation(true, SEL_OBJ_ID);

			b.append(");\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while fetching grid panel data for object with id '{}'!", "e", SEL_OBJ_ID);

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, DIALOG_INIT_FAIL, e);\n");
			b.append("}\n");
			b.append("}\n\n");

			generator.addMethod("void " + fetchMethodName + "()", b.toString());

			// Add all methods that are necessary for download operations
			new JSFDownloadGenerator(generator, panel).addDownloadMethods(selItemName + "." + dto.getPKAttribute().getGetterName());

			// Add additional methods for all data exchange operations
			if (!readOnly)
				new JSFImportGenerator(generator, panel).addDataImportMethods();

			new JSFExportGenerator(generator, panel).addDataExportMethods(selItemName + "." + dto.getPKAttribute().getGetterName());
		}
		else {
			addViewMethods();

			// Add all methods that are necessary for download operations
			new JSFDownloadGenerator(generator, form).addDownloadMethods(selItemName + "." + dto.getPKAttribute().getGetterName());

			// Add additional methods for all data exchange operations
			new JSFExportGenerator(generator, form).addDataExportMethods(selItemName + "." + dto.getPKAttribute().getGetterName());
			new JSFImportGenerator(generator, form).addDataImportMethods();
		}
	}

	/**
	 * @param targetForm
	 * @return true if a form of type 'ADD' can be added
	 */
	private boolean addMethodOfTypeAdd(Form targetForm) {
		// Form actions of type 'ADD' are basically not supported for views as they cannot provide a parent object ID!
		if (!dataGrid)
			return false;

		for (final FormField field : targetForm.getAllFormFields())
			if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
					&& field.getDTOAttribute().getReferencedDTOBean().getDomainObject().equals(assoc.getDomainObject()))
				return true;

		return false;
	}

	/**
	 * @return the generated XHTML fragment for this grid panel
	 */
	public String getXHTMLFragment() {
		final var b = new StringBuilder();
		var varName = "";

		if (dataGrid)
			varName = panel.getName().substring(0, 1).toLowerCase() + panel.getName().substring(1) + "Item";
		else {
			varName = form.getLowerCaseName() + "Item";

			b.append("\t<p:growl id=\"growl\" showDetail=\"true\"/>\n\n");
		}

		if (isSearchable)
			b.append(addInputDialog());

		b.append(addMenuBar());
		b.append("\t<p:dataTable id=\"" + panelId + "\" var=\"" + varName + "\" rows=\"50\"\n");
		b.append("\t\tselectionMode=\"single\"\n");
		b.append("\t\tselection=\"#{" + managedBeanName + "." + selItemName + "}\"\n");
		b.append("\t\tloadingMessage=\"#{" + EL_I18N_VAR + ".action_fetch_data}\"\n");
		b.append("\t\tvalue=\"#{" + managedBeanName + "." + listName + "}\" scrollable=\"false\"\n");
		b.append("\t\tpaginatorPosition=\"top\" paginatorAlwaysVisible=\"false\" paginator=\"true\" resizableColumns=\"true\"\n");
		b.append("\t\trowKey=\"#{" + varName + "." + dto.getPKAttribute().getName() + "}\"\n");
		b.append("\t\tpaginatorTemplate=\"{CurrentPageReport} {FirstPageLink} {PreviousPageLink} ");
		b.append("{PageLinks} {NextPageLink} {LastPageLink} {RowsPerPageDropdown}\"\n");
		b.append("\t\trowsPerPageTemplate=\"10,50,100,500,1000\">\n");
		b.append("\t\t<p:ajax event=\"rowDblselect\" listener=\"#{" + managedBeanName + "." + doubleClickListener + "}\"/>\n");

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		// Add all visible table columns
		int colIndex = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			b.append("\n\t\t<p:column sortBy=\"#{" + varName + ".");
			b.append(col.getDTOAttribute().getName() + "}\" width=\"" + col.getWidth() + "\" ");

			if (isSearchable)
				b.append("rendered=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].visible}\" ");

			b.append(">\n");
			b.append("\t\t\t<f:facet name=\"header\">\n");
			b.append("\t\t\t\t<h:outputText value=\"" + i18n.getI18N(col) + "\"/>\n");
			b.append("\t\t\t</f:facet>\n");

			final JavaType type = col.getDTOAttribute().getDomainAttribute().getJavaType();

			if (type.isBoolean()) {
				b.append("\t\t\t<p:selectBooleanCheckbox value=\"#{");
				b.append(varName + "." + col.getDTOAttribute().getName());
				b.append("}\" disabled=\"true\"/>\n");
			}
			else if (type.isEnum()) {
				// The enumeration literal value must be translated by a respective backing bean method!
				final var transMethodName = "translate" + col.getDTOAttribute().getUpperCaseName();

				b.append("\t\t\t<h:outputText value=\"#{");
				b.append(managedBeanName + "." + transMethodName + "(");
				b.append(varName + "." + col.getDTOAttribute().getName() + ")}\"/>\n");
			}
			else {
				b.append("\t\t\t<h:outputText value=\"#{");
				b.append(varName + "." + col.getDTOAttribute().getName());

				if (col.getFieldType() == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR)
					b.append(".time");

				b.append("}\">\n");

				if (col.getFieldType() == TableColumnFieldTypeEnumeration.DATE
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR) {
					if (col.hasDateFormat())
						b.append("\t\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateFormat}\" ");
					else
						b.append("\t\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\" ");

					b.append("timeZone=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");
				}
				else if (col.getFieldType() == TableColumnFieldTypeEnumeration.DOUBLE
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.FLOAT
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.BIG_DECIMAL)
					b.append("\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE) {
					b.append("\t\t\t\t<f:converter converterId=\"");
					b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateConverter\"/>\n");
					b.append("\t\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateFormat}\"/>\n");
				}
				else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME) {
					b.append("\t\t\t\t<f:converter converterId=\"");
					b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateTimeConverter\"/>\n");
					b.append("\t\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\"/>\n");
				}

				if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME)
					b.append("\t\t\t\t<f:attribute name=\"timeZone\" value=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");

				b.append("\t\t\t</h:outputText>\n");
			}

			b.append("\t\t</p:column>\n");

			colIndex++;
		}

		b.append("\t</p:dataTable>\n");
		b.append("\t<p/>\n\n");
		b.append("\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{" + EL_I18N_VAR);
		b.append(".result_total_number_records} #{" + managedBeanName + "." + listName + ".size()}\"/>\n");

		if (!dataGrid) {
			if (isSearchable) {
				b.append("\t<h:outputText styleClass=\"label-field-mandatory\" rendered=\"#{");
				b.append(managedBeanName + ".searchObject.count}\" value=\"&#160;#{");
				b.append(EL_I18N_VAR + ".search_input_count_of}&#160;\"/>\n");
				b.append("\t<h:outputText styleClass=\"label-field-mandatory\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.count}\" value=\"#{" + managedBeanName + ".countResult}\"/>\n");
			}

			b.append("\t<p/>\n");
		}

		b.append("\n");
		b.append(addContextMenu());

		if (!readOnly && deleteAction != null) {
			final var deleteEL = managedBeanName + "." + deleteAction.getBoundaryMethod().getName();
			final var cmdYesId = panelId + "_cmdYesDelete";

			b.append("\t<p:confirmDialog modal=\"true\" message=\"#{" + EL_I18N_VAR + ".dialog_delete_question}\" header=\"#{");
			b.append(EL_I18N_VAR + ".dialog_delete_title}\" severity=\"alert\" widgetVar=\"" + deleteConfirmWidgetVar + "\">\n");
			b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_yes}\" oncomplete=\"PF('" + deleteConfirmWidgetVar);
			b.append("').hide()\" actionListener=\"#{");
			b.append(deleteEL + "}\" id=\"" + cmdYesId + "\" ajax=\"false\"/>\n");
			b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_no}\" onclick=\"PF('" + deleteConfirmWidgetVar);
			b.append("').hide()\" type=\"button\"/>\n");
			b.append("\t</p:confirmDialog>\n\n");
		}

		if (!readOnly && copyAction != null) {
			final var copyEL = managedBeanName + "." + copyAction.getBoundaryMethod().getName();
			final var cmdYesId = panelId + "_cmdYesCopy";

			b.append("\t<p:confirmDialog modal=\"true\" message=\"#{" + EL_I18N_VAR);
			b.append(".dialog_copy_question}\" header=\"#{" + EL_I18N_VAR);
			b.append(".dialog_copy_title}\" severity=\"info\" widgetVar=\"" + copyConfirmWidgetVar + "\">\n");
			b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_yes}\" oncomplete=\"PF('");
			b.append(copyConfirmWidgetVar + "').hide()\" ");
			b.append("action=\"#{" + copyEL + "}\" id=\"" + cmdYesId + "\" ajax=\"false\"/>\n");
			b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_no}\" onclick=\"PF('" + copyConfirmWidgetVar);
			b.append("').hide()\" type=\"button\"/>\n");
			b.append("\t</p:confirmDialog>\n\n");
		}

		if (saveQueries) {
			b.append("\t<p:dialog header=\"#{" + EL_I18N_VAR);
			b.append(".search_input_dlg_save_query}\" widgetVar=\"dlgSaveQuery\" modal=\"true\" height=\"100\">\n");
			b.append("\t\t<h:panelGrid columns=\"3\">\n");
			b.append("\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
			b.append(".search_input_lbl_save_query}\" styleClass=\"label-field-mandatory\"/>\n");
			b.append("\t\t\t<p:inputText id=\"txtQueryName\" value=\"#{" + managedBeanName + ".savedQueryName}\">\n");
			b.append("\t\t\t</p:inputText>\n");
			b.append("\t\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_save}\" action=\"#{" + managedBeanName);
			b.append(".saveNewQuery}\" ajax=\"false\" onclick=\"PF('dlgSaveQuery').hide()\"/>\n");
			b.append("\t\t</h:panelGrid>\n");
			b.append("\t</p:dialog>\n\n");
		}

		if (dataGrid) {
			if (!readOnly)
				b.append(new JSFImportGenerator(generator, panel).createDialogFragment());
		}
		else
			b.append(new JSFImportGenerator(generator, form).createDialogFragment());

		if (isSearchable)
			b.append("\t<p:defaultCommand target=\":form:cmdSearch\"/>\n\n");

		return b.toString();
	}

	/**
	 * Create the search input dialog
	 * @return the generated content
	 */
	private String addInputDialog() {
		final var b = new StringBuilder();
		b.append("\t<p:dialog height=\"510\" width=\"1000\" header=\"#{" + EL_I18N_VAR);
		b.append(".search_input_dlg}\" widgetVar=\"dlgSearchInput\" modal=\"true\">\n");
		b.append("\t\t<p:tabView id=\"tabViewSearchInput\" dynamic=\"false\">\n");
		b.append("\t\t\t<p:tab title=\"#{" + EL_I18N_VAR + ".search_input_filter}\">\n");
		b.append("\t\t\t<p:scrollPanel style=\"width:100%; height:400px;\" mode=\"native\">\n");
		b.append("\t\t\t<h:panelGrid columns=\"5\" cellpadding=\"3\">\n");
		b.append("\t\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_column}\"/>\n");
		b.append("\t\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_operator}\"/>\n");
		b.append("\t\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_sort}\"/>\n");
		b.append("\t\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_filter_input}\"/>\n");
		b.append("\t\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_and}\"/>\n\n");

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		int colIndex = 0;

		// Add all search input fields
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (!col.isSearchable()) {
				colIndex++;
				continue;
			}

			final String inputFieldId;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
				inputFieldId = "fc_" + colIndex;
			else
				inputFieldId = "fi_" + colIndex;

			b.append("\t\t\t\t<h:outputLabel styleClass=\"label-field-optional\" for=\"" + inputFieldId + "\" ");
			b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].colLabel}\"/>\n");
			b.append("\t\t\t\t<p:selectOneMenu valueChangeListener=\"#{");
			b.append(managedBeanName + ".onOperatorChanged}\" style=\"width:150px;\" ");
			b.append("styleClass=\"label-field-optional\" id=\"o_" + colIndex + "\" ");
			b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].operator}\" ");
			b.append("converter=\"net.codecadenza.runtime.webclient.primefaces.converter.SearchOperatorDTOConverter\">\n");
			b.append("\t\t\t\t\t<f:selectItems update=\"b_" + colIndex + "\" value=\"#{" + managedBeanName + ".");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.INTEGER
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.LONG
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.DOUBLE
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.FLOAT
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.BIG_DECIMAL)
				b.append("numberOperators");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN)
				b.append("boolOperators");
			else if (col.hasTemporalType())
				b.append("dateOperators");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.STRING
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.CHAR)
				b.append("textOperators");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
				b.append("enumOperators");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_BINARY)
				b.append("uuIDOperators");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_STRING)
				b.append("uuIDOperatorsWithLike");

			b.append("}\" ");
			b.append("var=\"advInputItem" + colIndex + "\" itemValue=\"#{advInputItem" + colIndex);
			b.append("}\" itemLabel=\"#{advInputItem" + colIndex + ".description}\"/>\n");
			b.append("\t\t\t\t\t<f:ajax render=\"p_" + colIndex + "\"/>\n");
			b.append("\t\t\t\t</p:selectOneMenu>\n");
			b.append("\t\t\t\t<p:selectOneMenu styleClass=\"label-field-optional\" widgetVar=\"st_" + colIndex + "\" ");
			b.append("id=\"s_" + colIndex + "\" value=\"#{" + managedBeanName);
			b.append(".searchObject.searchFields[" + colIndex + "].sortOrder}\">\n");
			b.append("\t\t\t\t\t<f:selectItems value=\"#{" + managedBeanName + ".sortOrderList}\"/>\n");
			b.append("\t\t\t\t</p:selectOneMenu>\n");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.INTEGER
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.LONG) {
				b.append("\t\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].integerCriterion}\" size=\"30\"/>\n");
				b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].between}\" ");
				b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex);
				b.append("].integerBetweenCriterion}\" size=\"30\"/>\n");
				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.DOUBLE
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.FLOAT) {
				b.append("\t\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].doubleCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t\t</p:inputText>\n");
				b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].between}\" ");
				b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex);
				b.append("].doubleBetweenCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t\t</p:inputText>\n");
				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.BIG_DECIMAL) {
				b.append("\t\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].bigDecimalCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t\t</p:inputText>\n");
				b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].between}\" ");
				b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex);
				b.append("].bigDecimalBetweenCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t\t</p:inputText>\n");
				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				b.append("\t\t\t\t<p:selectOneMenu id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\">\n");
				b.append("\t\t\t\t\t<f:selectItem itemLabel=\"\" itemValue=\"\"/>\n");
				b.append("\t\t\t\t\t<f:selectItem itemLabel=\"true\" itemValue=\"true\"/>\n");
				b.append("\t\t\t\t\t<f:selectItem itemLabel=\"false\" itemValue=\"false\"/>\n");
				b.append("\t\t\t\t</p:selectOneMenu>\n");
				b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\"/>\n");
			}
			else if (col.hasTemporalType()) {
				final var dateFormat = col.hasDateFormat() ? "dateFormat" : "dateTimeFormat";

				b.append("\t\t\t\t<p:datePicker id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].dateCriterion}\" ");
				b.append("pattern=\"#{" + USER_SESSION_BEAN + "." + dateFormat);
				b.append("}\" size=\"20\" showButtonBar=\"true\" showIcon=\"true\"");

				if (!col.hasDateFormat())
					b.append(" showTime=\"true\"");

				b.append("/>\n");
				b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t\t<p:datePicker id=\"fb_" + colIndex + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].dateBetweenCriterion}\" ");
				b.append("pattern=\"#{" + USER_SESSION_BEAN + "." + dateFormat);
				b.append("}\" size=\"20\" showButtonBar=\"true\" showIcon=\"true\" ");
				b.append("rendered=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].between}\"");

				if (!col.hasDateFormat())
					b.append(" showTime=\"true\"");

				b.append("/>\n");
				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.STRING
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.CHAR
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_BINARY
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_STRING) {
				final BoundaryMethod m = getAutoCompleteMethod(col);

				if (m != null) {
					final DomainAttribute attr = col.getDTOAttribute().getDomainAttribute();
					final var completeMethodName = "onComplete" + attr.getDomainObject().getName() + attr.getUpperCaseName();

					b.append("\t\t\t\t<p:autoComplete minQueryLength=\"2\" maxResults=\"10\" completeMethod=\"#{" + managedBeanName + ".");
					b.append(completeMethodName + "}\" ");
					b.append("id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\" size=\"30\"/>\n");
					b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
					b.append("\t\t\t\t<p:autoComplete minQueryLength=\"2\" rendered=\"#{");
					b.append(managedBeanName + ".searchObject.searchFields[" + colIndex);
					b.append("].between}\" maxResults=\"10\" completeMethod=\"#{" + managedBeanName + "." + completeMethodName + "}\" ");
					b.append("id=\"b_" + colIndex + "\" value=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].stringBetweenCriterion}\" size=\"30\"/>\n");
				}
				else {
					b.append("\t\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\" size=\"30\"/>\n");
					b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
					b.append("\t\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].between}\" ");
					b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex);
					b.append("].stringBetweenCriterion}\" size=\"30\"/>\n");
				}

				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var enumType = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				b.append("\t\t\t\t<p:selectOneMenu id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\">\n");
				b.append("\t\t\t\t\t<f:selectItem itemLabel=\"\" itemValue=\"\"/>\n");

				enumType.getEnumerationValues().forEach(literal -> {
					b.append("\t\t\t\t\t<f:selectItem itemLabel=\"#{" + EL_I18N_VAR + ".");
					b.append(enumType.getName().toLowerCase() + "_" + literal.getName().toLowerCase());
					b.append("}\" itemValue=\"" + literal.getName() + "\"/>\n");
				});

				b.append("\t\t\t\t</p:selectOneMenu>\n");
				b.append("\t\t\t\t<h:panelGroup id=\"p_" + colIndex + "\"/>\n");
			}

			b.append("\n");
			colIndex++;
		}

		b.append("\t\t\t</h:panelGrid>\n");
		b.append("\t\t\t</p:scrollPanel>\n");
		b.append("\t\t\t</p:tab>\n");
		b.append("\t\t\t<p:tab title=\"#{" + EL_I18N_VAR + ".search_input_adv}\">\n");
		b.append("\t\t\t<h:panelGrid columns=\"2\">\n");
		b.append("\t\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_case}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t\t<p:selectBooleanCheckbox value=\"#{" + managedBeanName + ".searchObject.caseSensitive}\"/>\n");
		b.append("\t\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_count}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t\t<p:selectBooleanCheckbox value=\"#{" + managedBeanName + ".searchObject.count}\"/>\n");
		b.append("\t\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_filter_match}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t\t<p:selectBooleanCheckbox value=\"#{" + managedBeanName + ".searchObject.exactFilterMatch}\"/>\n");
		b.append("\t\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_filter_fetch_size}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t\t<p:inputText value=\"#{" + managedBeanName + ".searchObject.maxResult}\" size=\"5\">\n");
		b.append("\t\t\t\t\t<f:validateLongRange minimum=\"1\" maximum=\"1000000\"/>\n");
		b.append("\t\t\t\t</p:inputText>\n");
		b.append("\t\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_visible_fields_label}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t\t<p:pickList value=\"#{" + managedBeanName);
		b.append(".visibleFields}\" var=\"savedQueryListItem\" itemLabel=\"#{savedQueryListItem.colLabel}\" ");
		b.append("itemValue=\"#{savedQueryListItem}\" ");
		b.append("converter=\"net.codecadenza.runtime.webclient.primefaces.converter.SearchFieldDTOConverter\" ");
		b.append("addLabel=\"#{" + EL_I18N_VAR + ".pick_list_add}\" ");
		b.append("addAllLabel=\"#{" + EL_I18N_VAR + ".pick_list_add_all}\" ");
		b.append("removeLabel=\"#{" + EL_I18N_VAR + ".pick_list_remove}\" ");
		b.append("removeAllLabel=\"#{" + EL_I18N_VAR + ".pick_list_remove_all}\">\n");
		b.append("\t\t\t\t\t<f:facet name=\"sourceCaption\">#{" + EL_I18N_VAR + ".search_input_all_fields}</f:facet>\n");
		b.append("\t\t\t\t\t<f:facet name=\"targetCaption\">#{" + EL_I18N_VAR + ".search_input_visible_fields}</f:facet>\n");
		b.append("\t\t\t\t</p:pickList>\n");
		b.append("\t\t\t</h:panelGrid>\n");
		b.append("\t\t\t</p:tab>\n");

		if (saveQueries) {
			b.append("\t\t\t<p:tab title=\"#{" + EL_I18N_VAR + ".search_input_saved_query_title}\">\n");
			b.append("\t\t\t\t<h:panelGrid columns=\"1\">\n");
			b.append("\t\t\t\t\t<h:outputLabel styleClass=\"label-field-mandatory\" value=\"#{");
			b.append(EL_I18N_VAR + ".search_input_lbl_saved_query}\"/>\n");
			b.append("\t\t\t\t\t<p:selectOneListbox id=\"savedQueryList\" value=\"#{" + managedBeanName + ".");
			b.append(SEL_SAVED_QUERY + "}\" style=\"height:150px; width:300px\">\n");
			b.append("\t\t\t\t\t\t<f:selectItems value=\"#{" + managedBeanName + ".savedQueries}\"/>\n");
			b.append("\t\t\t\t\t</p:selectOneListbox>\n");
			b.append("\t\t\t\t\t<h:panelGrid columns=\"2\">\n");
			b.append("\t\t\t\t\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".search_input_run_saved_query}\" action=\"#{");
			b.append(managedBeanName + ".runSavedQuery}\" ajax=\"false\"/>\n");
			b.append("\t\t\t\t\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".search_input_delete_saved_query}\" action=\"#{");
			b.append(managedBeanName + ".deleteSavedQuery}\" ajax=\"false\"/>\n");
			b.append("\t\t\t\t\t</h:panelGrid>\n");
			b.append("\t\t\t\t</h:panelGrid>\n");
			b.append("\t\t\t</p:tab>\n");
		}

		b.append("\t\t</p:tabView>\n");
		b.append("\t\t<br/>\n");
		b.append("\t\t<h:panelGrid columns=\"3\">\n");
		b.append("\t\t\t<p:commandButton id=\"cmdSearch\" value=\"#{" + EL_I18N_VAR + ".search_input_perform_search}\" ");
		b.append("action=\"#{" + managedBeanName + "." + fetchMethodName + "}\" ajax=\"false\"/>\n");
		b.append("\t\t\t<p:commandButton id=\"cmdReset\" value=\"#{" + EL_I18N_VAR);
		b.append(".command_reset}\" action=\"#{" + managedBeanName);
		b.append(".initSearchObject}\" onclick=\"PF('dlgSearchInput').hide()\" ajax=\"false\"/>\n");
		b.append("\t\t\t<p:commandButton id=\"cmdCount\" value=\"#{" + EL_I18N_VAR + ".search_input_perform_count}\" ");
		b.append("actionListener=\"#{" + managedBeanName + "." + PERFORM_COUNT_METHOD_NAME);
		b.append("}\" onclick=\"PF('dlgSearchInput').hide()\" update=\"growl\" ajax=\"true\"/>\n");
		b.append("\t\t</h:panelGrid>\n");
		b.append("\t</p:dialog>\n\n");

		return b.toString();
	}

	/**
	 * Add the menu bar
	 * @return the generated content
	 */
	private String addMenuBar() {
		final var b = new StringBuilder();
		boolean createActionAdded = false;
		boolean addActionAdded = false;

		b.append("\t<p:menubar>\n");
		b.append("\t\t<p:submenu icon=\"pi pi-file\" label=\"#{" + EL_I18N_VAR);
		b.append(".mnu_file}\" id=\"" + panelId + "_mniFile\">\n");

		for (final FormAction action : actions) {
			if (action.getBoundaryMethod() != null || readOnly)
				continue;

			final Form targetForm = action.getTargetForm();
			final var actionMethodName = "open" + targetForm.getName();

			if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
				final var idAttribute = createActionAdded ? "" : "id=\"" + panelId + "_mniCreate\" ";

				// Add a menu item to open a form for creating a new object
				b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_create}\" ajax=\"true\" " + idAttribute);
				b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(targetForm.getRoles(), readOnlyCheck));
				b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
				b.append("\t\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
				b.append(managedBeanName + ".currentPageURL}\"/>\n");
				b.append("\t\t\t</p:menuitem>\n");

				createActionAdded = true;
			}
			else if (targetForm.getFormType() == FormTypeEnumeration.ADD && addMethodOfTypeAdd(targetForm)) {
				final var idAttribute = addActionAdded ? "" : "id=\"" + panelId + "_mniAdd\" ";

				// Add a menu item to open a form for adding a new object
				b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_add}\" ajax=\"true\" " + idAttribute);
				b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(targetForm.getRoles(), readOnlyCheck));
				b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
				b.append("\t\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
				b.append(managedBeanName + ".currentPageURL}\"/>\n");
				b.append("\t\t\t</p:menuitem>\n");

				addActionAdded = true;
			}
		}

		if (isSearchable) {
			// Add a menu item for opening the search input dialog
			b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_search_input}\" ajax=\"true\" ");
			b.append("icon=\"pi pi-search\" id=\"" + panelId + "_mniSearch\" ");
			b.append("onclick=\"PF('dlgSearchInput').show()\"/>\n");

			// Add a menu item for saving a query
			b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR);
			b.append(".search_input_dlg_save_query}\" icon=\"pi pi-save\" onclick=\"PF('dlgSaveQuery').show()\"/>\n");
		}

		b.append("\t\t</p:submenu>\n");

		// Add a menu item to refresh the data table
		b.append("\t\t<p:submenu icon=\"pi pi-refresh\" label=\"#{" + EL_I18N_VAR + ".mnu_view}\">\n");
		b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_refresh}\" ajax=\"false\" ");
		b.append("icon=\"pi pi-refresh\" ");
		b.append("actionListener=\"#{" + managedBeanName + "." + fetchMethodName + "}\"/>\n");
		b.append("\t\t</p:submenu>\n");

		// Add a menu item to export the content of the data table
		b.append("\t\t<p:submenu icon=\"pi pi-print\" label=\"#{" + EL_I18N_VAR + ".mnu_export}\">\n");
		b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_export_xlsx}\" ajax=\"false\" icon=\"pi pi-file-excel\">\n");
		b.append("\t\t\t\t<p:dataExporter type=\"xlsx\" target=\"" + panelId + "\" fileName=\"");
		b.append(dto.getDomainObject().getName() + "List\"/>\n");
		b.append("\t\t\t</p:menuitem>\n");
		b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_export_pdf}\" ajax=\"false\" icon=\"pi pi-file-pdf\">\n");
		b.append("\t\t\t\t<p:dataExporter type=\"pdf\" target=\"" + panelId + "\" fileName=\"");
		b.append(dto.getDomainObject().getName() + "List\"/>\n");
		b.append("\t\t\t</p:menuitem>\n");
		b.append("\t\t</p:submenu>\n");
		b.append("\t</p:menubar>\n\n");
		b.append("\t<br/>\n\n");

		return b.toString();
	}

	/**
	 * Add the context menu
	 * @return the generated content
	 */
	private String addContextMenu() {
		final var b = new StringBuilder();
		final var idPrefix = panelId + "_";
		final var menuItemIds = new HashSet<String>();

		b.append("\t<p:contextMenu for=\"" + panelId + "\">\n");

		actions.forEach(action -> {
			// When adding an arbitrary number of context menu items it must be checked if every generated item ID is unique! If a
			// duplicate is found the ID attribute won't be filled!
			if (action.getBoundaryMethod() != null) {
				if (!readOnly && action.getType() == ActionType.DELETE) {
					final var itemId = " id=\"" + idPrefix + "mniDelete\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to delete the selected object
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_delete}\" ajax=\"true\" ");
					b.append("icon=\"pi pi-trash\"" + idAttribute + securityHelper.addSecurityCode(action.getRoles(), readOnlyCheck));
					b.append("onclick=\"PF('" + deleteConfirmWidgetVar + "').show()\"/>\n");
				}
				else if (!readOnly && action.getType() == ActionType.COPY) {
					final var itemId = " id=\"" + idPrefix + "mniCopy\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to create a copy of the selected object
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_copy}\" ajax=\"true\" ");
					b.append("icon=\"pi pi-copy\"" + idAttribute + securityHelper.addSecurityCode(action.getRoles(), readOnlyCheck));
					b.append("onclick=\"PF('" + copyConfirmWidgetVar + "').show()\"/>\n");
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					final DomainAttribute downloadAttr = action.getBoundaryMethod().getDomainAttribute();
					final var itemId = " id=\"" + idPrefix + "mniDownload\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to download a file
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_download}\" ajax=\"false\" ");
					b.append("icon=\"pi pi-download\"" + idAttribute + ">\n");
					b.append("\t\t\t<p:fileDownload value=\"#{" + managedBeanName + ".");
					b.append(JavaBeanHelper.getPropertyName(downloadAttr.getName()) + "}\"/>\n");
					b.append("\t\t</p:menuitem>\n");
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					final String methodName = boundaryMethod.getName();
					final var itemId = " id=\"" + idPrefix + "mniExport\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to perform a data export operation
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_export}\" ajax=\"false\" ");
					b.append("icon=\"pi pi-download\"" + idAttribute + securityHelper.addSecurityCode(action.getRoles()));

					if (exchangeMethod.returnsContent() || exchangeMethod.returnsPath()) {
						String exportPropertyName = action.getName();

						if (action.getName().startsWith(ACTION_PREFIX))
							exportPropertyName = ACTION_PREFIX_PATTERN.matcher(exportPropertyName).replaceFirst("");

						b.append(">\n");
						b.append("\t\t\t<p:fileDownload value=\"#{" + managedBeanName + ".");
						b.append(JavaBeanHelper.getPropertyName(exportPropertyName) + "}\"/>\n");
					}
					else
						b.append("action=\"#{" + managedBeanName + "." + methodName + "}\">\n");

					b.append("\t\t</p:menuitem>\n");
				}
				else if (!readOnly && action.getType() == ActionType.UPLOAD_IMPORT) {
					final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
					final var itemId = " id=\"" + idPrefix + "mniImport\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to perform a data import operation
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_import}\" ajax=\"false\" ");
					b.append("icon=\"pi pi-upload\"" + idAttribute + securityHelper.addSecurityCode(action.getRoles(), readOnlyCheck));

					if (!boundaryMethod.getMethodParameters().isEmpty()) {
						String widgetVar = action.getName();

						if (action.getName().startsWith(ACTION_PREFIX))
							widgetVar = ACTION_PREFIX_PATTERN.matcher(widgetVar).replaceFirst("");

						widgetVar = widgetVar.substring(0, 1).toUpperCase() + widgetVar.substring(1);
						widgetVar = "dlg" + widgetVar;

						b.append("onclick=\"PF('" + widgetVar + "').show();return false\"/>\n");
					}
					else {
						final String methodName = boundaryMethod.getName();

						b.append("action=\"#{" + managedBeanName + "." + methodName + "}\"/>\n");
					}
				}
			}
			else {
				final Form targetForm = action.getTargetForm();
				final var actionMethodName = "open" + targetForm.getName();

				if (!readOnly && targetForm.getFormType() == FormTypeEnumeration.CREATE) {
					// Add a context menu item to open a dialog for creating a new object
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_create}\" ajax=\"true\" ");
					b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(targetForm.getRoles(), readOnlyCheck));
					b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
					b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
					b.append(managedBeanName + ".currentPageURL}\"/>\n");
					b.append("\t\t</p:menuitem>\n");
				}
				else if (!readOnly && targetForm.getFormType() == FormTypeEnumeration.ADD && addMethodOfTypeAdd(targetForm)) {
					// Add a context menu item to open a dialog for adding a new object
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_add}\" ajax=\"true\" ");
					b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(targetForm.getRoles(), readOnlyCheck));
					b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
					b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
					b.append(managedBeanName + ".currentPageURL}\"/>\n");
					b.append("\t\t</p:menuitem>\n");
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.READONLY) {
					final var itemId = " id=\"" + idPrefix + "mniView\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to open a form of type 'READONLY'
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_view}\" ajax=\"true\" ");
					b.append("icon=\"pi pi-file\"" + idAttribute + securityHelper.addSecurityCode(targetForm.getRoles()));
					b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
					b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
					b.append(managedBeanName + ".currentPageURL}\"/>\n");
					b.append("\t\t</p:menuitem>\n");
				}
				else if (!readOnly && targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
					final var itemId = " id=\"" + idPrefix + "mniUpdate\"";
					final var idAttribute = menuItemIds.contains(itemId) ? "" : itemId;

					menuItemIds.add(idAttribute);

					// Add a context menu item to open a form of type 'UPDATE'
					b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_edit}\" ajax=\"true\" ");
					b.append("icon=\"pi pi-pencil\"" + idAttribute);
					b.append(securityHelper.addSecurityCode(targetForm.getRoles(), readOnlyCheck));
					b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
					b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
					b.append(managedBeanName + ".currentPageURL}\"/>\n");
					b.append("\t\t</p:menuitem>\n");
				}
			}
		});

		// Add a context menu item to refresh the data table
		b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_refresh}\" ajax=\"false\" type=\"push\" ");
		b.append("icon=\"pi pi-refresh\" id=\"" + idPrefix + "mniRefresh\" ");
		b.append("actionListener=\"#{" + managedBeanName + "." + fetchMethodName + "}\"/>\n");
		b.append("\t</p:contextMenu>\n\n");

		return b.toString();
	}

	/**
	 * @return true if a view saves the queries in the database
	 */
	public boolean isSaveQueries() {
		return this.saveQueries;
	}

}
