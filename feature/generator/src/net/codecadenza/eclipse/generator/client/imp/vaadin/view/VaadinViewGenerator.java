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
package net.codecadenza.eclipse.generator.client.imp.vaadin.view;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.List;
import java.util.Optional;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
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
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for views of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinViewGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final Project project;
	private final DTOBean dto;
	private final VaadinI18NGenerator i18n;
	private final VaadinSecurityHelper securityHelper;
	private final VaadinCommonDataTableGenerator tableGenerator;
	private boolean importLov;
	private boolean saveQueries;
	private final FormPanel panel;
	private String logOnPKGetter;

	/**
	 * Constructor
	 * @param form
	 */
	public VaadinViewGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new VaadinI18NGenerator(project);
		this.securityHelper = new VaadinSecurityHelper(project);
		this.tableGenerator = new VaadinCommonDataTableGenerator(this, form, i18n);
		this.panel = form.getViewFormPanel();

		if (project.getApplicationLogOnDTO() != null)
			this.logOnPKGetter = project.getApplicationLogOnDTO().getPKAttribute().getModelGetterName();

		// Check if the form uses one list-of-values field at least
		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			if (securityHelper.isSecurityAdded() && project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null)
				this.saveQueries = true;

			for (final TableColumnField col : panel.getFormTable().getFields())
				if (col.getLovForm() != null) {
					this.importLov = true;
					break;
				}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage("java.util");
		importPackage("net.codecadenza.runtime.search.dto");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		importPackage("net.codecadenza.runtime.webclient.vaadin.search");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");
		importPackage("com.vaadin.flow.router");
		importPackage(project.getClientNamespace().toString());
		importPackage(form.getDTO().getNamespace().toString());

		addImports(securityHelper.getSecurityImports());

		tableGenerator.addActionImports();

		if (importLov)
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_LOV);

		if (saveQueries || securityHelper.isSecurityAdded()) {
			importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");

			if (saveQueries)
				importPackage(project.getRootNamespace().toString() + PACK_SERVICE);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Route(value = " + form.getName() + ".ROUTE");

		if (saveQueries)
			b.append(" + \"/:id?\"");

		b.append(", layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + form.getName() + " extends ");

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			b.append("AbstractDataGridView");
		else
			b.append("AbstractDataGridSearchView");

		b.append("<" + form.getDTO().getName() + "> implements HasDynamicTitle");

		if (saveQueries || securityHelper.isSecurityAdded())
			b.append(", BeforeEnterObserver");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPublicConstant(JavaType.STRING, "ROUTE", "\"view/" + form.getName() + "\"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		new ServiceDeclarationGenerator(this, form.getBoundaryMethod().getBoundaryBean()).addField();

		if (securityHelper.isSecurityAdded())
			addProtectedField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();

		if (saveQueries) {
			addPublicConstant(JavaType.STRING, "ID",
					"\"" + project.getClientNamespace().toString() + PACK_CLIENT_VIEW + "." + form.getName() + "\"").create();
			addPrivateField(SAVED_QUERY_SERVICE, "queryManager").inject().create();
		}

		if (!project.isBoundaryMode()) {
			final Optional<BoundaryMethod> boundaryMethod = form.getActions().stream()
					.filter(a -> a.getType() == ActionType.DOWNLOAD_EXPORT || a.getType() == ActionType.UPLOAD_IMPORT)
					.map(FormAction::getBoundaryMethod).findFirst();

			if (boundaryMethod.isPresent())
				new ServiceDeclarationGenerator(this, boundaryMethod.get(), null).addField();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final List<String> injectedFieldsOfSuperClass = List.of("I18NService i18n", "PreferencesStore preferences");

		addDefaultConstructorForInjection(injectedFieldsOfSuperClass);
		addConstructorForInjection(null, injectedFieldsOfSuperClass);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();

		// Add the method for adding buttons to the button bar
		tableGenerator.createButtonBar();

		// Add the method for handling double-click events
		tableGenerator.createDoubleClick();

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.webclient.vaadin.search.AbstractDataGridView#performCountOperation()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public Long performCountOperation()\n");
			b.append("{\n");

			for (final FormPanel formPanel : form.getFormPanels()) {
				b.append("return ");

				new ServiceInvocationGenerator(formPanel.getBoundaryMethod(), b).addInvocation("searchObj");
			}

			b.append("}\n\n");

			addMethod("Long performCountOperation()", b.toString());
		}

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.search.AbstractDataGridView#init()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public SearchDTO init()\n");
		b.append("{\n");
		b.append("final var searchObj = new SearchDTO();\n");
		b.append("int colOrderId = -1;\n");
		b.append("var fieldLabel = \"\";\n\n");
		b.append("// Initialize search object\n");
		b.append("searchObj.setMaxResult(1000);\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(false);\n");
		b.append("searchObj.setCount(true);\n\n");

		// Add all search input fields
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			boolean addLine = false;

			b.append("fieldLabel = " + i18n.getI18N(col) + ";\n\n");
			b.append("final var field" + col.getColIndex() + " = new BindingSearchField<>(++colOrderId, ");
			b.append(col.getDTOAttribute().getSelectTokenConstant());
			b.append(", ");
			b.append("fieldLabel");
			b.append(", SearchFieldDataTypeEnum.");
			b.append(col.getFieldType().name());
			b.append(", ");
			b.append(col.getWidth() + ", ");
			b.append(dto.getName() + col.getDTOAttribute().getGetterReference());
			b.append(");\n");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b.append("\nfinal var enumListValues" + col.getColIndex() + " = new HashMap<String, String>();\n");

				for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
					b.append("enumListValues" + col.getColIndex() + ".put(\"" + value.getName() + "\", i18n.getTranslation");
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

		addMethod("SearchDTO init()", b.toString());

		var methodSignature = "List<" + form.getDTO().getName() + "> fetchData()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#fetchData()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (saveQueries) {
			b.append("try\n");
			b.append("{\n");
			b.append("// Overwrite the last saved query with this one\n");
			b.append("queryManager.saveQuery(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO().");
			b.append(logOnPKGetter + ", ID, null, searchObj);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while saving query!", "e");

			b.append("}\n\n");
		}

		b.append("return ");

		new ServiceInvocationGenerator(form.getBoundaryMethod(), b).addInvocation("searchObj");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String getPageTitle()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.HasDynamicTitle#getPageTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return " + i18n.getI18N(form) + ";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (saveQueries || securityHelper.isSecurityAdded()) {
			final String permissionCheck = securityHelper.addFormPermissionCheck(i18n, form.getRoles());

			methodSignature = "void beforeEnter(BeforeEnterEvent event)";

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#");
			b.append("beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			if (!permissionCheck.isEmpty()) {
				b.append(permissionCheck);

				if (saveQueries)
					b.append("\n");
			}

			if (saveQueries) {
				b.append("final String savedQueryTitle = getIdParameter(event);\n");
				b.append("searchObj = null;\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("if(savedQueryTitle != null && !savedQueryTitle.isEmpty())\n");
				b.append("searchObj = queryManager.getSavedQuery(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO().");
				b.append(logOnPKGetter + ", ID, savedQueryTitle);\n\n");
				b.append("if(searchObj == null)\n");
				b.append("searchObj = queryManager.getLastQuery(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO().");
				b.append(logOnPKGetter + ", ID);\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while loading saved query!", "e");

				b.append("}\n");
			}

			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		// Add the method that implements getDataGridLogger()
		addGetLoggerMethod("net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid", "getDataGridLogger");

		// Add the method for adding context menu items
		tableGenerator.addContextMenuItems();

		// Add methods for all form actions
		tableGenerator.addActionMethods();

		i18n.save();
	}

}
