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
package net.codecadenza.eclipse.generator.client.imp.angular.form;

import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.OBSERVABLE;

import java.util.ArrayList;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.common.TypeScriptFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.file.AngularDownloadGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.file.AngularUploadGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.form.field.AngularFieldGeneratorFactory;
import net.codecadenza.eclipse.generator.client.imp.angular.security.AngularSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularURLGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldComparator;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormPanelComparator;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;

/**
 * <p>
 * Generator for single-record forms of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularSingleRecordFormGenerator extends AbstractTypeScriptSourceGenerator {
	private static final String GRID_PANEL_PREFIX = "grid";

	private final Form form;
	private final FormTypeEnumeration formType;
	private final DomainObject domainObject;
	private final Project project;
	private final AngularI18NGenerator i18n;
	private final AngularSecurityHelper securityHelper;
	private final String baseClassName;

	/**
	 * Constructor
	 * @param form
	 */
	public AngularSingleRecordFormGenerator(Form form) {
		super(form.getTypeScriptSourceFile(), form.getTitle());

		this.form = form;
		this.formType = form.getFormType();
		this.domainObject = form.getDomainObject();
		this.project = domainObject.getNamespace().getProject();
		this.i18n = new AngularI18NGenerator(project);
		this.securityHelper = new AngularSecurityHelper(project);

		if (formType == FormTypeEnumeration.ADD)
			this.baseClassName = "AbstractAddRecordForm";
		else if (formType == FormTypeEnumeration.UPDATE)
			this.baseClassName = "AbstractUpdateRecordForm";
		else if (formType == FormTypeEnumeration.CREATE)
			this.baseClassName = "AbstractCreateRecordForm";
		else
			this.baseClassName = "AbstractReadRecordForm";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importType("Component", "@angular/core");

		if (formType == FormTypeEnumeration.ADD)
			importType(baseClassName, "../../common/components/abstract-form/abstract-add-record-form");
		else if (formType == FormTypeEnumeration.UPDATE)
			importType(baseClassName, "../../common/components/abstract-form/abstract-update-record-form");
		else if (formType == FormTypeEnumeration.CREATE)
			importType(baseClassName, "../../common/components/abstract-form/abstract-create-record-form");
		else
			importType(baseClassName, "../../common/components/abstract-form/abstract-read-record-form");

		form.getAllFormFields()
				.forEach(field -> addImports(AngularFieldGeneratorFactory.getFieldGenerator(getContentFormatter(), field).getImports()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("@Component({");
		formatter.increaseIndent();
		formatter.addLine("templateUrl: './" + form.getName().toLowerCase() + ".html'");
		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine("export class " + form.getName() + " extends " + baseClassName + "<" + form.getDTO().getName() + "> {");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final var fields = new ArrayList<TypeScriptFieldGenerator>();
		final var fileDownloadGenerator = new AngularDownloadGenerator(this, form, i18n);
		final var fileUploadGenerator = new AngularUploadGenerator(this, form, i18n);
		final boolean addFileService = fileDownloadGenerator.isDownloadFragmentAdded() || fileUploadGenerator.isUploadFragmentAdded();
		boolean addAuthService = securityHelper.addClientCheck(form);
		boolean addNumberConverter = false;

		addService(form.getDTO());
		addServiceOfSuperclass("ActivatedRoute", "route", "@angular/router");
		addServiceOfSuperclass("Location", "location", "@angular/common");
		addServiceOfSuperclass("Router", "router", "@angular/router");
		addServiceOfSuperclass("NavigationHistoryService", "navigationHistoryService",
				"../../common/services/navigation-history.service");
		addServiceOfSuperclass("I18NService", "i18n", "../../common/services/i18n.service");

		for (final FormField field : form.getAllFormFields()) {
			final AbstractAngularFieldGenerator fieldGenerator = AngularFieldGeneratorFactory.getFieldGenerator(getContentFormatter(),
					field);
			final JavaEnum javaEnum = fieldGenerator.getJavaEnum();

			addService(fieldGenerator.getListDTO());

			fields.addAll(fieldGenerator.getFields());

			if (fieldGenerator.isAuthServiceRequired())
				addAuthService = true;

			if (fieldGenerator.isNumberConverterRequired())
				addNumberConverter = true;

			if (javaEnum != null)
				addDependentEnum(javaEnum);

			if (field.getDTOAttribute().getReferencedDTOBean() != null)
				addDependentDTO(field.getDTOAttribute().getReferencedDTOBean());
		}

		if (addAuthService)
			addService("AuthService", "authService", "../../common/services/auth.service");

		if (addNumberConverter)
			addService("NumberConverter", "numberConverter", null);

		if (addFileService) {
			addService("FileService", "fileService", "../../common/services/file.service");

			if (fileUploadGenerator.isUploadFragmentAdded())
				addService("MessageService", "messageService", "primeng/api");
		}

		// Add all fields
		fields.forEach(TypeScriptFieldGenerator::create);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addConstructorStatements(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addConstructorStatements(AngularContentFormatter formatter) {
		formatter.addBlankLine();
		formatter.addLine("this.initForm();");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(domainObject);

		formatter.addBlockComment("Add controls");
		formatter.addLine("addControls() {");
		formatter.increaseIndent();

		// Add the binding for all form fields
		form.getAllFormFields()
				.forEach(field -> AngularFieldGeneratorFactory.getFieldGenerator(formatter, field).addBindingFragment());

		// Add the binding for all file upload operations
		new AngularUploadGenerator(this, form, i18n).addBindingFragments();

		boolean firstField = true;

		// Add field value conversions
		for (final FormField field : form.getAllFormFields()) {
			final String convFragment = AngularFieldGeneratorFactory.getFieldGenerator(formatter, field)
					.getFieldValueConversionFragment();

			if (convFragment == null)
				continue;

			if (firstField) {
				formatter.addBlankLine();
				firstField = false;
			}

			formatter.addLine(convFragment);
		}

		firstField = true;

		for (final FormField field : form.getAllFormFields()) {
			final String initFragment = AngularFieldGeneratorFactory.getFieldGenerator(formatter, field)
					.getFieldInitializationFragment();

			if (initFragment == null)
				continue;

			if (firstField) {
				formatter.addBlankLine();
				firstField = false;
			}

			formatter.addLine(initFragment);
		}

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(form.getDTO(),
					BoundaryMethodTypeEnumeration.FIND_BY_ID);

			importType(OBSERVABLE, "rxjs");

			formatter.addBlockComment("Load the " + domainObject.getLabel() + " from the back-end");
			formatter.addLine("loadObject(id: string): " + OBSERVABLE + "<" + form.getDTO().getName() + "> {");
			formatter.increaseIndent();
			formatter.addLine("return " + new AngularServiceInvocationGenerator(method).createInvocation("id") + ";");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.CREATE || a.getType() == ActionType.UPDATE) {
				formatter.addBlockComment("Save the " + domainObject.getLabel());
				formatter.addLine("saveObject(object: " + form.getDTO().getName() + ") {");
				formatter.increaseIndent();

				boolean fieldConversionAdded = false;

				for (final FormField field : form.getAllFormFields()) {
					final String convFragment = AngularFieldGeneratorFactory.getFieldGenerator(formatter, field)
							.getFieldValueReconversionFragment();

					if (convFragment != null) {
						fieldConversionAdded = true;

						formatter.addLine(convFragment);
					}
				}

				if (fieldConversionAdded)
					formatter.addBlankLine();

				formatter
						.addLine("return " + new AngularServiceInvocationGenerator(a.getBoundaryMethod()).createInvocation("object") + ";");
				formatter.decreaseIndent();
				formatter.addLine("}");
				formatter.addBlankLine();
			}

		if (form.isOpenEditAfterCreate())
			for (final Form targetForm : project.getAllFormsOfProject())
				if (targetForm.getFormType() == FormTypeEnumeration.UPDATE && targetForm.getDomainObject().equals(domainObject)) {
					final var pathParameter = "this.object." + form.getDTO().getPKAttribute().getName();

					formatter
							.addBlockComment("Navigate to the corresponding update form after creating the new " + domainObject.getLabel());
					formatter.addLine("override getNavigationTargetAfterSave(): string {");
					formatter.increaseIndent();
					formatter.addLine("return '" + AngularURLGenerator.createURL(targetForm, false) + "/' + " + pathParameter + ";");
					formatter.decreaseIndent();
					formatter.addLine("}");
					formatter.addBlankLine();
					break;
				}

		securityHelper.addClientCheckMethod(formatter, form);

		for (final FormField field : form.getAllFormFields()) {
			if (!field.isVisible())
				continue;

			AngularFieldGeneratorFactory.getFieldGenerator(formatter, field).addMethods(i18n);
		}

		// Add download methods
		new AngularDownloadGenerator(this, form, i18n).addDownloadMethods();

		// Add upload methods
		new AngularUploadGenerator(this, form, i18n).addUploadMethods();

		i18n.save();
	}

	/**
	 * Create the form template file
	 * @throws Exception if an internal error has occurred
	 */
	public void createTemplateFile() throws Exception {
		final var panelsOfFirstRow = new BasicEList<FormPanel>();
		final var panelsOfSecondRow = new BasicEList<FormPanel>();
		boolean hasLOVDialog = false;

		final var formatter = new AngularContentFormatter();
		formatter.addLine("<cc-error-dialog [error]=\"error\" [leavePage]=\"leavePage\"></cc-error-dialog>");
		formatter.addBlankLine();

		// Add list-of-values dialogs
		for (final FormField formField : form.getAllFormFields()) {
			if (!formField.isVisible() || formField.isReadonly() || formField.getFieldType() != FormFieldTypeEnumeration.LOV)
				continue;

			if (!formField.getDTOAttribute().getDTOBean().equals(form.getDTO()))
				continue;

			final DTOBean formDTO = form.getDTO();
			final DTOBean listDTO = formField.getDTOAttribute().getReferencedDTOBean();
			final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute() == null ? listDTO.getPKAttribute()
					: listDTO.getDisplayAttribute();
			final var lovSelector = SELECTOR_PREFIX + formField.getListOfValues().getName().toLowerCase();
			final var lovStatusName = "show" + formField.getDTOAttribute().getUpperCaseName() + "Lov";
			final var selectionHandlerMethodName = "on" + formField.getDTOAttribute().getUpperCaseName() + "Selected";

			hasLOVDialog = true;

			final var lov = new StringBuilder("<" + lovSelector + " ");
			lov.append("[visible]=\"" + lovStatusName + "\" ");
			lov.append("(closeDialog)=\"" + lovStatusName + " = false\" ");

			if (formDTO.getAttributes().contains(formField.getDTOAttribute()))
				lov.append("(selectItem)=\"" + selectionHandlerMethodName + "($event)\" ");

			if (!formField.isMandatory())
				lov.append("[enableReset]=\"true\"");

			if (displayAttr.getDomainAttribute().getJavaType().isIntegerOrLong()) {
				if (!formField.isMandatory())
					lov.append(" ");

				lov.append("[minLength]=\"1\"");
			}

			lov.append("></" + lovSelector + ">");

			formatter.addLine(lov.toString());
		}

		if (hasLOVDialog)
			formatter.addBlankLine();

		final var container = new StringBuilder();
		container.append("<cc-view-container i18n-headerText=\"@@" + form.getName().toLowerCase() + "_title\" ");
		container.append("headerIcon=\"pi-file\" ");

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			container.append("headerText=\"" + form.getTitle() + " '{{id}}'\" *ngIf=\"object\"");
		else
			container.append("headerText=\"" + form.getTitle() + "\"");

		container.append(">");

		formatter.addLine(container.toString());
		formatter.increaseIndent();

		if (formType == FormTypeEnumeration.READONLY)
			formatter.addLine("<form [formGroup]=\"formGroup\">");
		else
			formatter.addLine("<form [formGroup]=\"formGroup\" (ngSubmit)=\"save()\">");

		formatter.increaseIndent();

		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == 1)
				panelsOfFirstRow.add(panel);
			else
				panelsOfSecondRow.add(panel);

		if (panelsOfFirstRow.size() > 1) {
			formatter.addLine("<p-tabView>");
			formatter.increaseIndent();
		}

		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		panelsOfFirstRow.forEach(panel -> addPanel(formatter, panel, panelsOfFirstRow.size() > 1));

		if (panelsOfFirstRow.size() > 1) {
			formatter.decreaseIndent();
			formatter.addLine("</p-tabView>");
		}

		if (!panelsOfSecondRow.isEmpty()) {
			formatter.addBlankLine();
			formatter.addLine("<br>");
			formatter.addBlankLine();
		}

		if (panelsOfSecondRow.size() > 1) {
			formatter.addLine("<p-tabView>");
			formatter.increaseIndent();
		}

		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		panelsOfSecondRow.forEach(panel -> addPanel(formatter, panel, panelsOfSecondRow.size() > 1));

		if (panelsOfSecondRow.size() > 1) {
			formatter.decreaseIndent();
			formatter.addLine("</p-tabView>");
		}

		formatter.addBlankLine();
		formatter.addLine("<cc-form-button-container>");
		formatter.increaseIndent();

		if (formType != FormTypeEnumeration.READONLY) {
			final var button = new StringBuilder();
			button.append("<button pButton type=\"submit\" icon=\"pi pi-check\" i18n-label=\"@@button_save\" ");
			button.append("[style]=\"{'margin': '0px 0px 0.5em 0.5em'}\" class=\"col xl:col-1 md:col-3 sm:col-12\" ");
			button.append("label=\"Save\" [disabled]=\"!formGroup.valid\" id=\"cmdSave\"></button>");

			formatter.addLine(button.toString());
		}

		final var button = new StringBuilder();
		button.append("<button pButton type=\"button\" icon=\"pi pi-angle-left\" i18n-label=\"@@button_back\" ");
		button.append("[style]=\"{'margin': '0px 0px 0.5em 0.5em'}\" class=\"col xl:col-1 md:col-3 sm:col-12\" ");
		button.append("label=\"Back\" (click)=\"goBack()\" id=\"cmdBack\"></button>");

		formatter.addLine(button.toString());

		// Add buttons for download operations
		new AngularDownloadGenerator(this, form, i18n).addButtons(formatter);

		// Add buttons for upload operations
		new AngularUploadGenerator(this, form, i18n).addButtons(formatter);

		formatter.decreaseIndent();
		formatter.addLine("</cc-form-button-container>");
		formatter.decreaseIndent();
		formatter.addLine("</form>");
		formatter.decreaseIndent();
		formatter.addLine("</cc-view-container>");

		final WorkspaceFile templateFile = form.getUserInterfaceFile();
		templateFile.setContent(formatter.getContent());

		EclipseIDEService.createOrUpdateFile(templateFile);
	}

	/**
	 * Add the given panel to the form's template
	 * @param formatter
	 * @param panel
	 * @param addTab
	 */
	private void addPanel(AngularContentFormatter formatter, FormPanel panel, boolean addTab) {
		final var translationKey = "@@" + form.getName().toLowerCase() + "_" + panel.getName().toLowerCase();

		if (addTab) {
			final var tabId = "id=\"tab" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1) + "\"";

			formatter.addLine("<p-tabPanel i18n-header=\"" + translationKey + "\" header=\"" + panel.getLabel() + "\" " + tabId + ">");
			formatter.increaseIndent();
		}
		else if (panel.isDrawBorder()) {
			formatter.addLine("<p-fieldset i18n-legend=\"" + translationKey + "\" legend=\"" + panel.getLabel() + "\">");
			formatter.increaseIndent();
		}

		if (panel.getBasePanel() == null) {
			formatter.addLine("<cc-form-container>");
			formatter.increaseIndent();
			formatter.addBlankLine();

			ECollections.sort(panel.getFields(), new FormFieldComparator());

			// Add the form fields to the panel
			panel.getFields().forEach(field -> AngularFieldGeneratorFactory.getFieldGenerator(formatter, field).addFieldToTemplate());

			formatter.decreaseIndent();
			formatter.addLine("</cc-form-container>");
		}
		else {
			final var gridPanelSelector = SELECTOR_PREFIX + panel.getBasePanel().getName().toLowerCase();
			final DTOBeanAttribute pkAttribute = form.getDTO().getPKAttribute();
			final String tableId = GRID_PANEL_PREFIX + panel.getBasePanel().getName();

			final var gridPanel = new StringBuilder();
			gridPanel.append("<" + gridPanelSelector + " [maxNumberOfItems]=\"100\" ");
			gridPanel.append("[parentObjectId]=\"");

			if (pkAttribute.getDomainAttribute().getJavaType().isIntegerOrLong())
				gridPanel.append("'' + ");

			gridPanel.append("object." + pkAttribute.getName() + "\"");

			if (formType == FormTypeEnumeration.READONLY
					&& panel.getBasePanel().getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
				gridPanel.append(" [readonly]=\"true\"");

			gridPanel.append(" tableId=\"" + tableId + "\" rowKey=\"" + pkAttribute.getName() + "\"");
			gridPanel.append("></" + gridPanelSelector + ">");

			formatter.addLine(gridPanel.toString());
		}

		if (addTab) {
			formatter.decreaseIndent();
			formatter.addLine("</p-tabPanel>");
		}
		else if (panel.isDrawBorder()) {
			formatter.decreaseIndent();
			formatter.addLine("</p-fieldset>");
		}
	}

}
