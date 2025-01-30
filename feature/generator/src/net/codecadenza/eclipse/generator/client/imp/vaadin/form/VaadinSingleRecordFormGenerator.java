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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form;

import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.BUTTON_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.generator.client.common.form.AbstractSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.VaadinFieldGeneratorFactory;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
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
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for single-record forms of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinSingleRecordFormGenerator extends AbstractSingleRecordFormGenerator {
	private final VaadinI18NGenerator i18n;
	private final VaadinSecurityHelper securityHelper;
	private final EList<FormPanel> panelsOfFirstRow = new BasicEList<>();
	private final EList<FormPanel> panelsOfSecondRow = new BasicEList<>();
	private final String locale;

	/**
	 * Constructor
	 * @param form
	 */
	public VaadinSingleRecordFormGenerator(Form form) {
		super(form);

		this.i18n = new VaadinI18NGenerator(project);
		this.securityHelper = new VaadinSecurityHelper(project);
		this.locale = i18n.getLocaleFragment();

		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == 1)
				this.panelsOfFirstRow.add(panel);
			else
				this.panelsOfSecondRow.add(panel);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage(project.getClientNamespace().toString());
		importPackage("com.vaadin.flow.component.button");
		importPackage("com.vaadin.flow.component.formlayout");
		importPackage("com.vaadin.flow.component.orderedlayout");
		importPackage("com.vaadin.flow.data.binder");
		importPackage("com.vaadin.flow.router");
		importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");

		addImports(securityHelper.getSecurityImports());

		if (project.isBoundaryMode())
			importPackage(dto.getNamespace().toString());
		else
			importPackage(dto.getDomainObject().getNamespace().toString());

		// Add imports for further data transfer objects that are used when creating initial one-to-many association objects!
		for (final DTOBean addDTO : additionalDTOs)
			if (!project.isBoundaryMode()) {
				importPackage(addDTO.getDomainObject().getNamespace().toString());

				// If we work directly with entities we must also import packages of domain objects that are referenced by one-to-one
				// associations!
				for (final DTOBeanAttribute attr : dto.getAttributes())
					if (attr.getAssociation() instanceof OneToOneAssociation)
						importPackage(attr.getAssociation().getTarget().getNamespace().toString());
			}
			else
				importPackage(addDTO.getNamespace().toString());

		// Search for additional imports concerning actions
		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DIRECT_UPLOAD || a.getType() == ActionType.INDIRECT_UPLOAD
					|| a.getType() == ActionType.DOWNLOAD) {
				importPackage("java.io");

				if (a.getType() == ActionType.DOWNLOAD)
					importPackage("net.codecadenza.runtime.webclient.vaadin.component");

				if (a.getType() == ActionType.DIRECT_UPLOAD && !project.isBoundaryMode()
						&& a.getBoundaryMethod().getDomainAttribute().isLob())
					importPackage("net.codecadenza.runtime.file");
			}

		// Analyze the form to add all necessary imports
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_PANEL);
				continue;
			}

			// Add imports for all form fields
			panel.getFields().forEach(field -> VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addImports());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		final var idFragment = formType != FormTypeEnumeration.CREATE ? " + \"/:id\"" : "";

		b.append("@Route(value = " + form.getName() + ".ROUTE" + idFragment + ", layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + form.getName());
		b.append(" extends VerticalLayout implements BeforeEnterObserver, HasDynamicTitle");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final var lovSet = new HashSet<Form>();

		addPublicConstant(JavaType.STRING, "ROUTE", "\"dialog/" + form.getName() + "\"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addProtectedField("PreferencesStore", "preferences").inject().create();
		addPrivateField("I18NService", "i18n").inject().create();
		addPrivateField("Navigator", "navigator").withDefaultValue("new Navigator(this)").withFinalModifier().create();

		if (securityHelper.isSecurityAdded())
			addProtectedField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();

		addPrivateField("Binder<" + dto.getModelClassName() + ">", "binder").withFinalModifier().withDefaultValue("new Binder<>()")
				.create();

		// Create binders for all additional data transfer objects
		additionalDTOs.forEach(addDTO -> {
			final var binderName = addDTO.getDomainObject().getLowerCaseName() + "Binder";

			addPrivateField("Binder<" + addDTO.getModelClassName() + ">", binderName).withFinalModifier()
					.withDefaultValue("new Binder<>()").create();
		});

		// Inject all list-of-values dialogs
		for (final FormField field : form.getAllFormFields()) {
			if (!field.isVisible() || field.isReadonly() || field.getFieldType() != FormFieldTypeEnumeration.LOV)
				continue;

			final Form lov = field.getListOfValues();

			if (lov == null || lovSet.contains(lov))
				continue;

			addPrivateField(lov.getName(), lov.getLowerCaseName()).inject().create();

			lovSet.add(lov);
		}

		// Add all form field declarations
		form.getAllFormFields()
				.forEach(field -> VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addFieldDeclaration());

		// Add the ID attribute
		final DTOBeanAttribute pkDTOAttr = dto.getPKAttribute();

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			if (pkDTOAttr.getDomainAttribute().getJavaType().getNamespace() != null)
				importPackage(pkDTOAttr.getDomainAttribute().getJavaType().getNamespace().toString());

			addPrivateField(pkDTOAttr.getDomainAttribute().getJavaType().getName(), "id").create();
		}

		// Add the declaration for the model object
		addPrivateField(dto.getModelClassName(), modelObjectName).setTransientModifier(!project.isBoundaryMode()).create();

		additionalDTOs.stream().forEach(
				addDTO -> addPrivateField(addDTO.getModelClassName(), INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName())
						.setTransientModifier(!project.isBoundaryMode()).create());

		// Add grid panels
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() == null)
				continue;

			importPackage("com.vaadin.flow.component");

			final var gridPanelName = "gd" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);

			addPrivateField(panel.getBasePanel().getName(), gridPanelName).inject().create();
		}

		final var serviceSet = new HashSet<String>();
		final BoundaryBean boundary = project.getBoundaryByDomainObject(dto.getDomainObject());

		// Add the declaration for the facade service
		if (boundary != null) {
			serviceSet.add(boundary.getInterfaceName());

			new ServiceDeclarationGenerator(this, boundary).addField();
		}

		// Add declarations for further services!
		for (final FormField field : form.getAllFormFields()) {
			if (formType == FormTypeEnumeration.READONLY || !field.isVisible() || field.isReadonly())
				continue;

			if (field.getDTOAttribute().getReferencedDTOBean() == null
					|| field.getDTOAttribute().getReferencedDTOBean().getDomainObject().equals(dto.getDomainObject()))
				continue;

			// If the field is driven by the parent form we won't need another service!
			if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
				continue;

			if (field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK)
				continue;

			if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_CLIENT)
				continue;

			if (field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO)
				continue;

			if (field.getFieldType() == FormFieldTypeEnumeration.LOV)
				continue;

			final DTOBean listDTO = field.getDTOAttribute().getReferencedDTOBean();
			final BoundaryBean listBoundary = project.getBoundaryByDomainObject(listDTO.getDomainObject());
			var interfaceName = "";

			if (listBoundary != null)
				interfaceName = listBoundary.getInterfaceName();

			// We should not declare a service twice!
			if (serviceSet.contains(interfaceName))
				continue;

			serviceSet.add(interfaceName);

			new ServiceDeclarationGenerator(this, listBoundary).addField();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			createFormInitializerMethod();
		else
			createFormInitializerCreateMethod();

		if (formType != FormTypeEnumeration.READONLY) {
			createValidationMethod();
			createFormDataSaveMethod();
		}

		var methodSignature = "String getPageTitle()";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.HasDynamicTitle#getPageTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
			b.append("return " + i18n.getI18N(form) + " + \" '\" + id + \"'\";\n");
		else
			b.append("return " + i18n.getI18N(form) + ";\n");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void buildDialog()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Build dialog\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Initialize dialog");

		b.append("\n");

		// Sort all panels of the first row
		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		// Add the panels of the first row
		for (final FormPanel panel : panelsOfFirstRow)
			b.append(addPanelToForm(panel, panelsOfFirstRow.size()));

		// Check if the form needs a tab folder
		if (panelsOfFirstRow.size() > 1) {
			importPackage("net.codecadenza.runtime.webclient.vaadin.component");

			b.append("final var tabSheetTop = new TabSheet(this);\n\n");

			for (final FormPanel panel : panelsOfFirstRow)
				b.append(addTabPage("tabSheetTop", panel));
		}

		// Sort all panels of the second row
		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		// Add the panels of the second row
		for (final FormPanel panel : panelsOfSecondRow)
			b.append(addPanelToForm(panel, panelsOfSecondRow.size()));

		if (panelsOfSecondRow.size() > 1) {
			importPackage("net.codecadenza.runtime.webclient.vaadin.component");

			b.append("final var tabSheetBottom = new TabSheet(this);\n\n");

			for (final FormPanel panel : panelsOfSecondRow)
				b.append(addTabPage("tabSheetBottom", panel));
		}

		b.append(addButtons());
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		createBindingMethod();

		i18n.save();
	}

	/**
	 * Add a tab page with the provided form panel to the given tab sheet
	 * @param tabSheetName
	 * @param panel
	 * @return the generated content
	 */
	private String addTabPage(String tabSheetName, FormPanel panel) {
		final var b = new StringBuilder();
		final var tabPageName = "tab" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);
		final String panelName;

		if (panel.getBasePanel() == null)
			panelName = "fl" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);
		else
			panelName = "gd" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);

		b.append("final var " + tabPageName + " = " + tabSheetName + ".addTab(" + panelName + ", " + i18n.getI18N(panel) + ");\n");
		b.append(tabPageName + ".setId(\"" + tabPageName + "\");\n\n");

		return b.toString();
	}

	/**
	 * Create the method to initialize the form
	 */
	private void createFormInitializerMethod() {
		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());
		final JavaType pkType = dto.getPKAttribute().getDomainAttribute().getJavaType();
		final String permissionCheck = securityHelper.addFormPermissionCheck(i18n, form.getRoles());

		if (boundaryBean == null)
			throw new IllegalStateException(
					"The boundary bean for domain object '" + dto.getDomainObject().getName() + "' could not be found!");

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void beforeEnter(BeforeEnterEvent event)\n");
		b.append("{\n");

		if (!permissionCheck.isEmpty()) {
			b.append(permissionCheck);
			b.append("\n");
		}

		b.append("buildDialog();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("id = navigator.");

		if (pkType.isLong())
			b.append("getLongIdParameter");
		else if (pkType.isInteger())
			b.append("getIntIdParameter");
		else if (pkType.isUUID())
			b.append("getUuidIdParameter");
		else
			b.append("getStringIdParameter");

		b.append("(event);\n\n");

		addDebugLog(b, "Fetch data for object with id '{}'", "id");

		b.append("\n");
		b.append(modelObjectName + " = ");

		if (project.isBoundaryMode()) {
			final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(dto, BoundaryMethodTypeEnumeration.FIND_BY_ID);

			new ServiceInvocationGenerator(method, b).addInvocation("id");
		}
		else {
			final RepositoryMethod method = boundaryBean.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

			new ServiceInvocationGenerator(method, b).addInvocation("id", "true");
		}

		b.append("\n");

		// Initialize all form fields
		form.getAllFormFields().forEach(field -> {
			final String fragment = VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getDefaultInitializationFragment();

			if (!fragment.isEmpty()) {
				b.append("\n");
				b.append(fragment);
			}
		});

		// Initialize all grid panels
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() == null)
				continue;

			final var gridPanelName = "gd" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);

			b.append("\n");
			b.append(gridPanelName + ".setParentId(id);\n");
			b.append(gridPanelName + ".refresh();\n\n");
		}

		b.append("initFieldBinding();\n\n");
		b.append("binder.readBean(" + modelObjectName + ");\n");
		b.append("\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("final String title = " + i18n.getI18NMessage("msg_title_init_dialog", "Dialog initialization") + ";\n");
		b.append("final String message = ");
		b.append(i18n.getI18NMessage("msg_err_init_dialog", "Initialization of dialog failed! Message: "));
		b.append(" + ex.getMessage();\n\n");

		addErrorLog(b, "Dialog initialization failed for object with id '{}'!", "ex", "id");

		b.append("\n");
		b.append("new ErrorMessageDialog(title, message, ex, " + locale + ").open();\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod("void beforeEnter(BeforeEnterEvent event)", b.toString());
	}

	/**
	 * Create the method to initialize the form
	 */
	private void createFormInitializerCreateMethod() {
		final var b = new StringBuilder();
		final var defaultValueInitFragments = new StringBuilder();
		final String permissionCheck = securityHelper.addFormPermissionCheck(i18n, form.getRoles());

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void beforeEnter(BeforeEnterEvent event)\n");
		b.append("{\n");

		if (!permissionCheck.isEmpty()) {
			b.append(permissionCheck);
			b.append("\n");
		}

		b.append("buildDialog();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append(modelObjectName + " = new " + dto.getModelClassName() + "();\n");
		b.append(addInitializationOfAdditionalFields());

		// Search for domain object fields that must be filled with a default value
		form.getAllFormFields().forEach(field -> {
			final DTOBean fieldDTO = field.getDTOAttribute().getDTOBean();
			final AbstractVaadinFieldGenerator fb = VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n);

			if (!fieldDTO.equals(dto))
				fb.setModelObjectName(INIT_MODEL_OBJ_NAME_PREFIX + fieldDTO.getDomainObject().getName());

			defaultValueInitFragments.append(fb.getDefaultValueInitialization());
		});

		if (!defaultValueInitFragments.toString().isEmpty()) {
			b.append("// Preset domain object fields\n");
			b.append(defaultValueInitFragments.toString());
			b.append("\n");
		}

		// Initialize all form fields
		form.getAllFormFields().forEach(field -> {
			final DTOBean fieldDTO = field.getDTOAttribute().getDTOBean();

			final AbstractVaadinFieldGenerator fb = VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n);
			fb.setModelObjectName(fieldDTO.getDomainObject().getLowerCaseName());

			final String fragment = fb.getCreateInitializationFragment();

			if (!fragment.isEmpty()) {
				b.append("\n");
				b.append(fragment);
			}
		});

		b.append("\n");
		b.append("initFieldBinding();\n\n");
		b.append("binder.readBean(" + modelObjectName + ");\n");

		additionalDTOs.forEach(addDTO -> {
			final var binderName = addDTO.getDomainObject().getLowerCaseName() + "Binder";
			final String objectName = INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName();

			b.append(binderName + ".readBean(" + objectName + ");\n");
		});

		b.append("\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("final String title = " + i18n.getI18NMessage("msg_title_init_dialog", "Dialog initialization") + ";\n");
		b.append("final String message = ");
		b.append(i18n.getI18NMessage("msg_err_init_dialog", "Initialization of dialog failed! Message: "));
		b.append(" + ex.getMessage();\n\n");

		addErrorLog(b, "Dialog initialization failed!", "ex");

		b.append("\n");
		b.append("new ErrorMessageDialog(title, message, ex, " + locale + ").open();\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod("void beforeEnter(BeforeEnterEvent event)", b.toString());
	}

	/**
	 * Create the method that is responsible for field binding
	 */
	private void createBindingMethod() {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize field binding\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void initFieldBinding()\n");
		b.append("{\n");

		boolean firstField = true;

		for (final FormField field : form.getAllFormFields()) {
			if (!field.isVisible())
				continue;

			final DTOBeanAttribute attr = field.getDTOAttribute();
			final DTOBean fieldDTO = attr.getDTOBean();
			var binderName = "binder";

			if (additionalDTOs.contains(fieldDTO))
				binderName = fieldDTO.getDomainObject().getLowerCaseName() + "Binder";

			final String bindingFragment = VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getBinding(binderName);

			if (!bindingFragment.isEmpty()) {
				if (!firstField)
					b.append("\n");

				b.append(bindingFragment);
			}

			firstField = false;
		}

		b.append("}\n\n");

		addMethod("void initFieldBinding()", b.toString());
	}

	/**
	 * Create the method to validate form data
	 */
	private void createValidationMethod() {
		final var b = new StringBuilder();
		final List<FormAction> directUploadActions = form.getActions().stream().filter(a -> a.getType() == ActionType.DIRECT_UPLOAD)
				.toList();

		b.append("/**\n");
		b.append(" * Check user input\n");
		b.append(" * @return true if the validation was finished successfully!\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private boolean validateUserInput()\n");
		b.append("{\n");

		if (additionalDTOs.isEmpty() && directUploadActions.isEmpty())
			b.append("return binder.validate().getFieldValidationErrors().isEmpty();\n");
		else {
			b.append("if(!binder.validate().getFieldValidationErrors().isEmpty())\n");
			b.append("return false;\n\n");

			additionalDTOs.forEach(addDTO -> {
				final var binderName = addDTO.getDomainObject().getLowerCaseName() + "Binder";

				b.append("if(!" + binderName + ".validate().getFieldValidationErrors().isEmpty())\n");
				b.append("return false;\n\n");
			});

			// If a form contains an action of type 'DIRECT_UPLOAD' we have to add a check if the corresponding field isn't nullable!
			for (final FormAction action : directUploadActions) {
				final DTOBeanAttribute dataAttribute = dto.getAttributes().stream()
						.filter(attribute -> attribute.getDomainAttribute() != null
								&& attribute.getDomainAttribute().equals(action.getBoundaryMethod().getDomainAttribute()))
						.findFirst().orElse(null);

				if (dataAttribute == null)
					throw new IllegalStateException("The DTO attribute for form action '" + action.getName() + "' could not be found!");

				// Test if the file upload is mandatory!
				if (dataAttribute.getDomainAttribute().isLob()
						&& dataAttribute.getDomainAttribute().getDomainAttributeValidator().isNullable())
					continue;

				if (!dataAttribute.getDomainAttribute().isLob() && dataAttribute.getDomainAttribute().getMinFieldLength().isEmpty())
					continue;

				b.append("// Check if the user has already uploaded a file!\n");
				b.append("if(" + modelObjectName + "." + dataAttribute.getModelGetterName() + " == null");

				if (project.isBoundaryMode())
					b.append(" || " + modelObjectName + "." + dataAttribute.getModelGetterName() + ".isEmpty()");

				b.append(")\n");
				b.append("{\n");
				b.append("final String title = " + i18n.getI18NMessage("msg_err_field_validation", "Field validation") + ";\n");
				b.append("final String message = ");
				b.append(i18n.getI18NMessage("msg_err_missing_file", "A file must be attached to this object!") + ";\n\n");
				b.append("new InfoMessageDialog(title, message, " + locale + ").open();\n");
				b.append("return false;\n");
				b.append("}\n\n");
			}

			b.append("return true;\n");
		}

		b.append("}\n\n");

		addMethod("boolean validateUserInput()", b.toString());
	}

	/**
	 * Add the toolbar and its items
	 * @return the generated content
	 */
	private String addButtons() {
		final var b = new StringBuilder();
		b.append("final var hlButtons = new HorizontalLayout();\n\n");

		if (formType != FormTypeEnumeration.READONLY) {
			b.append("final var cmdSave = new Button(" + i18n.getI18NMessage("cmd_save", "Save") + ");\n");
			b.append("cmdSave.setId(\"cmdSave\");\n");
			b.append("cmdSave.addClickListener(event -> saveData());\n\n");
			b.append("hlButtons.add(cmdSave);\n\n");
		}

		b.append("final var cmdCancel = new Button(" + i18n.getI18NMessage("cmd_cancel", "Cancel") + ");\n");
		b.append("cmdCancel.setId(\"cmdCancel\");\n");
		b.append("cmdCancel.addClickListener(event -> navigator.navigateBack());\n\n");
		b.append("hlButtons.add(cmdCancel);\n\n");
		b.append("add(hlButtons);\n");

		form.getActions().forEach(action -> {
			if (action.getType() == ActionType.DOWNLOAD) {
				final DomainAttribute downloadAttribute = action.getBoundaryMethod().getDomainAttribute();
				final var buttonName = BUTTON_PREFIX + downloadAttribute.getUpperCaseName() + "Download";

				b.append("\n");
				b.append("final var " + buttonName + " = new FileDownloadAnchor(() ->\n");
				b.append("{\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final String path = ");

				new ServiceInvocationGenerator(action.getBoundaryMethod(), b).addInvocation("id");

				b.append("\n");
				b.append("return new File(path);\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing download operation!", "e");

				b.append("\n");
				b.append("return null;\n");
				b.append("}\n");
				b.append("});\n\n");
				b.append(buttonName + ".setId(\"" + buttonName + "\");\n");
				b.append(buttonName + ".add(new Button(" + i18n.getI18NMessage("cmd_download", "Download") + "));\n\n");
				b.append("hlButtons.add(" + buttonName + ");\n");
			}
			else if (action.getType() == ActionType.INDIRECT_UPLOAD || action.getType() == ActionType.DIRECT_UPLOAD)
				b.append(addUploadButton(action));
		});

		return b.toString();
	}

	/**
	 * Create a button for performing a file upload operation
	 * @param action
	 * @return the generated content
	 */
	private String addUploadButton(FormAction action) {
		final var b = new StringBuilder();
		final DomainAttribute uploadAttr = action.getBoundaryMethod().getDomainAttribute();
		final var buttonName = BUTTON_PREFIX + uploadAttr.getUpperCaseName() + "Upload";
		final String maxFileSize = uploadAttr.getMaxFileSize();

		b.append("\n");
		b.append("final var " + buttonName + " = new Button(" + i18n.getI18NMessage("cmd_upload", "Upload") + ");\n");
		b.append(buttonName + ".setId(\"" + buttonName + "\");\n\n");
		b.append(buttonName + ".addClickListener(event ->\n");
		b.append("{\n");
		b.append("final var dlg = new FileUploadDialog(" + locale + ");\n");
		b.append("dlg.setMaxFileSize(" + maxFileSize + ");\n\n");
		b.append("dlg.setUploadFinishedListener((File uploadFile, String originalFileName) ->\n");
		b.append("{\n");

		if (action.getType() == ActionType.DIRECT_UPLOAD) {
			var setter = "";

			for (final DTOBeanAttribute dtoAttr : dto.getAttributes()) {
				if (dtoAttr.getDomainAttribute() == null)
					continue;

				if (dtoAttr.getDomainAttribute().equals(action.getBoundaryMethod().getDomainAttribute())) {
					setter = dtoAttr.getModelSetterName();
					break;
				}
			}

			b.append(modelObjectName + "." + setter + "(");

			if (project.isBoundaryMode() || !uploadAttr.isLob())
				b.append("uploadFile.getAbsolutePath()");
			else if (uploadAttr.getJavaType().isType(JavaType.BYTE_ARRAY))
				b.append("FileUtil.getBytesFromFile(uploadFile)");
			else
				b.append("FileUtil.convertToByteArray(FileUtil.getBytesFromFile(uploadFile))");

			b.append(");\n");
		}
		else
			new ServiceInvocationGenerator(action.getBoundaryMethod(), b).addInvocation("id", "uploadFile.getAbsolutePath()");

		boolean updateBinding = false;

		// Check if fields with appropriate tagging exist that should be filled automatically!
		for (final FormField field : form.getAllFormFields()) {
			if (!field.isVisible())
				continue;

			final DomainObject domainObject = dto.getDomainObject();
			final DTOBeanAttribute dtoAttr = field.getDTOAttribute();
			boolean addFragment = false;

			if (dtoAttr.getDomainAttribute() == null)
				continue;

			// Test if the attribute belongs to a valid domain object!
			for (final DomainObject obj : domainObject.getFullInheritanceTree()) {
				if (obj.isMappedSuperClass())
					continue;

				if (obj.equals(dtoAttr.getDomainAttribute().getDomainObject())) {
					addFragment = true;
					break;
				}
			}

			if (!addFragment)
				continue;

			final DomainAttribute attr = dtoAttr.getDomainAttribute();

			if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME || attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE)
				updateBinding = true;

			if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME)
				b.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "(originalFileName);\n");

			if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE)
				if (attr.getJavaType().isInteger())
					b.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "((int) uploadFile.length());\n");
				else
					b.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "(uploadFile.length());\n");
		}

		if (updateBinding)
			b.append("\nbinder.readBean(" + modelObjectName + ");\n");

		b.append("});\n\n");
		b.append("dlg.open();\n");
		b.append("});\n\n");
		b.append("hlButtons.add(" + buttonName + ");\n");

		return b.toString();
	}

	/**
	 * Create the method to save form data
	 */
	private void createFormDataSaveMethod() {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Save form data\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void saveData()\n");
		b.append("{\n");
		b.append("if(!validateUserInput())\n");
		b.append("return;\n\n");

		addDebugLog(b, "Perform save operation");

		b.append("\n");

		// Add fragments for saving data for all form fields
		form.getAllFormFields().forEach(field -> {
			final DTOBean fieldDTO = field.getDTOAttribute().getDTOBean();
			String objectName = modelObjectName;

			if (!fieldDTO.equals(dto))
				objectName = INIT_MODEL_OBJ_NAME_PREFIX + fieldDTO.getDomainObject().getName();

			final String fragment = VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getSaveDataFragment(objectName);

			if (!fragment.isEmpty()) {
				b.append("\n");
				b.append(fragment);
			}
		});

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.CREATE || a.getType() == ActionType.UPDATE) {
				b.append("\n");
				b.append("try\n");
				b.append("{\n");
				b.append("binder.writeBean(" + modelObjectName + ");\n");

				additionalDTOs.forEach(addDTO -> {
					final var binderName = addDTO.getDomainObject().getLowerCaseName() + "Binder";
					final String objectName = INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName();

					b.append(binderName + ".writeBean(" + objectName + ");\n");
				});

				b.append("\n");

				if (!a.getBoundaryMethod().getReturnType().isVoid())
					b.append(dto.getDomainObject().getLowerCaseName() + " = ");

				new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation(getSaveInvocationParameters());

				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing save operation!", "e");

				b.append("\n");
				b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_save", "Save record") + ";\n");
				b.append("final String dialogMsg = " + i18n.getI18NMessage("msg_err_save", "Save operation failed!") + ";\n\n");
				b.append("new ErrorMessageDialog(dialogTitle, dialogMsg, e, " + locale + ").open();\n");
				b.append("return;\n");
				b.append("}\n\n");

				boolean navigateToUpdateForm = false;

				if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && form.isOpenEditAfterCreate()) {
					for (final Form f : project.getAllFormsOfProject())
						if (f.getFormType() == FormTypeEnumeration.UPDATE && form.getDomainObject().equals(f.getDomainObject())) {
							final var fullGetter = dto.getDomainObject().getLowerCaseName() + "."
									+ f.getDTO().getPKAttribute().getModelGetterName();
							navigateToUpdateForm = true;

							b.append("navigator.navigateTo(" + f.getName() + ".class, " + fullGetter + ");\n");
							break;
						}
				}

				if (!navigateToUpdateForm)
					b.append("navigator.navigateBack();\n");

				b.append("}\n\n");
				break;
			}

		addMethod("void saveData()", b.toString());
	}

	/**
	 * Add the panel to the form
	 * @param panel
	 * @param rowPanelCount
	 * @return the generated content
	 */
	private String addPanelToForm(FormPanel panel, int rowPanelCount) {
		final var b = new StringBuilder();
		final var fieldDefinition = new StringBuilder();
		final var layoutName = "fl" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);
		final var gridPanelName = "gd" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);
		boolean hasGrid = false;
		boolean hasOneColumn = true;

		if (panel.getBasePanel() == null) {
			for (final FormField field : panel.getFields())
				if (field.isVisible() && field.getColIndex() == 2) {
					hasOneColumn = false;
					break;
				}
		}
		else
			hasGrid = true;

		boolean addToTabFolder = true;

		if (rowPanelCount == 1)
			addToTabFolder = false;

		if (hasGrid) {
			b.append(gridPanelName + ".setHeight(500, Unit.PIXELS);\n");

			if (!addToTabFolder && !panel.isDrawBorder())
				b.append(gridPanelName + ".setWidthFull();\n");

			if (formType == FormTypeEnumeration.READONLY)
				b.append(gridPanelName + ".setReadOnly(true);\n");

			b.append(gridPanelName + ".initGridColumns();\n\n");
		}
		else {
			// Sort all form fields of this panel
			ECollections.sort(panel.getFields(), new FormFieldComparator());

			int rowIndex = 0;
			int rowIndexForSpacer = -1;

			// Add the form fields of this panel
			for (final FormField field : panel.getFields()) {
				if (field.isHidden())
					continue;

				rowIndex = field.getRowIndex() - 1;

				final AbstractVaadinFieldGenerator fb = VaadinFieldGeneratorFactory.getFieldGenerator(this, field, i18n);
				final String fragment = fb.getFieldDefinitionFragment(hasOneColumn);

				fieldDefinition.append(fragment);

				// Search for the first row the spacer can be added to!
				if (!hasOneColumn && !field.isSpanCols() && rowIndexForSpacer < 0)
					rowIndexForSpacer = rowIndex;
			}

			b.append("final var " + layoutName + " = new FormLayout();\n");
			b.append(layoutName + ".setId(\"" + panel.getName() + "\");\n");

			if (!addToTabFolder && !panel.isDrawBorder())
				b.append(layoutName + ".setWidthFull();\n");

			if (hasOneColumn)
				b.append(layoutName + ".setResponsiveSteps(new FormLayout.ResponsiveStep(\"1em\", 1));\n");

			b.append("\n");
			b.append(fieldDefinition.toString());
		}

		if (!addToTabFolder) {
			if (panel.isDrawBorder()) {
				importPackage("net.codecadenza.runtime.webclient.vaadin.component");

				b.append("final var " + panel.getName() + " = new BorderPanel(" + i18n.getI18N(panel) + ");\n");
				b.append(panel.getName() + ".setWidthFull();\n");

				if (!hasGrid)
					b.append(panel.getName() + ".setId(\"" + panel.getName() + "\");\n");

				b.append(panel.getName() + ".");
			}

			b.append("add(" + (hasGrid ? gridPanelName : layoutName) + ");\n\n");
		}

		if (!addToTabFolder && panel.isDrawBorder())
			b.append("add(" + panel.getName() + ");\n\n");

		return b.toString();
	}

}
