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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.FORM_TITLE;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.SEL_OBJ_ID;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.UI_DIALOG_FOLDER;

import java.util.HashSet;
import net.codecadenza.eclipse.generator.client.common.form.AbstractSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.file.JSFDownloadGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.file.JSFUploadGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.JSFFieldGeneratorFactory;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.JSFIntLinkFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
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
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for single-record forms of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFSingleRecordFormGenerator extends AbstractSingleRecordFormGenerator {
	private final JSFI18NGenerator i18n;
	private final JSFSecurityGenerator securityHelper;
	private final JavaType idAttributeType;
	private final JSFUploadGenerator uploadGenerator;
	private BoundaryMethod boundaryMethod;

	/**
	 * Constructor
	 * @param form
	 */
	public JSFSingleRecordFormGenerator(Form form) {
		super(form);

		this.securityHelper = new JSFSecurityGenerator(project);
		this.i18n = new JSFI18NGenerator(project);
		this.idAttributeType = dto.getPKAttribute().getDomainAttribute().getJavaType();
		this.uploadGenerator = new JSFUploadGenerator(this, form);

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.CREATE || a.getType() == ActionType.UPDATE || a.getType() == ActionType.READ) {
				this.boundaryMethod = a.getBoundaryMethod();
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		if (project.isBoundaryMode())
			importPackage(dto.getNamespace().toString());
		else
			importPackage(dto.getDomainObject().getNamespace().toString());

		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS);
		importStatic(project.getClientNamespace().toString() + "." + USER_SESSION_BEAN_TYPE);
		importPackage(project.getClientNamespace().toString());
		importPackage("java.util");
		importPackage("java.io");
		importPackage("jakarta.servlet.http");
		importPackage("jakarta.faces.context");
		importPackage("jakarta.inject");

		if (formType != FormTypeEnumeration.READONLY) {
			importPackage("net.codecadenza.runtime.webclient.primefaces.util");
			importClass("jakarta.faces.application.FacesMessage");
		}

		if (project.isJakartaEEApplication())
			importPackage("jakarta.faces.view");
		else
			importClass("org.springframework.web.context.annotation.SessionScope");

		// Add imports for file upload operations
		uploadGenerator.addImports();

		// Add imports for file download operations
		new JSFDownloadGenerator(this, form).addImports();

		// Check if further imports for formatting data are necessary
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_PANEL);
				continue;
			}

			panel.getFields().forEach(field -> JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addImports());
		}

		if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) {
			form.getActions().forEach(action -> {
				// Add imports for further data transfer objects that are used when creating initial one-to-many association objects!
				for (final MethodParameter param : action.getBoundaryMethod().getMethodParameters())
					if (param.getType() instanceof final DTOBean paramDTO) {
						String packageName;

						if (project.isBoundaryMode())
							packageName = paramDTO.getNamespace().toString();
						else
							packageName = paramDTO.getDomainObject().getNamespace().toString();

						importPackage(packageName);
					}
			});

			// If we work directly with entities we must import packages of domain objects referenced by one-to-one associations!
			if (!project.isBoundaryMode())
				for (final DTOBeanAttribute attr : dto.getAttributes())
					if (attr.getAssociation() instanceof OneToOneAssociation)
						importPackage(attr.getAssociation().getTarget().getNamespace().toString());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(form.getName()) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@ViewScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class ");
		b.append(form.getName() + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final String pageURL;

		if (formType != FormTypeEnumeration.CREATE)
			pageURL = "\"" + UI_DIALOG_FOLDER + "/" + form.getName() + ".jsf?faces-redirect=true&" + SEL_OBJ_ID + "=\"";
		else
			pageURL = "\"" + UI_DIALOG_FOLDER + "/" + form.getName() + ".jsf?faces-redirect=true\"";

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPublicConstant(JavaType.STRING, "PAGE_INIT_URL", pageURL).create();
		addPrivateField(dto.getModelClassName(), modelObjectName).setTransientModifier(!project.isBoundaryMode()).create();

		new ServiceDeclarationGenerator(this, boundaryMethod.getBoundaryBean()).addField();

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
			addPrivateField(idAttributeType.getName(), SEL_OBJ_ID).create();
		else if (formType == FormTypeEnumeration.ADD)
			addPrivateField(JavaType.STRING, SEL_OBJ_ID).create();

		addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();
		addPrivateField(JavaType.STRING, FORM_TITLE).withDefaultValue("\"\"").create();
		addPrivateField("ResourceBundle", "bundle").withTransientModifier().create();

		additionalDTOs.stream().forEach(
				addDTO -> addPrivateField(addDTO.getModelClassName(), INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName())
						.setTransientModifier(!project.isBoundaryMode()).create());

		final var injectedServices = new HashSet<String>();
		injectedServices.add(boundaryMethod.getBoundaryBean().getInterfaceName());

		// Check if further services should be injected
		for (final FormField field : form.getAllFormFields()) {
			if (formType == FormTypeEnumeration.READONLY)
				continue;

			if (field.getDTOAttribute().getReferencedDTOBean() == null)
				continue;

			if (!field.isVisible())
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

			// We don't need a service for a read-only proposal text field
			if (field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT && field.isReadonly())
				continue;

			final BoundaryBean boundary = project.getBoundaryByDomainObject(listDTO.getDomainObject());
			var listInterfaceName = "";

			if (boundary != null)
				listInterfaceName = boundary.getInterfaceName();

			// We should not inject a service twice!
			if (injectedServices.contains(listInterfaceName))
				continue;

			injectedServices.add(listInterfaceName);

			new ServiceDeclarationGenerator(this, boundary).addField();
		}

		// Add a field for each grid panel
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() == null)
				continue;

			addPrivateField(panel.getBasePanel().getName(), panel.getName()).inject().create();
		}

		// Add further declarations for all form fields
		form.getAllFormFields().forEach(field -> JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addFieldDeclaration());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		// We have to add a public method that returns an empty string if the form contains a combobox using numeric attributes!
		for (final FormField field : form.getAllFormFields())
			if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX && field.getDTOAttribute().getAssociation() != null) {
				final DomainObject target = field.getDTOAttribute().getAssociation().getTarget();

				if (target.getDisplayAttribute() != null)
					continue;

				if (field.getDTOAttribute().getAssociation().getTarget().getPKAttribute().getJavaType().isString())
					continue;

				final var b = new StringBuilder();
				b.append("/**\n");
				b.append(" * @return an empty string to be used within EL-expressions in combo-boxes using numeric attribute values\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public String getEmptyString()\n");
				b.append("{\n");
				b.append("return \"\";\n");
				b.append("}\n\n");

				addMethod("String getEmptyString()", b.toString());
				break;
			}

		addGetterAndSetter(dto.getModelClassName(), modelObjectName, "the model object");
		addGetterAndSetter(JavaType.STRING, FORM_TITLE, "the form title");

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
			addGetterAndSetter(idAttributeType.getName(), SEL_OBJ_ID, "the ID of the selected object");
		else if (formType == FormTypeEnumeration.ADD)
			addGetterAndSetter(JavaType.STRING, SEL_OBJ_ID, "the ID of the parent object");

		// Add further methods for all form fields
		form.getAllFormFields().forEach(field -> JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addFieldMethods());

		additionalDTOs.stream().forEach(addDTO -> {
			final var fieldName = INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName();

			addGetterAndSetter(addDTO.getModelClassName(), fieldName,
					"the object that is used to create an element of a given one-to-many association");
		});

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize dialog\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void initView()\n");
		b.append("{\n");

		addDebugLog(b, "Initialize dialog");

		b.append("\n");
		b.append("bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, " + USER_SESSION_BEAN + ".getLocale());\n");
		b.append(securityHelper.addSecurityCheck(form.getRoles()));
		b.append("\n");
		b.append("try\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			addDebugLog(b, "Fetch data for object with id '{}'", SEL_OBJ_ID);

			b.append("\n");
			b.append(modelObjectName + " = ");

			if (project.isBoundaryMode()) {
				final BoundaryMethod findMethod = boundaryMethod.getBoundaryBean().getBoundaryMethodByReturnType(dto,
						BoundaryMethodTypeEnumeration.FIND_BY_ID);

				new ServiceInvocationGenerator(findMethod, b).addInvocation(SEL_OBJ_ID);
			}
			else {
				final RepositoryMethod findMethod = boundaryMethod.getBoundaryBean().getRepository()
						.getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

				new ServiceInvocationGenerator(findMethod, b).addInvocation(SEL_OBJ_ID, "true");
			}

			b.append(securityHelper.addClientCheck(form));

			// Add the initialization for all form fields
			for (final FormPanel panel : form.getFormPanels()) {
				if (panel.getBasePanel() != null) {
					b.append("\n");
					b.append(panel.getName() + ".setSelectedObjectId(" + SEL_OBJ_ID + ");\n");
					b.append(panel.getName() + ".setCurrentPageURL(" + form.getName() + ".PAGE_INIT_URL + " + SEL_OBJ_ID + ");\n");

					if (panel.getBasePanel().getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
						if (formType == FormTypeEnumeration.READONLY)
							b.append(panel.getName() + ".setReadOnly(true);\n");
						else
							b.append(panel.getName() + ".setReadOnly(false);\n");

					b.append(panel.getName() + ".initView();\n\n");

					continue;
				}

				for (final FormField field : panel.getFields())
					b.append(JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getDefaultInitializationFragment());
			}

			b.append("\n");
			b.append(FORM_TITLE + " = " + i18n.getI18NBundleFragment(form) + " + \" '\" + " + SEL_OBJ_ID + " + \"'\";\n");
		}
		else {
			b.append(modelObjectName + " = new " + dto.getModelClassName() + "();\n");
			b.append(addInitializationOfAdditionalFields());

			// Add the initialization for all form fields
			for (final FormField field : form.getAllFormFields())
				b.append(JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getCreateInitializationFragment());

			b.append("\n");
			b.append(FORM_TITLE + " = " + i18n.getI18NBundleFragment(form) + ";\n");
		}

		b.append("\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Dialog initialization failed!", "e");

		b.append("\n");
		b.append("final FacesContext facesContext = FacesContext.getCurrentInstance();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final String errorMessage = bundle.getString(DIALOG_INIT_FAIL);\n\n");
		b.append("facesContext.getExternalContext().responseSendError(");
		b.append("HttpServletResponse.SC_INTERNAL_SERVER_ERROR, errorMessage);\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		addErrorLog(b, "Failed to send error code!", "ex");

		b.append("}\n\n");
		b.append("facesContext.responseComplete();\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod("void initView()", b.toString());

		boolean needsSaveMethod = true;

		if (formType == FormTypeEnumeration.READONLY)
			needsSaveMethod = false;

		if (needsSaveMethod) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Save model object\n");
			b.append(" * @return the navigation target\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public String save()\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");

			addDebugLog(b, "Perform save operation");

			b.append("\n");

			// Add further expressions concerning saving a bean for all form fields
			for (final FormField field : form.getAllFormFields()) {
				String objectName = modelObjectName;

				if (!field.getDTOAttribute().getDTOBean().equals(dto))
					objectName = INIT_MODEL_OBJ_NAME_PREFIX + field.getDTOAttribute().getDTOBean().getDomainObject().getName();

				b.append(JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getSaveDataFragment(objectName));
			}

			new ServiceInvocationGenerator(boundaryMethod, b).addInvocation(getSaveInvocationParameters());

			b.append("\n");

			if (form.isOpenEditAfterCreate()) {
				boolean formFound = false;

				for (final Form f : project.getAllFormsOfProject())
					if (f.getFormType() == FormTypeEnumeration.UPDATE && form.getDomainObject().equals(f.getDomainObject())) {
						final String getter = f.getDTO().getPKAttribute().getModelGetterName();
						formFound = true;

						b.append("return " + f.getName() + ".PAGE_INIT_URL + " + modelObjectName + "." + getter + ";\n");
						break;
					}

				if (!formFound)
					b.append("return " + USER_SESSION_BEAN + ".getLastPage();\n");
			}
			else
				b.append("return " + USER_SESSION_BEAN + ".getLastPage();\n");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while performing save operation!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_SAVE_FAIL, e);\n");
			b.append("return \"\";\n");
			b.append("}\n");
			b.append("}\n\n");

			addMethod("String save()", b.toString());
		}

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the URL of the current page\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getCurrentPageURL()\n");
		b.append("{\n");

		if (formType != FormTypeEnumeration.CREATE) {
			if (idAttributeType.isString()) {
				b.append("return " + form.getName() + ".PAGE_INIT_URL + ");
				b.append("java.net.URLEncoder.encode(" + SEL_OBJ_ID + ", java.nio.charset.StandardCharsets.UTF_8);\n");
			}
			else
				b.append("return " + form.getName() + ".PAGE_INIT_URL + " + SEL_OBJ_ID + ";\n");
		}
		else
			b.append("return " + form.getName() + ".PAGE_INIT_URL;\n");

		b.append("}\n\n");

		addMethod("String getCurrentPageURL()", b.toString());

		// Add methods for link fields
		final var methodMap = new HashSet<String>();

		for (final FormField field : form.getAllFormFields()) {
			if (field.getFieldType() != FormFieldTypeEnumeration.FORM_LINK)
				continue;

			final var linkFieldGenerator = new JSFIntLinkFieldGenerator(field, this);
			linkFieldGenerator.setModelObjectName(dto.getDomainObject().getLowerCaseName());

			final String signature = linkFieldGenerator.getLinkTargetMethodSignature();

			if (signature.isEmpty())
				continue;

			if (methodMap.contains(signature))
				continue;

			b = new StringBuilder();
			b.append(linkFieldGenerator.getFormLinkNavigationMethod());

			methodMap.add(signature);

			addMethod(signature, b.toString());
		}

		// Add file upload listener methods
		uploadGenerator.addUploadListenerMethods();

		// Add file download methods
		new JSFDownloadGenerator(this, form).addDownloadMethods(SEL_OBJ_ID);

		i18n.save();
	}

	/**
	 * @return the generated content
	 */
	public String createXHTMLForm() {
		final var b = new StringBuilder();
		final var panelsOfFirstRow = new BasicEList<FormPanel>();
		final var panelsOfSecondRow = new BasicEList<FormPanel>();
		final String managedBeanName = JSFGeneratorUtil.createManagedBeanName(form.getName());

		b.append(JSFGeneratorUtil.createCompositeHeader());
		b.append("<f:metadata>\n");
		b.append("\t<f:viewAction action=\"#{" + managedBeanName + ".initView()}\"/>\n");

		if (formType != FormTypeEnumeration.CREATE)
			b.append("\t<f:viewParam name=\"" + SEL_OBJ_ID + "\" value=\"#{" + managedBeanName + "." + SEL_OBJ_ID + "}\"/>\n");

		b.append("</f:metadata>\n\n");
		b.append("<ui:define name=\"title\">#{" + managedBeanName + "." + FORM_TITLE + "}</ui:define>\n\n");
		b.append("<ui:define name=\"content\">\n");
		b.append("\t<p:dialog header=\"#{" + EL_I18N_VAR);
		b.append(".feedback_dialog_header}\" modal=\"true\" id=\"msgBox\" ");
		b.append("visible=\"#{(not empty facesContext.messageList)}\">\n");
		b.append("\t\t<p:messages id=\"messages\" showDetail=\"true\" showSummary=\"true\"/>\n");
		b.append("\t</p:dialog>\n\n");
		b.append("\t<h:panelGrid columns=\"3\">\n");

		if (formType == FormTypeEnumeration.READONLY)
			b.append("\t\t<div class=\"pi pi-file\" style=\"font-size: 2em\"/>\n");
		else
			b.append("\t\t<div class=\"pi pi-file-edit\" style=\"font-size: 2em\"/>\n");

		b.append("\t\t<h:outputText id=\"lblFormTitle\" value=\"&#160;#{" + managedBeanName);
		b.append("." + FORM_TITLE + "}\" styleClass=\"label-form-title\"/>\n");
		b.append("\t\t<p:ajaxStatus style=\"width:16px;height:16px;\">\n");
		b.append("\t\t\t<f:facet name=\"start\">\n");
		b.append("\t\t\t\t<h:graphicImage value=\"/images/ajaxloading.gif\"/>\n");
		b.append("\t\t\t</f:facet>\n\n");
		b.append("\t\t\t<f:facet name=\"complete\">\n");
		b.append("\t\t\t\t<h:outputText value=\"\"/>\n");
		b.append("\t\t\t</f:facet>\n");
		b.append("\t\t</p:ajaxStatus>\n");
		b.append("\t</h:panelGrid>\n\n");

		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == 1)
				panelsOfFirstRow.add(panel);
			else
				panelsOfSecondRow.add(panel);

		if (panelsOfFirstRow.size() > 1) {
			b.append("\t<p:tabView id=\"tabview1\" dynamic=\"" + dynamicTabView(panelsOfFirstRow));
			b.append("\" effect=\"fade\" effectDuration=\"fast\">\n");
		}

		// Sort all panels of the first row
		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		panelsOfFirstRow.forEach(thisPanel -> b.append(addPanel(thisPanel, panelsOfFirstRow.size() > 1)));

		if (panelsOfFirstRow.size() > 1)
			b.append("\t</p:tabView>\n");

		if (!panelsOfSecondRow.isEmpty())
			b.append("\n\t<p/>\n\n");

		if (panelsOfSecondRow.size() > 1) {
			b.append("\t<p:tabView id=\"tabview2\" dynamic=\"" + dynamicTabView(panelsOfSecondRow));
			b.append("\" effect=\"fade\" effectDuration=\"fast\">\n");
		}

		// Sort all panels of the second row
		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		panelsOfSecondRow.forEach(thisPanel -> b.append(addPanel(thisPanel, panelsOfSecondRow.size() > 1)));

		if (panelsOfSecondRow.size() > 1)
			b.append("\t</p:tabView>\n");

		b.append("\n\t<p/>\n\n");
		b.append(uploadGenerator.createDialogFragment());

		int buttonCount = 1;

		if (formType != FormTypeEnumeration.READONLY)
			buttonCount++;

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD
					|| a.getType() == ActionType.DOWNLOAD)
				buttonCount++;

		b.append("\t<h:panelGrid columns=\"" + buttonCount + "\">\n");

		if (formType != FormTypeEnumeration.READONLY) {
			b.append("\t\t<p:commandButton id=\"cmdSave\" action=\"#{" + managedBeanName + ".save}\"");
			b.append(" value=\"#{" + EL_I18N_VAR + ".command_save}\" ajax=\"false\"/>\n");
		}

		b.append("\t\t<p:commandButton id=\"cmdBack\" value=\"#{" + EL_I18N_VAR + ".command_back}\" action=\"#{");
		b.append(USER_SESSION_BEAN + ".getLastPage}\" immediate=\"true\" ajax=\"false\"/>\n");

		// Add command buttons for file upload operations
		b.append(uploadGenerator.createCommandFragment());

		// Add command buttons for file download operations
		b.append(new JSFDownloadGenerator(this, form).createCommandFragment());

		b.append("\t</h:panelGrid>\n");
		b.append(addHiddenFields());

		if (formType == FormTypeEnumeration.READONLY)
			b.append("\n\t<p:defaultCommand target=\":form:cmdBack\"/>\n");

		b.append("\n</ui:define>\n");
		b.append("</ui:composition>\n");

		i18n.save();

		return b.toString();
	}

	/**
	 * Add further hidden fields to the form
	 * @return the generated content
	 */
	private String addHiddenFields() {
		final var b = new StringBuilder();
		boolean isFirstItem = true;

		for (final FormField f : form.getAllFormFields()) {
			boolean addHiddenField = false;
			String elementId = f.getName();
			var value = JSFGeneratorUtil.createManagedBeanName(form.getName()) + ".";
			final DTOBean listDTO = f.getDTOAttribute().getReferencedDTOBean();
			String objectName = modelObjectName;

			if (listDTO == null)
				continue;

			if (!f.getDTOAttribute().getDTOBean().equals(dto))
				objectName = INIT_MODEL_OBJ_NAME_PREFIX + f.getDTOAttribute().getDTOBean().getDomainObject().getName();

			if (!f.isReadonly() && f.getFieldType() == FormFieldTypeEnumeration.LOV && listDTO.getDisplayAttribute() != null) {
				addHiddenField = true;
				value += objectName + "." + f.getDTOAttribute().getModelFieldName() + "." + listDTO.getPKAttribute().getModelFieldName();
			}

			if (f.isAddFormLinkToLabel() && f.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT) {
				addHiddenField = true;
				elementId = "sel" + f.getDTOAttribute().getUpperCaseName();
				value += elementId;
			}

			if (!addHiddenField)
				continue;

			if (isFirstItem) {
				isFirstItem = false;
				b.append("\n");
			}

			b.append("\t<h:inputHidden id=\"" + elementId + "\" ");
			b.append("value=\"#{" + value + "}\"/>\n");
		}

		boolean elementAdded = false;

		// If a panel exists that contains an action of type 'CREATE' (SELECTION_BY_PARENT_FORM) we will have to add another hidden
		// element!
		for (final FormPanel panel : form.getFormPanels()) {
			// We need that element only once!
			if (elementAdded)
				break;

			if (panel.getBasePanel() == null)
				continue;

			for (final FormAction a : panel.getBasePanel().getActions())
				if (a.getType() == ActionType.CREATE && a.getTargetForm() != null
						&& a.getTargetForm().getFormType() == FormTypeEnumeration.ADD) {
					final DTOBeanAttribute pkAttr = dto.getPKAttribute();

					if (isFirstItem) {
						isFirstItem = false;

						b.append("\n");
					}

					b.append("\t<h:inputHidden id=\"" + SEL_OBJ_ID + "\" ");
					b.append("value=\"#{" + JSFGeneratorUtil.createManagedBeanName(form.getName()));
					b.append("." + modelObjectName + "." + pkAttr.getModelFieldName() + "}\"/>\n");

					elementAdded = true;
					break;
				}
		}

		return b.toString();
	}

	/**
	 * Add the panel to the form
	 * @param panel
	 * @param addTab
	 * @return the generated content
	 */
	private String addPanel(FormPanel panel, boolean addTab) {
		final var b = new StringBuilder();
		boolean hasOneColumn = true;

		// Check how many columns are used on this panel
		if (addTab) {
			final var tabId = "tab" + panel.getName().substring(0, 1).toUpperCase() + panel.getName().substring(1);

			b.append("\t<p:tab id=\"" + tabId + "\" title=\"" + i18n.getI18N(panel) + "\">\n");
		}
		else {
			if (panel.isDrawBorder())
				b.append("\t<p:panel>\n");

			b.append("\t<h:panelGrid columns=\"1\">\n");
			b.append("\t\t<h:outputText styleClass=\"header-label\" value=\"" + i18n.getI18N(panel) + "\"/>\n");
			b.append("\t</h:panelGrid>\n\n");
		}

		b.append("\t<h:panelGrid columns=\"");

		if (panel.getBasePanel() == null) {
			int colCount = 3;

			// Check how many columns should be used!
			for (final FormField f : panel.getFields())
				if (f.isVisible() && f.getColIndex() > 1) {
					colCount = 6;
					hasOneColumn = false;
					break;
				}

			b.append(colCount);
		}
		else
			b.append("1");

		b.append("\">\n\n");

		if (panel.getBasePanel() == null) {
			ECollections.sort(panel.getFields(), new FormFieldComparator());

			for (final FormField field : panel.getFields())
				b.append(JSFFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getFieldDefinitionFragment(hasOneColumn));
		}
		else
			b.append("\t\t<ui:include src=\"panel/" + panel.getBasePanel().getName() + ".xhtml\"/>\n\n");

		b.append("\t</h:panelGrid>\n");

		if (addTab)
			b.append("\t</p:tab>\n");
		else if (panel.isDrawBorder())
			b.append("\t</p:panel>\n");

		return b.toString();
	}

	/**
	 * Test if tab-view can be initialized dynamically
	 * @param panelList
	 * @return true (as string) if tabs can be initialized dynamically
	 */
	private String dynamicTabView(EList<FormPanel> panelList) {
		// In case of forms of type 'READONLY' and 'UPATE' the tab-view can be dynamic!
		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
			return "true";

		// If the tab-view has at least one panel that contains regular form fields (e.g. input fields, comboboxes etc.) the tab-view
		// must not be dynamic as respective fields may not be initialized correctly!
		return panelList.stream().filter(panel -> panel.getBasePanel() == null).findFirst().map(panel -> "false").orElse("true");
	}

}
