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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.CSS_MANDATORY_FIELD;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.CSS_OPTIONAL_FIELD;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.shared.Constants.CONVERSION_SUFFIX;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;

/**
 * <p>
 * Generator for fields that display proposal items after entering a filter text
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFProposalTextFieldGenerator extends AbstractJSFFieldGenerator {
	private final DTOBean listDTO;
	private final DTOBeanAttribute pkAttr;
	private final String completeMethodName;
	private boolean hasDisplayAttribute;
	private final String hiddenSelectElement;
	private final String selectUpdateMethod;
	private final Form linkForm;
	private final String openLinkMethodName;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFProposalTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.pkAttr = listDTO.getPKAttribute();
		this.completeMethodName = "onComplete" + field.getDTOAttribute().getUpperCaseName();
		this.openLinkMethodName = "open" + field.getDTOAttribute().getUpperCaseName() + "Link";
		this.hiddenSelectElement = "sel" + field.getDTOAttribute().getUpperCaseName();
		this.selectUpdateMethod = "on" + field.getDTOAttribute().getUpperCaseName() + "Change";
		this.linkForm = JSFGeneratorUtil.getFormForLink(field);
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);

		if (listDTO.getDisplayAttribute() != null)
			this.hasDisplayAttribute = true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible() || field.isReadonly())
			return;

		formGenerator.importPackage("java.util");

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		if (linkForm != null) {
			formGenerator.importPackage("org.primefaces.event");

			if (pkAttr.getDomainAttribute().getJavaType().isString())
				formGenerator.importPackage("java.io");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		if (linkForm != null)
			formGenerator.addPrivateField(pkAttr.getDomainAttribute().getJavaType().getName(), hiddenSelectElement).create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		final var b = new StringBuilder();

		if (!field.isVisible() || linkForm == null)
			return "";

		if (!field.isMandatory())
			b.append("\nif(" + modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + " != null)\n");

		b.append(hiddenSelectElement + " = " + modelObjectName + ".");
		b.append(field.getDTOAttribute().getModelGetterName() + "." + pkAttr.getModelGetterName());
		b.append(";\n");

		if (!field.isMandatory()) {
			b.append("else\n");
			b.append(hiddenSelectElement + " = " + pkAttr.getDomainAttribute().getEmptyItemDefaultValue() + ";\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.AbstractJSFFieldGenerator#addFieldMethods()
	 */
	@Override
	public void addFieldMethods() {
		if (!field.isVisible())
			return;

		final String pkTypeName = pkAttr.getDomainAttribute().getJavaType().getName();

		if (linkForm != null)
			formGenerator.addGetterAndSetter(pkTypeName, hiddenSelectElement, "the id of the selected element");

		if (field.isReadonly())
			return;

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Callback method for auto-complete field '" + field.getName() + "'\n");
		b.append(" * @param filter the filter criterion inserted the by the user\n");
		b.append(" * @return the proposal list\n");
		b.append(" */\n");
		b.append(formGenerator.getAnnotationForGeneratedElement());
		b.append("public List<" + listDTO.getModelClassName() + "> " + completeMethodName + "(String filter)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation("filter");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while fetching data for proposal text field '" + field.getName() + "'!", "e");

		b.append("\n");
		b.append("return Collections.emptyList();\n");
		b.append("}\n");
		b.append("}\n\n");

		formGenerator.addMethod("List<" + listDTO.getModelClassName() + "> " + completeMethodName + "(String filter)", b.toString());

		if (linkForm != null) {
			final var methodSignature = JavaType.VOID + " " + selectUpdateMethod + "(SelectEvent<" + listDTO.getModelClassName()
					+ "> event)";
			final String filterParam;

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * @param event\n");
			b.append(" */\n");
			b.append(formGenerator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");
			b.append("final ");

			if (hasDisplayAttribute)
				b.append(JavaType.STRING);
			else
				b.append(pkTypeName);

			b.append(" selection = event.getObject().");

			if (hasDisplayAttribute)
				b.append(listDTO.getDisplayAttribute().getModelGetterName() + ";\n");
			else
				b.append(pkAttr.getModelGetterName() + ";\n");

			b.append("final " + listDTO.getModelClassName() + " item = ");

			if (hasDisplayAttribute)
				filterParam = "selection";
			else
				filterParam = pkAttr.getDomainAttribute().convertToString("selection");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation(true, filterParam);

			b.append(".stream().findFirst().orElseThrow();\n\n");
			b.append(modelObjectName + "." + field.getDTOAttribute().getModelSetterName() + "(item);\n");
			b.append(hiddenSelectElement + " = item." + pkAttr.getModelGetterName() + ";\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while setting selected proposal item!", "e");

			b.append("}\n");
			b.append("}\n\n");

			formGenerator.addMethod(methodSignature, b.toString());

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * @return the navigation target\n");
			b.append(" */\n");
			b.append(formGenerator.getAnnotationForGeneratedElement());
			b.append("public " + JavaType.STRING + " " + openLinkMethodName + "()\n");
			b.append("{\n");
			b.append("// Check if an appropriate element is selected!\n");

			final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();

			if (pkType.isString()) {
				b.append("if(" + hiddenSelectElement + " == null || " + hiddenSelectElement + ".isEmpty())\n");
				b.append("{\n");
				b.append("// Remove last entry from history list!\n");
				b.append(USER_SESSION_BEAN + ".getLastPage();\n");
				b.append("return \"\";\n");
				b.append("}\n\n");
				b.append("return " + linkForm.getName() + ".PAGE_INIT_URL + ");
				b.append("java.net.URLEncoder.encode(" + hiddenSelectElement + ", java.nio.charset.StandardCharsets.UTF_8);\n");
			}
			else {
				b.append("if(" + hiddenSelectElement + " == " + pkAttr.getDomainAttribute().getEmptyItemDefaultValue() + ")\n");
				b.append("{\n");
				b.append("// Remove last entry from history list!\n");
				b.append(USER_SESSION_BEAN + ".getLastPage();\n");
				b.append("return \"\";\n");
				b.append("}\n\n");
				b.append("return " + linkForm.getName() + ".PAGE_INIT_URL + " + hiddenSelectElement + ";\n");
			}

			b.append("}\n\n");

			formGenerator.addMethod(JavaType.STRING + " " + openLinkMethodName + "()", b.toString());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		boolean linkAdded = false;

		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String converterName = JSFGeneratorUtil.createManagedBeanName(listDTO.getModelClassName() + CONVERSION_SUFFIX);
		DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();
		final var securityHelper = new JSFSecurityGenerator(project);
		final var varName = field.getName() + "Item";

		if (displayAttr == null)
			displayAttr = pkAttr;

		b.append(fillGridColumn(field, hasOneColumn, true));

		if (linkForm != null) {
			b.append("\t\t<p:commandLink" + securityHelper.addSecurityCode(linkForm.getRoles()));

			// Currently using a style class doesn't work as expected!
			b.append("id=\"" + field.getName() + "Link\" style=\"font-weight: bold;color: #585858\" ");

			// For some form types we must initialize the link in a different way! This needs deeper analysis...
			if (field.getPanel().getForm().getFormType() == FormTypeEnumeration.ADD
					|| field.getPanel().getForm().getFormType() == FormTypeEnumeration.CREATE)
				b.append("ajax=\"false\" immediate=\"true\" ");

			b.append("action=\"#{" + managedBeanName + "." + openLinkMethodName + "}\">\n");
			b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
			b.append(managedBeanName + ".currentPageURL}\"/>\n");
			b.append("\t\t\t<h:outputText value=\"" + i18n.getI18N(field));

			if (field.isMandatory() && !field.isReadonly())
				b.append(" *");

			b.append(":\" ");
			b.append("styleClass=\"");

			if (field.isMandatory())
				b.append(CSS_MANDATORY_FIELD);
			else
				b.append(CSS_OPTIONAL_FIELD);

			b.append("\"/>\n");
			b.append("\t\t</p:commandLink>\n");

			// If security is enabled we must check if the user has permission to open the target form!
			if (securityHelper.isSecurityAdded()) {
				// If the user is not allowed to open the respective form we just display a label!
				final String renderCheck = securityHelper.addSecurityCode(linkForm.getRoles()).replace("{", "{!");

				b.append(JSFGeneratorUtil.createFieldLabel(field, i18n, null, renderCheck));
			}

			linkAdded = true;
		}
		else
			b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));

		if (!field.isReadonly()) {
			int minQueryLength = 1;

			if (displayAttr.getDomainAttribute().getJavaType().isString())
				minQueryLength = 2;

			b.append("\t\t<p:autoComplete ");
			b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName + ".");
			b.append(field.getDTOAttribute().getModelFieldName() + "}\" ");
			b.append("converter=\"#{" + converterName + "}\" minQueryLength=\"" + minQueryLength + "\" forceSelection=\"true\" ");
			b.append("completeMethod=\"#{" + managedBeanName + "." + completeMethodName + "}\" ");
			b.append("var=\"" + varName + "\" itemValue=\"#{" + varName + "}\" itemLabel=\"#{");
			b.append(varName + "." + displayAttr.getModelFieldName() + "}\">\n");

			if (linkAdded) {
				b.append("\t\t\t<p:ajax event=\"itemSelect\" listener=\"#{" + managedBeanName + "." + selectUpdateMethod);
				b.append("}\" update=\":form:" + hiddenSelectElement + "\"/>\n");
			}

			if (field.isMandatory() && (project.isBoundaryMode() || project.getValidationType() == ValidationTypeEnumeration.INTERNAL))
				b.append("\t\t\t<f:validateRequired/>\n");

			b.append("\t\t</p:autoComplete>\n");
		}
		else {
			b.append("\t\t<p:inputText readonly=\"true\" ");
			b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName + ".");
			b.append(field.getDTOAttribute().getModelFieldName() + "." + displayAttr.getModelFieldName() + "}\"/>\n");
		}

		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
