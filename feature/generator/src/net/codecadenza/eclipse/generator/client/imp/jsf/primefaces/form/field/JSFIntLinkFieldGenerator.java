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

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for fields that open a form by using a link
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFIntLinkFieldGenerator extends AbstractJSFFieldGenerator {
	private Form readonlyForm;
	private DTOBeanAttribute listAttr;
	private final DTOBeanAttribute listPKAttr;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFIntLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listAttr = field.getDTOAttribute().getReferencedDTOBean().getDisplayAttribute();
		this.listPKAttr = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute();

		if (listAttr == null)
			this.listAttr = listPKAttr;

		for (final Form f : project.getAllFormsOfProject()) {
			if (!f.getDomainObject().equals(field.getDTOAttribute().getReferencedDTOBean().getDomainObject()))
				continue;

			if (f.getFormType() == FormTypeEnumeration.READONLY) {
				this.readonlyForm = f;
				break;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		if (readonlyForm == null)
			return;

		if (listPKAttr.getDomainAttribute().getJavaType().isString())
			formGenerator.importPackage("java.io");
	}

	/**
	 * @return the name of the action method
	 */
	public String getLinkTargetMethodName() {
		if (readonlyForm == null)
			return "";

		return "open" + readonlyForm.getName() + "Link";
	}

	/**
	 * @return the action method signature
	 */
	public String getLinkTargetMethodSignature() {
		if (readonlyForm == null)
			return "";

		return "String " + getLinkTargetMethodName() + "()";
	}

	/**
	 * @return the target form
	 */
	public Form getLinkTargetForm() {
		return readonlyForm;
	}

	/**
	 * @return the generated content
	 */
	public String getFormLinkNavigationMethod() {
		final var b = new StringBuilder();

		if (!field.isVisible())
			return b.toString();

		if (readonlyForm == null)
			return b.toString();

		b.append("/**\n");
		b.append(" * @return the navigation target\n");
		b.append(" */\n");
		b.append(formGenerator.getAnnotationForGeneratedElement());
		b.append("public " + getLinkTargetMethodSignature() + "\n");
		b.append("{\n");

		final var accessString = modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + "."
				+ listPKAttr.getModelGetterName();

		if (listPKAttr.getDomainAttribute().getJavaType().isString()) {
			b.append("return " + readonlyForm.getName() + ".PAGE_INIT_URL + ");
			b.append("java.net.URLEncoder.encode(" + accessString + ", java.nio.charset.StandardCharsets.UTF_8);\n");
		}
		else
			b.append("return " + readonlyForm.getName() + ".PAGE_INIT_URL + " + accessString + ";\n");

		b.append("}\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		final var securityHelper = new JSFSecurityGenerator(project);

		if (!field.isVisible())
			return "";

		b.append(fillGridColumn(field, hasOneColumn, true));

		if (readonlyForm != null) {
			b.append(JSFGeneratorUtil.createFieldLabel(field, i18n, field.getName(),
					securityHelper.addSecurityCode(readonlyForm.getRoles())));
			b.append("\t\t<p:commandLink" + securityHelper.addSecurityCode(readonlyForm.getRoles()));
			b.append("id=\"" + field.getName() + "\" ");

			// Currently using a style class doesn't work as expected!
			b.append("style=\"font-weight: bold;color: #0063ac\" ");
			b.append("action=\"#{" + managedBeanName + "." + getLinkTargetMethodName() + "}\">\n");
			b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
			b.append(managedBeanName + ".currentPageURL}\"/>\n");
			b.append("\t\t\t<h:outputText ");
			b.append("value=\"#{" + managedBeanName + "." + modelObjectName + "." + field.getDTOAttribute().getModelFieldName() + ".");
			b.append(listAttr.getModelFieldName() + "}\"/>\n");
			b.append("\t\t</p:commandLink>\n");

			// If security is enabled we must check if the user has permission to open the target form!
			if (securityHelper.isSecurityAdded()) {
				// If the user is not allowed to open the respective form we just display a label!
				final String renderCheck = securityHelper.addSecurityCode(readonlyForm.getRoles()).replace("{", "{!");
				final var altFieldId = field.getName() + "AltLbl";

				b.append(JSFGeneratorUtil.createFieldLabel(field, i18n, altFieldId, renderCheck));
				b.append("\t\t<h:outputText styleClass=\"label-field-value\"" + renderCheck);
				b.append("id=\"" + altFieldId + "\" value=\"#{" + managedBeanName + "." + modelObjectName + ".");
				b.append(field.getDTOAttribute().getModelFieldName() + "." + listAttr.getModelFieldName() + "}\"/>\n");
			}
		}
		else {
			// No appropriate form found! We just generate a label!
			b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));
			b.append("\t\t<h:outputText styleClass=\"label-field-value\" ");
			b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName + ".");
			b.append(field.getDTOAttribute().getModelFieldName() + "." + listAttr.getModelFieldName() + "}\"/>\n");
		}

		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
