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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field;

import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.LOV_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_LOV;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for list-of-values fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseListOfValuesFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean lovDTO;
	private final BoundaryBean boundaryBean;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseListOfValuesFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.lovDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(lovDTO.getDomainObject());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isReadonly()) {
			formGenerator.importPackage("org.eclipse.swt.events");
			formGenerator.importClass("org.eclipse.jface.dialogs.Dialog");

			// Import the package that contains all list-of-values dialogs
			formGenerator.importPackage(project.getClientNamespace().toString() + PACK_CLIENT_LOV);

			if (project.isBoundaryMode())
				formGenerator.importPackage(lovDTO.getNamespace().toString());
			else
				formGenerator.importPackage(lovDTO.getDomainObject().getNamespace().toString());
		}

		if (field.isAddFormLinkToLabel()) {
			formGenerator.importPackage("org.eclipse.swt.events");
			formGenerator.addImports(new EclipseSecurityHelper(project).getSecurityImports());
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

		formGenerator.addPrivateField("Text", field.getName()).create();

		if (!field.isReadonly()) {
			final DTOBeanAttribute pkAttr = lovDTO.getPKAttribute();
			final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();
			final var initValue = pkType.isString() ? "\"\"" : "";
			String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
			lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

			if (pkType.getNamespace() != null)
				formGenerator.importPackage(pkType.getNamespace().toString());

			formGenerator.addPrivateField(pkType.getName(), lovIdValue).withDefaultValue(initValue).create();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		final var b = new StringBuilder();
		final String validationMessage = i18n.getI18NMessage("msg_err_no_selection", "Field \"{0}\" requires selection!",
				FIELD_LABEL_VALIDATION);

		// Do not check fields that are either invisible, read-only or optional!
		if (!field.isVisible() || field.isReadonly() || !field.isMandatory())
			return "";

		// It makes no sense to add the validation fragment if the field belongs to a form of type 'UPDATE' as it cannot be deselected
		// in the client!
		if (field.getPanel().getForm().getFormType() == FormTypeEnumeration.UPDATE)
			return "";

		b.append("// Check list-of-values field '" + field.getName() + "'\n");
		b.append("if(" + field.getName() + ".getText().isEmpty())\n");
		b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
		b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String getter = field.getDTOAttribute().getModelGetterName();
		final DTOBeanAttribute pkAttr = lovDTO.getPKAttribute();
		DTOBeanAttribute attr = lovDTO.getDisplayAttribute();
		final String pkGetter = pkAttr.getModelGetterName();

		// If no display attribute exists we have to use the primary key attribute!
		if (attr == null)
			attr = lovDTO.getPKAttribute();

		final String fieldGetter = attr.getModelGetterName();
		final String checkStatement = getCheckFragment();
		final String attributeGetter = modelObjectName + "." + getter + "." + fieldGetter;

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);

		if (!checkStatement.isEmpty())
			b.append("{\n");

		b.append(field.getName() + ".setText(" + attr.getDomainAttribute().convertToString(attributeGetter) + ");\n");

		if (!field.isReadonly()) {
			String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
			lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

			b.append(lovIdValue + " = " + modelObjectName + "." + getter + "." + pkGetter + ";\n");
		}

		if (!checkStatement.isEmpty())
			b.append("}\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (field.isReadonly() || !field.isVisible())
			return "";

		String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
		lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

		if (!field.isMandatory())
			b.append("\nif(!" + field.getName() + ".getText().isEmpty())\n");

		if (project.isBoundaryMode())
			b.append(objectName + "." + setter + "(new " + lovDTO.getModelClassName() + "(" + lovIdValue + "));\n");
		else {
			final RepositoryMethod method = boundaryBean.getRepository().getMethodByType(RepositoryMethodTypeEnumeration.FIND_BY_ID);

			// Find and attach the selected domain object
			b.append(objectName + "." + setter + "(");

			new ServiceInvocationGenerator(method, b).addInvocation(true, lovIdValue);

			b.append(");\n");
		}

		if (!field.isMandatory()) {
			b.append("else\n");
			b.append(objectName + "." + setter + "(null);\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();
		final String toolTipFragment = addToolTipFragment();

		b.append(field.getName() + " = new Text(" + panel.getName() + ", SWT.BORDER);\n");
		b.append(field.getName() + ".setEditable(false);\n");

		if (!field.isReadonly()) {
			if (toolTipFragment.isEmpty()) {
				b.append(field.getName() + ".setToolTipText(");
				b.append(i18n.getI18NMessage("lov_tooltip", "Double-click to open selection!") + ");\n");
			}

			b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW));\n\n");
			b.append(field.getName() + ".addMouseListener(new MouseAdapter()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void mouseDoubleClick(MouseEvent e)\n");
			b.append("{\n");

			final Form lovForm = field.getListOfValues();
			final var mto = (ManyToOneAssociation) field.getDTOAttribute().getAssociation();

			b.append("final var lov = new " + lovForm.getName() + "(parentShell, ");
			b.append(field.getName() + ".getText(), false, " + mto.isOptional() + ");\n");
			b.append("final int returnCode = lov.open();\n\n");
			b.append("if(returnCode == Dialog.CANCEL)\n");
			b.append("return;\n\n");

			final DTOBeanAttribute dtoPkAttr = lovDTO.getPKAttribute();
			final JavaType pkType = dtoPkAttr.getDomainAttribute().getJavaType();
			String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
			lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);
			final DTOBeanAttribute displayAttr = lovDTO.getDisplayAttribute();

			if (!pkType.isString()) {
				b.append("if(lov.getIdValue() != null)\n");
				b.append(lovIdValue + " = " + dtoPkAttr.getDomainAttribute().convertFromString("lov.getIdValue()") + ";\n");
				b.append("else\n");
				b.append(lovIdValue + " = " + pkType.getLocalVariableDefaultValue() + ";\n");
			}
			else
				b.append(lovIdValue + " = lov.getIdValue();\n");

			b.append("\n");

			if (displayAttr == null) {
				b.append("if(lov.getIdValue() != null)\n");
				b.append(field.getName() + ".setText(lov.getIdValue());\n");
			}
			else {
				b.append("if(lov.getDisplayValue() != null)\n");
				b.append(field.getName() + ".setText(lov.getDisplayValue());\n");
			}

			b.append("else\n");
			b.append(field.getName() + ".setText(\"\");\n");
			b.append("}\n");
			b.append("});\n\n");
		}
		else
			b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));\n");

		return b.toString();
	}

}
