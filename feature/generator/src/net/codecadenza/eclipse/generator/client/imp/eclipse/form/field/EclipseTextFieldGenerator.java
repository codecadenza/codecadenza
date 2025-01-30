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

import net.codecadenza.eclipse.generator.client.common.field.RichClientTextFieldHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for text fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseTextFieldGenerator extends AbstractEclipseFieldGenerator {
	private final JavaType javaType;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.javaType = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final FormTypeEnumeration type = field.getPanel().getForm().getFormType();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (type == FormTypeEnumeration.READONLY)
			return;

		if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME || attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE)
			formGenerator.importPackage("java.io");

		if (attr.getTag() == AttributeTagEnumeration.USER_PASSWORD) {
			if (!field.isVisible() || field.isReadonly()) {
				if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
					formGenerator.importPackage("net.codecadenza.runtime.crypto");
			}
			else
				formGenerator.importPackage("net.codecadenza.runtime.crypto");
		}

		if (javaType.isBigDecimal()) {
			if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
				formGenerator.importPackage("java.math");

			if (field.isVisible() && !field.isReadonly())
				formGenerator.importPackage("java.math");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#needsDecimalFormatter()
	 */
	@Override
	public boolean needsDecimalFormatter() {
		if (!field.isVisible())
			return false;

		return javaType.isDecimalNumber();
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
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		return new RichClientTextFieldHelper(this).getValidationFragment(hasTitleArea);
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
		final String checkStatement = getCheckFragment();
		final DomainAttribute domainAttribute = field.getDTOAttribute().getDomainAttribute();

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append(field.getName() + ".setText(" + domainAttribute.convertToString(modelObjectName + "." + getter) + ");\n");

		if (!checkStatement.isEmpty())
			b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return new RichClientTextFieldHelper(this).getCreateInitializationFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		return new RichClientTextFieldHelper(this).getSaveDataFragment(formGenerator, objectName);
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

		if (field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT) {
			if (field.isReadonly()) {
				b.append(field.getName() + " = new Text(" + panel.getName() + ", SWT.BORDER | SWT.READ_ONLY);\n");
				b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));\n");
			}
			else
				b.append(field.getName() + " = new Text(" + panel.getName() + ", SWT.BORDER);\n");
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
			b.append(getFieldLabelName() + ".setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));\n\n");
			b.append(field.getName() + " = new Text(" + panel.getName());

			if (field.isReadonly()) {
				b.append(", SWT.V_SCROLL | SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY);\n");
				b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));\n");
			}
			else
				b.append(", SWT.V_SCROLL | SWT.MULTI | SWT.WRAP | SWT.BORDER);\n");
		}

		return b.toString();
	}

}
