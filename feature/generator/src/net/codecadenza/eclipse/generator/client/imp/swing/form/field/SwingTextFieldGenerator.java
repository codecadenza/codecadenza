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
package net.codecadenza.eclipse.generator.client.imp.swing.form.field;

import net.codecadenza.eclipse.generator.client.common.field.RichClientTextFieldHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
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
public class SwingTextFieldGenerator extends AbstractSwingFieldGenerator {
	private final JavaType type;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public SwingTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.type = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final FormTypeEnumeration formType = field.getPanel().getForm().getFormType();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (formType == FormTypeEnumeration.READONLY)
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

		if (type.isBigDecimal()) {
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

		return type.isDecimalNumber();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		if (field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
			formGenerator.addPrivateField("JTextPane", field.getName()).create();
		else
			formGenerator.addPrivateField("JTextField", field.getName()).create();
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
		final String getter = modelObjectName + "." + field.getDTOAttribute().getModelGetterName();
		final String checkStatement = getCheckFragment();

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append(field.getName() + ".setText(" + field.getDTOAttribute().getDomainAttribute().convertToString(getter) + ");\n");

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

}
