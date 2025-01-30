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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form.field;

import java.util.Optional;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
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
public class VaadinTextFieldGenerator extends AbstractVaadinFieldGenerator {
	private final JavaType type;
	private final DomainAttribute attr;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.attr = field.getDTOAttribute().getDomainAttribute();
		this.type = attr.getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		return "TextField";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("com.vaadin.flow.component.textfield");

		if (!type.isString())
			formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.converter");

		if (!field.isReadonly()) {
			if (!getValidationFragment(false).isEmpty())
				formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.validation");

			if (attr.getTag() == AttributeTagEnumeration.USER_PASSWORD)
				formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.converter");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#addToolTipFragment(java.lang.
	 * String)
	 */
	@Override
	public String addToolTipFragment(String fieldName) {
		if (attr.getUserComment() != null && !attr.getUserComment().isEmpty())
			return fieldName + ".setTitle(" + i18n.getI18N(attr) + ");\n";

		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		final var b = new StringBuilder();

		if (!field.isVisible() || field.isReadonly())
			return "";

		final var locale = i18n.getLocaleFragment();

		if (type.isString()) {
			final Optional<Integer> minLength = attr.getMinFieldLength();
			final Optional<Integer> maxLength = attr.getMaxFieldLenght();

			if (minLength.isPresent() || maxLength.isPresent()) {
				b.append(".withValidator(new StringLengthValidator(");

				if (minLength.isPresent())
					b.append(minLength.get());
				else
					b.append("null");

				b.append(", ");

				if (maxLength.isPresent())
					b.append(maxLength.get());
				else
					b.append("null");

				b.append(", " + locale + "))");
			}

			if (!attr.getDomainAttributeValidator().getRegularExpression().isEmpty()) {
				b.append(".withValidator(new StringRegExValidator(\"");
				b.append(attr.getDomainAttributeValidator().getRegularExpression() + "\", " + locale + "))");
			}
		}

		if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()
				|| !attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
			var typeSuffix = "";

			if (type.isLong()) {
				b.append(".withValidator(new LongValidator(");
				typeSuffix = "L";
			}
			else if (type.isFloat()) {
				b.append(".withValidator(new FloatValidator(");
				typeSuffix = "F";
			}
			else if (type.isDouble()) {
				b.append(".withValidator(new DoubleValidator(");
				typeSuffix = "D";
			}
			else if (type.isInteger())
				b.append(".withValidator(new IntegerValidator(");
			else if (type.isBigDecimal())
				b.append(".withValidator(new BigDecimalValidator(");

			if (type.isIntegerOrLong() || type.isFloat() || type.isDouble()) {
				if (!attr.getDomainAttributeValidator().getMinValue().isEmpty())
					b.append(attr.getDomainAttributeValidator().getMinValue() + typeSuffix + ", ");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append(attr.getDomainAttributeValidator().getMaxValue() + typeSuffix);
				else
					b.append("null");

				b.append(", " + locale);
				b.append("))");
			}
			else if (type.isBigDecimal()) {
				if (!attr.getDomainAttributeValidator().getMinValue().isEmpty())
					b.append("new java.math.BigDecimal(\"" + attr.getDomainAttributeValidator().getMinValue() + "\"), ");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append("new java.math.BigDecimal(\"" + attr.getDomainAttributeValidator().getMaxValue() + "\")");
				else
					b.append("null");

				b.append(", " + locale);
				b.append("))");
			}
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getConversionFragment()
	 */
	@Override
	public String getConversionFragment() {
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		if (!attr.getJavaType().isPrimitive() && !attr.getJavaType().isString())
			b.append(".withNullRepresentation(\"\")\n");

		if (field.isMandatory()) {
			final var validationMessage = "Field must not be empty!";

			b.append(".asRequired(" + i18n.getI18NMessage("msg_err_empty_field", validationMessage) + ")");
		}

		final var locale = i18n.getLocaleFragment();

		if (!field.isReadonly() && attr.getTag() == AttributeTagEnumeration.USER_PASSWORD)
			b.append(".withConverter(new PasswordEncryptionConverter())");

		if (type.isDouble())
			b.append(".withConverter(new StringToDoubleConverter(preferences.getNumberFormat(), " + locale + "))");
		else if (type.isFloat())
			b.append(".withConverter(new StringToFloatConverter(preferences.getNumberFormat(), " + locale + "))");
		else if (type.isBigDecimal())
			b.append(".withConverter(new StringToBigDecimalConverter(preferences.getNumberFormat(), " + locale + "))");
		else if (type.isInteger())
			b.append(".withConverter(new StringToIntegerConverter(" + locale + "))");
		else if (type.isLong())
			b.append(".withConverter(new StringToLongConverter(" + locale + "))");
		else if (type.isChar())
			b.append(".withConverter(new StringToCharacterConverter())");
		else if (type.isUUID())
			b.append(".withConverter(new StringToUuidConverter(" + locale + "))");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#
	 * getDefaultValueInitialization()
	 */
	@Override
	public String getDefaultValueInitialization() {
		if (field.getDefaultValue() == null || field.getDefaultValue().isEmpty())
			return "";

		final var fullSetter = modelObjectName + "." + field.getDTOAttribute().getModelSetterName();

		return fullSetter + "(" + field.getConvertedDefaultValue() + ");\n";
	}

}
