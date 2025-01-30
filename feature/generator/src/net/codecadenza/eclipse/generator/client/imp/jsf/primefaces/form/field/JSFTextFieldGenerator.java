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

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;

import java.util.Optional;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;

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
public class JSFTextFieldGenerator extends AbstractJSFFieldGenerator {
	private final JavaType type;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFTextFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.type = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));

		if (field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT)
			b.append("\t\t<p:inputText ");
		else
			b.append("\t\t<p:inputTextarea style=\"width:200px;height:80px;\" ");

		b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName);
		b.append("." + field.getDTOAttribute().getModelFieldName() + "}\"");

		if (field.isReadonly())
			b.append(" readonly=\"true\"");

		b.append(">\n");

		if (type.isDecimalNumber())
			b.append("\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
		else if (type.isUUID()) {
			b.append("\t\t\t<f:converter converterId=\"");
			b.append("net.codecadenza.runtime.webclient.primefaces.converter.UUIDConverter\"/>\n");
		}

		if (project.isBoundaryMode() || project.getValidationType() == ValidationTypeEnumeration.INTERNAL)
			b.append(getValidationFragment(false));

		if (field.getFieldType() == FormFieldTypeEnumeration.SIMPLE_TEXT)
			b.append("\t\t</p:inputText>\n");
		else
			b.append("\t\t</p:inputTextarea>\n");

		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String dtoName) {
		final var b = new StringBuilder();

		if (field.isReadonly())
			return "";

		// If the field isn't visible but a default value exists we must check if the field value represents a password!
		if (!field.isVisible() && (field.getDefaultValue() == null || field.getDefaultValue().isEmpty()))
			return "";

		if (field.getDTOAttribute().getDomainAttribute().getTag() != AttributeTagEnumeration.USER_PASSWORD)
			return "";

		final String setter = field.getDTOAttribute().getModelSetterName();

		b.append("\n");
		b.append("// Encrypt password\n");
		b.append("try\n");
		b.append("{\n");
		b.append(modelObjectName + "." + setter + "(HashGenerator.encryptSHA256(");
		b.append(modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + "));\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while encrypting password!", "e");

		b.append("}\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		if (field.getDefaultValue() == null || field.getDefaultValue().isEmpty())
			return "";

		final String setter = field.getDTOAttribute().getModelSetterName();

		return modelObjectName + "." + setter + "(" + field.getConvertedDefaultValue() + ");\n";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (field.isReadonly())
			return;

		if (!field.isVisible() && (field.getDefaultValue() == null || field.getDefaultValue().isEmpty()))
			return;

		if (field.getDTOAttribute().getDomainAttribute().getTag() != AttributeTagEnumeration.USER_PASSWORD)
			return;

		formGenerator.importPackage("net.codecadenza.runtime.crypto");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		final var b = new StringBuilder();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		// Do not check fields that are either invisible or read-only!
		if (!field.isVisible() || field.isReadonly())
			return "";

		if (field.isMandatory())
			b.append("\t\t\t<f:validateRequired/>\n");

		if (type.isString()) {
			final Optional<Integer> minLength = attr.getMinFieldLength();
			final Optional<Integer> maxLength = attr.getMaxFieldLenght();

			if (minLength.isPresent() || maxLength.isPresent()) {
				b.append("\t\t\t<f:validateLength");

				if (minLength.isPresent())
					b.append(" minimum=\"" + minLength.get() + "\"");

				if (maxLength.isPresent())
					b.append(" maximum=\"" + maxLength.get() + "\"");

				b.append("/>\n");
			}

			if (!attr.getDomainAttributeValidator().getRegularExpression().isEmpty())
				b.append("\t\t\t<f:validateRegex pattern=\"" + attr.getDomainAttributeValidator().getRegularExpression() + "\"/>\n");
		}
		else if (type.isChar())
			b.append("\t\t\t<f:validateLength maximum=\"1\"/>\n");
		else if (type.isIntegerOrLong()) {
			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				b.append("\t\t\t<f:validateLongRange");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append(" maximum=\"" + attr.getDomainAttributeValidator().getMaxValue() + "\"");

				if (!attr.getDomainAttributeValidator().getMinValue().isEmpty())
					b.append(" minimum=\"" + attr.getDomainAttributeValidator().getMinValue() + "\"");

				b.append("/>\n");
			}
		}
		else if (type.isType(JavaType.FLOAT, JavaType.FLOAT_OBJ, JavaType.DOUBLE, JavaType.DOUBLE_OBJ)
				&& (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
						|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())) {
			b.append("\t\t\t<f:validateDoubleRange");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
				b.append(" maximum=\"" + attr.getDomainAttributeValidator().getMaxValue() + "\"");

			if (!attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append(" minimum=\"" + attr.getDomainAttributeValidator().getMinValue() + "\"");

			b.append("/>\n");
		}

		return b.toString();
	}

}
