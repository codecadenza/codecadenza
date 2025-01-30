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
package net.codecadenza.eclipse.generator.client.imp.javafx.form.field;

import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;

import net.codecadenza.eclipse.generator.client.common.converter.DateConversionGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for date time fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXDateTimeFieldGenerator extends AbstractJavaFXFieldGenerator {
	private final JavaType type;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JavaFXDateTimeFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.type = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#needsDateTimeFormatter()
	 */
	@Override
	public boolean needsDateTimeFormatter() {
		return field.isVisible();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("TextField", field.getName()).create();
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

		b.append("// Check field that is mapped to attribute '" + field.getDTOAttribute().getDomainAttribute().getLabel() + "'\n");
		b.append("inputToCheck = " + field.getName() + ".getText();\n\n");

		if (field.isMandatory()) {
			final String validationMessage = i18n.getI18NMessage("msg_err_empty_field", "Field \"{0}\" must not be empty!",
					FIELD_LABEL_VALIDATION);

			b.append("if(inputToCheck.isEmpty())\n");
			b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}

		b.append("if(!inputToCheck.isEmpty())\n");
		b.append("try\n");
		b.append("{\n");

		if (!attr.getDomainAttributeValidator().isFutureDate() && !attr.getDomainAttributeValidator().isPastDate())
			b.append("dateTimeFormat.parse(inputToCheck);\n");

		if (attr.getDomainAttributeValidator().isFutureDate()) {
			final String validationMessage = i18n.getI18NMessage("msg_err_future_date", "Field \"{0}\" must represent a future date!",
					FIELD_LABEL_VALIDATION);

			b.append("if(" + PACK_JAVA_TIME + ".LocalDateTime.from(dateTimeFormat.parse(inputToCheck)).isBefore(");
			b.append(PACK_JAVA_TIME + ".LocalDateTime.now()))\n");
			b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
		}

		if (attr.getDomainAttributeValidator().isPastDate()) {
			final String validationMessage = i18n.getI18NMessage("msg_err_past_date",
					"Field \"{0}\" must represent a date in the past!", FIELD_LABEL_VALIDATION);

			b.append("if(" + PACK_JAVA_TIME + ".LocalDateTime.from(dateTimeFormat.parse(inputToCheck)).isAfter(");
			b.append(PACK_JAVA_TIME + ".LocalDateTime.now()))\n");
			b.append(getFieldValidationMessageFragment(validationMessage, hasTitleArea));
		}

		final String validationMessage = i18n.getI18NMessage("msg_err_invalid_date",
				"Field \"{0}\" doesn't represent a valid date value!", FIELD_LABEL_VALIDATION);

		b.append("}\n");
		b.append("catch (final Exception p)\n");
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
		final String checkStatement = getCheckFragment();

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append(field.getName() + ".setText(dateTimeFormat.format(");
		b.append(DateConversionGenerator.toInstant(field, modelObjectName + "." + getter));
		b.append("));\n");

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
		if (!field.isVisible() || field.getDefaultValue() == null || field.getDefaultValue().isEmpty())
			return "";

		final var b = new StringBuilder();
		b.append(field.getName() + ".setText(dateTimeFormat.format(" + PACK_JAVA_TIME + ".LocalDateTime.now()));\n");

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
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (attr.isSetDateOnPersist() || attr.isSetDateOnUpdate())
			return "";

		if (!field.isVisible() || field.isReadonly()) {
			// Set the default values of invisible or read-only fields
			if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty()) {
				if (type.isDateOrCalendar())
					b.append(objectName + "." + setter + "(new " + type.getNamespace().toString() + "." + type.getName() + "());\n");
				else
					b.append(objectName + "." + setter + "(" + type.getNamespace().toString() + "." + type.getName() + ".now());\n");
			}
		}
		else {
			b.append("\n");

			if (!field.isMandatory())
				b.append("if(!" + field.getName() + ".getText().isEmpty())\n");

			b.append(objectName + "." + setter + "(" + type.getNamespace().toString() + "." + type.getName() + ".from(");

			if (type.isDateOrCalendar())
				b.append(PACK_JAVA_TIME + ".");

			if (type.isDate()) {
				b.append("LocalDateTime.from(dateTimeFormat.parse(" + field.getName());
				b.append(".getText())).atZone(" + PACK_JAVA_TIME + ".ZoneId.systemDefault()).toInstant()");
			}
			else if (type.isCalendar()) {
				b.append("LocalDateTime.from(dateTimeFormat.parse(" + field.getName());
				b.append(".getText())).atZone(" + PACK_JAVA_TIME + ".ZoneId.systemDefault())");
			}
			else if (type.isLocalDateTime())
				b.append("dateTimeFormat.parse(" + field.getName() + ".getText())");

			b.append("));\n");

			if (!field.isMandatory()) {
				b.append("else\n");
				b.append(objectName + "." + setter + "(null);\n");
			}

			b.append("\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		b.append(field.getName() + " = new TextField();\n");

		if (field.isReadonly())
			b.append(field.getName() + ".setEditable(false);\n");

		b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
