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
package net.codecadenza.eclipse.generator.client.common.field;

import static net.codecadenza.eclipse.model.java.JavaType.BIG_DECIMAL;
import static net.codecadenza.eclipse.model.java.JavaType.DOUBLE_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.FLOAT_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.INTEGER;
import static net.codecadenza.eclipse.model.java.JavaType.LONG_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.UUID;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;

import java.util.Optional;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Utility class that creates the validation fragment for text fields of rich-client applications (Swing, JavaFX and Eclipse
 * RCP/RAP)
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RichClientTextFieldHelper {
	private final FormField field;
	private final RichClientI18NGenerator i18n;
	private final AbstractRichClientFieldGenerator fieldGenerator;
	private final JavaType type;

	/**
	 * Constructor
	 * @param fieldGenerator
	 */
	public RichClientTextFieldHelper(AbstractRichClientFieldGenerator fieldGenerator) {
		this.fieldGenerator = fieldGenerator;
		this.i18n = fieldGenerator.getI18n();
		this.field = fieldGenerator.getField();
		this.type = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/**
	 * @param hasTitleArea
	 * @return the generated content
	 */
	public String getValidationFragment(boolean hasTitleArea) {
		final var b = new StringBuilder();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		// Do not check fields that are either invisible or read-only!
		if (!field.isVisible() || field.isReadonly())
			return "";

		if (field.isMandatory()) {
			final String validationMessage = i18n.getI18NMessage("msg_err_empty_field", "Field \"{0}\" must not be empty!",
					FIELD_LABEL_VALIDATION);

			b.append("if(inputToCheck.isEmpty())\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}

		if (type.isString()) {
			final Optional<Integer> minLength = attr.getMinFieldLength();
			final Optional<Integer> maxLength = attr.getMaxFieldLenght();

			if (minLength.isPresent() && minLength.get() > 1) {
				final var messageText = "Length of field \"{0}\" must be greater or equal than {1}!";
				final String length = Integer.toString(minLength.get());
				final String validationMessage = i18n.getI18NMessage("msg_err_min_field_length", messageText,
						FIELD_LABEL_VALIDATION + ", " + length);

				b.append("if(inputToCheck.length() < " + length + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
				b.append("\n");
			}

			if (!attr.getDomainAttributeValidator().getRegularExpression().isEmpty()) {
				// Add a regular expression check
				final var messageText = "Value of field \"{0}\" does not match expression {1}!";
				final String regEx = attr.getDomainAttributeValidator().getRegularExpression();
				final String validationMessage = i18n.getI18NMessage("msg_err_regex", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + regEx + "\"");

				b.append("if(!inputToCheck.matches(\"" + attr.getDomainAttributeValidator().getRegularExpression() + "\"))\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
				b.append("\n");
			}

			if (maxLength.isPresent()) {
				final var messageText = "Length of field \"{0}\" must be less or equal than {1}!";
				final String length = Integer.toString(maxLength.get());
				final String validationMessage = i18n.getI18NMessage("msg_err_max_field_length", messageText,
						FIELD_LABEL_VALIDATION + ", " + length);

				b.append("if(inputToCheck.length() > " + length + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
				b.append("\n");
			}
		}
		else if (type.isChar()) {
			final var messageText = "Length of field \"{0}\" must be one character!";
			final String validationMessage = i18n.getI18NMessage("msg_err_char_length", messageText, FIELD_LABEL_VALIDATION);

			if (!field.isMandatory())
				b.append("if(inputToCheck.length() > 1)\n");
			else
				b.append("if(inputToCheck.length() != 1)\n");

			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}
		else if (type.isInteger()) {
			b.append("if(!inputToCheck.isEmpty())\n");
			b.append("try\n");
			b.append("{\n");
			b.append("Integer.valueOf(inputToCheck);\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append("\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be less or equal than {1}!";
				final String maxValue = attr.getDomainAttributeValidator().getMaxValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_max_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + maxValue + "\"");

				b.append("if(Integer.valueOf(inputToCheck) > " + attr.getDomainAttributeValidator().getMaxValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be greater or equal than {1}!";
				final String minValue = attr.getDomainAttributeValidator().getMinValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_min_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + minValue + "\"");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append("\n");

				b.append("if(Integer.valueOf(inputToCheck) < " + attr.getDomainAttributeValidator().getMinValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			final var messageText = "Field \"{0}\" does not contain valid integer value!";
			final String validationMessage = i18n.getI18NMessage("msg_err_no_integer", messageText, FIELD_LABEL_VALIDATION);

			b.append("}\n");
			b.append("catch (final NumberFormatException p)\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}
		else if (type.isLong()) {
			b.append("if(!inputToCheck.isEmpty())\n");
			b.append("try\n");
			b.append("{\n");
			b.append("Long.valueOf(inputToCheck);\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append("\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be less or equal than {1}!";
				final String maxValue = attr.getDomainAttributeValidator().getMaxValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_max_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + maxValue + "\"");

				b.append("if(Long.valueOf(inputToCheck) > " + attr.getDomainAttributeValidator().getMaxValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be greater or equal than {1}!";
				final String minValue = attr.getDomainAttributeValidator().getMinValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_min_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + minValue + "\"");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append("\n");

				b.append("if(Long.valueOf(inputToCheck) < " + attr.getDomainAttributeValidator().getMinValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			final var messageText = "Field \"{0}\" does not contain valid integer value!";
			final String validationMessage = i18n.getI18NMessage("msg_err_no_integer", messageText, FIELD_LABEL_VALIDATION);

			b.append("}\n");
			b.append("catch (final NumberFormatException p)\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}
		else if (type.isFloat()) {
			b.append("if(!inputToCheck.isEmpty())\n");
			b.append("try\n");
			b.append("{\n");
			b.append("decimalFormat.parse(inputToCheck).floatValue();\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append("\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be less or equal than {1}!";
				final String maxValue = attr.getDomainAttributeValidator().getMaxValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_max_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + maxValue + "\"");

				b.append("if(decimalFormat.parse(inputToCheck).floatValue() > ");
				b.append(attr.getDomainAttributeValidator().getMaxValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be greater or equal than {1}!";
				final String minValue = attr.getDomainAttributeValidator().getMinValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_min_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + minValue + "\"");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append("\n");

				b.append("if(decimalFormat.parse(inputToCheck).floatValue() < ");
				b.append(attr.getDomainAttributeValidator().getMinValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			final var messageText = "Field \"{0}\" does not contain valid floating-point value!";
			final String validationMessage = i18n.getI18NMessage("msg_err_no_double", messageText, FIELD_LABEL_VALIDATION);

			b.append("}\n");
			b.append("catch (final ParseException p)\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}
		else if (type.isDouble()) {
			b.append("if(!inputToCheck.isEmpty())\n");
			b.append("try\n");
			b.append("{\n");
			b.append("decimalFormat.parse(inputToCheck);\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append("\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be less or equal than {1}!";
				final String maxValue = attr.getDomainAttributeValidator().getMaxValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_max_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + maxValue + "\"");

				b.append("if(decimalFormat.parse(inputToCheck).doubleValue() > ");
				b.append(attr.getDomainAttributeValidator().getMaxValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be greater or equal than {1}!";
				final String minValue = attr.getDomainAttributeValidator().getMinValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_min_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + minValue + "\"");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append("\n");

				b.append("if(decimalFormat.parse(inputToCheck).doubleValue() < ");
				b.append(attr.getDomainAttributeValidator().getMinValue() + ")\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			final var messageText = "Field \"{0}\" does not contain valid floating-point value!";
			final String validationMessage = i18n.getI18NMessage("msg_err_no_double", messageText, FIELD_LABEL_VALIDATION);

			b.append("}\n");
			b.append("catch (final ParseException p)\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}
		else if (type.isBigDecimal()) {
			b.append("if(!inputToCheck.isEmpty())\n");
			b.append("try\n");
			b.append("{\n");
			b.append("decimalFormat.setParseBigDecimal(true);\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append("final var input = (BigDecimal) ");

			b.append("decimalFormat.parse(inputToCheck);\n");
			b.append("decimalFormat.setParseBigDecimal(false);\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()
					|| !attr.getDomainAttributeValidator().getMinValue().isEmpty())
				b.append("\n");

			if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be less or equal than {1}!";
				final String maxValue = attr.getDomainAttributeValidator().getMaxValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_max_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + maxValue + "\"");

				b.append("if(input.compareTo(new BigDecimal(\"" + attr.getDomainAttributeValidator().getMaxValue() + "\")) == 1)\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				final var messageText = "Value of field \"{0}\" must be greater or equal than {1}!";
				final String minValue = attr.getDomainAttributeValidator().getMinValue();
				final String validationMessage = i18n.getI18NMessage("msg_err_min_field_value", messageText,
						FIELD_LABEL_VALIDATION + ", \"" + minValue + "\"");

				if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty())
					b.append("\n");

				b.append("if(input.compareTo(new BigDecimal(\"" + attr.getDomainAttributeValidator().getMinValue() + "\")) == -1)\n");
				b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			}

			final var messageText = "Field \"{0}\" does not contain valid floating-point value!";
			final String validationMessage = i18n.getI18NMessage("msg_err_no_double", messageText, FIELD_LABEL_VALIDATION);

			b.append("}\n");
			b.append("catch (final ParseException p)\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}
		else if (type.isUUID()) {
			final var messageText = "Value of field \"{0}\" is not a valid UUID!";
			final String validationMessage = i18n.getI18NMessage("msg_err_no_uuid", messageText, FIELD_LABEL_VALIDATION);

			b.append("if(!inputToCheck.isEmpty())\n");
			b.append("try\n");
			b.append("{\n");
			b.append("java.util.UUID.fromString(inputToCheck);\n");
			b.append("}\n");
			b.append("catch (final IllegalArgumentException i)\n");
			b.append(fieldGenerator.getFieldValidationMessageFragment(validationMessage, hasTitleArea));
			b.append("\n");
		}

		if (b.isEmpty())
			return "";

		final var validationFragment = new StringBuilder();
		validationFragment.append("// Check field that is mapped to domain attribute '");
		validationFragment.append(field.getDTOAttribute().getDomainAttribute().getLabel() + "'\n");
		validationFragment.append("inputToCheck = " + field.getName() + ".getText();\n\n");
		validationFragment.append(b);

		return validationFragment.toString();
	}

	/**
	 * @param formGenerator
	 * @param objectName
	 * @return the generated content
	 */
	public String getSaveDataFragment(AbstractJavaSourceGenerator formGenerator, String objectName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (attr.isPk() && attr.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
			return "";

		if (attr.isTrackVersion() || attr.isSetDateOnPersist() || attr.isSetDateOnUpdate())
			return "";

		if (!field.isVisible() || field.isReadonly()) {
			// Set the default values of invisible or read-only fields
			if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty()) {
				// Passwords must be encrypted!
				if (attr.getTag() == AttributeTagEnumeration.USER_PASSWORD) {
					b.append("\n");
					b.append("try\n");
					b.append("{\n");
					b.append(objectName + "." + setter + "(HashGenerator.encryptSHA256(");
					b.append(field.getConvertedDefaultValue() + "));\n");
					b.append("}\n");
					b.append("catch (final Exception e)\n");
					b.append("{\n");
					b.append("// Something went wrong with encryption!\n");

					formGenerator.addErrorLog(b, "Error while encrypting password!", "e");

					b.append("}\n\n");
				}
				else
					b.append(objectName + "." + setter + "(" + field.getConvertedDefaultValue() + ");\n");
			}
		}
		else if (type.isString()) {
			if (attr.getTag() == AttributeTagEnumeration.USER_PASSWORD) {
				b.append("\n");
				b.append("try\n");
				b.append("{\n");
				b.append(objectName + "." + setter + "(HashGenerator.encryptSHA256(" + field.getName() + ".getText()));\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");
				b.append("// Something went wrong with encryption!\n");

				formGenerator.addErrorLog(b, "Error while encrypting password!", "e");

				b.append("}\n\n");
			}
			else
				b.append(objectName + "." + setter + "(" + field.getName() + ".getText());\n");
		}
		else {
			if (!field.isMandatory())
				b.append("\nif(!" + field.getName() + ".getText().isEmpty())\n");

			if (type.isInteger())
				b.append(objectName + "." + setter + "(Integer.valueOf(" + field.getName() + ".getText()));\n");
			else if (type.isChar())
				b.append(objectName + "." + setter + "(" + field.getName() + ".getText().charAt(0));\n");
			else if (type.isLong())
				b.append(objectName + "." + setter + "(Long.valueOf(" + field.getName() + ".getText()));\n");
			else if (type.isUUID())
				b.append(objectName + "." + setter + "(java.util.UUID.fromString(" + field.getName() + ".getText()));\n");
			else if (type.isDouble()) {
				if (field.isMandatory())
					b.append("\n");

				b.append("try\n");
				b.append("{\n");
				b.append(objectName + "." + setter + "(decimalFormat.parse(" + field.getName() + ".getText()).doubleValue());\n");
				b.append("}\n");
				b.append("catch (final ParseException p)\n");
				b.append("{\n");
				b.append("// Ignored!\n");
				b.append("}\n");

				if (field.isMandatory())
					b.append("\n");
			}
			else if (type.isFloat()) {
				if (field.isMandatory())
					b.append("\n");

				b.append("try\n");
				b.append("{\n");
				b.append(objectName + "." + setter + "(decimalFormat.parse(" + field.getName() + ".getText()).floatValue());\n");
				b.append("}\n");
				b.append("catch (final ParseException p)\n");
				b.append("{\n");
				b.append("// Ignored!\n");
				b.append("}\n");

				if (field.isMandatory())
					b.append("\n");
			}
			else if (type.isBigDecimal()) {
				if (field.isMandatory())
					b.append("\n");

				b.append("try\n");
				b.append("{\n");
				b.append("decimalFormat.setParseBigDecimal(true);\n");
				b.append(objectName + "." + setter + "((BigDecimal) decimalFormat.parse(" + field.getName() + ".getText()));\n");
				b.append("decimalFormat.setParseBigDecimal(false);\n");
				b.append("}\n");
				b.append("catch (final ParseException p)\n");
				b.append("{\n");
				b.append("// Ignored!\n");
				b.append("}\n");

				if (field.isMandatory())
					b.append("\n");
			}

			if (!field.isMandatory()) {
				if (type.isType(UUID, FLOAT_OBJ, BIG_DECIMAL, DOUBLE_OBJ, INTEGER, LONG_OBJ)) {
					b.append("else\n");
					b.append(objectName + "." + setter + "(null);\n\n");
				}
				else if (type.isChar()) {
					b.append("else\n");
					b.append(objectName + "." + setter + "('\\u0000');\n\n");
				}
			}
		}

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String getCreateInitializationFragment() {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();

		if (field.getDefaultValue() == null || field.getDefaultValue().isEmpty())
			return b.toString();

		if (type.isString() || type.isIntegerOrLong())
			b.append(field.getName() + ".setText(\"" + field.getDefaultValue() + "\");\n");
		else if (type.isDecimalNumber())
			b.append(field.getName() + ".setText(decimalFormat.format(" + field.getDefaultValue() + "));\n");
		else if (type.isUUID())
			b.append(field.getName() + ".setText(java.util.UUID.randomUUID().toString());\n");
		else if (type.isChar())
			b.append(field.getName() + ".setText(Character.toString('" + field.getDefaultValue() + "'));\n");

		return b.toString();
	}

}
