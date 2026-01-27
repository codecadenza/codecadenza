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
package net.codecadenza.eclipse.generator.client.imp.angular.form.field;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.common.TypeScriptFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Abstract base class for all Angular form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractAngularFieldGenerator {
	protected final FormField field;
	protected final AngularContentFormatter formatter;
	protected final boolean readonly;
	protected final boolean disabled;
	protected final FormTypeEnumeration formType;
	protected final DomainAttribute domainAttr;
	protected final JavaType type;
	protected final Project project;
	protected boolean validate;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 * @param readonly
	 */
	protected AbstractAngularFieldGenerator(FormField field, AngularContentFormatter formatter, boolean readonly) {
		this.field = field;
		this.formatter = formatter;
		this.readonly = readonly;
		this.disabled = !field.getDTOAttribute().getDTOBean().equals(field.getPanel().getForm().getDTO());
		this.formType = field.getPanel().getForm().getFormType();
		this.domainAttr = field.getDTOAttribute().getDomainAttribute() != null ? field.getDTOAttribute().getDomainAttribute() : null;
		this.type = domainAttr != null ? domainAttr.getJavaType() : null;
		this.project = field.getDTOAttribute().getDTOBean().getNamespace().getProject();
		this.validate = !readonly && !disabled && field.isVisible();

		if (readonly && field.isVisible() && !disabled
				&& (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && domainAttr != null
				&& domainAttr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME)
			this.validate = true;
	}

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	protected AbstractAngularFieldGenerator(FormField field, AngularContentFormatter formatter) {
		this(field, formatter, field.isReadonly());
	}

	/**
	 * Add the field binding fragment
	 */
	public void addBindingFragment() {
		final String validationFragment = createValidationFragment();

		final var fieldBinding = new StringBuilder();
		fieldBinding.append("this.addControl('" + field.getDTOAttribute().getName() + "', ");

		if (!validationFragment.isEmpty())
			fieldBinding.append("[" + validationFragment + "]");
		else
			fieldBinding.append("[]");

		final String initialValue = getInitialValue();

		if (disabled)
			fieldBinding.append(", true");

		if (initialValue != null) {
			if (!disabled)
				fieldBinding.append(", false");

			fieldBinding.append(", " + initialValue);
		}

		fieldBinding.append(");");

		formatter.addLine(fieldBinding.toString());
	}

	/**
	 * Add the visual representation of the field to the form's template file
	 */
	public void addFieldToTemplate() {
		if (!field.isVisible())
			return;

		final var controlContainer = new StringBuilder();
		controlContainer.append("<cc-form-control-container [formGroup]=\"formGroup\" ");
		controlContainer
				.append("name=\"" + field.getDTOAttribute().getName() + "\" label=\"" + field.getLabel() + ":\" i18n-label=\"@@");
		controlContainer.append(field.getPanel().getForm().getName().toLowerCase() + "_" + field.getName().toLowerCase() + "\"");

		if (field.getColIndex() == 1 && field.isSpanCols())
			controlContainer.append(" [span]=\"true\"");

		if (validate) {
			// Fields that are mapped to attributes of type char must always be mandatory!
			if (field.isMandatory() || (type != null && type.isChar()))
				controlContainer.append(" [required]=\"true\"");

			if (type != null && type.isString()) {
				final Optional<Integer> minLength = domainAttr.getMinFieldLength();
				final Optional<Integer> maxLength = domainAttr.getMaxFieldLenght();

				if (minLength.isPresent()) {
					controlContainer.append(" [minLength]=\"");
					controlContainer.append(minLength.get());
					controlContainer.append("\"");
				}

				if (maxLength.isPresent()) {
					controlContainer.append(" [maxLength]=\"");
					controlContainer.append(maxLength.get());
					controlContainer.append("\"");
				}

				if (!domainAttr.getDomainAttributeValidator().getRegularExpression().isEmpty()) {
					controlContainer.append(" [pattern]=\"'");
					controlContainer.append(domainAttr.getDomainAttributeValidator().getRegularExpression());
					controlContainer.append("'\"");
				}
			}

			if (type != null && type.isNumber()) {
				if (!domainAttr.getDomainAttributeValidator().getMinValue().isEmpty()) {
					controlContainer.append(" [minValue]=\"");
					controlContainer.append(domainAttr.getDomainAttributeValidator().getMinValue());
					controlContainer.append("\"");
				}

				if (!domainAttr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
					controlContainer.append(" [maxValue]=\"");
					controlContainer.append(domainAttr.getDomainAttributeValidator().getMaxValue());
					controlContainer.append("\"");
				}

				if (type.isIntegerOrLong())
					controlContainer.append(" [integer]=\"true\"");
				else
					controlContainer.append(" [decimal]=\"true\"");
			}

			if (type != null && type.isUUID()) {
				if (field.isMandatory())
					controlContainer.append(" [pattern]=\"UUID_PATTERN\"");
				else
					controlContainer.append(" [pattern]=\"OPTIONAL_UUID_PATTERN\"");
			}

			if (type != null && type.isChar())
				controlContainer.append(" [maxLength]=\"1\"");
		}

		controlContainer.append(">");

		fillGridColumn(true);

		formatter.addLine(controlContainer.toString());
		formatter.increaseIndent();
		formatter.addLine(addControlToTemplate());
		formatter.decreaseIndent();
		formatter.addLine("</cc-form-control-container>");
		formatter.addBlankLine();

		fillGridColumn(false);
	}

	/**
	 * @return the fragment to initialize this field
	 */
	public String getFieldInitializationFragment() {
		return null;
	}

	/**
	 * @return the fragment to convert the field's value after loading the corresponding object
	 */
	public String getFieldValueConversionFragment() {
		return null;
	}

	/**
	 * @return the fragment to reconvert the field's value before saving the corresponding object
	 */
	public String getFieldValueReconversionFragment() {
		return null;
	}

	/**
	 * @return a data transfer object on which this field depends
	 */
	public DTOBean getListDTO() {
		return null;
	}

	/**
	 * @return all imports that are necessary when adding the field to the respective form
	 */
	public Map<String, String> getImports() {
		final var imports = new HashMap<String, String>();

		if (!createValidationFragment().isEmpty()) {
			imports.put("Validators", "@angular/forms");

			if (type != null && type.isNumber())
				imports.put("NumberValidator", "../../common/validators/number-validator");
		}

		return imports;
	}

	/**
	 * @return a list with fields that should be declared in the corresponding form
	 */
	public List<TypeScriptFieldGenerator> getFields() {
		return Collections.emptyList();
	}

	/**
	 * Add necessary methods for this form field
	 * @param i18n
	 */
	@SuppressWarnings("unused")
	public void addMethods(AngularI18NGenerator i18n) {
		// No implementation required!
	}

	/**
	 * @return true if the field requires the authorization service
	 */
	public boolean isAuthServiceRequired() {
		return false;
	}

	/**
	 * @return true if the field requires the number converter
	 */
	public boolean isNumberConverterRequired() {
		return false;
	}

	/**
	 * @return an enumeration that is used by this field
	 */
	public JavaEnum getJavaEnum() {
		return null;
	}

	/**
	 * Create the control that should be added to the template
	 * @return the generated content
	 */
	protected abstract String addControlToTemplate();

	/**
	 * @return the initial value of the field or null if no initial value should be set
	 */
	protected String getInitialValue() {
		if (type == null)
			return null;

		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty()) {
			if (type.isString() || type.isChar())
				return "'" + field.getDefaultValue() + "'";
			else if (type.isBoolean())
				return "true";
			else if (type.isTemporalType())
				return "new Date()";
			else if (type.isNumber())
				return field.getDefaultValue();
			else if (type.isUUID())
				return "uuid()";
		}
		else if (field.isVisible() && !field.isMandatory() && type.isString()
				&& !domainAttr.getDomainAttributeValidator().isNullable())
			return "''";

		return null;
	}

	/**
	 * Create the validation fragment for this field
	 * @return the generated fragment or an empty string if no validator should be added
	 */
	private String createValidationFragment() {
		final var validation = new StringBuilder();

		if (!validate)
			return validation.toString();

		if (field.isMandatory() || (type != null && type.isChar()))
			validation.append("Validators.required");

		if (type == null)
			return validation.toString();

		// Fields that contain boolean values should not define a validator!
		if (type.isBoolean())
			return "";

		if (type.isString()) {
			final Optional<Integer> minLength = domainAttr.getMinFieldLength();
			final Optional<Integer> maxLength = domainAttr.getMaxFieldLenght();

			if (minLength.isPresent()) {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("Validators.minLength(");
				validation.append(minLength.get());
				validation.append(")");
			}

			if (maxLength.isPresent()) {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("Validators.maxLength(");
				validation.append(maxLength.get());
				validation.append(")");
			}

			if (!domainAttr.getDomainAttributeValidator().getRegularExpression().isEmpty()) {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("Validators.pattern('");
				validation.append(domainAttr.getDomainAttributeValidator().getRegularExpression());
				validation.append("')");
			}
		}
		else if (type.isNumber()) {
			if (!domainAttr.getDomainAttributeValidator().getMinValue().isEmpty()) {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("Validators.min(");
				validation.append(domainAttr.getDomainAttributeValidator().getMinValue());
				validation.append(")");
			}

			if (!domainAttr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("Validators.max(");
				validation.append(domainAttr.getDomainAttributeValidator().getMaxValue());
				validation.append(")");
			}

			if (type.isIntegerOrLong()) {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("NumberValidator.integer");
			}
			else {
				if (!validation.isEmpty())
					validation.append(", ");

				validation.append("NumberValidator.decimal(this.numberConverter)");
			}
		}
		else if (type.isUUID()) {
			if (!validation.isEmpty())
				validation.append(", ");

			if (field.isMandatory())
				validation.append("Validators.pattern(this.UUID_PATTERN)");
			else
				validation.append("Validators.pattern(this.OPTIONAL_UUID_PATTERN)");
		}
		else if (type.isChar())
			validation.append(", Validators.maxLength(1)");

		return validation.toString();
	}

	/**
	 * Fill empty columns in the form's grid layout
	 * @param firstCol
	 */
	private void fillGridColumn(boolean firstCol) {
		boolean fillSpace = false;

		// Search for another visible field in the same row
		final boolean anotherFieldInSameRow = field.getPanel().getFields().stream()
				.anyMatch(f -> f.isVisible() && f.getRowIndex() == field.getRowIndex() && !f.equals(field));

		if (!anotherFieldInSameRow && ((firstCol && field.getColIndex() == 2) || (!firstCol && field.getColIndex() == 1)))
			fillSpace = true;

		// There is no empty column if this field really spans over two columns!
		if (field.getColIndex() == 1 && field.isSpanCols())
			fillSpace = false;

		if (fillSpace) {
			formatter
					.addLine("<cc-form-control-container [fillEmptySpace]=\"true\" style=\"margin: 0px\"></cc-form-control-container>");
			formatter.addBlankLine();
		}
	}

}
