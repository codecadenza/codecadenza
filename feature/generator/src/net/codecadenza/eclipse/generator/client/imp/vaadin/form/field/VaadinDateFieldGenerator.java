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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for date fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinDateFieldGenerator extends AbstractVaadinFieldGenerator {
	private final JavaType type;
	private final DomainAttribute attr;
	private final boolean dateAndTime;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinDateFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.attr = field.getDTOAttribute().getDomainAttribute();
		this.type = attr.getJavaType();
		this.dateAndTime = type.isLocalDateTime() || attr.getTemporalType() == TemporalTypeEnumeration.TIMESTAMP;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		if (!field.isReadonly()) {
			if (dateAndTime)
				return "DateTimePicker";

			return "DatePicker";
		}

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

		if (field.isReadonly() || type.isDateOrCalendar())
			formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.converter");

		if (!field.isReadonly()) {
			if (attr.getDomainAttributeValidator().isFutureDate() || attr.getDomainAttributeValidator().isPastDate())
				formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.validation");

			if (dateAndTime)
				formGenerator.importPackage("com.vaadin.flow.component.datetimepicker");
			else
				formGenerator.importPackage("com.vaadin.flow.component.datepicker");
		}
		else
			formGenerator.importPackage("com.vaadin.flow.component.textfield");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#
	 * addToolTipFragment(java.lang.String)
	 */
	@Override
	public String addToolTipFragment(String fieldName) {
		if (field.isReadonly() && attr.getUserComment() != null && !attr.getUserComment().isEmpty())
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

		// Do not check fields that are either invisible or read-only!
		if (!field.isVisible() || field.isReadonly())
			return "";

		if (field.isMandatory()) {
			final var validationMessage = "Field must not be empty!";

			b.append(".asRequired(" + i18n.getI18NMessage("msg_err_empty_field", validationMessage) + ")");
		}

		final var locale = i18n.getLocaleFragment();

		if (attr.getDomainAttributeValidator().isFutureDate()) {
			if (type.isCalendar()) {
				b.append(".withValidator(new CalendarValidator(new " + type.getNamespace().toString());
				b.append(".GregorianCalendar(), null, " + locale + "))");
			}
			else if (type.isDate()) {
				b.append(".withValidator(new DateValidator(new " + type.getNamespace().toString());
				b.append(".Date(), null, " + locale + "))");
			}
			else if (type.isLocalDate()) {
				b.append(".withValidator(new LocalDateValidator(" + type.getNamespace().toString());
				b.append(".LocalDate.now(), null, " + locale + "))");
			}
			else if (type.isLocalDateTime()) {
				b.append(".withValidator(new LocalDateTimeValidator(" + type.getNamespace().toString());
				b.append(".LocalDateTime.now(), null, " + locale + "))");
			}
		}

		if (attr.getDomainAttributeValidator().isPastDate()) {
			if (type.isCalendar()) {
				b.append(".withValidator(new CalendarValidator(new " + type.getNamespace().toString());
				b.append(".GregorianCalendar(), " + locale + "))");
			}
			else if (type.isDate()) {
				b.append(".withValidator(new DateValidator(new " + type.getNamespace().toString());
				b.append(".Date(), " + locale + "))");
			}
			else if (type.isLocalDate()) {
				b.append(".withValidator(new LocalDateValidator(" + type.getNamespace().toString());
				b.append(".LocalDate.now(), " + locale + "))");
			}
			else if (type.isLocalDateTime()) {
				b.append(".withValidator(new LocalDateTimeValidator(" + type.getNamespace().toString());
				b.append(".LocalDateTime.now(), " + locale + "))");
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

		if (field.isReadonly()) {
			b.append(".withNullRepresentation(\"\")");

			if (type.isCalendar()) {
				if (dateAndTime)
					b.append(".withConverter(new StringToCalendarConverter(preferences.getDateTimeFormat()))");
				else
					b.append(".withConverter(new StringToCalendarConverter(preferences.getDateFormat()))");
			}
			else if (type.isDate()) {
				if (dateAndTime)
					b.append(".withConverter(new StringToDateConverter(preferences.getDateTimeFormat()))");
				else
					b.append(".withConverter(new StringToDateConverter(preferences.getDateFormat()))");
			}
			else if (dateAndTime)
				b.append(".withConverter(new StringToLocalDateTimeConverter(preferences.getDateTimeFormat()))");
			else
				b.append(".withConverter(new StringToLocalDateConverter(preferences.getDateFormat()))");
		}
		else if (type.isCalendar()) {
			if (dateAndTime)
				b.append(".withConverter(new LocalDateTimeToCalendarConverter())");
			else
				b.append(".withConverter(new LocalDateToCalendarConverter())");
		}
		else if (type.isDate()) {
			if (dateAndTime)
				b.append(".withConverter(new LocalDateTimeToDateConverter())");
			else
				b.append(".withConverter(new LocalDateToDateConverter())");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#
	 * getDefaultValueInitialization()
	 */
	@Override
	public String getDefaultValueInitialization() {
		final var b = new StringBuilder();
		final var fullSetter = modelObjectName + "." + field.getDTOAttribute().getModelSetterName();

		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty()) {
			if (type.isDateOrCalendar())
				b.append(fullSetter + "(new " + type.getNamespace().toString() + "." + type.getName() + "());\n");
			else
				b.append(fullSetter + "(" + type.getNamespace().toString() + "." + type.getName() + ".now());\n");
		}

		return b.toString();
	}

}
