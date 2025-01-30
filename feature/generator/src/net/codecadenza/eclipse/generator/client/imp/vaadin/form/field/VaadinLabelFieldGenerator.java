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
 * Generator for label fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinLabelFieldGenerator extends AbstractVaadinFieldGenerator {
	private final JavaType type;
	private final DomainAttribute attr;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinLabelFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
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
		return "LabelField";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.component");

		if (!type.isString())
			formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.converter");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		b.append(field.getName() + " = new " + getFieldTypeName() + "();\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");
		b.append(field.getName() + ".setTitle(" + i18n.getI18N(field) + ");\n");
		b.append(getFieldLayout(hasOneColumn));

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

		final var locale = i18n.getLocaleFragment();

		if (!attr.getJavaType().isPrimitive())
			b.append(".withNullRepresentation(\"\")");

		if (type.isCalendar()) {
			if (attr.getTemporalType() == TemporalTypeEnumeration.DATE)
				b.append(".withConverter(new StringToCalendarConverter(preferences.getDateFormat()))");
			else
				b.append(".withConverter(new StringToCalendarConverter(preferences.getDateTimeFormat()))");
		}
		else if (type.isDate()) {
			if (attr.getTemporalType() == TemporalTypeEnumeration.DATE)
				b.append(".withConverter(new StringToDateConverter(preferences.getDateFormat()))");
			else
				b.append(".withConverter(new StringToDateConverter(preferences.getDateTimeFormat()))");
		}
		else if (type.isDouble())
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
		else if (type.isLocalDate())
			b.append(".withConverter(new StringToLocalDateConverter(preferences.getDateFormat()))");
		else if (type.isLocalDateTime())
			b.append(".withConverter(new StringToLocalDateTimeConverter(preferences.getDateTimeFormat()))");
		else if (type.isUUID())
			b.append(".withConverter(new StringToUuidConverter(" + locale + "))");

		return b.toString();
	}

}
