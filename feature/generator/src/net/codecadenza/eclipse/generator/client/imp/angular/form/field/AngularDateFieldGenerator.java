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

import java.util.Map;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;

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
public class AngularDateFieldGenerator extends AbstractAngularFieldGenerator {
	private final boolean hasDefaultValue;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularDateFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter);

		this.hasDefaultValue = field.getDefaultValue() != null && !field.getDefaultValue().isEmpty();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();
		control.append("<p-calendar formControlName=\"" + field.getDTOAttribute().getName() + "\" dateFormat=\"dd.mm.yy\" ");
		control.append("[style]=\"{'width':'100%'}\" id=\"" + field.getName() + "\"");

		if (field.getFieldType() == FormFieldTypeEnumeration.DATE_TIME)
			control.append(" [showTime]=\"true\"");

		if (readonly || disabled)
			control.append(" [disabled]=\"true\"");

		control.append("></p-calendar>");

		return control.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * getFieldValueConversionFragment()
	 */
	@Override
	public String getFieldValueConversionFragment() {
		if (!field.isVisible() || hasDefaultValue || disabled)
			return null;

		final var attributeAccessor = "this.object." + field.getDTOAttribute().getName();

		return attributeAccessor + " = DateConverter.convertToDate(" + attributeAccessor + ");";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * getFieldValueReconversionFragment()
	 */
	@Override
	public String getFieldValueReconversionFragment() {
		if (disabled || (!field.isVisible() && !hasDefaultValue))
			return null;

		final var attributeAccessor = "object." + field.getDTOAttribute().getName();

		if (project.isDeployedOnPayara()) {
			if (type.isLocalDateTime())
				return attributeAccessor + " = DateConverter.removeMilliseconds(" + attributeAccessor + ");";
		}
		else if (type.isDateOrCalendar())
			return attributeAccessor + " = DateConverter.convertToMilliseconds(" + attributeAccessor + ");";

		if (type.isLocalDate())
			return attributeAccessor + " = DateConverter.removeTime(" + attributeAccessor + ");";

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		final Map<String, String> imports = super.getImports();

		if (getFieldValueConversionFragment() != null || getFieldValueReconversionFragment() != null)
			imports.put("DateConverter", "../../common/converter/date-converter");

		return imports;
	}

}
