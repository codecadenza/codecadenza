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
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;

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
public class AngularTextFieldGenerator extends AbstractAngularFieldGenerator {
	private final boolean multiLine;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 * @param multiLine
	 * @param readonly
	 */
	public AngularTextFieldGenerator(FormField field, AngularContentFormatter formatter, boolean multiLine, boolean readonly) {
		super(field, formatter, readonly);

		this.multiLine = multiLine;
	}

	/**
	 * Constructor
	 * @param field
	 * @param multiLine
	 * @param formatter
	 */
	public AngularTextFieldGenerator(FormField field, AngularContentFormatter formatter, boolean multiLine) {
		this(field, formatter, multiLine, field.isReadonly());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();

		if (multiLine)
			control.append("<textarea pInputTextarea [cols]=\"50\" [rows]=\"10\" style=\"width: 100%;\" ");
		else
			control.append("<input pInputText ");

		control.append("formControlName=\"" + field.getDTOAttribute().getName() + "\" ");
		control.append("id=\"" + field.getName() + "\" ");
		control.append("class=\"inputfield w-full");

		if (disabled || readonly)
			control.append(" form-field-readonly\" [readonly]=\"true\"");
		else
			control.append("\"");

		if (type.isTemporalType()) {
			if (type.isLocalDateTime()
					|| (type.isDateOrCalendar() && domainAttr.getTemporalType() == TemporalTypeEnumeration.TIMESTAMP))
				control.append(" ccDateTimeFormatter");
			else
				control.append(" ccDateFormatter");
		}
		else if (type.isDecimalNumber())
			control.append(" ccNumberFormatter");

		if (multiLine)
			control.append("></textarea>");
		else
			control.append("/>");

		return control.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * getFieldValueReconversionFragment()
	 */
	@Override
	public String getFieldValueReconversionFragment() {
		final var attributeAccessor = "object." + field.getDTOAttribute().getName();

		if (isNumberConverterRequired())
			return attributeAccessor + " = this.numberConverter.convertToNumber(" + attributeAccessor + ");";

		if (encryptPassword())
			return attributeAccessor + " = SHA256(" + attributeAccessor + ").toString();";

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		final Map<String, String> imports = super.getImports();

		if (isNumberConverterRequired())
			imports.put("NumberConverter", "../../common/converter/number-converter");

		if (encryptPassword())
			imports.put("SHA256", "crypto-es/lib/sha256");

		if (type.isUUID() && field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
			imports.put("v4 as uuid", "uuid");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#isNumberConverterRequired()
	 */
	@Override
	public boolean isNumberConverterRequired() {
		if (!field.isVisible() || disabled || readonly)
			return false;

		// An optional field that is mapped to an integer must be converted as the field might contain an empty string!
		return type.isDecimalNumber() || (type.isIntegerOrLong() && !field.isMandatory());
	}

	/**
	 * @return true if the field value represents a password that should be encrypted
	 */
	private boolean encryptPassword() {
		if (!field.isVisible() || disabled || readonly)
			return false;

		return domainAttr.getTag() == AttributeTagEnumeration.USER_PASSWORD;
	}

}
