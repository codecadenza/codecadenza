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
package net.codecadenza.eclipse.generator.client.common.converter;

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Utility class for generating the conversion of date values
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DateConversionGenerator {
	/**
	 * Prevent instantiation
	 */
	private DateConversionGenerator() {

	}

	/**
	 * Create a fragment to convert the value of either a java.util.Date or a java.util.GregorianCalendar field to a
	 * java.time.Instant
	 * @param formField
	 * @param accessor
	 * @return the generated content
	 */
	public static String toInstant(FormField formField, String accessor) {
		return toInstant(formField.getDTOAttribute(), accessor);
	}

	/**
	 * Create a fragment to convert the value of either a java.util.Date or a java.util.GregorianCalendar column to a
	 * java.time.Instant
	 * @param column
	 * @param accessor
	 * @return the generated content
	 */
	public static String toInstant(TableColumnField column, String accessor) {
		return toInstant(column.getDTOAttribute(), accessor);
	}

	/**
	 * Create a fragment to convert the value of either a java.util.Date or a java.util.GregorianCalendar DTO attribute to a
	 * java.time.Instant
	 * @param attr
	 * @param accessor
	 * @return the generated content
	 */
	public static String toInstant(DTOBeanAttribute attr, String accessor) {
		final JavaType type = attr.getDomainAttribute().getJavaType();

		if (!type.isDateOrCalendar())
			return accessor;

		return attr.getDomainAttribute().convertToInstant(accessor);
	}

}
