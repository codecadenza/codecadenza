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

/**
 * <p>
 * Interface that must be implemented by all form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IFieldGenerator {
	/**
	 * Add all imports necessary for this field
	 */
	void addImports();

	/**
	 * @return true if the field requires a date time formatter
	 */
	boolean needsDateTimeFormatter();

	/**
	 * @return true if the field requires a decimal formatter
	 */
	boolean needsDecimalFormatter();

	/**
	 * @return true if the field requires a date formatter
	 */
	boolean needsDateFormatter();

	/**
	 * Add all field declarations for this form field
	 */
	void addFieldDeclaration();

	/**
	 * Create the validation fragment
	 * @param hasTitleArea
	 * @return the generated content
	 */
	String getValidationFragment(boolean hasTitleArea);

	/**
	 * Create the default field initialization fragment
	 * @return the generated content
	 */
	String getDefaultInitializationFragment();

	/**
	 * Create the field initialization fragment for forms of type 'CREATE' or 'ADD'
	 * @return the generated content
	 */
	String getCreateInitializationFragment();

	/**
	 * Create the save data fragment
	 * @param dtoName
	 * @return the generated content
	 */
	String getSaveDataFragment(String dtoName);

	/**
	 * Create the field definition fragment
	 * @param hasOneColumn
	 * @return the generated content
	 */
	String getFieldDefinitionFragment(boolean hasOneColumn);

}
