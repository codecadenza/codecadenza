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
package net.codecadenza.runtime.ddt.model;

import java.util.List;
import java.util.UUID;

/**
 * <p>
 * Interface for a test field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface TestField {
	/**
	 * @return the ID of the test field
	 */
	UUID getId();

	/**
	 * @return the ID of the referenced test field
	 */
	UUID getReferencedId();

	/**
	 * @return the field name
	 */
	String getName();

	/**
	 * @return the value. This is usually the value of a simple type (e.g. double, Long, Date etc.) in a special string format.
	 */
	String getValue();

	/**
	 * Set the field value
	 * @param value
	 */
	void setValue(String value);

	/**
	 * @return a list of test objects if the field cannot be defined by {@link #getValue}
	 */
	List<TestObject> getTestObjects();

	/**
	 * @return the expected size of an attribute that is mapped to a list
	 */
	Integer getExpectedSize();

	/**
	 * @return true if a default value should be used (e.g. 0 for an int or null for non-primitive types)
	 */
	default boolean useDefaultValue() {
		return getValue() == null && (getTestObjects() == null || getTestObjects().isEmpty());
	}

}
