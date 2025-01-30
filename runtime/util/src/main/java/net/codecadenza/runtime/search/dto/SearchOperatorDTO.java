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
package net.codecadenza.runtime.search.dto;

import java.io.Serializable;

/**
 * <p>
 * Data transfer object for search operators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchOperatorDTO implements Serializable {
	private static final long serialVersionUID = 1L;

	private int id;
	private String value;
	private String description;
	private boolean expectsArgument;
	private boolean dateSupport;
	private final boolean booleanSupport;
	private boolean textSupport;
	private final boolean numberSupport;

	/**
	 * Constructor
	 * @param id
	 * @param value
	 * @param description
	 * @param expectsArgument
	 * @param dateSupport
	 * @param booleanSupport
	 * @param textSupport
	 * @param numberSupport
	 */
	public SearchOperatorDTO(int id, String value, String description, boolean expectsArgument, boolean dateSupport,
			boolean booleanSupport, boolean textSupport, boolean numberSupport) {
		this.id = id;
		this.value = value;
		this.description = description;
		this.expectsArgument = expectsArgument;
		this.dateSupport = dateSupport;
		this.booleanSupport = booleanSupport;
		this.textSupport = textSupport;
		this.numberSupport = numberSupport;
	}

	/**
	 * @return true if the operator supports boolean operations
	 */
	public boolean isBooleanSupport() {
		return booleanSupport;
	}

	/**
	 * @return the ID
	 */
	public int getId() {
		return id;
	}

	/**
	 * Set the ID
	 * @param id
	 */
	public void setId(int id) {
		this.id = id;
	}

	/**
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Set the description text
	 * @param description
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Set the value
	 * @param value
	 */
	public void setValue(String value) {
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.description + " " + this.value;
	}

	/**
	 * @return true if the operator needs an argument
	 */
	public boolean isExpectsArgument() {
		return expectsArgument;
	}

	/**
	 * @param expectsArgument
	 */
	public void setExpectsArgument(boolean expectsArgument) {
		this.expectsArgument = expectsArgument;
	}

	/**
	 * @return true if the operator supports date fields
	 */
	public boolean isDateSupport() {
		return dateSupport;
	}

	/**
	 * @param dateSupport
	 */
	public void setDateSupport(boolean dateSupport) {
		this.dateSupport = dateSupport;
	}

	/**
	 * @return true if the operator supports text fields
	 */
	public boolean isTextSupport() {
		return textSupport;
	}

	/**
	 * @return true if the operator supports numeric fields
	 */
	public boolean isNumberSupport() {
		return numberSupport;
	}

	/**
	 * @param textSupport the text support flag to set
	 */
	public void setTextSupport(boolean textSupport) {
		this.textSupport = textSupport;
	}

}
