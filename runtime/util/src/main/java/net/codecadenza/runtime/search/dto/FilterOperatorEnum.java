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

import net.codecadenza.runtime.search.SearchService;

/**
 * <p>
 * Enumeration of supported filter operators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public enum FilterOperatorEnum {
	IN(SearchService.OPERATOR_IN), NOT_IN(SearchService.OPERATOR_NOT_IN), BETWEEN(SearchService.OPERATOR_BETWEEN), IS_NULL(
			SearchService.OPERATOR_IS_NULL), IS_NOT_NULL(SearchService.OPERATOR_IS_NOT_NULL), LIKE(
					SearchService.OPERATOR_LIKE), NOT_LIKE(SearchService.OPERATOR_NOT_LIKE), EQUAL(SearchService.OPERATOR_EQUAL), GREATER(
							SearchService.OPERATOR_GREATER), SMALLER(SearchService.OPERATOR_SMALLER), GREATER_OR_EQUAL(
									SearchService.OPERATOR_GREATER_OR_EQUAL), SMALLER_OR_EQUAL(SearchService.OPERATOR_SMALLER_OR_EQUAL);

	private final String value;

	/**
	 * Constructor
	 * @param value
	 */
	FilterOperatorEnum(String value) {
		this.value = value;
	}

	/**
	 * @return the operator's value
	 */
	public String getValue() {
		return value;
	}

}
