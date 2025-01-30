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
package net.codecadenza.runtime.search;

import java.util.Collection;

/**
 * <p>
 * Generic search service
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface SearchService {
	String OPERATOR_IN = "in";
	String OPERATOR_NOT_IN = "not in";
	String OPERATOR_BETWEEN = "between";
	String OPERATOR_IS_NULL = "is null";
	String OPERATOR_IS_NOT_NULL = "is not null";
	String OPERATOR_LIKE = "like";
	String OPERATOR_NOT_LIKE = "not like";
	String OPERATOR_EQUAL = "=";
	String OPERATOR_GREATER = ">";
	String OPERATOR_SMALLER = "<";
	String OPERATOR_GREATER_OR_EQUAL = ">=";
	String OPERATOR_SMALLER_OR_EQUAL = "<=";
	String TOKEN_DELIMITER_IN = ";;";
	String TOKEN_DELIMITER_BETWEEN = "  ";

	/**
	 * Search for values that begin with the specified prefix
	 * @param command
	 * @param prefix
	 * @return the result list
	 */
	Collection<String> getListOfValues(String command, String prefix);
}
