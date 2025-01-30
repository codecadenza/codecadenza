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
package net.codecadenza.runtime.webclient.primefaces.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;

/**
 * <p>
 * Helper class for search operators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFSearchOperatorHelper {
	private static HashMap<String, SearchOperatorDTO> operators;

	/**
	 * Prevent instantiation
	 */
	private JSFSearchOperatorHelper() {

	}

	/**
	 * Initialize the search operators
	 */
	private static void initOperators() {
		int operatorId = 1;

		operators = new HashMap<>();
		operators.put("", new SearchOperatorDTO(operatorId++, "", "", true, true, true, true, true));
		operators.put(SearchService.OPERATOR_EQUAL,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_EQUAL, "equal", true, true, true, true, true));
		operators.put(SearchService.OPERATOR_BETWEEN,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_BETWEEN, "between", true, true, false, true, true));
		operators.put(SearchService.OPERATOR_LIKE,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_LIKE, "like", true, false, false, true, false));
		operators.put(SearchService.OPERATOR_NOT_LIKE,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_NOT_LIKE, "not like", true, false, false, true, false));
		operators.put(SearchService.OPERATOR_IN,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_IN, "in", true, false, false, true, false));
		operators.put(SearchService.OPERATOR_NOT_IN,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_NOT_IN, "not in", true, false, false, true, false));
		operators.put(SearchService.OPERATOR_GREATER,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_GREATER, "greater", true, true, false, false, true));
		operators.put(SearchService.OPERATOR_SMALLER,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_SMALLER, "smaller", true, true, false, false, true));
		operators.put(SearchService.OPERATOR_GREATER_OR_EQUAL, new SearchOperatorDTO(operatorId++,
				SearchService.OPERATOR_GREATER_OR_EQUAL, "greater or equal", true, true, false, false, true));
		operators.put(SearchService.OPERATOR_SMALLER_OR_EQUAL, new SearchOperatorDTO(operatorId++,
				SearchService.OPERATOR_SMALLER_OR_EQUAL, "smaller or equal", true, true, false, false, true));
		operators.put(SearchService.OPERATOR_IS_NULL,
				new SearchOperatorDTO(operatorId++, SearchService.OPERATOR_IS_NULL, "is null", false, true, false, true, true));
		operators.put(SearchService.OPERATOR_IS_NOT_NULL,
				new SearchOperatorDTO(operatorId, SearchService.OPERATOR_IS_NOT_NULL, "is not null", false, true, false, true, true));
	}

	/**
	 * @return the default operator
	 */
	public static SearchOperatorDTO getDefaultOperator() {
		return getOperator("");
	}

	/**
	 * Get all search operators
	 * @return a list of all operators
	 */
	public static List<SearchOperatorDTO> getAllOperators() {
		if (operators == null)
			initOperators();

		return operators.values().stream().toList();
	}

	/**
	 * @return a list containing all operators for text fields
	 */
	public static List<SearchOperatorDTO> getTextOperators() {
		return getAllOperators().stream().filter(SearchOperatorDTO::isTextSupport).toList();
	}

	/**
	 * @return a list containing all operators for enumeration fields
	 */
	public static List<SearchOperatorDTO> getEnumOperators() {
		final var ops = new ArrayList<SearchOperatorDTO>();

		for (final SearchOperatorDTO op : getAllOperators()) {
			if (!op.isTextSupport())
				continue;

			// For enumerations it makes no sense to support the 'between' operator
			if (op.getValue().equals(SearchService.OPERATOR_BETWEEN))
				continue;

			// The 'in' and the 'not-in' operators are not supported for enumeration fields!
			if (op.getValue().equals(SearchService.OPERATOR_IN))
				continue;

			if (op.getValue().equals(SearchService.OPERATOR_NOT_IN))
				continue;

			ops.add(op);
		}

		return ops;
	}

	/**
	 * @return a list containing all operators for UUID fields that support using 'like'-operators
	 */
	public static List<SearchOperatorDTO> getUUIDOperatorsWithLike() {
		final var likeOperators = getAllOperators().stream()
				.filter(op -> op.getValue().equals(SearchService.OPERATOR_LIKE) || op.getValue().equals(SearchService.OPERATOR_NOT_LIKE))
				.toList();

		final var operatorList = new ArrayList<>(getUUIDOperators());
		operatorList.addAll(likeOperators);

		return operatorList;
	}

	/**
	 * @return a list containing all operators for UUID fields
	 */
	public static List<SearchOperatorDTO> getUUIDOperators() {
		return getAllOperators().stream()
				.filter(
						op -> op.getValue().equals(SearchService.OPERATOR_IS_NULL) || op.getValue().equals(SearchService.OPERATOR_IS_NOT_NULL)
								|| op.getValue().equals(SearchService.OPERATOR_IN) || op.getValue().equals(SearchService.OPERATOR_NOT_IN)
								|| op.getValue().equals(SearchService.OPERATOR_EQUAL) || op.getValue().isEmpty())
				.toList();
	}

	/**
	 * @return a list containing all operators for boolean fields
	 */
	public static List<SearchOperatorDTO> getBoolOperators() {
		return getAllOperators().stream().filter(SearchOperatorDTO::isBooleanSupport).toList();
	}

	/**
	 * @return a list containing all operators for date and date time fields
	 */
	public static List<SearchOperatorDTO> getDateOperators() {
		return getAllOperators().stream().filter(SearchOperatorDTO::isDateSupport).toList();
	}

	/**
	 * @return a list containing all operators for numeric fields
	 */
	public static List<SearchOperatorDTO> getNumberOperators() {
		return getAllOperators().stream().filter(SearchOperatorDTO::isNumberSupport).toList();
	}

	/**
	 * Get an operator by its name
	 * @param name the name of the operator
	 * @return the operator identified by its name
	 */
	public static SearchOperatorDTO getOperator(String name) {
		if (operators == null)
			initOperators();

		return operators.get(name);
	}

}
