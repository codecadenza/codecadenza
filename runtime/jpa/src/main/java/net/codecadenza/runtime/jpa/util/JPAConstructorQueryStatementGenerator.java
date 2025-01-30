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
package net.codecadenza.runtime.jpa.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * <p>
 * Generator for JPA constructor query statements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAConstructorQueryStatementGenerator {
	private static final String SELECT_NEW = "select new ";
	private static final String FIELD_DELIMITER = ",";
	private static final String FIELD_LIST_START = "(";
	private static final String FIELD_LIST_END = ") ";

	private List<String> selectTokens = new ArrayList<>();
	private String className;

	/**
	 * Set the select tokens
	 * @param selectTokens the select tokens to be used
	 */
	public void setSelectTokens(List<String> selectTokens) {
		this.selectTokens = selectTokens;
	}

	/**
	 * Set the fully qualified name of the class for the objects that the query should return
	 * @param className the name of the class
	 */
	public void setClassName(String className) {
		this.className = className;
	}

	/**
	 * Create the query statement
	 * @return the generated constructor query statement
	 * @throws IllegalArgumentException if either no select tokens or no class name has been provided
	 */
	public String createStatement() {
		final Optional<String> selectedFields = selectTokens.stream().reduce((a, b) -> a + FIELD_DELIMITER + b);

		if (selectedFields.isEmpty())
			throw new IllegalArgumentException("A query statement without select tokens cannot be created!");

		if (className == null || className.isEmpty())
			throw new IllegalArgumentException("A query statement without a class name cannot be created!");

		final var queryStatement = new StringBuilder();
		queryStatement.append(SELECT_NEW);
		queryStatement.append(className);
		queryStatement.append(FIELD_LIST_START);
		queryStatement.append(selectedFields.get());
		queryStatement.append(FIELD_LIST_END);

		return queryStatement.toString();
	}

}
