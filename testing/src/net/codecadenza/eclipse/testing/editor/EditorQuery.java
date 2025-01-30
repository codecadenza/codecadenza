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
package net.codecadenza.eclipse.testing.editor;

/**
 * <p>
 * Objects of this class contain information about a query that should be executed in either a SQL or a JPA editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditorQuery {
	private final String command;
	private final boolean valid;
	private final boolean checkResults;

	/**
	 * Constructor
	 * @param command
	 * @param valid
	 * @param checkResults
	 */
	public EditorQuery(String command, boolean valid, boolean checkResults) {
		this.command = command;
		this.valid = valid;
		this.checkResults = checkResults;
	}

	/**
	 * @return the command
	 */
	public String getCommand() {
		return command;
	}

	/**
	 * @return true if the query is expected to be valid
	 */
	public boolean isValid() {
		return valid;
	}

	/**
	 * @return true if the query returns a result
	 */
	public boolean isCheckResults() {
		return checkResults;
	}

}
