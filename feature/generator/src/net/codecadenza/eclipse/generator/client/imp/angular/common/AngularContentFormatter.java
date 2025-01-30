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
package net.codecadenza.eclipse.generator.client.imp.angular.common;

import net.codecadenza.eclipse.generator.common.AbstractContentFormatter;

/**
 * <p>
 * Utility class for formatting TypeScript and HTML template files of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularContentFormatter extends AbstractContentFormatter {
	private static final String INDENT = "  ";

	/**
	 * Default constructor
	 */
	public AngularContentFormatter() {
	}

	/**
	 * Constructor for setting the initial indent
	 * @param indents
	 */
	public AngularContentFormatter(int indents) {
		super(indents);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractContentFormatter#getDefaultIndent()
	 */
	@Override
	public String getDefaultIndent() {
		return INDENT;
	}

	/**
	 * Add a block comment by using the given text. Note that this method should not be used for HTML template files!
	 * @param text
	 */
	public void addBlockComment(String text) {
		if (text == null)
			return;

		addLine("/**");

		final String[] lines = text.split("\n");

		for (String line : lines) {
			line = line.trim();

			if (!line.isEmpty())
				addLine(" * " + line);
		}

		addLine(" */");
	}

	/**
	 * Add a line comment by using the given text. Note that this method should not be used for HTML template files!
	 * @param text
	 */
	public void addLineComment(String text) {
		if (text == null)
			return;

		final String[] lines = text.split("\n");

		for (String line : lines) {
			line = line.trim();

			if (!line.isEmpty())
				addLine("// " + line);
		}
	}

	/**
	 * Add an if-statement
	 * @param expression a boolean expression that should be evaluated
	 * @param statement the statement that should be executed if the expression evaluates to true
	 * @param addBlankLine flag that controls if a blank line should be added at the end
	 */
	public void addIfStatement(String expression, String statement, boolean addBlankLine) {
		addLine("if (" + expression + ") {");
		increaseIndent();
		addLine(statement);
		decreaseIndent();
		addLine("}");

		if (addBlankLine)
			addBlankLine();
	}

}
