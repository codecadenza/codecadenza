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
package net.codecadenza.eclipse.generator.common;

/**
 * <p>
 * Abstract base class for all content formatters
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractContentFormatter {
	private final StringBuilder b = new StringBuilder();
	private int indents;

	/**
	 * Default constructor
	 */
	protected AbstractContentFormatter() {

	}

	/**
	 * Constructor for setting the initial indent
	 * @param indents
	 */
	protected AbstractContentFormatter(int indents) {
		this.indents = indents;
	}

	/**
	 * Every subclass must define the indent string
	 * @return the string that should be used for the indent
	 */
	public abstract String getDefaultIndent();

	/**
	 * Add the given line and prefix it with the current indent
	 * @param line
	 */
	public void addLine(String line) {
		b.append(getIndent());
		b.append(line + "\n");
	}

	/**
	 * Add a blank line
	 */
	public void addBlankLine() {
		b.append("\n");
	}

	/**
	 * Increase the indent
	 */
	public void increaseIndent() {
		indents++;
	}

	/**
	 * Decrease the indent
	 */
	public void decreaseIndent() {
		if (indents > 0)
			indents--;
	}

	/**
	 * @return the current number of indents
	 */
	public int getIndents() {
		return indents;
	}

	/**
	 * @return the current indent
	 */
	public String getIndent() {
		final var indent = new StringBuilder();

		for (int i = 0; i < indents; i++)
			indent.append(getDefaultIndent());

		return indent.toString();
	}

	/**
	 * @return the content
	 */
	public String getContent() {
		return b.toString();
	}

	/**
	 * Add the given content. It is in the responsibility of the caller that the content is formatted properly!
	 * @param content
	 */
	public void addContent(String content) {
		b.append(content);
	}

}
