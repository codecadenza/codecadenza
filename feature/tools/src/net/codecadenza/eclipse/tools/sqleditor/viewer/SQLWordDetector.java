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
package net.codecadenza.eclipse.tools.sqleditor.viewer;

import java.util.Objects;
import java.util.function.Predicate;
import org.eclipse.jface.text.rules.IWordDetector;

/**
 * <p>
 * Determine whether a given character is valid as part of an SQL keyword, table or column name
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLWordDetector implements IWordDetector {
	private final SQLSyntax syntax;

	/**
	 * Constructor
	 * @param syntax
	 */
	public SQLWordDetector(SQLSyntax syntax) {
		this.syntax = syntax;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordStart(char)
	 */
	@Override
	public boolean isWordStart(char c) {
		return syntax.getReservedWords().stream().filter(Objects::nonNull).filter(Predicate.not(String::isEmpty))
				.anyMatch(word -> word.substring(0, 1).equalsIgnoreCase(Character.toString(c)));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordPart(char)
	 */
	@Override
	public boolean isWordPart(char c) {
		return syntax.getReservedWords().stream().filter(Objects::nonNull).filter(Predicate.not(String::isEmpty))
				.anyMatch(word -> word.toLowerCase().indexOf(Character.toString(c).toLowerCase()) != -1);
	}

}
