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
package net.codecadenza.runtime.richclient.swing.util.type;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

/**
 * <p>
 * Utility class to split strings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Splitter {
	private final Pattern pat;
	private final boolean trimResults;

	/**
	 * Constructor
	 * @param pat
	 * @param trimResults
	 */
	private Splitter(Pattern pat, boolean trimResults) {
		this.pat = pat;
		this.trimResults = trimResults;
	}

	/**
	 * @param sep
	 * @return a new splitter instance
	 */
	public static Splitter on(String sep) {
		return new Splitter(Pattern.compile(Pattern.quote(sep)), false);
	}

	/**
	 * Perform split
	 * @param text
	 * @return all tokens
	 */
	public List<String> split(String text) {
		final String[] strs = pat.split(text);

		if (trimResults)
			return Arrays.asList(strs).stream().map(String::trim).toList();

		return Arrays.asList(strs);
	}

	/**
	 * @return a new splitter instance
	 */
	public Splitter trimResults() {
		return new Splitter(this.pat, true);
	}

}
