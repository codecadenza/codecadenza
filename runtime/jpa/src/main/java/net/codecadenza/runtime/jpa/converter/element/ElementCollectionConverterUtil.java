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
package net.codecadenza.runtime.jpa.converter.element;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Utility class for element collection converters
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ElementCollectionConverterUtil {
	public static final String DELIMITER = ",";
	public static final char QUOTE = '\'';
	public static final String QUOTE_STRING = "" + QUOTE;
	public static final String ESCAPE = "''";

	/**
	 * Constructor
	 */
	private ElementCollectionConverterUtil() {
		// Prevent instantiation
	}

	/**
	 * Extract the values from the given string
	 * @param string
	 * @return a list with the extracted string values
	 */
	public static List<String> extractElements(String string) {
		final var elements = new ArrayList<String>();
		int startIndex = -1;
		int endIndex = -1;
		boolean skipNextQuote = false;

		for (int i = 0; i < string.length(); i++) {
			final var c = string.charAt(i);

			if (c == QUOTE) {
				if (skipNextQuote) {
					skipNextQuote = false;
					continue;
				}

				// Check if the next character is also a quote
				if (i + 1 < string.length() && startIndex != -1) {
					final var nextChar = string.charAt(i + 1);

					if (nextChar == QUOTE) {
						skipNextQuote = true;
						continue;
					}
				}

				if (startIndex >= 0)
					endIndex = i;

				if (endIndex == -1)
					startIndex = i;

				if (startIndex >= 0 && endIndex >= 0) {
					final String element = string.substring(startIndex + 1, endIndex);

					elements.add(element.replace(ESCAPE, QUOTE_STRING));
					startIndex = -1;
					endIndex = -1;
				}
			}
		}

		return elements;
	}

	/**
	 * Escape all quote characters
	 * @param string the string to escape the quotes
	 * @return a string where the quotes has been escaped
	 */
	public static String escapeQuotes(String string) {
		return string.replace(QUOTE_STRING, ESCAPE);
	}

}
