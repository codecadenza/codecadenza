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

/**
 * <p>
 * Utility class for joining strings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Joiner {
	private final String separator;
	private final boolean skipNull;
	private final String nullReplacement;

	/**
	 * Constructor
	 * @param separator
	 * @param skipNull
	 * @param nullReplacement
	 */
	public Joiner(String separator, boolean skipNull, String nullReplacement) {
		this.separator = separator;
		this.skipNull = skipNull;
		this.nullReplacement = nullReplacement;
	}

	/**
	 * @param sep
	 * @return a joiner instance
	 */
	public static Joiner on(char sep) {
		return on(sep + "");
	}

	/**
	 * @param sep
	 * @return a joiner instance
	 */
	public static Joiner on(String sep) {
		return new Joiner(sep, false, null);
	}

	/**
	 * @return a new joiner instance
	 */
	public Joiner skipNulls() {
		return new Joiner(separator, true, nullReplacement);
	}

	/**
	 * @param nullReplacement
	 * @return a new joiner instance
	 */
	public Joiner useForNull(String nullReplacement) {
		return new Joiner(separator, skipNull, nullReplacement);
	}

	/**
	 * @param builder
	 * @param values
	 * @param surroundWithQuotes
	 * @throws IllegalArgumentException if the field 'nullReplacement' is null
	 */
	public void appendTo(StringBuilder builder, Iterable<?> values, boolean surroundWithQuotes) {
		var sep = "";

		for (final Object val : values) {
			String str;

			if (val == null) {
				if (skipNull)
					continue;

				if (nullReplacement == null)
					throw new IllegalArgumentException("The field 'nullReplacement' must not be null!");

				str = nullReplacement;
			}
			else
				str = val.toString();

			builder.append(sep);

			if (surroundWithQuotes)
				builder.append("\"" + str + "\"");
			else
				builder.append(str);

			sep = separator;
		}
	}

	/**
	 * @param values
	 * @param surroundWithQuotes
	 * @return the joined string
	 */
	public String join(Iterable<?> values, boolean surroundWithQuotes) {
		final var builder = new StringBuilder();

		appendTo(builder, values, surroundWithQuotes);

		return builder.toString();
	}

}
