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
package net.codecadenza.eclipse.tools.ide;

import java.util.Random;

/**
 * <p>
 * Utility class that generates ID values of variable length. The generated ID contains characters from A-Z, a-z and 0-9!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IDGenerator {
	private static final char[] symbols;

	// Initialize the length of the generated ID with a reasonable default value
	private int length = 10;

	private final Random random = new Random();
	private String prefix;
	private String suffix;

	// Initialize an internal buffer with the allowed character set!
	static {
		final var tmp = new StringBuilder();

		for (char ch = '0'; ch <= '9'; ++ch)
			tmp.append(ch);

		for (char ch = 'A'; ch <= 'Z'; ++ch)
			tmp.append(ch);

		for (char ch = 'a'; ch <= 'z'; ++ch)
			tmp.append(ch);

		symbols = tmp.toString().toCharArray();
	}

	/**
	 * Set the prefix that should be added to every ID
	 * @param prefix
	 */
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	/**
	 * Set the suffix that should be added to every ID
	 * @param suffix
	 */
	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	/**
	 * Set the length of the generated ID
	 * @param length
	 */
	public void setLength(int length) {
		if (length < 1)
			throw new IllegalArgumentException("The length must be greater than 0!");

		this.length = length;
	}

	/**
	 * Generate a random ID
	 * @return the generated ID
	 */
	public String generateNextID() {
		final var buf = new char[length];
		var id = "";

		for (int idx = 0; idx < buf.length; ++idx)
			buf[idx] = symbols[this.random.nextInt(symbols.length)];

		if (prefix != null)
			id += prefix;

		id += new String(buf);

		if (suffix != null)
			id += suffix;

		return id;
	}

}
