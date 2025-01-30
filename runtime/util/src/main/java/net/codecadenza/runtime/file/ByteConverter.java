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
package net.codecadenza.runtime.file;

import java.text.DecimalFormat;

/**
 * <p>
 * Utility class to convert the number of the given bytes into a human-readable format
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ByteConverter {
	private static final short KB_EXPONENT = 10;
	private static final short MB_EXPONENT = 20;
	private static final short GB_EXPONENT = 30;

	/**
	 * Prevent instantiation
	 */
	private ByteConverter() {

	}

	/**
	 * Convert the given number of bytes into a human-readable format
	 * @param numberOfBytes
	 * @return the formatted result
	 */
	public static String convert(long numberOfBytes) {
		final var dc = new DecimalFormat(".00");

		double size = (numberOfBytes / Math.pow(2, GB_EXPONENT));

		if (size > 1)
			return dc.format(size) + " GB";

		size = (numberOfBytes / Math.pow(2, MB_EXPONENT));

		if (size > 1)
			return dc.format(size) + " MB";

		size = (numberOfBytes / Math.pow(2, KB_EXPONENT));

		if (size > 1)
			return dc.format(size) + " kB";

		return numberOfBytes + " Bytes";
	}

}
