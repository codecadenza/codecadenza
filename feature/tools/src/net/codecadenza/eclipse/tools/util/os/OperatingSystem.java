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
package net.codecadenza.eclipse.tools.util.os;

/**
 * <p>
 * Utility class that provides information about the operating system the Eclipse workbench is running on
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class OperatingSystem {
	private static final String ARCHITECTURE_64_BIT = "64";
	private static final String WINDOWS = "win";
	private static final String MAC = "mac";

	/**
	 * Prevent instantiation
	 */
	private OperatingSystem() {

	}

	/**
	 * @return true if the Eclipse workbench is running on Windows
	 */
	public static boolean isWindows() {
		return System.getProperty("os.name").toLowerCase().contains(WINDOWS);
	}

	/**
	 * @return true if the Eclipse workbench is running on macOS
	 */
	public static boolean isMacOS() {
		return System.getProperty("os.name").toLowerCase().contains(MAC);
	}

	/**
	 * @return true if the operating system has a 64-bit architecture
	 */
	public static boolean has64BitArchitecture() {
		return System.getProperty("os.arch").endsWith(ARCHITECTURE_64_BIT);
	}

}
