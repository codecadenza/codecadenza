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
package net.codecadenza.eclipse.tools.util.eclipse;

/**
 * <p>
 * Record that holds the name and the auto-start configuration of an Eclipse plug-in
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @param name the name of the plug-in
 * @param autoStartConfiguration the auto-start configuration
 * @author Martin Ganserer
 * @version 1.0.0
 */
public record PluginRecord(String name, String autoStartConfiguration) {
	private static final String DEFAULT_AUTO_START_CONFIG = "default:default";

	/**
	 * Constructor using the default auto-start configuration
	 * @param name
	 */
	public PluginRecord(String name) {
		this(name, DEFAULT_AUTO_START_CONFIG);
	}

}
