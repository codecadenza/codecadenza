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
package net.codecadenza.runtime.webclient.vaadin.util;

import jakarta.enterprise.context.SessionScoped;
import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <p>
 * Utility class for storing key-value pairs in a user session
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SessionScoped
public class SessionStore implements Serializable {
	private static final long serialVersionUID = -2701391693026155092L;

	private final ConcurrentHashMap<String, String> store = new ConcurrentHashMap<>();

	/**
	 * Get a value from the store
	 * @param key
	 * @return the respective value
	 */
	public String getValue(String key) {
		return store.get(key);
	}

	/**
	 * Put a value into the store
	 * @param key
	 * @param value
	 */
	public void putValue(String key, String value) {
		store.put(key, value);
	}

}
