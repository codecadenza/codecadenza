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
package net.codecadenza.runtime.richclient.property;

import net.codecadenza.runtime.richclient.persistence.PersistenceHelper;
import net.codecadenza.runtime.richclient.persistence.entity.UserProperty;

/**
 * <p>
 * Class with static methods to handle user-specific properties
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UserPropertyManager {
	/**
	 * Prevent instantiation
	 */
	private UserPropertyManager() {

	}

	/**
	 * Set a property
	 * @param userName the name of the user
	 * @param key
	 * @param value the value to set
	 */
	public static void setProperty(String userName, String key, String value) {
		UserProperty userProperty = getProperty(userName, key);

		if (userProperty != null) {
			userProperty.setValue(value);
			return;
		}

		userProperty = new UserProperty();
		userProperty.setKey(key);
		userProperty.setUserName(userName);
		userProperty.setValue(value);

		PersistenceHelper.addPersistentObject(userProperty);
	}

	/**
	 * Get a property
	 * @param userName the name of the user
	 * @param key the property key
	 * @return the user property object
	 */
	public static UserProperty getProperty(String userName, String key) {
		return PersistenceHelper.findPersistentObjects(UserProperty.class).stream()
				.filter(userProperty -> userProperty.getKey().equals(key) && userProperty.getUserName().equals(userName)).findFirst()
				.orElse(null);
	}

	/**
	 * Get the property value
	 * @param userName the name of the user
	 * @param key the property key
	 * @return the property value
	 */
	public static String getPropertyValue(String userName, String key) {
		final UserProperty userProperty = getProperty(userName, key);

		return userProperty != null ? userProperty.getValue() : null;
	}

}
