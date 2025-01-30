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
package net.codecadenza.runtime.richclient.eclipse.rap.services;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.richclient.persistence.entity.LastLogOn;
import org.eclipse.rap.rwt.RWT;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Manager for finding and saving the last logged on user
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LogOnManager {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String ATTR_LOGGED_ON_USER = "logged_on_user";
	private static final String ATTR_HOST_NAME = "host_name";

	/**
	 * Prevent instantiation
	 */
	private LogOnManager() {

	}

	/**
	 * @return information about last logged on user
	 */
	public static LastLogOn getLastLogOn() {
		final var lastLogOn = new LastLogOn();

		String userName = RWT.getSettingStore().getAttribute(ATTR_LOGGED_ON_USER);
		String host = RWT.getSettingStore().getAttribute(ATTR_HOST_NAME);

		if (userName == null)
			userName = "";

		if (host == null)
			host = "";

		lastLogOn.setUserName(userName);
		lastLogOn.setHost(host);

		return lastLogOn;
	}

	/**
	 * Save the last logged on user
	 * @param userName the name of the user
	 * @param host the selected host
	 */
	public static void saveLastLogOn(String userName, String host) {
		try {
			RWT.getSettingStore().setAttribute(ATTR_LOGGED_ON_USER, userName);
			RWT.getSettingStore().setAttribute(ATTR_HOST_NAME, host);
		}
		catch (final IOException e) {
			logger.error("Could not save log on information for user '{}'!", userName, e);
		}
	}

}
