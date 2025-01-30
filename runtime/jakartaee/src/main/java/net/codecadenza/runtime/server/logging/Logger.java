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
package net.codecadenza.runtime.server.logging;

import static net.codecadenza.runtime.server.logging.LoggingService.DEBUG_LEVEL;
import static net.codecadenza.runtime.server.logging.LoggingService.DISABLED;
import static net.codecadenza.runtime.server.logging.LoggingService.ERROR_LEVEL;
import static net.codecadenza.runtime.server.logging.LoggingService.INFO_LEVEL;
import static net.codecadenza.runtime.server.logging.LoggingService.NOT_INITIALIZED;
import static net.codecadenza.runtime.server.logging.LoggingService.WARN_LEVEL;

import net.codecadenza.runtime.property.IllegalPropertyValueException;
import net.codecadenza.runtime.property.PropertyService;

/**
 * <p>
 * Utility class that reads the logging level from the application.properties file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Logger {
	private static int loggingLevelValue = NOT_INITIALIZED;

	static {
		init();
	}

	/**
	 * This class is a singleton
	 */
	private Logger() {
	}

	/**
	 * Initialize the log level
	 */
	private static synchronized void init() {
		try {
			loggingLevelValue = new PropertyService().getIntProperty(PropertyService.PROP_LOGGING_LEVEL);
		}
		catch (final IllegalPropertyValueException e) {
			// It might be the case that the configuration file does not contain the respective property!
			loggingLevelValue = DISABLED;
		}
	}

	/**
	 * @return true if the log level is 'DEBUG'
	 */
	public static boolean isDebugEnabled() {
		return loggingLevelValue >= DEBUG_LEVEL;
	}

	/**
	 * @return true if the log level is 'INFO'
	 */
	public static boolean isInfoEnabled() {
		return loggingLevelValue >= INFO_LEVEL;
	}

	/**
	 * @return true if the log level is 'WARN'
	 */
	public static boolean isWarnEnabled() {
		return loggingLevelValue >= WARN_LEVEL;
	}

	/**
	 * @return true if the application should log errors only
	 */
	public static boolean isErrorEnabled() {
		return loggingLevelValue >= ERROR_LEVEL;
	}

}
