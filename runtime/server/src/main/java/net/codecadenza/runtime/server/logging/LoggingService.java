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

/**
 * <p>
 * Interface that must be implemented by the logging service
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface LoggingService {
	int DEBUG_LEVEL = 4;
	int INFO_LEVEL = 3;
	int WARN_LEVEL = 2;
	int ERROR_LEVEL = 1;
	int DISABLED = -1;
	int NOT_INITIALIZED = 0;

	/**
	 * Save an information log entry
	 * @param logEntry
	 */
	void info(LoggingDTO logEntry);

	/**
	 * Save a warning log entry
	 * @param logEntry
	 */
	void warn(LoggingDTO logEntry);

	/**
	 * Save a debug log entry
	 * @param logEntry
	 */
	void debug(LoggingDTO logEntry);

	/**
	 * Save an error log entry
	 * @param logEntry
	 */
	void error(LoggingDTO logEntry);

}
