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
package net.codecadenza.eclipse.tools.util.db;

/**
 * <p>
 * This class encapsulates all kinds of database manager exceptions
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBManagerException extends Exception {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor
	 */
	public DBManagerException() {
	}

	/**
	 * Constructor
	 * @param message the error message
	 */
	public DBManagerException(String message) {
		super(message);
	}

	/**
	 * Constructor
	 * @param cause the cause of the exception
	 */
	public DBManagerException(Throwable cause) {
		super(cause);
	}

	/**
	 * Constructor
	 * @param message the error message
	 * @param cause the cause of the exception
	 */
	public DBManagerException(String message, Throwable cause) {
		super(message, cause);
	}

}
