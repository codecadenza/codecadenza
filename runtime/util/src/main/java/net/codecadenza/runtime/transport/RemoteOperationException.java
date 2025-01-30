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
package net.codecadenza.runtime.transport;

/**
 * <p>
 * This class represents a general exception that is thrown if a remote method invocation has failed. The root cause of the
 * failure can be manifold (e.g. a connection problem or another exception that has been thrown while invoking the method). This
 * exception is especially useful if the original exception isn't available in the invoker's classpath.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RemoteOperationException extends RuntimeException {
	private static final long serialVersionUID = 6780188587264587314L;

	/**
	 * Constructor
	 * @param message
	 */
	public RemoteOperationException(String message) {
		super(message);
	}

}
