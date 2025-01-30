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

import java.io.Serializable;

/**
 * <p>
 * Instances of this class represent the result of a remote method invocation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MarshalledInvocationResult implements Serializable {
	private static final long serialVersionUID = -2197689124013966103L;

	private final Serializable returnValue;
	private final Throwable throwable;

	/**
	 * Constructor
	 * @param returnValue
	 */
	public MarshalledInvocationResult(Serializable returnValue) {
		this.returnValue = returnValue;
		this.throwable = null;
	}

	/**
	 * Constructor
	 * @param throwable
	 */
	public MarshalledInvocationResult(Throwable throwable) {
		this.returnValue = null;
		this.throwable = throwable;
	}

	/**
	 * Get the invocation result
	 * @return the result of the invocation
	 * @throws Throwable if the remote method has thrown an exception
	 */
	public Serializable getReturnValue() throws Throwable {
		if (hasException())
			throw throwable;

		return returnValue;
	}

	/**
	 * @return true if an exception was thrown
	 */
	public boolean hasException() {
		return throwable != null;
	}

	/**
	 * @return an available invocation exception or null
	 */
	public Throwable getException() {
		return throwable;
	}

}
