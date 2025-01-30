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
 * Instances of this class are used to collect invocation data that is exchanged between a local application and a remote system
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MarshalledInvocation implements Serializable {
	private static final long serialVersionUID = 833047993710449290L;

	private boolean asynchronous;
	private String serviceInterfaceName;
	private String userName;
	private String password;
	private String methodName;
	private Serializable[] arguments;
	private Serializable returnValue;

	/**
	 * @return the asynchronous flag
	 */
	public boolean isAsynchronous() {
		return asynchronous;
	}

	/**
	 * @param asynchronous the asynchronous flag to set
	 */
	public void setAsynchronous(boolean asynchronous) {
		this.asynchronous = asynchronous;
	}

	/**
	 * @return the user name
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @param userName the user name to set
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @param password the password to set
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * @return the arguments
	 */
	public Object[] getArguments() {
		return arguments;
	}

	/**
	 * @param arguments the arguments to set
	 */
	public void setArguments(Serializable[] arguments) {
		this.arguments = arguments;
	}

	/**
	 * @return the method name
	 */
	public String getMethodName() {
		return methodName;
	}

	/**
	 * @param methodName the method name to set
	 */
	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}

	/**
	 * @return the return value
	 */
	public Object getReturnValue() {
		return returnValue;
	}

	/**
	 * @param returnValue the return value to set
	 */
	public void setReturnValue(Serializable returnValue) {
		this.returnValue = returnValue;
	}

	/**
	 * @return the serial version UID
	 */
	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	/**
	 * @return the fully qualified class name of the service interface
	 */
	public String getServiceInterfaceName() {
		return serviceInterfaceName;
	}

	/**
	 * @param serviceInterfaceName the fully qualified class name of the service interface to set
	 */
	public void setServiceInterfaceName(String serviceInterfaceName) {
		this.serviceInterfaceName = serviceInterfaceName;
	}

}
