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
package net.codecadenza.runtime.ddt.service.completion;

import net.codecadenza.runtime.ddt.service.util.AbstractFileProperties;

/**
 * <p>
 * Properties that are used to initialize an {@link IInvocationCompletionHandler}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class InvocationCompletionHandlerProperties extends AbstractFileProperties {
	private static final long serialVersionUID = -532068038412219206L;

	private static final String PROPERTY_INVOCATION_HANDLER_CLASS_NAME = "INVOCATION_HANDLER_CLASS_NAME";
	private static final String PROPERTY_RESOURCE_URL = "RESOURCE_URL";
	private static final String PROPERTY_USER_NAME = "USER_NAME";
	private static final String PROPERTY_PASSWORD = "PASSWORD";
	private static final String PROPERTY_DEFAULT_TIMEOUT = "DEFAULT_TIMEOUT_MS";

	/**
	 * @return the fully-qualified provider class name of the {@link IInvocationCompletionHandler} that should be used
	 */
	public String getInvocationHandlerClassName() {
		return getStringProperty(PROPERTY_INVOCATION_HANDLER_CLASS_NAME);
	}

	/**
	 * @return the URL of the system that should be accessed
	 */
	public String getResourceURL() {
		return getStringProperty(PROPERTY_RESOURCE_URL);
	}

	/**
	 * @return the user name
	 */
	public String getUserName() {
		return getStringProperty(PROPERTY_USER_NAME);
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return getStringProperty(PROPERTY_PASSWORD);
	}

	/**
	 * @return the default timeout in milliseconds
	 */
	public long getDefaultTimeout() {
		return getLongProperty(PROPERTY_DEFAULT_TIMEOUT);
	}
}
