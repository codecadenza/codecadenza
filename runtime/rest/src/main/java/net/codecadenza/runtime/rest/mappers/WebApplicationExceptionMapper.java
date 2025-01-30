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
package net.codecadenza.runtime.rest.mappers;

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response.StatusType;
import jakarta.ws.rs.ext.Provider;
import java.lang.invoke.MethodHandles;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Base class for all JAX-RS exception mappers. Note that this exception mapper ignores most information that is already contained
 * in the exception's response object in order return the same response entity structure for all kinds of exceptions that are
 * thrown by either the application or the JAX-RS implementation at runtime.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Provider
public class WebApplicationExceptionMapper extends AbstractExceptionMapper<WebApplicationException> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.rest.mappers.AbstractExceptionMapper#getStatus(java.lang.Throwable)
	 */
	@Override
	protected StatusType getStatus(WebApplicationException exception) {
		return exception.getResponse().getStatusInfo();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.rest.mappers.AbstractExceptionMapper#getLogger()
	 */
	@Override
	protected Logger getLogger() {
		return logger;
	}

}
