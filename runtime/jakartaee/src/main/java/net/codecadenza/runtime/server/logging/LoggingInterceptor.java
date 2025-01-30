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

import jakarta.ejb.ApplicationException;
import jakarta.ejb.EJB;
import jakarta.interceptor.AroundInvoke;
import jakarta.interceptor.InvocationContext;
import net.codecadenza.runtime.exchange.DataExportException;
import net.codecadenza.runtime.exchange.DataImportException;
import net.codecadenza.runtime.repository.ConcurrentEntityModificationException;
import net.codecadenza.runtime.repository.DuplicateCollectionEntryException;
import net.codecadenza.runtime.repository.UniqueConstraintViolationException;
import net.codecadenza.runtime.search.exception.GeneralSearchException;
import net.codecadenza.runtime.validation.PropertyConstraintViolationException;

/**
 * <p>
 * Logging interceptor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingInterceptor {
	private @EJB LoggingService logService;

	/**
	 * Log the method invocation
	 * @param invocationContext
	 * @return the invocation result
	 * @throws Exception if the invocation of the intercepted method has failed
	 */
	@AroundInvoke
	public Object log(InvocationContext invocationContext) throws Exception {
		final long start = System.currentTimeMillis();
		final String className = invocationContext.getTarget().getClass().getName();

		try {
			final Object obj = invocationContext.proceed();

			if (Logger.isDebugEnabled()) {
				final long end = System.currentTimeMillis();
				final var logEntry = new LoggingDTO(className, invocationContext.getMethod().getName(), null, end - start, null);

				logService.debug(logEntry);
			}

			return obj;
		}
		catch (final Exception e) {
			final long end = System.currentTimeMillis();
			boolean isApplicationException = false;
			final var logEntry = new LoggingDTO(className, invocationContext.getMethod().getName(), null, end - start, e);

			// We just define that an application exception should be logged as warning! Everything else represents an error!
			if (e.getClass().isAnnotationPresent(ApplicationException.class))
				isApplicationException = true;

			// Check if an internal application exception was thrown! Note that these exceptions should be declared as application
			// exceptions in the respective ejb-jar.xml!
			if (e.getClass() == ConcurrentEntityModificationException.class)
				isApplicationException = true;

			if (e.getClass() == UniqueConstraintViolationException.class)
				isApplicationException = true;

			if (e.getClass() == DuplicateCollectionEntryException.class)
				isApplicationException = true;

			if (e.getClass() == PropertyConstraintViolationException.class)
				isApplicationException = true;

			if (e.getClass() == GeneralSearchException.class)
				isApplicationException = true;

			if (e.getClass() == DataExportException.class)
				isApplicationException = true;

			if (e.getClass() == DataImportException.class)
				isApplicationException = true;

			if (isApplicationException) {
				if (Logger.isWarnEnabled())
					logService.warn(logEntry);
			}
			else if (Logger.isErrorEnabled())
				logService.error(logEntry);

			throw e;
		}
	}

}
