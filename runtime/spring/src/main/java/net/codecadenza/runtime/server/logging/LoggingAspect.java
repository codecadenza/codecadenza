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

import net.codecadenza.runtime.exchange.DataExportException;
import net.codecadenza.runtime.exchange.DataImportException;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.repository.ConcurrentEntityModificationException;
import net.codecadenza.runtime.repository.DuplicateCollectionEntryException;
import net.codecadenza.runtime.repository.UniqueConstraintViolationException;
import net.codecadenza.runtime.search.exception.GeneralSearchException;
import net.codecadenza.runtime.validation.PropertyConstraintViolationException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.springframework.beans.factory.annotation.Value;

/**
 * <p>
 * Simple logging aspect
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingAspect {
	private final LoggingService logger;
	private @Value("${" + PropertyService.PROP_LOGGING_LEVEL + "}") int loggingLevel;

	/**
	 * Constructor
	 * @param logger
	 */
	public LoggingAspect(LoggingService logger) {
		this.logger = logger;
	}

	/**
	 * @param joinPoint
	 * @return the invocation result
	 * @throws Throwable if the invocation of the target method has failed
	 */
	public Object logAround(ProceedingJoinPoint joinPoint) throws Throwable {
		final long start = System.currentTimeMillis();

		try {
			final Object result = joinPoint.proceed();

			if (loggingLevel == LoggingService.DEBUG_LEVEL) {
				final long end = System.currentTimeMillis();
				final String methodName = joinPoint.getSignature().getName();
				final String className = joinPoint.getSignature().getDeclaringTypeName();
				final var logEntry = new LoggingDTO(className, methodName, null, end - start, null);

				logger.debug(logEntry);
			}

			return result;
		}
		catch (final Exception e) {
			final String methodName = joinPoint.getSignature().getName();
			final String className = joinPoint.getSignature().getDeclaringTypeName();
			final long end = System.currentTimeMillis();
			boolean isError = true;
			final var logEntry = new LoggingDTO(className, methodName, e.getMessage(), end - start, e);

			// Internal runtime exceptions should be logged as warnings!
			if (e.getClass() == ConcurrentEntityModificationException.class)
				isError = false;

			if (e.getClass() == UniqueConstraintViolationException.class)
				isError = false;

			if (e.getClass() == DuplicateCollectionEntryException.class)
				isError = false;

			if (e.getClass() == PropertyConstraintViolationException.class)
				isError = false;

			if (e.getClass() == GeneralSearchException.class)
				isError = false;

			if (e.getClass() == DataExportException.class)
				isError = false;

			if (e.getClass() == DataImportException.class)
				isError = false;

			if (isError) {
				if (loggingLevel >= LoggingService.ERROR_LEVEL)
					logger.error(logEntry);
			}
			else if (loggingLevel >= LoggingService.WARN_LEVEL)
				logger.warn(logEntry);

			// We just rethrow the original exception!
			throw e;
		}
	}

	/**
	 * @return the logging level
	 */
	public int getLoggingLevel() {
		return loggingLevel;
	}

	/**
	 * @param loggingLevel
	 */
	public void setLoggingLevel(int loggingLevel) {
		this.loggingLevel = loggingLevel;
	}

}
