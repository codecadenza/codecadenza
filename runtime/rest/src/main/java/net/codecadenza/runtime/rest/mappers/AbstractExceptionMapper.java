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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.ResponseBuilder;
import jakarta.ws.rs.core.Response.Status;
import jakarta.ws.rs.core.Response.StatusType;
import jakarta.ws.rs.ext.ExceptionMapper;
import java.util.List;
import java.util.Optional;
import net.codecadenza.runtime.exception.ExceptionHelper;
import net.codecadenza.runtime.rest.util.AcceptHeaderParser;
import org.slf4j.Logger;

/**
 * <p>
 * Base class for all {@link ExceptionMapper exception mappers} in order to guarantee a consistent error handling
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <E> the type of the exception that should be handled by this {@link ExceptionMapper}
 */
public abstract class AbstractExceptionMapper<E extends Throwable> implements ExceptionMapper<E> {
	protected HttpServletRequest request;

	/**
	 * Determine the {@link Status} that should be returned
	 * @param exception
	 * @return the {@link Status} that should be returned in the case of an error
	 */
	protected abstract StatusType getStatus(E exception);

	/**
	 * An implementation must provide a {@link Logger}
	 * @return the logger that should be used
	 */
	protected abstract Logger getLogger();

	/**
	 * Set the HTTP servlet request
	 * @param request
	 */
	@Context
	public void setRequest(HttpServletRequest request) {
		this.request = request;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.ws.rs.ext.ExceptionMapper#toResponse(java.lang.Throwable)
	 */
	@Override
	public Response toResponse(E exception) {
		final StatusType status = getStatus(exception);
		final int statusCode = status.getStatusCode();
		final String reasonPhrase = status.getReasonPhrase();
		final String message = ExceptionHelper.getRootCause(exception).getMessage();
		final Logger logger = getLogger();
		final E exceptionToPrint = logger.isTraceEnabled() ? exception : null;

		if (statusCode >= Status.INTERNAL_SERVER_ERROR.getStatusCode())
			logger.error("Error while processing {} request on resource '{}'. Message: {}", request.getMethod(), request.getPathInfo(),
					message, exceptionToPrint);
		else
			logger.warn("Could not process {} request on resource '{}'. Message: {}", request.getMethod(), request.getPathInfo(),
					message, exceptionToPrint);

		// Extract the HTTP 'Accept' header to determine what representation of the response entity should be returned
		final List<MediaType> acceptedMediaTypes = AcceptHeaderParser.parse(request.getHeader(HttpHeaders.ACCEPT));
		final ResponseBuilder responseBuilder = Response.status(statusCode, reasonPhrase);
		final Optional<MediaType> supportedMediaType;

		if (acceptedMediaTypes.isEmpty())
			supportedMediaType = Optional.of(MediaType.APPLICATION_JSON_TYPE);
		else
			supportedMediaType = acceptedMediaTypes.stream()
					.filter(m -> m.equals(MediaType.APPLICATION_XML_TYPE) || m.equals(MediaType.APPLICATION_JSON_TYPE)
							|| m.equals(MediaType.TEXT_PLAIN_TYPE) || m.equals(MediaType.WILDCARD_TYPE))
					.findFirst();

		if (supportedMediaType.isPresent()) {
			if (supportedMediaType.get().equals(MediaType.WILDCARD_TYPE))
				responseBuilder.type(MediaType.APPLICATION_JSON);
			else
				responseBuilder.type(supportedMediaType.get());

			if (supportedMediaType.get().equals(MediaType.TEXT_PLAIN_TYPE))
				responseBuilder.entity(message);
			else {
				final var errorDetails = new ErrorDetails();
				errorDetails.setStatus(statusCode);
				errorDetails.setMessage(message);
				errorDetails.setRequestMethod(request.getMethod());
				errorDetails.setResourcePath(request.getPathInfo());

				responseBuilder.entity(errorDetails);
			}
		}
		else
			responseBuilder.type(acceptedMediaTypes.stream().findFirst().orElse(null));

		return responseBuilder.build();
	}

}
