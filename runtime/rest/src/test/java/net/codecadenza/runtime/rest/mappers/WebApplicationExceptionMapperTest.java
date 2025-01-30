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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.ForbiddenException;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link WebApplicationExceptionMapper}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class WebApplicationExceptionMapperTest {
	private static final String ERROR_MSG = "There was an error!";
	private static final String RESOURCE_PATH = "path/to/resource";
	private static final long MAX_TIMESTAMP_DIF_MILLIS = 50;

	private WebApplicationExceptionMapper cut;
	private HttpServletRequest mockRequest;

	@BeforeEach
	void init() {
		mockRequest = mock(HttpServletRequest.class);

		cut = new WebApplicationExceptionMapper();
		cut.setRequest(mockRequest);
	}

	@Test
	void testValidTextResponse() {
		final var exception = new WebApplicationException(ERROR_MSG);
		final var mediaType = MediaType.TEXT_PLAIN_TYPE;

		mockRequest(mediaType);

		final Response response = cut.toResponse(exception);

		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
		assertEquals(ERROR_MSG, response.getEntity());
		assertEquals(mediaType, response.getMediaType());
	}

	@Test
	void testValidJSONResponse() {
		final var exception = new ForbiddenException(ERROR_MSG);
		final var mediaType = MediaType.APPLICATION_JSON_TYPE;

		mockRequest(mediaType);

		final ErrorDetails expectedErrorDetails = createExpectedErrorDetails(Status.FORBIDDEN.getStatusCode());

		checkResponse(exception, expectedErrorDetails, mediaType);
	}

	@Test
	void testValidJSONResponseIfWildcardTypeIsRequested() {
		final var exception = new ForbiddenException(ERROR_MSG);
		final var mediaType = MediaType.WILDCARD_TYPE;

		mockRequest(mediaType);

		final ErrorDetails expectedErrorDetails = createExpectedErrorDetails(Status.FORBIDDEN.getStatusCode());

		checkResponse(exception, expectedErrorDetails, MediaType.APPLICATION_JSON_TYPE);
	}

	@Test
	void testValidXMLResponse() {
		final var exception = new ForbiddenException(ERROR_MSG);
		final var mediaType = MediaType.APPLICATION_XML_TYPE;

		mockRequest(mediaType);

		final ErrorDetails expectedErrorDetails = createExpectedErrorDetails(Status.FORBIDDEN.getStatusCode());

		checkResponse(exception, expectedErrorDetails, mediaType);
	}

	@Test
	void testNoResponseEntityIfNoSupportedMediaType() {
		final var exception = new ForbiddenException(ERROR_MSG);
		final var mediaType = MediaType.APPLICATION_OCTET_STREAM_TYPE;

		mockRequest(mediaType);

		final Response response = cut.toResponse(exception);

		assertNull(response.getEntity());
		assertEquals(exception.getResponse().getStatus(), response.getStatus());
		assertEquals(mediaType, response.getMediaType());
	}

	@Test
	void testJSONResponseIfNoAcceptHeader() {
		final var exception = new ForbiddenException(ERROR_MSG);

		mockRequest(null);

		final ErrorDetails expectedErrorDetails = createExpectedErrorDetails(Status.FORBIDDEN.getStatusCode());

		checkResponse(exception, expectedErrorDetails, MediaType.APPLICATION_JSON_TYPE);
	}

	private void mockRequest(MediaType mediaType) {
		if (mediaType != null)
			when(mockRequest.getHeader(HttpHeaders.ACCEPT)).thenReturn(mediaType.toString());

		when(mockRequest.getMethod()).thenReturn(HttpMethod.GET);
		when(mockRequest.getPathInfo()).thenReturn(RESOURCE_PATH);
	}

	private ErrorDetails createExpectedErrorDetails(int statusCode) {
		final var expectedErrorResponse = new ErrorDetails();
		expectedErrorResponse.setMessage(ERROR_MSG);
		expectedErrorResponse.setResourcePath(RESOURCE_PATH);
		expectedErrorResponse.setRequestMethod(HttpMethod.GET);
		expectedErrorResponse.setStatus(statusCode);
		expectedErrorResponse.setTimestamp(System.currentTimeMillis());
		return expectedErrorResponse;
	}

	private void checkResponse(final WebApplicationException exception, final ErrorDetails expectedErrorDetails,
			final MediaType expectedMediaType) {
		final Response response = cut.toResponse(exception);
		final ErrorDetails actualErrorDetails = (ErrorDetails) response.getEntity();
		final long timestampDif = actualErrorDetails.getTimestamp() - expectedErrorDetails.getTimestamp();

		assertEquals(expectedErrorDetails.getStatus(), response.getStatus());
		assertEquals(expectedErrorDetails.getMessage(), actualErrorDetails.getMessage());
		assertEquals(expectedErrorDetails.getStatus(), actualErrorDetails.getStatus());
		assertEquals(expectedErrorDetails.getResourcePath(), actualErrorDetails.getResourcePath());
		assertEquals(expectedErrorDetails.getRequestMethod(), actualErrorDetails.getRequestMethod());
		assertEquals(expectedMediaType, response.getMediaType());
		assertTrue(timestampDif <= MAX_TIMESTAMP_DIF_MILLIS);
	}

}
