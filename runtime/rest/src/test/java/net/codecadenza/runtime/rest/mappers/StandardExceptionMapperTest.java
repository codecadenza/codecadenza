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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link StandardExceptionMapper}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class StandardExceptionMapperTest {
	private static final String RESOURCE_PATH = "path/to/resource";
	private static final String ERROR_MSG = "There was an error!";

	private StandardExceptionMapper cut;
	private HttpServletRequest mockRequest;

	@BeforeEach
	void init() {
		mockRequest = mock(HttpServletRequest.class);

		cut = new StandardExceptionMapper();
		cut.setRequest(mockRequest);
	}

	@Test
	void testValidResponse() {
		final var exception = new IllegalStateException(ERROR_MSG);
		final var mediaType = MediaType.TEXT_PLAIN_TYPE;

		when(mockRequest.getHeader(HttpHeaders.ACCEPT)).thenReturn(mediaType.toString());
		when(mockRequest.getMethod()).thenReturn(HttpMethod.GET);
		when(mockRequest.getPathInfo()).thenReturn(RESOURCE_PATH);

		final Response response = cut.toResponse(exception);

		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
		assertEquals(ERROR_MSG, response.getEntity());
		assertEquals(mediaType, response.getMediaType());
	}

}
