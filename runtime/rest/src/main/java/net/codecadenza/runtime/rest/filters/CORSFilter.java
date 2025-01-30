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
package net.codecadenza.runtime.rest.filters;

import jakarta.annotation.PostConstruct;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.container.ContainerResponseContext;
import jakarta.ws.rs.container.ContainerResponseFilter;
import jakarta.ws.rs.container.PreMatching;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.Provider;
import java.io.IOException;
import net.codecadenza.runtime.property.PropertyService;

/**
 * <p>
 * Filter for adding CORS-related HTTP headers that allow single-page applications to make requests to another domain
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Provider
@PreMatching
public class CORSFilter implements ContainerResponseFilter, ContainerRequestFilter {
	private static final String PROP_ACCESS_CONTROL_ORIGIN = "codecadenza.application.rest.access-control-origin";
	private static final String PROP_ACCESS_CONTROL_HEADERS = "codecadenza.application.rest.access-control-headers";
	private static final String PROP_ACCESS_CONTROL_MAX_AGE = "codecadenza.application.rest.access-control-max-age";
	private static final String PROP_ACCESS_CONTROL_METHODS = "codecadenza.application.rest.access-control-methods";
	private static final String HTTP_HEADER_ORIGIN = "Origin";
	private static final String HTTP_HEADER_ALLOW_CREDENTIALS = "Access-Control-Allow-Credentials";
	private static final String HTTP_HEADER_ALLOW_HEADERS = "Access-Control-Allow-Headers";
	private static final String HTTP_HEADER_ALLOW_METHODS = "Access-Control-Allow-Methods";
	private static final String HTTP_HEADER_ALLOW_ORIGIN = "Access-Control-Allow-Origin";
	private static final String HTTP_HEADER_MAX_AGE = "Access-Control-Max-Age";

	private String accessControlOrigin;
	private String accessControlHeaders;
	private String accessControlMethods;
	private long accessControlMaxAge;

	/**
	 * Initialize the filter by loading the access control properties
	 */
	@PostConstruct
	public void init() {
		final var propertyService = new PropertyService();

		accessControlOrigin = propertyService.getStringProperty(PROP_ACCESS_CONTROL_ORIGIN);
		accessControlHeaders = propertyService.getStringProperty(PROP_ACCESS_CONTROL_HEADERS);
		accessControlMethods = propertyService.getStringProperty(PROP_ACCESS_CONTROL_METHODS);
		accessControlMaxAge = propertyService.getLongProperty(PROP_ACCESS_CONTROL_MAX_AGE);
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.ws.rs.container.ContainerRequestFilter#filter(jakarta.ws.rs.container.ContainerRequestContext)
	 */
	@Override
	public void filter(ContainerRequestContext request) throws IOException {
		// In case of a preflight request, abort the request with a 200 status.
		// The CORS headers are added in the response filter method.
		if (isPreflightRequest(request))
			request.abortWith(Response.ok().build());
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.ws.rs.container.ContainerResponseFilter#filter(jakarta.ws.rs.container.ContainerRequestContext,
	 * jakarta.ws.rs.container.ContainerResponseContext)
	 */
	@Override
	public void filter(ContainerRequestContext request, ContainerResponseContext responseContext) throws IOException {
		// If there is no 'Origin' header, it isn't a cross-origin request!
		if (request.getHeaderString(HTTP_HEADER_ORIGIN) == null)
			return;

		// Add all CORS headers if it is a preflight request!
		if (isPreflightRequest(request)) {
			addResponseHeader(responseContext, HTTP_HEADER_ALLOW_CREDENTIALS, Boolean.TRUE.toString());
			addResponseHeader(responseContext, HTTP_HEADER_ALLOW_HEADERS, accessControlHeaders);
			addResponseHeader(responseContext, HTTP_HEADER_ALLOW_METHODS, accessControlMethods);
			addResponseHeader(responseContext, HTTP_HEADER_MAX_AGE, String.valueOf(accessControlMaxAge));
		}

		// Cross-origin requests can be either simple requests or preflight requests. This header must be added to both request types!
		addResponseHeader(responseContext, HTTP_HEADER_ALLOW_ORIGIN, accessControlOrigin);
	}

	/**
	 * @param request
	 * @return true if the request is a preflight request
	 */
	private boolean isPreflightRequest(ContainerRequestContext request) {
		return request.getHeaderString(HTTP_HEADER_ORIGIN) != null && request.getMethod().equalsIgnoreCase(HttpMethod.OPTIONS);
	}

	/**
	 * Add a key-value pair to the response header
	 * @param responseContext
	 * @param header
	 * @param value
	 */
	private void addResponseHeader(ContainerResponseContext responseContext, String header, String value) {
		// Don't add the header if it is already there!
		if (!responseContext.getHeaders().containsKey(header))
			responseContext.getHeaders().add(header, value);
	}

}
