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
package net.codecadenza.runtime.webclient.primefaces.util;

import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import java.util.Map;

/**
 * <p>
 * Bean for collecting details about an error
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Named
@RequestScoped
public class ErrorDetails {
	private static final String ERROR_NS = "jakarta.servlet.error.";
	private static final String KEY_STATUS_CODE = ERROR_NS + "status_code";
	private static final String KEY_ERROR_MSG = ERROR_NS + "message";
	private static final String KEY_EXCEPTION_TYPE = ERROR_NS + "exception_type";
	private static final String KEY_EXCEPTION = ERROR_NS + "exception";
	private static final String KEY_REQUEST_URI = ERROR_NS + "request_uri";
	private static final String KEY_SERVLET_NAME = ERROR_NS + "servlet_name";

	/**
	 * @return the HTTP status code
	 */
	public String getStatusCode() {
		final Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

		if (requestMap.get(KEY_STATUS_CODE) == null)
			return "";

		return String.valueOf(requestMap.get(KEY_STATUS_CODE));
	}

	/**
	 * @return the error message
	 */
	public String getMessage() {
		final Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

		if (requestMap.get(KEY_ERROR_MSG) == null)
			return "";

		return (String) requestMap.get(KEY_ERROR_MSG);
	}

	/**
	 * @return the exception type
	 */
	public String getExceptionType() {
		final Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

		if (requestMap.get(KEY_EXCEPTION_TYPE) == null)
			return "";

		return requestMap.get(KEY_EXCEPTION_TYPE).toString();
	}

	/**
	 * @return the exception
	 */
	public String getException() {
		final Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

		if (requestMap.get(KEY_EXCEPTION) == null)
			return "";

		final var ex = (Exception) requestMap.get(KEY_EXCEPTION);
		return ex.getMessage();
	}

	/**
	 * @return the request URI
	 */
	public String getRequestURI() {
		final Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

		if (requestMap.get(KEY_REQUEST_URI) == null)
			return "";

		return (String) requestMap.get(KEY_REQUEST_URI);
	}

	/**
	 * @return the servlet name
	 */
	public String getServletName() {
		final Map<String, Object> requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

		if (requestMap.get(KEY_SERVLET_NAME) == null)
			return "";

		return (String) requestMap.get(KEY_SERVLET_NAME);
	}

}
