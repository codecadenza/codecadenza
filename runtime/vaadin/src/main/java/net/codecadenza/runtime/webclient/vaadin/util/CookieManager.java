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
package net.codecadenza.runtime.webclient.vaadin.util;

import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinResponse;
import jakarta.enterprise.context.SessionScoped;
import jakarta.servlet.http.Cookie;
import java.io.Serializable;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * <p>
 * Utility class for handling cookies
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SessionScoped
public class CookieManager implements Serializable {
	private static final long serialVersionUID = 3102360041631782253L;

	/**
	 * @param name
	 * @return the cookie or null if a cookie with this name could not be found!
	 */
	public Cookie getCookieByName(String name) {
		if (VaadinRequest.getCurrent() == null || VaadinRequest.getCurrent().getCookies() == null)
			return null;

		// Fetch all cookies from the request
		final Cookie[] cookies = VaadinRequest.getCurrent().getCookies();

		// Find a cookie by its name
		for (final Cookie cookie : cookies)
			if (name.equals(cookie.getName()))
				return cookie;

		return null;
	}

	/**
	 * Save the cookie
	 * @param cookie
	 */
	public void saveCookie(Cookie cookie) {
		if (VaadinResponse.getCurrent() == null)
			return;

		VaadinResponse.getCurrent().addCookie(cookie);
	}

	/**
	 * Save a cookie
	 * @param name
	 * @param value
	 */
	public void saveCookie(String name, String value) {
		saveCookie(name, value, Integer.MAX_VALUE, false);
	}

	/**
	 * Save a cookie
	 * @param name
	 * @param value
	 * @param maxAge
	 * @param secure
	 */
	public void saveCookie(String name, String value, int maxAge, boolean secure) {
		if (VaadinResponse.getCurrent() == null || VaadinRequest.getCurrent() == null)
			return;

		final var cookie = new Cookie(name, URLEncoder.encode(value, StandardCharsets.UTF_8));
		cookie.setMaxAge(maxAge);
		cookie.setPath(VaadinRequest.getCurrent().getContextPath());
		cookie.setSecure(secure);

		VaadinResponse.getCurrent().addCookie(cookie);
	}

}
