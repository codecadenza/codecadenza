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
package net.codecadenza.runtime.server.transport;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.crypto.HashGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * <p>
 * Implementation of the invocation servlet for Spring applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@WebServlet(value = "/invocation", name = "InvocationServlet", loadOnStartup = 1)
public class InvocationServlet extends AbstractInvocationServlet {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -4547109536056246514L;

	private final transient ApplicationContext appContext;
	private final transient AuthenticationManager authenticationManager;

	/**
	 * Constructor
	 * @param appContext
	 * @param authenticationManager
	 */
	public InvocationServlet(ApplicationContext appContext, AuthenticationManager authenticationManager) {
		this.appContext = appContext;
		this.authenticationManager = authenticationManager;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet#lookup(java.lang.Class)
	 */
	@Override
	protected Object lookup(Class<?> serviceInterface) {
		if (appContext.getBeansOfType(serviceInterface).isEmpty())
			throw new IllegalStateException("Implementation of given interface doesn't exist!");

		if (appContext.getBeansOfType(serviceInterface).size() > 1)
			throw new IllegalStateException("Service interface is implemented by more than one bean!");

		logger.debug("Lookup for service '{}'", serviceInterface.getName());

		return appContext.getBean(serviceInterface);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet#login(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse, java.lang.String, java.lang.String)
	 */
	@Override
	protected boolean login(HttpServletRequest request, HttpServletResponse response, String userName, String password) {
		Authentication authenticate = null;

		try {
			logger.debug("Login user '{}'", userName);

			final String encryptedPassword = HashGenerator.encryptSHA256(password);

			authenticate = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(userName, encryptedPassword));
		}
		catch (final Exception e) {
			logger.error("Error while performing login for user '{}'!", userName, e);

			return false;
		}

		SecurityContextHolder.getContext().setAuthentication(authenticate);

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet# logout(jakarta.servlet.http.HttpServletRequest)
	 */
	@Override
	protected void logout(HttpServletRequest request) {
		SecurityContextHolder.getContext().setAuthentication(null);
	}

}
