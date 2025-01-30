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

import jakarta.ejb.ApplicationException;
import jakarta.inject.Inject;
import jakarta.security.enterprise.AuthenticationStatus;
import jakarta.security.enterprise.SecurityContext;
import jakarta.security.enterprise.authentication.mechanism.http.AuthenticationParameters;
import jakarta.security.enterprise.credential.Password;
import jakarta.security.enterprise.credential.UsernamePasswordCredential;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Implementation of the invocation servlet for applications that are deployed on a Jakarta EE container
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
	private static final long serialVersionUID = 6780201849419661049L;
	private static final String DOT = ".";
	private static final String JNDI_PREFIX = "java:module/";
	private static final String BEAN_SUFFIX = "Bean";
	private static HashMap<String, Object> serviceCache = new HashMap<>();

	private final transient SecurityContext securityContext;

	/**
	 * Default constructor
	 */
	public InvocationServlet() {
		this.securityContext = null;
	}

	/**
	 * Constructor
	 * @param securityContext
	 */
	@Inject
	public InvocationServlet(SecurityContext securityContext) {
		this.securityContext = securityContext;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet#lookup(java.lang.Class)
	 */
	@Override
	protected Object lookup(Class<?> serviceInterface) {
		Object bean = null;
		Context context = null;
		final String serviceName = serviceInterface.getName();

		// Build the JNDI name
		final var jndiNameBuilder = new StringBuilder(JNDI_PREFIX);
		jndiNameBuilder.append(serviceName.substring(serviceName.lastIndexOf(DOT) + 1));
		jndiNameBuilder.append(BEAN_SUFFIX);

		final String jndiName = jndiNameBuilder.toString();

		try {
			// Initialize the initial context
			context = new InitialContext();

			if (!serviceCache.containsKey(jndiName)) {
				logger.debug("Lookup for JNDI entry '{}'", jndiName);

				bean = context.lookup(jndiName);

				synchronized (serviceCache) {
					serviceCache.put(jndiName, bean);
				}
			}

			return serviceCache.get(jndiName);
		}
		catch (final NamingException e) {
			logger.error("Could not find JNDI entry '{}'!", jndiName, e);

			throw new IllegalStateException("Cannot connect to bean: " + jndiName + " Reason: " + e.getMessage(), e);
		}
		catch (final Exception e) {
			logger.error("Error while perfoming lookup operation for service '{}'!", jndiName, e);

			throw new IllegalStateException("Unexpected exception: " + jndiName + " Reason: " + e.getMessage(), e);
		}
		finally {
			try {
				if (context != null)
					context.close();
			}
			catch (final NamingException e) {
				logger.warn("Could not close naming context!", e);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet#convertException(java.lang.Throwable)
	 */
	@Override
	protected Throwable convertException(Throwable rootCause) {
		// We don't have to convert an ApplicationException!
		if (rootCause.getClass().isAnnotationPresent(ApplicationException.class))
			return rootCause;

		return super.convertException(rootCause);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet#login(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse, java.lang.String, java.lang.String)
	 */
	@Override
	protected boolean login(HttpServletRequest request, HttpServletResponse response, String userName, String password) {
		logger.debug("Login user '{}'", userName);

		try {
			final var credential = new UsernamePasswordCredential(userName, new Password(password));
			final AuthenticationStatus status = securityContext.authenticate(request, response,
					AuthenticationParameters.withParams().credential(credential));

			logger.debug("Authentication status: {}", status);

			if (status != AuthenticationStatus.SEND_CONTINUE && status != AuthenticationStatus.SUCCESS)
				return false;
		}
		catch (final Exception e) {
			logger.error("Error while performing login for user '{}'!", userName, e);

			return false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.server.transport.AbstractInvocationServlet# logout(jakarta.servlet.http.HttpServletRequest)
	 */
	@Override
	protected void logout(HttpServletRequest request) {
		try {
			request.logout();
		}
		catch (final ServletException e) {
			logger.error("Error while performing logout!", e);
		}
	}

}
