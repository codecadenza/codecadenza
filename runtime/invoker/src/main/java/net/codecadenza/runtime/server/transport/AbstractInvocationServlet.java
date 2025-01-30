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

import jakarta.persistence.OptimisticLockException;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import java.io.IOException;
import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import net.codecadenza.runtime.exception.ExceptionHelper;
import net.codecadenza.runtime.exchange.DataExportException;
import net.codecadenza.runtime.exchange.DataImportException;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.reflect.MethodFinder;
import net.codecadenza.runtime.repository.ConcurrentEntityModificationException;
import net.codecadenza.runtime.repository.DuplicateCollectionEntryException;
import net.codecadenza.runtime.repository.UniqueConstraintViolationException;
import net.codecadenza.runtime.search.exception.GeneralSearchException;
import net.codecadenza.runtime.stream.StreamWorker;
import net.codecadenza.runtime.transport.MarshalledInvocation;
import net.codecadenza.runtime.transport.MarshalledInvocationResult;
import net.codecadenza.runtime.transport.RemoteOperationException;
import net.codecadenza.runtime.transport.TransportConstants;
import net.codecadenza.runtime.validation.PropertyConstraintViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Server side interface that a client uses for communication
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractInvocationServlet extends HttpServlet {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -7111710755336680642L;
	private static final boolean PERFORM_AUTHENTICATION = new PropertyService()
			.getBooleanProperty(PropertyService.PROP_TRANSPORT_AUTHENTICATION);
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);
	private static final String MSG_NO_INFO = "Further information is not available!";

	/**
	 * The implementation class is responsible for defining how to search for a given service
	 * @param serviceInterface
	 * @return the service bean
	 */
	protected abstract Object lookup(Class<?> serviceInterface);

	/**
	 * @param request
	 * @param response
	 * @param userName
	 * @param password
	 * @return true if the login was successful
	 */
	protected abstract boolean login(HttpServletRequest request, HttpServletResponse response, String userName, String password);

	/**
	 * @param request
	 */
	protected abstract void logout(HttpServletRequest request);

	/**
	 * Convert the given exception in order to avoid sending back an exception that the client might not know!
	 * @param rootCause
	 * @return the converted exception
	 */
	protected Throwable convertException(Throwable rootCause) {
		boolean convert = true;

		if (rootCause.getClass() == ConcurrentEntityModificationException.class
				|| rootCause.getClass() == PropertyConstraintViolationException.class)
			convert = false;

		if (rootCause.getClass() == UniqueConstraintViolationException.class
				|| rootCause.getClass() == DuplicateCollectionEntryException.class)
			convert = false;

		if (rootCause.getClass() == GeneralSearchException.class)
			convert = false;

		if (rootCause.getClass() == DataImportException.class || rootCause.getClass() == DataExportException.class)
			convert = false;

		if (!convert) {
			// Return the original exception
			return rootCause;
		}

		Throwable convertedThrowable = null;

		if (rootCause.getClass() == OptimisticLockException.class) {
			final var lockException = (OptimisticLockException) rootCause;

			convertedThrowable = new ConcurrentEntityModificationException(lockException.getMessage());
		}
		else if (rootCause.getClass() == ConstraintViolationException.class) {
			final var c = (ConstraintViolationException) rootCause;
			final var msg = new StringBuilder();

			for (final ConstraintViolation<?> constraintViolation : c.getConstraintViolations()) {
				msg.append(constraintViolation.getMessage());
				msg.append("\n");
			}

			convertedThrowable = new RemoteOperationException(msg.toString());
		}
		else if (rootCause.getMessage() == null || rootCause.getMessage().isEmpty())
			convertedThrowable = new RemoteOperationException(MSG_NO_INFO);
		else
			convertedThrowable = new RemoteOperationException(rootCause.getMessage());

		return convertedThrowable;
	}

	/**
	 * Invoke the service method
	 * @param serviceName the fully qualified class name of the service interface
	 * @param methodName the method name
	 * @param args the method arguments
	 * @return the result object that should be send back to the client containing the method's return value or an exception that
	 *         has been thrown
	 */
	public MarshalledInvocationResult invoke(String serviceName, String methodName, Object... args) {
		try {
			final Class<?> serviceInterface = Class.forName(serviceName);

			// Get the appropriate method
			final Method m = MethodFinder.findMethod(serviceInterface, methodName, args);

			// Lookup for the business interface
			final Object bean = lookup(serviceInterface);

			logger.debug("Invoke method '{}' of service '{}'", methodName, serviceName);

			// Invoke the business method
			final Serializable result = (Serializable) m.invoke(bean, args);

			return new MarshalledInvocationResult(result);
		}
		catch (final Exception e) {
			logger.error("Error while invoking method '{}' of service '{}'!", methodName, serviceName, e);

			final Throwable convertedThrowable = convertException(ExceptionHelper.getRootCause(e));

			// Return the converted exception
			return new MarshalledInvocationResult(convertedThrowable);
		}
	}

	/**
	 * Process the request
	 * @param request the HTTP request object
	 * @param response the HTTP response object
	 * @throws IOException if sending of an error has failed
	 */
	protected void processRequest(HttpServletRequest request, HttpServletResponse response) throws IOException {
		final var streamWorker = new StreamWorker(SECRET);

		// Check if the data is encrypted
		final String propertyEncrypt = request.getHeader(TransportConstants.REQ_PROP_ENCRYPTION);
		final Boolean encrypted = Boolean.valueOf(propertyEncrypt);

		logger.debug("Process invocation request from '{}'", request.getRemoteAddr());

		try {
			// Extract the invocation object from the stream
			final var mi = (MarshalledInvocation) streamWorker.readObjectFromStream(request.getInputStream(), encrypted);

			if (mi == null) {
				final var illegalContentException = new RemoteOperationException("Invocation data is either missing or incorrect!");
				streamWorker.writeObjectToStream(new MarshalledInvocationResult(illegalContentException), response.getOutputStream(),
						encrypted);
				return;
			}

			// Check if the application requires authentication!
			if (PERFORM_AUTHENTICATION && !login(request, response, mi.getUserName(), mi.getPassword())) {
				streamWorker.writeObjectToStream(new MarshalledInvocationResult(new SecurityException("Authentication failed!")),
						response.getOutputStream(), encrypted);
				return;
			}

			// Send the results back to the client
			final MarshalledInvocationResult resultObject = invoke(mi.getServiceInterfaceName(), mi.getMethodName(), mi.getArguments());

			streamWorker.writeObjectToStream(resultObject, response.getOutputStream(), encrypted);

			if (PERFORM_AUTHENTICATION)
				logout(request);
		}
		catch (final Exception ex) {
			logger.error("Error while processing invocation request from '{}'!", request.getRemoteAddr(), ex);

			try {
				final var unexpectedError = new RemoteOperationException(
						"The server encountered an unexpected error! Message: " + ex.getMessage());
				streamWorker.writeObjectToStream(new MarshalledInvocationResult(unexpectedError), response.getOutputStream(), encrypted);
			}
			catch (final Exception e) {
				logger.error("Could not create invocation result object!", e);

				// If it is not possible to provide an exception we can only send back an error!
				response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.servlet.http.HttpServlet#doGet(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		try {
			processRequest(req, resp);
		}
		catch (final IOException e) {
			logger.error("An internal error occurred while processing the GET request!", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.servlet.http.HttpServlet#doPost(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		try {
			processRequest(req, resp);
		}
		catch (final IOException e) {
			logger.error("An internal error occurred while processing the POST request!", e);
		}
	}

}
