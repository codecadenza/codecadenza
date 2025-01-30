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
package net.codecadenza.runtime.richclient.transport.handler;

import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URI;
import java.util.stream.Stream;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.stream.StreamWorker;
import net.codecadenza.runtime.transport.MarshalledInvocation;
import net.codecadenza.runtime.transport.MarshalledInvocationResult;
import net.codecadenza.runtime.transport.TransportConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Invocation handler
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class HTTPInvocationHandler implements InvocationHandler {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String INVOCATION_CONTEXT = "/invocation";
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);

	private final String targetURL;
	private final String serviceName;
	private final String userName;
	private final String password;
	private final boolean encrypt;

	/**
	 * Constructor
	 * @param userName the name of the user
	 * @param password
	 * @param serviceName the fully qualified class name of the service interface
	 * @param url the URL to connect to
	 * @param encrypt flag that indicates if encryption should be used
	 */
	public HTTPInvocationHandler(String userName, String password, String serviceName, String url, boolean encrypt) {
		this.userName = userName;
		this.password = password;
		this.targetURL = url;
		this.serviceName = serviceName;
		this.encrypt = encrypt;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	@Override
	public Object invoke(Object object, Method method, Object[] args) throws Throwable {
		final var streamWorker = new StreamWorker(SECRET);
		final URI targetURI = URI.create(targetURL + INVOCATION_CONTEXT);
		final var arguments = args != null ? Stream.of(args).map(Serializable.class::cast).toArray(Serializable[]::new) : null;
		final MarshalledInvocationResult invocationResult;
		MarshalledInvocation invocation;
		HttpURLConnection connection = null;

		// Do some further initialization on invocation
		invocation = new MarshalledInvocation();
		invocation.setUserName(userName);
		invocation.setPassword(password);
		invocation.setArguments(arguments);
		invocation.setMethodName(method.getName());
		invocation.setReturnValue(method.getReturnType());
		invocation.setServiceInterfaceName(serviceName);

		logger.debug("Invoke remote method '{}' of service '{}' by using URL '{}'!", method.getName(), serviceName, targetURL);

		try {
			connection = (HttpURLConnection) targetURI.toURL().openConnection();
			connection.setDoOutput(true);
			connection.addRequestProperty(TransportConstants.REQ_PROP_ENCRYPTION, Boolean.toString(encrypt));
			connection.addRequestProperty(TransportConstants.CONTENT_TYPE_KEY, TransportConstants.CONTENT_TYPE_VALUE);

			// Send the invocation object
			streamWorker.writeObjectToStream(invocation, connection.getOutputStream(), encrypt);

			// Read the result from the server
			invocationResult = (MarshalledInvocationResult) streamWorker.readObjectFromStream(connection.getInputStream(), encrypt);
		}
		catch (final Exception e) {
			logger.error("Error while invoking remote method '{}'!", method.getName(), e);

			throw new RuntimeException(e);
		}

		if (invocationResult.getException() != null)
			throw invocationResult.getException();

		return invocationResult.getReturnValue();
	}

}
