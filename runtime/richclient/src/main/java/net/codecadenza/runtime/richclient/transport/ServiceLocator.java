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
package net.codecadenza.runtime.richclient.transport;

import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.SERVICE_LOCATOR_ERR_NOT_INITIALIZED;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.SERVICE_LOCATOR_ERR_NULL_PARAM;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.getTranslation;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import net.codecadenza.runtime.richclient.transport.handler.FileTransportHandler;
import net.codecadenza.runtime.richclient.transport.handler.HTTPInvocationHandler;
import net.codecadenza.runtime.richclient.transport.handler.LocalInvocationHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Service locator that encapsulates access to services that are provided by local or remote systems
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServiceLocator {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String MSG_ILLEGAL_ARGUMENT = getTranslation(SERVICE_LOCATOR_ERR_NULL_PARAM);
	private static final String MSG_ILLEGAL_STATE = getTranslation(SERVICE_LOCATOR_ERR_NOT_INITIALIZED);

	private static String url;
	private static String userName;
	private static String password;
	private static boolean encrypt;
	private static boolean initialized;
	private static TransportType transportType;
	private static String alias;
	private static boolean chunkedStreamingMode;

	/**
	 * Prevent instantiation
	 */
	private ServiceLocator() {

	}

	/**
	 * Initialize the service locator in order to use it properly
	 * @param settings
	 */
	public static synchronized void initialize(ServiceLocatorDTO settings) {
		ServiceLocator.url = settings.getConnectionURL();
		ServiceLocator.userName = settings.getUserName();
		ServiceLocator.password = settings.getPassword();
		ServiceLocator.alias = settings.getAlias();
		ServiceLocator.transportType = settings.getTransportType();
		ServiceLocator.chunkedStreamingMode = settings.isChunkedStreamingMode();
		ServiceLocator.encrypt = settings.isEncrypt();
		ServiceLocator.initialized = true;
	}

	/**
	 * @return the current settings of the service locator
	 */
	public static ServiceLocatorDTO getServiceLocatorSettings() {
		final var settings = new ServiceLocatorDTO(alias, url, encrypt, transportType);
		settings.setPassword(password);
		settings.setUserName(userName);
		settings.setChunkedStreamingMode(chunkedStreamingMode);

		return settings;
	}

	/**
	 * Request a service from the service locator
	 * @param <T> the type of the given service
	 * @param serviceInterface
	 * @return the service
	 * @throws IllegalStateException if either the service locator hasn't been initialized, or if the service locator wasn't able to
	 *           initialize the service
	 * @throws IllegalArgumentException if no service interface has been provided
	 */
	@SuppressWarnings("unchecked")
	public static <T> T getService(Class<T> serviceInterface) {
		InvocationHandler transportHandler = null;

		if (serviceInterface == null)
			throw new IllegalArgumentException(MSG_ILLEGAL_ARGUMENT);

		final String serviceName = serviceInterface.getName();

		if (!ServiceLocator.initialized)
			throw new IllegalStateException(MSG_ILLEGAL_STATE);

		try {
			final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

			// Create and initialize a transport handler
			if (transportType == TransportType.HTTP)
				transportHandler = new HTTPInvocationHandler(userName, password, serviceName, url, encrypt);
			else
				transportHandler = new LocalInvocationHandler(serviceName);

			return (T) Proxy.newProxyInstance(classLoader, new Class[] { Class.forName(serviceName) }, transportHandler);
		}
		catch (final Exception e) {
			logger.error("Error while creating invocation handler for service '{}'!", serviceName, e);

			throw new IllegalStateException(e);
		}
	}

	/**
	 * Upload a file to the server
	 * @param file
	 * @return the file identifier if the upload was successful
	 * @throws IllegalStateException if the service locator has not been initialized
	 * @throws IllegalArgumentException if the given file either doesn't exist, or if it is a directory
	 * @throws Exception if the upload operation has failed
	 */
	public static String uploadFile(File file) throws Exception {
		if (!ServiceLocator.initialized)
			throw new IllegalStateException(MSG_ILLEGAL_STATE);

		return new FileTransportHandler(url, encrypt, chunkedStreamingMode).uploadFile(file);
	}

	/**
	 * Download a file from the server
	 * @param clientPath the fully qualified path name (including the name of the file) to save the file to
	 * @param serverPath
	 * @throws IllegalStateException if the service locator has not been initialized
	 * @throws IllegalArgumentException if one of the provided path parameters is either null or empty
	 * @throws Exception if the download operation has failed
	 */
	public static void downloadFile(String clientPath, String serverPath) throws Exception {
		if (!ServiceLocator.initialized)
			throw new IllegalStateException(MSG_ILLEGAL_STATE);

		new FileTransportHandler(url, encrypt, chunkedStreamingMode).downloadFile(clientPath, serverPath);
	}

	/**
	 * @return the file transport handler
	 */
	public static FileTransportHandler getFileTransportHandler() {
		if (!ServiceLocator.initialized)
			throw new IllegalStateException(MSG_ILLEGAL_STATE);

		return new FileTransportHandler(url, encrypt, chunkedStreamingMode);
	}

	/**
	 * @return the user name
	 */
	public static String getUserName() {
		return ServiceLocator.userName;
	}

}
