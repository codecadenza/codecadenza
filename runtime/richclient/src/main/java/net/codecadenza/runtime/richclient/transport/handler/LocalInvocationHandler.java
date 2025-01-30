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

import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.LOCAL_INVOCATION_HANDLER_IMP_NOT_FOUND;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.getTranslation;

import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Properties;
import net.codecadenza.runtime.reflect.MethodFinder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Invocation handler for services that are available locally! This handler requires a service.properties file in the
 * application's configuration folder in order to find an implementation of the given service interface.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LocalInvocationHandler implements InvocationHandler {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String CONFIG_FILE_PATH = "config/service.properties";

	private static Properties properties = new Properties();
	private final String interfaceName;

	// The service configuration file should only be loaded once!
	static {
		final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

		try (final InputStream inputStream = classLoader.getResourceAsStream(CONFIG_FILE_PATH)) {
			properties.load(inputStream);
		}
		catch (final Exception e) {
			logger.error("Error while reading service configuration from file {}!", CONFIG_FILE_PATH, e);
		}
	}

	/**
	 * Constructor
	 * @param interfaceName
	 */
	public LocalInvocationHandler(String interfaceName) {
		this.interfaceName = interfaceName;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	@Override
	public Object invoke(Object obj, Method method, Object[] args) throws Throwable {
		try {
			final String beanName = properties.getProperty(interfaceName);

			if (beanName == null)
				throw new IllegalArgumentException(getTranslation(LOCAL_INVOCATION_HANDLER_IMP_NOT_FOUND, interfaceName));

			final Class<?> beanClass = Class.forName(beanName);

			// Get the appropriate method
			final Method m = MethodFinder.findMethod(beanClass, method.getName(), args);

			final Object bean = Class.forName(beanName).getDeclaredConstructor().newInstance();

			logger.debug("Invoke local method '{}' of service '{}'!", method.getName(), interfaceName);

			// Invoke the service method
			return m.invoke(bean, args);
		}
		catch (final Exception e) {
			logger.error("Error while invoking method '{}'!", method.getName(), e);

			if (e.getCause() == null)
				throw e;

			throw e.getCause();
		}
	}

}
