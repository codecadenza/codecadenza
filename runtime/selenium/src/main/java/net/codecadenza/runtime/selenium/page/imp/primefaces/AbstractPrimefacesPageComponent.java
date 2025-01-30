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
package net.codecadenza.runtime.selenium.page.imp.primefaces;

import java.lang.reflect.Constructor;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import net.codecadenza.runtime.selenium.page.AbstractPageComponent;

/**
 * <p>
 * Base class for all page object components of a Primefaces application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractPrimefacesPageComponent extends AbstractPageComponent {
	private static final String HTTP_REQUEST_CHECK_SCRIPT = """
			if (window.PrimeFaces) {
			return PrimeFaces.ajax.Queue.isEmpty();
			} else {
			return true;
			}
			""";

	/**
	 * Constructor
	 * @param testContext
	 */
	protected AbstractPrimefacesPageComponent(SeleniumTestContext testContext) {
		super(testContext);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.page.AbstractPageComponent#getJSCommandForPendingHTTPRequests()
	 */
	@Override
	public String getJSCommandForPendingHTTPRequests() {
		return HTTP_REQUEST_CHECK_SCRIPT;
	}

	/**
	 * Create an instance of the selected page object class
	 * @param <T> the type of the page object to be returned
	 * @param pageClass
	 * @return an instance of the selected page class
	 * @throws AssertionError if the creation of the page object has failed
	 */
	protected <T extends AbstractPageObject> T createPageObject(Class<T> pageClass) {
		delayPageLoad();

		try {
			final Constructor<T> constructor = pageClass.getConstructor(SeleniumTestContext.class);

			return constructor.newInstance(testContext);
		}
		catch (final Exception e) {
			fail("Could not create page object '" + pageClass.getName() + "'!", e);

			return null;
		}
	}

}
