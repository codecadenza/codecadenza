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
package net.codecadenza.runtime.selenium.page.imp.vaadin;

import java.lang.reflect.Constructor;
import java.util.Map;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import net.codecadenza.runtime.selenium.page.AbstractPageComponent;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.RemoteWebElement;

/**
 * <p>
 * Base class for all page object components of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractVaadinPageComponent extends AbstractPageComponent {
	private static final String JS_SHADOW_ROOT = "return arguments[0].shadowRoot;";
	private static final String SHADOW_ROOT_KEY = "shadow-6066-11e4-a52e-4f735466cecf";
	private static final String HTTP_REQUEST_CHECK_SCRIPT = """
			let result = document.evaluate("count(//body/vaadin-connection-indicator[@loading])",
			document, null, XPathResult.ANY_TYPE, null);
			if (result.numberValue == 1) {
			return false;
			} else {
			return true;
			}
			""";

	/**
	 * Constructor
	 * @param testContext
	 */
	protected AbstractVaadinPageComponent(SeleniumTestContext testContext) {
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

	/**
	 * @param host
	 * @return the shadow root element of the given host element
	 * @throws IllegalStateException if the shadow root could not be determined
	 */
	protected SearchContext getShadowRoot(SearchContext host) {
		final Object result = ((JavascriptExecutor) driver).executeScript(JS_SHADOW_ROOT, host);

		if (result instanceof final SearchContext searchContext)
			return searchContext;
		else if (result instanceof final Map<?, ?> map) {
			final var shadowRoot = new RemoteWebElement();
			shadowRoot.setParent((RemoteWebDriver) driver);
			shadowRoot.setId((String) map.get(SHADOW_ROOT_KEY));
			shadowRoot.setFileDetector(((RemoteWebDriver) driver).getFileDetector());

			return shadowRoot;
		}

		throw new IllegalStateException("The shadow root could not be determined!");
	}

}
