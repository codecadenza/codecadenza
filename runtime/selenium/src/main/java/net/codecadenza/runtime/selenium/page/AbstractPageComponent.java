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
package net.codecadenza.runtime.selenium.page;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import org.junit.jupiter.api.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.FluentWait;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for all page object components
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractPageComponent {
	public static final String ATTR_NAME_VALUE = "value";
	public static final String ATTR_NAME_CLASS = "class";
	public static final String ATTR_NAME_STYLE = "style";
	public static final String ATTR_NAME_SELECTED = "selected";
	public static final String NEW_LINE = "!NEW_LINE!";
	public static final String ITEM_DELIMITER = ";";
	public static final String SLASH = "/";
	public static final int HTTP_POLLING_INTERVAL_MILLISECONDS = 10;
	private static final String SCROLL_SCRIPT = "arguments[0].scrollIntoView({ behavior: 'instant', block: 'center', inline: 'center' });";

	protected final SeleniumTestContext testContext;
	protected final WebDriver driver;
	protected final long httpTimeout;
	protected final long httpCheckDelay;
	protected final long explicitWaitTime;
	protected Logger logger;

	/**
	 * Constructor
	 * @param testContext the Selenium test context
	 */
	protected AbstractPageComponent(SeleniumTestContext testContext) {
		this.testContext = testContext;
		this.driver = testContext.getDriver();
		this.httpTimeout = testContext.getProperties().getHTTPTimeout();
		this.httpCheckDelay = testContext.getProperties().getHTTPCheckDelay();
		this.explicitWaitTime = testContext.getProperties().getExplicitWaitTime();
		this.logger = LoggerFactory.getLogger(getClass());
	}

	/**
	 * Every implementation must define a JavaScript command that is responsible for finding pending HTTP requests
	 * @return the JavaScript command
	 */
	public abstract String getJSCommandForPendingHTTPRequests();

	/**
	 * @param logger the {@link Logger} that should be used
	 */
	public void setLogger(Logger logger) {
		this.logger = logger;
	}

	/**
	 * @return the logger
	 */
	public Logger getLogger() {
		return logger;
	}

	/**
	 * @return the Selenium WebDriver
	 */
	public WebDriver getDriver() {
		return driver;
	}

	/**
	 * @return the Selenium test context
	 */
	public SeleniumTestContext getTestContext() {
		return testContext;
	}

	/**
	 * Find an element by using the given ID
	 * @param elementId the ID of the element
	 * @param clickable controls if the element must be clickable
	 * @return the web element
	 * @throws AssertionError if the element either could not be found, or the parameter <code>elementId</code> is null or empty
	 */
	public WebElement findWebElement(String elementId, boolean clickable) {
		logger.trace("Search for element with ID '{}'", elementId);

		if (elementId == null || elementId.isEmpty())
			fail("Parameter 'elementId' must not be null or empty!");

		waitForPendingHTTPRequests();

		final var wait = new WebElementWait(driver, Duration.ofMillis(explicitWaitTime), logger);
		WebElement element = null;

		try {
			if (clickable)
				element = wait.untilClickable(By.id(elementId));
			else
				element = wait.untilVisible(By.id(elementId));
		}
		catch (final TimeoutException e) {
			fail("Could not find element with ID '" + elementId + "'!", e);
		}

		// Make sure that the element is in the visible area of the browser!
		scrollTo(element);

		moveToElement(element);

		return element;
	}

	/**
	 * Find an element by using the given ID
	 * @param elementId the ID of the element
	 * @return the web element
	 * @throws AssertionError if the element either could not be found, or the parameter <code>elementId</code> is null or empty
	 */
	public WebElement findWebElement(String elementId) {
		return findWebElement(elementId, false);
	}

	/**
	 * Find an element by using an XPath expression
	 * @param xpathExpression the XPath expression to search for the element
	 * @param clickable controls if the element must be clickable
	 * @return the web element
	 * @throws AssertionError if the element either could not be found, or the XPath expression is null or empty
	 */
	public WebElement findWebElementByXPath(String xpathExpression, boolean clickable) {
		logger.trace("Search for element by using XPath expression '{}'", xpathExpression);

		if (xpathExpression == null || xpathExpression.isEmpty())
			fail("Parameter 'xpathExpression' must not be null or empty!");

		waitForPendingHTTPRequests();

		final var wait = new WebElementWait(driver, Duration.ofMillis(explicitWaitTime), logger);
		WebElement element = null;

		try {
			if (clickable)
				element = wait.untilClickable(By.xpath(xpathExpression));
			else
				element = wait.untilVisible(By.xpath(xpathExpression));
		}
		catch (final TimeoutException _) {
			fail("Could not find element using XPath '" + xpathExpression + "'!");
		}

		// Make sure that the element is in the visible area of the browser!
		scrollTo(element);

		moveToElement(element);

		return element;
	}

	/**
	 * Find an element by using an XPath expression
	 * @param xpathExpression the XPath expression to search for the element
	 * @return the web element
	 * @throws AssertionError if the element either could not be found, or the XPath expression is null or empty
	 */
	public WebElement findWebElementByXPath(String xpathExpression) {
		return findWebElementByXPath(xpathExpression, false);
	}

	/**
	 * Find an element by using the given ID and click it
	 * @param elementId the ID of the element
	 * @throws AssertionError if the element either could not be found, or the parameter <code>elementId</code> is null or empty
	 */
	public void clickWebElement(String elementId) {
		findWebElement(elementId, true).click();
	}

	/**
	 * Find an element by using an XPath expression and click it
	 * @param xpathExpression the XPath expression to search for the element
	 * @throws AssertionError if the element either could not be found, or the XPath expression is null or empty
	 */
	public void clickWebElementByXPath(String xpathExpression) {
		findWebElementByXPath(xpathExpression, true).click();
	}

	/**
	 * Find elements by using an XPath expression
	 * @param xpathExpression the XPath expression to search for the elements
	 * @param wait a flag that controls if an explicit wait should be performed
	 * @return a list containing all web elements
	 * @throws AssertionError if the XPath expression is null or empty
	 */
	public List<WebElement> findWebElementsByXPath(String xpathExpression, boolean wait) {
		logger.trace("Search for elements by using XPath expression '{}'", xpathExpression);

		if (xpathExpression == null || xpathExpression.isEmpty())
			fail("Parameter 'xpathExpression' must not be null or empty!");

		waitForPendingHTTPRequests();

		if (wait) {
			final var elementWait = new WebElementWait(driver, Duration.ofMillis(explicitWaitTime), logger);

			return elementWait.untilElementsNotEmpty(By.xpath(xpathExpression));
		}
		else
			return driver.findElements(By.xpath(xpathExpression));
	}

	/**
	 * Perform a double-click on the given element
	 * @param elementId the ID of the element
	 */
	public void doubleClickElement(String elementId) {
		final WebElement webElement = findWebElement(elementId, true);

		logger.trace("Perform double-click on element with ID '{}'", elementId);

		new Actions(driver).doubleClick(webElement).perform();
	}

	/**
	 * Perform a double-click on the element identified by the given XPath expression
	 * @param xpathExpression the XPath expression to search for the element
	 */
	public void doubleClickElementByXPath(String xpathExpression) {
		final WebElement webElement = findWebElementByXPath(xpathExpression, true);

		logger.trace("Perform double-click on element identified by XPath expression '{}'", xpathExpression);

		new Actions(driver).doubleClick(webElement).perform();
	}

	/**
	 * Enter data into an input field
	 * @param testData the field's test data object that provides necessary information
	 */
	public void setInputFieldValue(PageElementTestData testData) {
		assertNotNull("Text for input field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Enter text '{}' into input field '{}'", testData.getNewValue(), testData.getElementId());

		final WebElement inputField = findWebElement(testData.getElementId());
		inputField.clear();

		// The ENTER key must be pressed for all new-line characters!
		final String textToEnter = testData.getNewValue().replace(NEW_LINE, Keys.ENTER);

		inputField.sendKeys(textToEnter);
	}

	/**
	 * Validate an input field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateInputFieldValue(PageElementTestData testData) {
		assertNotNull("Expected value for field '" + testData.getElementId() + "' must not be null!", testData.getExpectedValue());

		logger.debug("Validate text of input field '{}'", testData.getElementId());

		final WebElement inputField = findWebElement(testData.getElementId());
		final var message = "Validation of input field '" + testData.getElementId() + "' has failed!";
		final String expectedText = testData.getExpectedValue().replace(NEW_LINE, "\\n");
		String actualText = inputField.getAttribute(ATTR_NAME_VALUE);

		if (actualText != null)
			actualText = actualText.replace("\n", "\\n");

		assertEquals(message, expectedText, actualText);
	}

	/**
	 * @param webElement the element to move to
	 * @throws AssertionError if the parameter <code>webElement</code> is null
	 */
	public void moveToElement(WebElement webElement) {
		assertNotNull("Parameter 'webElement' must not be null!", webElement);

		// Move the mouse to the respective element
		new Actions(driver).moveToElement(webElement).perform();
	}

	/**
	 * Assert that a condition is true and create a log record if the assertion test has failed
	 * @param message the failure message
	 * @param condition the condition to check
	 * @throws AssertionError if the condition is not true
	 */
	public void assertTrue(String message, boolean condition) {
		if (!condition)
			fail(message);
	}

	/**
	 * Assert that an object isn't null and create a log record if the assertion test has failed
	 * @param message the failure message
	 * @param object the object to be checked
	 * @throws AssertionError if the object is null
	 */
	public void assertNotNull(String message, Object object) {
		if (object == null)
			fail(message);
	}

	/**
	 * Assert that two objects are equal and create a log record if the assertion test has failed
	 * @param message the failure message
	 * @param expected the expected value
	 * @param actual the actual value
	 * @throws AssertionError if both objects are not equal
	 */
	public void assertEquals(String message, Object expected, Object actual) {
		if (expected == null && actual == null)
			return;

		logger.trace("Compare expected value '{}' with actual value '{}'", expected, actual);

		if (expected != null && !expected.equals(actual))
			fail(message);

		if (actual != null && !actual.equals(expected))
			fail(message);
	}

	/**
	 * Throw an assertion error and log the message
	 * @param message the failure message
	 * @throws AssertionError in any case
	 */
	public void fail(String message) {
		fail(message, null);
	}

	/**
	 * Throw an assertion error and log the message
	 * @param message the failure message
	 * @param t the exception that has been thrown
	 * @throws AssertionError in any case
	 */
	public void fail(String message, Throwable t) {
		logger.error(message, t);

		Assertions.fail(() -> message);
	}

	/**
	 * Search for pending HTTP requests and wait until they are finished!
	 */
	public void waitForPendingHTTPRequests() {
		if (httpTimeout <= 0) {
			logger.trace("Skip waiting for pending HTTP requests");

			return;
		}

		if (!(driver instanceof JavascriptExecutor)) {
			logger.warn("Driver doesn't support execution of JavaScript!");

			return;
		}

		logger.trace("Wait for pending HTTP requests");

		if (httpCheckDelay > 0)
			testContext.delayTest(httpCheckDelay);

		final long start = Instant.now().toEpochMilli();

		final FluentWait<WebDriver> wait = new FluentWait<>(driver);
		wait.withTimeout(Duration.ofMillis(httpTimeout));
		wait.pollingEvery(Duration.ofMillis(HTTP_POLLING_INTERVAL_MILLISECONDS));

		wait.until(webDriver -> {
			final var js = (JavascriptExecutor) webDriver;
			boolean finished;

			try {
				finished = Boolean.TRUE.equals(js.executeScript(getJSCommandForPendingHTTPRequests()));

				if (finished)
					logger.trace("No pending HTTP requests found after {} milliseconds", (Instant.now().toEpochMilli() - start));
				else
					logger.trace("HTTP requests are still pending!");
			}
			catch (final Exception e) {
				logger.error("Error while executing JavaScript!", e);

				// If the JavaScript invocation fails the wait loop should be finished!
				return true;
			}

			return finished;
		});
	}

	/**
	 * Delay the test after creating a page object in order to make sure that the page has been fully rendered
	 */
	protected void delayPageLoad() {
		final long pageLoadDelay = testContext.getProperties().getPageLoadDelay();

		if (pageLoadDelay > 0) {
			// Wait a short period of time until the page has been fully rendered
			testContext.delayTest(pageLoadDelay);
		}
	}

	/**
	 * Scroll the browser window to the location of the given element
	 * @param element the element to scroll to
	 */
	protected void scrollTo(WebElement element) {
		if (!(driver instanceof final JavascriptExecutor jsExecutor)) {
			logger.warn("Driver doesn't support execution of JavaScript!");
			return;
		}

		jsExecutor.executeScript(SCROLL_SCRIPT, element);
	}

	/**
	 * Check if both list contain the same items without checking if the ordering is correct
	 * @param <T> the generic list type
	 * @param expected the list with the expected items
	 * @param actual the list with the actual items
	 */
	protected <T> void assertEqualsIgnoringOrder(List<T> expected, List<T> actual) {
		for (final T item : expected)
			assertTrue("List with actual items doesn't contain '" + item + "'!", actual.contains(item));

		for (final T item : actual)
			assertTrue("List with expected items doesn't contain '" + item + "'!", expected.contains(item));
	}

	/**
	 * Return an XPath string literal that safely contains the supplied text, regardless of the presence of single or double quotes
	 * @param text the raw text that should be prepared
	 * @return the converted text
	 */
	public static String prepareXPathText(String text) {
		// If the text contains no single quotes, use a single‑quoted literal
		if (!text.contains("'"))
			return "'" + text + "'";

		// If the text contains no double quotes, use a double‑quoted literal
		if (!text.contains("\""))
			return "\"" + text + "\"";

		// Otherwise, build a concat() expression alternating between the parts that are safe for single‑quote and the literal
		// single‑quote
		final StringBuilder sb = new StringBuilder("concat(");
		boolean first = true;

		for (final String part : text.split("'")) {
			if (!first)
				sb.append(", \"'\", ");

			sb.append("'").append(part).append("'");
			first = false;
		}

		sb.append(")");

		return sb.toString();
	}

}
