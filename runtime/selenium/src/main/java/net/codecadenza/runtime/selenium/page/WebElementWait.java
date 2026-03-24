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
import java.util.NoSuchElementException;
import java.util.function.Function;
import org.openqa.selenium.By;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.slf4j.Logger;

/**
 * <p>
 * Utility class for explicit waits in Selenium
 * </p>
 * <p>
 * Copyright 2026 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class WebElementWait {

	private static final int DEFAULT_POLL_INTERVAL_MS = 100;

	private final WebDriver driver;
	private final Duration timeout;
	private final Logger logger;

	/**
	 * Create a new instance with a custom timeout
	 * @param driver the {@link WebDriver} instance used for all waits
	 * @param timeout maximum time to wait for a condition to become true
	 * @param logger the {@link Logger} that should be used
	 */
	public WebElementWait(WebDriver driver, Duration timeout, Logger logger) {
		this.driver = driver;
		this.timeout = timeout;
		this.logger = logger;
	}

	/**
	 * Execute a custom {@link ExpectedCondition} and return its value
	 * @param <V> the type of value that the condition returns when it succeeds
	 * @param condition a {@link Function} that receives the driver and returns null while the condition is not satisfied, or a
	 *          non‑null value when it is.
	 * @return the non‑null value produced by the condition
	 * @throws TimeoutException if the condition does not become true within {@link #timeout}
	 */
	public <V> V until(Function<? super WebDriver, V> condition) {
		final long start = Instant.now().toEpochMilli();

		final WebDriverWait wait = new WebDriverWait(driver, timeout);
		wait.pollingEvery(Duration.ofMillis(DEFAULT_POLL_INTERVAL_MS));
		wait.ignoring(NoSuchElementException.class);
		wait.ignoring(StaleElementReferenceException.class);

		final V element = wait.until(condition);

		logger.trace("Waited {} milliseconds", (Instant.now().toEpochMilli() - start));

		return element;
	}

	/**
	 * Wait until the element located by {@code locator} is <strong>clickable</strong> (i.e. it is both
	 * {@link WebElement#isDisplayed() visible} and {@link WebElement#isEnabled() enabled})
	 * @param locator the {@link By} used to locate the element
	 * @return the {@link WebElement} once it is clickable
	 * @throws TimeoutException if the element does not become clickable within the configured timeout
	 */
	public WebElement untilClickable(By locator) {
		logger.trace("Wait until element {} is clickable", locator);

		return until(ExpectedConditions.elementToBeClickable(locator));
	}

	/**
	 * Wait until the element located by {@code locator} is {@link WebElement#isDisplayed() visible} on the page
	 * @param locator the {@link By} used to locate the element
	 * @return the visible {@link WebElement}
	 * @throws TimeoutException if the element is not displayed within the configured timeout
	 */
	public WebElement untilVisible(By locator) {
		logger.trace("Wait until element {} is visible", locator);

		return until(ExpectedConditions.visibilityOfElementLocated(locator));
	}

	/**
	 * Wait until the element is no longer visible
	 * @param locator the {@link By} used to locate the element
	 * @throws TimeoutException if element is still visible after {@code timeout}
	 */
	public void untilInvisible(By locator) {
		logger.trace("Wait until element {} is invisible", locator);

		until(ExpectedConditions.invisibilityOfElementLocated(locator));
	}

	/**
	 * Wait until the list returned by {@code locator} is **not empty** and return it
	 * @param locator the {@link By} used to locate the element
	 * @return a live list of WebElements (still bound to the DOM)
	 * @throws TimeoutException if the condition is not satisfied within {@code timeout}
	 */
	public List<WebElement> untilElementsNotEmpty(By locator) {
		logger.trace("Wait until element list {} is not empty", locator);

		return until(_ -> {
			final List<WebElement> elements = driver.findElements(locator);
			return elements.isEmpty() ? null : elements;
		});
	}

	/**
	 * Wait until {@code driver.findElements(by)} returns an empty list
	 * @param locator the {@link By} used to locate the element
	 * @throws TimeoutException if the list is still non‑empty after {@code timeout}
	 */
	public void untilElementsEmpty(By locator) {
		logger.trace("Wait until element list {} is empty", locator);

		until(_ -> {
			final List<WebElement> elements = driver.findElements(locator);
			return elements.isEmpty() ? elements : null;
		});
	}

}
