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
package net.codecadenza.runtime.selenium.page.imp.angular;

import com.google.common.collect.Lists;
import java.time.Duration;
import java.util.List;
import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.FluentWait;

/**
 * <p>
 * Page object component that contains a list with all available items and a list with selected items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DualDataListComponent extends AbstractAngularPageComponent {
	private static final long ITEM_WAIT_TIMEOUT_SECONDS = 2;
	private static final long ITEM_POLLING_MILLISECONDS = 50;
	private static final long LIST_ITEM_DELAY_MILLISECONDS = 500;

	protected final String elementId;
	protected final boolean hasSearchField;

	/**
	 * Constructor
	 * @param pageObject
	 * @param elementId
	 * @param hasSearchField
	 */
	public DualDataListComponent(AbstractPageObject pageObject, String elementId, boolean hasSearchField) {
		super(pageObject.getTestContext());

		this.elementId = elementId;
		this.hasSearchField = hasSearchField;
		this.logger = pageObject.getLogger();
	}

	/**
	 * Search for items that can be selected afterwards
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the component doesn't provide a search field
	 */
	public void searchItems(PageElementTestData testData) {
		assertTrue("Component doesn't provide a search field!", hasSearchField);

		final String filterText = testData.getNewValue();

		logger.debug("Search for items by entering '{}' into filter field of list '{}'", filterText, elementId);

		final WebElement inputField = findWebElementByXPath("//cc-multi-selection-list[@id='" + elementId + "']//input");
		inputField.clear();
		inputField.sendKeys(testData.getFilterValue());
	}

	/**
	 * Select items by moving them from the source to the target list
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if an item could not be selected
	 */
	public void selectItems(PageElementTestData testData) {
		logger.debug("Select items '{}' of list '{}'", testData.getNewValue(), elementId);

		// The respective string of the test data object must be split in order to determine the items!
		final String[] items = testData.getNewValue().split(ITEM_DELIMITER);

		for (final String item : items) {
			boolean itemFound = false;

			// Search for all available items
			for (final WebElement itemElement : findWebElementsByXPath(getSourceItemsExpression())) {
				if (item.equals(itemElement.getText())) {
					logger.debug("Select list item '{}' by performing a double-click", item);

					itemFound = true;

					// The item must be selected before performing a double-click
					itemElement.click();

					// Perform a double-click on the item in order to select it!
					doubleClickElement(itemElement);
					break;
				}
			}

			assertTrue("Could not select item '" + item + "'!", itemFound);
		}

		// Wait a short period of time in order to make sure that the last item doesn't get lost!
		if (items.length > 0)
			testContext.delayTest(LIST_ITEM_DELAY_MILLISECONDS);
	}

	/**
	 * Validate if all items defined by the given test data object are currently selected
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if both lists are different
	 */
	public void validateSelection(PageElementTestData testData) {
		logger.debug("Validate if items '{}' are selected in list '{}'", testData.getExpectedValue(), elementId);

		// The respective string of the test data object must be split in order to determine the items!
		final List<String> items = Lists.newArrayList(testData.getExpectedValue().split(ITEM_DELIMITER));

		// Extract all currently selected items
		final String expression = getTargetItemsExpression();
		final List<String> currentSelection = findWebElementsByXPath(expression).stream().map(WebElement::getText).toList();

		if (logger.isTraceEnabled()) {
			final Optional<String> selectedItems = currentSelection.stream().reduce((a, b) -> a + ITEM_DELIMITER + b);

			if (selectedItems.isPresent())
				logger.trace("The items '{}' are selected in list '{}'", selectedItems.get(), elementId);
			else
				logger.trace("The list '{}' contains no selected items!", elementId);
		}

		// Compare both lists
		assertTrue("Current selection doesn't contain all expected items!", currentSelection.containsAll(items));
		assertTrue("List of expected items doesn't contain all selected items!", items.containsAll(currentSelection));
	}

	/**
	 * Remove all selected items
	 */
	public void removeAllSelectedItems() {
		logger.debug("Remove all selected items from list '{}'", elementId);

		var expression = "//cc-multi-selection-list[@id='" + elementId + "']//";
		expression += "button[contains(@data-pc-section, 'moveAllToSourceButton')]";

		final WebElement removeAllButton = findWebElementByXPath(expression);

		// If no selected item exists the respective button cannot be clicked as it is disabled!
		if (!removeAllButton.isEnabled())
			return;

		removeAllButton.click();

		// Wait until the list that contains all selected items is really empty!
		final FluentWait<WebDriver> waitForEmptyTargetList = new FluentWait<>(driver);
		waitForEmptyTargetList.withTimeout(Duration.ofSeconds(ITEM_WAIT_TIMEOUT_SECONDS));
		waitForEmptyTargetList.pollingEvery(Duration.ofMillis(ITEM_POLLING_MILLISECONDS));

		waitForEmptyTargetList.until(_ -> {
			final List<WebElement> selectedItems = findWebElementsByXPath(getTargetItemsExpression());

			if (selectedItems.isEmpty())
				return true;

			logger.trace("List '{}' that contains selected items is not empty!", elementId);

			return false;
		});
	}

	/**
	 * @return the XPath expression for finding all items in the source list
	 */
	protected String getSourceItemsExpression() {
		var expression = "//cc-multi-selection-list[@id='" + elementId + "']//";
		expression += "div[contains(@class, 'p-picklist-source-wrapper')]//li/div";

		return expression;
	}

	/**
	 * @return the XPath expression for finding all items in the target list
	 */
	protected String getTargetItemsExpression() {
		var expression = "//cc-multi-selection-list[@id='" + elementId + "']//";
		expression += "div[contains(@class, 'p-picklist-target-wrapper')]//li/div";

		return expression;
	}

}
