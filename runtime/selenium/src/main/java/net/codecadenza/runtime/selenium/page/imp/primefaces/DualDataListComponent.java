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

import com.google.common.collect.Lists;
import java.time.Duration;
import java.util.List;
import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.page.WebElementWait;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

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
public class DualDataListComponent extends AbstractPrimefacesPageComponent {
	private static final String SEARCH_INPUT_SUFFIX = "Filter";
	private static final String SEARCH_BUTTON_SUFFIX = "Button";

	protected final String elementId;
	protected final boolean hasSearchField;

	/**
	 * Constructor
	 * @param pageObject the page object the component belongs to
	 * @param elementId the ID of the element
	 * @param hasSearchField flag that indicates if the component provides a search field
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

		logger.debug("Search for items by entering '{}' into filter field of list '{}'", testData.getFilterValue(), elementId);

		final String searchInputId = testData.getElementId() + SEARCH_INPUT_SUFFIX;
		final String searchButtonId = testData.getElementId() + SEARCH_BUTTON_SUFFIX;

		final WebElement inputField = findWebElement(searchInputId);
		inputField.clear();
		inputField.sendKeys(testData.getFilterValue());

		clickWebElement(searchButtonId);
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
			final String itemText = prepareXPathText(item);
			final String sourceItemExpression = getSourceItemsExpression() + "[text()=" + itemText + "]";
			final var targetItemExpression = getTargetItemsExpression() + "[@data-item-value=" + itemText + "]";

			logger.debug("Select list item '{}' by performing a double-click", item);

			doubleClickElementByXPath(sourceItemExpression);

			// Wait until the item has been added to the target list!
			findWebElementByXPath(targetItemExpression);
		}
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
		final List<String> currentSelection = findWebElementsByXPath(expression, true).stream()
				.filter(e -> e.getAttribute("data-item-value") != null).map(e -> e.getAttribute("data-item-value")).toList();

		if (currentSelection == null) {
			fail("The current selection must not be null!");
			return;
		}

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
	 * Select all available items
	 */
	public void selectAllItems() {
		logger.debug("Select all available items in list '{}'", elementId);

		final var expression = "//div[@id='" + elementId + "']/div/div/button[contains(@class, 'ui-picklist-button-add-all')]";

		clickWebElementByXPath(expression);
	}

	/**
	 * Remove all selected items
	 */
	public void removeAllSelectedItems() {
		logger.debug("Remove all selected items from list '{}'", elementId);

		final var expression = "//div[@id='" + elementId + "']/div/div/button[contains(@class, 'ui-picklist-button-remove-all')]";
		final WebElement removeAllButton = findWebElementByXPath(expression, true);

		// If no selected item exists the respective button cannot be clicked as it is disabled!
		if (!removeAllButton.isEnabled())
			return;

		removeAllButton.click();

		// Wait until the list that contains all selected items is really empty!
		new WebElementWait(driver, Duration.ofMillis(explicitWaitTime), logger)
				.untilElementsEmpty(By.xpath(getTargetItemsExpression()));
	}

	/**
	 * @return the XPath expression for finding all items in the source list
	 */
	protected String getSourceItemsExpression() {
		return "//div[@id='" + elementId + "']/div[@class='ui-picklist-list-wrapper'][1]/ul/li";
	}

	/**
	 * @return the XPath expression for finding all items in the target list
	 */
	protected String getTargetItemsExpression() {
		return "//div[@id='" + elementId + "']/div[@class='ui-picklist-list-wrapper'][2]/ul/li";
	}

}
