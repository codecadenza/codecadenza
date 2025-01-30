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

import com.google.common.collect.Lists;
import java.util.List;
import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import org.openqa.selenium.Keys;
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
public class DualDataListComponent extends AbstractVaadinPageComponent {
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

		logger.debug("Search for items by entering '{}' into filter field of list '{}'", testData.getFilterValue(), elementId);

		final WebElement inputField = findWebElementByXPath(getContainerXPath() + "vaadin-text-field");
		inputField.sendKeys(Keys.CONTROL + "a");
		inputField.sendKeys(Keys.DELETE);
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

		// Search for the source list
		findWebElementByXPath(getSourceListXPath());

		for (final String item : items) {
			final String expression = getSourceItemsXPath() + "[text()='" + item + "']";

			logger.debug("Select list item '{}' by performing a double-click", item);

			// Perform a double-click on the item in order to select it!
			doubleClickElement(findWebElementByXPath(expression));
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
		final List<String> currentSelection = getSelectedItems();

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

		getAvailableItems().forEach(item -> {
			final var expression = getSourceItemsXPath() + "[text()='" + item + "']";

			logger.debug("Select list item '{}' by performing a double-click", item);

			// Perform a double-click on the item in order to select it!
			doubleClickElement(findWebElementByXPath(expression));
		});
	}

	/**
	 * Remove all selected items
	 */
	public void removeAllSelectedItems() {
		logger.debug("Remove all selected items from list '{}'", elementId);

		getSelectedItems().forEach(item -> {
			final var expression = getTargetItemsXPath() + "[text()='" + item + "']";

			logger.debug("Remove list item '{}' by performing a double-click", item);

			// Remove the item by performing a double-click
			doubleClickElement(findWebElementByXPath(expression));
		});
	}

	/**
	 * @return a list containing all selected items
	 */
	protected List<String> getSelectedItems() {
		return getItems(getTargetListXPath());
	}

	/**
	 * @return a list containing all available items
	 */
	protected List<String> getAvailableItems() {
		return getItems(getSourceListXPath());
	}

	/**
	 * @param listExpression the XPath expression for finding the list
	 * @return a list containing all items of the given list
	 */
	protected List<String> getItems(String listExpression) {
		final var itemExpression = listExpression + "/vaadin-grid-cell-content[count(*)=0 and string-length()>0]";

		// Search for the respective list
		findWebElementByXPath(listExpression);

		// Get all items that have a text but no children
		return findWebElementsByXPath(itemExpression).stream().map(WebElement::getText).toList();
	}

	/**
	 * @return the XPath statement for the source list items
	 */
	protected String getSourceItemsXPath() {
		return getSourceListXPath() + "/vaadin-grid-cell-content";
	}

	/**
	 * @return the XPath statement for the target list items
	 */
	protected String getTargetItemsXPath() {
		return getTargetListXPath() + "/vaadin-grid-cell-content";
	}

	/**
	 * @return the XPath statement for the source list
	 */
	protected String getSourceListXPath() {
		return getContainerXPath() + "vaadin-grid[@list-type='source']";
	}

	/**
	 * @return the XPath statement for the target list
	 */
	protected String getTargetListXPath() {
		return getContainerXPath() + "vaadin-grid[@list-type='target']";
	}

	/**
	 * @return the XPath statement for the container element
	 */
	protected String getContainerXPath() {
		return "//div[@id='" + elementId + "']//";
	}

}
