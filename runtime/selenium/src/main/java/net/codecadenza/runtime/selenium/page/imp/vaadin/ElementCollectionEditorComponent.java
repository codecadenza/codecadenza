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

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;

/**
 * <p>
 * Page object component for testing the element collection editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ElementCollectionEditorComponent extends AbstractVaadinPageComponent {
	private static final long LIST_ITEM_DELAY_MILLISECONDS = 50;
	private static final String MENU_ITEM_DELETE_ALL = "mniDeleteAll";

	protected final String elementId;
	protected final String editorXPath;

	/**
	 * Constructor
	 * @param pageObject
	 * @param elementId
	 */
	public ElementCollectionEditorComponent(AbstractPageObject pageObject, String elementId) {
		super(pageObject.getTestContext());

		this.elementId = elementId;
		this.editorXPath = "//vaadin-vertical-layout[@id='" + elementId + "']";
		this.logger = pageObject.getLogger();
	}

	/**
	 * Add new elements to the editor
	 * @param testData the field's test data object that provides necessary information
	 * @param removeExistingElements flag that controls if all existing elements should be removed
	 */
	public void addElements(PageElementTestData testData, boolean removeExistingElements) {
		logger.debug("Add the elements '{}' to the editor '{}'", testData.getNewValue(), elementId);

		final String[] elements = testData.getNewValue().split(ITEM_DELIMITER);

		if (removeExistingElements)
			removeAllElements();

		for (final String element : elements) {
			final WebElement inputField = findWebElementByXPath(editorXPath + "//vaadin-text-field//input");
			inputField.clear();
			inputField.sendKeys(element);

			final WebElement addButton = findWebElementByXPath(editorXPath + "//vaadin-button");
			addButton.click();

			// Wait a short period of time to ensure the element is added before adding the next one
			testContext.delayTest(LIST_ITEM_DELAY_MILLISECONDS);
		}
	}

	/**
	 * Validate if all elements defined by the given test data object are currently in the editor
	 * @param testData the field's test data object that provides necessary information
	 */
	public void validateElements(PageElementTestData testData) {
		logger.debug("Validate if the elements '{}' are contained in the editor '{}'", testData.getExpectedValue(), elementId);

		final List<String> expectedElements = Arrays.asList(testData.getExpectedValue().split(ITEM_DELIMITER));
		final List<WebElement> gridCells = findWebElementsByXPath(editorXPath + "//vaadin-grid//vaadin-grid-cell-content//span");
		final List<String> actualElements = gridCells.stream().map(WebElement::getText).toList();

		if (logger.isTraceEnabled()) {
			final Optional<String> actualValue = actualElements.stream().reduce((a, b) -> a + ITEM_DELIMITER + b);

			if (actualValue.isPresent())
				logger.trace("The editor '{}' contains the elements '{}'", elementId, actualValue.get());
			else
				logger.trace("The editor '{}' is empty!", elementId);
		}

		assertEqualsIgnoringOrder(expectedElements, actualElements);
	}

	/**
	 * Remove all elements from the editor
	 */
	protected void removeAllElements() {
		logger.debug("Remove all elements from the editor '{}'", elementId);

		openContextMenu();

		final WebElement deleteAllMenuItem = findWebElementByXPath("//vaadin-context-menu-item[@id='" + MENU_ITEM_DELETE_ALL + "']");
		deleteAllMenuItem.click();
	}

	/**
	 * Open the context-menu
	 */
	protected void openContextMenu() {
		logger.debug("Open the context-menu");

		final WebElement editor = findWebElementByXPath(editorXPath + "//vaadin-grid");

		waitForPendingHTTPRequests();

		// Move the mouse pointer to a valid position that can be used to open the context-menu!
		new Actions(driver).moveToElement(editor).contextClick().build().perform();
	}

}
