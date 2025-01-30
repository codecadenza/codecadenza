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

import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_CANCEL_XPATH;
import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_COUNT_XPATH;
import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_OK_XPATH;
import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_RESET_XPATH;

import java.util.List;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;

/**
 * <p>
 * Abstract page object for views that provide a search dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSearchPageObject extends AbstractViewPageObject {
	private static final String BUTTON_SEARCH_XPATH = "//vaadin-button[@id='cmdSearch']";
	private static final String FILTER_INPUT_ID_PREFIX = "fi_";
	private static final String FILTER_DATE_ID_PREFIX = "fd_";
	private static final String FILTER_COMBO_ID_PREFIX = "fc_";
	private static final String OPERATOR_ID_PREFIX = "o_";
	private static final String SORT_ORDER_ID_PREFIX = "s_";

	/**
	 * Constructor
	 * @param testContext
	 */
	protected AbstractSearchPageObject(SeleniumTestContext testContext) {
		super(testContext);
	}

	/**
	 * Open the search dialog, enter respective data and perform the search operation
	 * @param searchInput
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void performSearchOperation(List<PageElementTestData> searchInput) {
		logger.debug("Perform search operation");

		enterSearchInputData(searchInput);

		findWebElementByXPath(DIALOG_BUTTON_OK_XPATH).click();
	}

	/**
	 * Open the search dialog, enter respective data and perform the count operation
	 * @param searchInput
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void performCountOperation(List<PageElementTestData> searchInput) {
		logger.debug("Perform count operation");

		enterSearchInputData(searchInput);

		findWebElementByXPath(DIALOG_BUTTON_COUNT_XPATH).click();
	}

	/**
	 * Open the search dialog and reset the search settings
	 * @throws AssertionError if an element could not be found
	 */
	public void resetSearchSettings() {
		logger.debug("Reset search dialog");

		openSearchDialog();

		findWebElementByXPath(DIALOG_BUTTON_RESET_XPATH).click();

		// Close the search dialog after performing the reset
		findWebElementByXPath(DIALOG_BUTTON_CANCEL_XPATH).click();
	}

	/**
	 * Enter data into the search dialog
	 * @param searchInput
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	protected void enterSearchInputData(List<PageElementTestData> searchInput) {
		openSearchDialog();

		// Apply search input data
		searchInput.forEach(fieldData -> {
			final String elementId = fieldData.getElementId();

			if (elementId.startsWith(FILTER_INPUT_ID_PREFIX))
				setInputFieldValue(fieldData);
			else if (elementId.startsWith(FILTER_DATE_ID_PREFIX))
				setDateFieldValue(fieldData);
			else if (elementId.startsWith(SORT_ORDER_ID_PREFIX) || elementId.startsWith(OPERATOR_ID_PREFIX)
					|| elementId.startsWith(FILTER_COMBO_ID_PREFIX))
				selectComboboxItem(fieldData);
			else
				fail("Cannot enter value for field '" + elementId + "' as it doesn't provide a supported element ID prefix!");
		});
	}

	/**
	 * Open the search dialog
	 * @throws AssertionError if the search button could not be found
	 */
	protected void openSearchDialog() {
		findWebElementByXPath(BUTTON_SEARCH_XPATH).click();
	}

}
