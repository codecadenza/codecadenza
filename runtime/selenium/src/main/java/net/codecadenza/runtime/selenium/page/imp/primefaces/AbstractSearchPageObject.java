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
	private static final String SEARCH_BUTTON_ID = FORM_ID_PREFIX + "cmdSearch";
	private static final String RESET_BUTTON_ID = FORM_ID_PREFIX + "cmdReset";
	private static final String COUNT_BUTTON_ID = FORM_ID_PREFIX + "cmdCount";
	private static final String MENU_ITEM_ID_SEARCH = "mniSearch";
	private static final String FILTER_INPUT_ID_PREFIX = ":fi_";
	private static final String FILTER_COMBO_ID_PREFIX = ":fc_";
	private static final String SORT_ORDER_ID_PREFIX = ":s_";
	private static final String OPERATOR_ID_PREFIX = ":o_";

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

		findWebElement(SEARCH_BUTTON_ID).click();
	}

	/**
	 * Open the search dialog, enter respective data and perform the count operation
	 * @param searchInput
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void performCountOperation(List<PageElementTestData> searchInput) {
		logger.debug("Perform count operation");

		enterSearchInputData(searchInput);

		findWebElement(COUNT_BUTTON_ID).click();
	}

	/**
	 * Open the search dialog and reset the search settings
	 * @throws AssertionError if an element could not be found
	 */
	public void resetSearchSettings() {
		logger.debug("Reset search dialog");

		openSearchDialog();

		findWebElement(RESET_BUTTON_ID).click();
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

			if (elementId.contains(FILTER_INPUT_ID_PREFIX))
				setInputFieldValue(fieldData);
			else if (elementId.contains(SORT_ORDER_ID_PREFIX) || elementId.contains(OPERATOR_ID_PREFIX)
					|| elementId.contains(FILTER_COMBO_ID_PREFIX))
				selectComboboxItem(fieldData);
			else
				fail("Cannot enter value for field '" + elementId + "' as it doesn't provide a supported element ID prefix!");
		});
	}

	/**
	 * Open the search dialog
	 * @throws AssertionError if the menu item could not be found
	 */
	protected void openSearchDialog() {
		dataTable.clickMenuBarItem(MENU_ITEM_ID_SEARCH);
	}

}
