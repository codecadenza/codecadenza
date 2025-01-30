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

import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_OK_XPATH;
import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_YES_XPATH;

import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;

/**
 * <p>
 * Base class for page object components that represent data tables
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataTableComponent extends AbstractVaadinPageComponent {
	private static final String DEFAULT_DATA_TABLE_ELEMENT_ID = "dataTable";
	private static final long MAX_NUMBER_OF_PAGES = 20;
	private static final String MENU_ITEM_ID_COPY = "mniCopy";
	private static final String MENU_ITEM_ID_DELETE = "mniDelete";
	private static final String MENU_ITEM_ID_DOWNLOAD = "mniDownload";
	private static final String MENU_ITEM_ID_EXPORT = "mniExport";
	private static final String MENU_ITEM_ID_IMPORT = "mniImport";
	private static final String MENU_ITEM_ID_UPDATE = "mniEdit";
	private static final String MENU_ITEM_ID_VIEW = "mniView";
	private static final long MENU_ITEM_WAIT_TIME_MILLI_SECONDS = 20;

	protected final String tableElementId;

	/**
	 * Constructor
	 * @param testContext
	 * @param tableElementId
	 */
	public DataTableComponent(SeleniumTestContext testContext, String tableElementId) {
		super(testContext);

		this.tableElementId = tableElementId;
	}

	/**
	 * Constructor
	 * @param testContext
	 */
	public DataTableComponent(SeleniumTestContext testContext) {
		this(testContext, DEFAULT_DATA_TABLE_ELEMENT_ID);
	}

	/**
	 * Press the 'Refresh' button in the toolbar
	 * @throws AssertionError if the button could not be found
	 */
	public void pressRefreshButton() {
		logger.debug("Press 'Refresh' button");

		findWebElementByXPath("//vaadin-button[@id='" + tableElementId + "_cmdRefresh']").click();
	}

	/**
	 * Perform a double-click on the given row
	 * @param <T> the type of the page object to be returned
	 * @param rowElement
	 * @param pageClass
	 * @return an instance of the selected page class. It returns null if no page class has been specified!
	 * @throws AssertionError if the page object either could not be created, or the parameter <code>rowElement</code> is null
	 */
	public <T extends AbstractPageObject> T doubleClickRow(WebElement rowElement, Class<T> pageClass) {
		logger.debug("Double-click row");

		new Actions(driver).moveToElement(rowElement).doubleClick().build().perform();

		if (pageClass != null)
			return createPageObject(pageClass);

		return null;
	}

	/**
	 * Perform a double-click on the given row
	 * @param rowElement
	 * @throws AssertionError if the parameter <code>rowElement</code> is null
	 */
	public void doubleClickRow(WebElement rowElement) {
		doubleClickRow(rowElement, null);
	}

	/**
	 * Select a row
	 * @param rowElement
	 * @throws AssertionError if the parameter <code>rowElement</code> is null
	 */
	public void selectRow(WebElement rowElement) {
		logger.debug("Select row");

		assertNotNull("Parameter 'rowElement' must not be null!", rowElement);

		waitForPendingHTTPRequests();

		// Move the mouse pointer to a valid position that can be used to click on the row!
		new Actions(driver).moveToElement(rowElement).click().build().perform();
	}

	/**
	 * @param rowIndex
	 * @return the row with the given index or null if a row could not be found
	 * @throws AssertionError if the row either could not be found, or the row index is smaller than 1
	 */
	public WebElement getRowByRowIndex(int rowIndex) {
		logger.debug("Search for row with row index '{}'", rowIndex);

		assertTrue("Parameter 'rowIndex' must be greater than 0!", rowIndex > 0);

		return findWebElementByXPath(
				getTableXPath() + "/vaadin-grid-cell-content[@slot='vaadin-grid-cell-content-" + rowIndex + "']");
	}

	/**
	 * @param objectId
	 * @param skipPagination if set to true searching in subsequent pages will be skipped
	 * @return the row with the given object ID or null if the row could not be found
	 * @throws AssertionError if the parameter <code>objectId</code> is null or empty
	 */
	public WebElement getRowByObjectId(String objectId, boolean skipPagination) {
		// Search the given string in all cells as Vaadin doesn't provide an object ID attribute for a table row!
		return getRowByCellValue(objectId, skipPagination);
	}

	/**
	 * @param objectId
	 * @return the row with the given object ID or null if the row could not be found
	 * @throws AssertionError if the parameter <code>objectId</code> is null or empty
	 */
	public WebElement getRowByObjectId(String objectId) {
		return getRowByObjectId(objectId, false);
	}

	/**
	 * @param cellValue
	 * @return the first row that contains a cell with the given value or null if an appropriate row could not be found
	 * @throws AssertionError if the parameter <code>cellValue</code> is null or empty
	 */
	public WebElement getRowByCellValue(String cellValue) {
		return getRowByCellValue(cellValue, false);
	}

	/**
	 * @param cellValue
	 * @param skipPagination if set to true searching in subsequent pages will be skipped
	 * @return the first row that contains a cell with the given value or null if an appropriate row could not be found
	 * @throws AssertionError if the parameter <code>cellValue</code> is null or empty
	 */
	public WebElement getRowByCellValue(String cellValue, boolean skipPagination) {
		// Omit further processing if no rows are being displayed!
		if (getRowCount() == 0)
			return null;

		// Click on the data table component in order to gain focus
		findWebElement(tableElementId).click();

		int pageIndex = 1;

		while (true) {
			if (pageIndex == 1) {
				logger.debug("Search for row containing cell value '{}'", cellValue);

				if (cellValue == null || cellValue.isEmpty())
					fail("Parameter 'cellValue' must not be null or empty!");
			}

			// Search for the row within the current page
			final Optional<WebElement> optionalRow = findWebElementsByXPath(
					getTableXPath() + "/vaadin-grid-cell-content[text()='" + cellValue + "']").stream().findFirst();

			if (optionalRow.isPresent()) {
				logger.debug("Row found!");

				return optionalRow.get();
			}

			if (skipPagination)
				return null;

			logger.debug("Load next page by pressing the page-down key");

			// Load the next page
			new Actions(driver).sendKeys(Keys.PAGE_DOWN).build().perform();

			// After reaching an internal threshold we skip further processing!
			if (pageIndex == MAX_NUMBER_OF_PAGES) {
				logger.warn("Stop loading of further pages as the maximum number of pages ('{}') has been reached!", MAX_NUMBER_OF_PAGES);

				return null;
			}

			pageIndex++;
		}
	}

	/**
	 * @return the number of rows
	 * @throws AssertionError if the table element could not be found
	 */
	public int getRowCount() {
		logger.debug("Determine number of rows");

		final WebElement gridElement = findWebElementByXPath(getTableXPath());

		return Integer.parseInt(gridElement.getAttribute("row-count"));
	}

	/**
	 * Validate if the number of visible rows in the first page of the table component is equal to the expected row count
	 * @param testData
	 * @throws AssertionError if the validation either has failed, or the expected row count could not be determined
	 */
	public void validateRowCount(PageElementTestData testData) {
		final int expectedRowCount = getExpectedRowCount(testData);
		final int currentRowCount = getRowCount();

		logger.debug("Test if current row count '{}' is equal to expected row count '{}'", currentRowCount, expectedRowCount);

		assertTrue("Row count '" + currentRowCount + "' doesn't match expected result: " + expectedRowCount + "!",
				expectedRowCount == currentRowCount);
	}

	/**
	 * Validate if the number of visible rows in the first page of the table component is greater (or equal) than a given lower
	 * limit
	 * @param testData
	 * @throws AssertionError if the validation either has failed, or the expected row count could not be determined
	 */
	public void validateMinRowCount(PageElementTestData testData) {
		final int lowerLimit = getExpectedRowCount(testData);
		final int currentRowCount = getRowCount();

		logger.debug("Test if the current row count '{}' is greater than lower limit '{}'", currentRowCount, lowerLimit);

		assertTrue("Row count '" + currentRowCount + "' is smaller than lower limit '" + lowerLimit + "'!",
				lowerLimit <= currentRowCount);
	}

	/**
	 * Validate if the number of visible rows in the first page of the table component is smaller (or equal) than a given upper
	 * limit
	 * @param testData
	 * @throws AssertionError if the validation either has failed, or the expected row count could not be determined
	 */
	public void validateMaxRowCount(PageElementTestData testData) {
		final int upperLimit = getExpectedRowCount(testData);
		final int currentRowCount = getRowCount();

		logger.debug("Test if the current row count '{}' is smaller than upper limit '{}'", currentRowCount, upperLimit);

		assertTrue("Row count '" + currentRowCount + "' is greater than upper limit '" + upperLimit + "'!",
				upperLimit >= currentRowCount);
	}

	/**
	 * Open the context-menu of the given row
	 * @param rowElement
	 * @throws AssertionError if the context-menu either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void openContextMenu(WebElement rowElement) {
		logger.debug("Open the context-menu of the given row");

		assertNotNull("Parameter 'rowElement' must not be null!", rowElement);

		waitForPendingHTTPRequests();

		// Move the mouse pointer to a valid position that can be used to open the context-menu!
		new Actions(driver).moveToElement(rowElement).contextClick().build().perform();
	}

	/**
	 * Open the context-menu and select the item with the given ID
	 * @param rowElement
	 * @param elementId
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuItem(WebElement rowElement, String elementId) {
		logger.debug("Click context-menu item '{}'", elementId);

		selectRow(rowElement);

		openContextMenu(rowElement);

		// Wait a short period of time in order to avoid an occasionally occurring StaleElementReferenceException!
		testContext.delayTest(MENU_ITEM_WAIT_TIME_MILLI_SECONDS);

		findContextMenuItem(elementId).click();
	}

	/**
	 * Open the context-menu and select the item with the given ID
	 * @param elementId
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuItem(String elementId) {
		logger.debug("Click context-menu item '{}'", elementId);

		// Open the context menu
		new Actions(driver).moveToElement(findWebElementByXPath(getTableXPath())).contextClick().build().perform();

		// Wait a short period of time in order to avoid an occasionally occurring StaleElementReferenceException!
		testContext.delayTest(MENU_ITEM_WAIT_TIME_MILLI_SECONDS);

		findContextMenuItem(elementId).click();
	}

	/**
	 * Click on the context-menu item "Delete" in order to delete the object that is bound to the given row
	 * @param rowElement
	 * @throws AssertionError if an element either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuDelete(WebElement rowElement) {
		// Click on the respective context-menu item
		clickContextMenuItem(rowElement, MENU_ITEM_ID_DELETE);

		logger.debug("Click 'Yes' button in order to confirm the delete operation");

		// Search for the confirmation button in the pop-up dialog!
		findWebElementByXPath(PopUpDialog.DIALOG_BUTTON_YES_XPATH).click();
	}

	/**
	 * Click on the context-menu item 'Copy' in order to copy the object that is bound to the given row
	 * @param <T> the type of the page object to be returned
	 * @param rowElement
	 * @param pageClass
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or an element could not be found
	 */
	public <T extends AbstractPageObject> T clickContextMenuCopy(WebElement rowElement, Class<T> pageClass) {
		// Click on the respective context-menu item
		clickContextMenuItem(rowElement, MENU_ITEM_ID_COPY);

		logger.debug("Click 'Yes' button in order to confirm the copy operation");

		// Search for the confirmation button in the pop-up dialog!
		findWebElementByXPath(DIALOG_BUTTON_YES_XPATH).click();

		return createPageObject(pageClass);
	}

	/**
	 * Click on the context-menu item 'Copy' in order to copy the object that is bound to the given row
	 * @param rowElement
	 * @throws AssertionError if an element either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuCopy(WebElement rowElement) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_COPY);

		logger.debug("Click 'Yes' button in order to confirm the copy operation");

		// Search for the confirmation button in the pop-up dialog!
		findWebElementByXPath(DIALOG_BUTTON_YES_XPATH).click();

		logger.debug("Click 'OK' button in order to close the message dialog");

		// If the copy operation has been finished successfully a pop-up dialog will appear that must be closed!
		findWebElementByXPath(DIALOG_BUTTON_OK_XPATH).click();
	}

	/**
	 * Click on the context-menu item 'Edit'
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @param rowElement
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the context-menu item could not be found
	 */
	public <T extends AbstractPageObject> T clickContextMenuUpdate(WebElement rowElement, Class<T> pageClass) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_UPDATE);

		return createPageObject(pageClass);
	}

	/**
	 * Click on the context-menu item 'View'
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @param rowElement
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the context-menu item could not be found
	 */
	public <T extends AbstractPageObject> T clickContextMenuView(WebElement rowElement, Class<T> pageClass) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_VIEW);

		return createPageObject(pageClass);
	}

	/**
	 * Click on the context-menu item 'Import data'
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuImport() {
		clickContextMenuItem(MENU_ITEM_ID_IMPORT);
	}

	/**
	 * Click on the context-menu item 'Import data' and upload the file that is defined in the respective test data object
	 * @param testData the test data object that provides the absolute path to the import file
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void clickContextMenuImport(PageElementTestData testData) {
		clickContextMenuItem(MENU_ITEM_ID_IMPORT);

		final WebElement upload = findWebElementByXPath(PopUpDialog.DIALOG_FILE_UPLOAD_XPATH);

		// Search for the file input element and enter the path of the file to be uploaded
		final SearchContext shadowRoot = getShadowRoot(upload);
		shadowRoot.findElement(By.id(PopUpDialog.DIALOG_FILE_INPUT_ID)).sendKeys(testData.getNewValue());
	}

	/**
	 * Click on the context-menu item 'Export data'
	 * @param rowElement
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuExport(WebElement rowElement) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_EXPORT);
	}

	/**
	 * Click on the context-menu item 'Export data'
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuExport() {
		clickContextMenuItem(MENU_ITEM_ID_EXPORT);
	}

	/**
	 * Click on the context-menu item 'Download'
	 * @param rowElement
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuDownload(WebElement rowElement) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_DOWNLOAD);
	}

	/**
	 * Click on the button 'Create new'
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the button could not be found
	 */
	public <T extends AbstractPageObject> T clickButtonCreateNew(Class<T> pageClass) {
		findWebElementByXPath("//vaadin-button[@id='" + tableElementId + "_cmdCreate']").click();

		return createPageObject(pageClass);
	}

	/**
	 * Extract the expected row count from the given test data object
	 * @param testData
	 * @return the expected row count
	 * @throws AssertionError if the value could not be converted to an integer
	 */
	protected int getExpectedRowCount(PageElementTestData testData) {
		int expectedRowCount = 0;

		try {
			expectedRowCount = Integer.parseInt(testData.getExpectedValue());
		}
		catch (final NumberFormatException e) {
			fail("Could not convert '" + testData.getExpectedValue() + "' to an integer!", e);
		}

		return expectedRowCount;
	}

	/**
	 * @param elementId
	 * @return the context-menu item with the given ID
	 * @throws AssertionError if the context-menu item could not be found
	 */
	protected WebElement findContextMenuItem(String elementId) {
		final var expression = "//vaadin-context-menu-item[@id='" + tableElementId + "_" + elementId + "']";

		return findWebElementByXPath(expression);
	}

	/**
	 * @return a XPath expression for finding the 'vaadin-grid' element of the data table component
	 */
	protected String getTableXPath() {
		return "//vaadin-vertical-layout[@id='" + tableElementId + "']/vaadin-grid";
	}

}
