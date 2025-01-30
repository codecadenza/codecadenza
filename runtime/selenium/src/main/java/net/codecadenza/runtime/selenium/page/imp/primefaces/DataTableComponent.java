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
import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
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
public class DataTableComponent extends AbstractPrimefacesPageComponent {
	private static final String MENU_ITEM_ID_FILE = "mniFile";
	private static final String MENU_ITEM_ID_CREATE = "mniCreate";
	private static final String MENU_ITEM_ID_ADD = "mniAdd";
	private static final String MENU_ITEM_ID_UPDATE = "mniUpdate";
	private static final String MENU_ITEM_ID_VIEW = "mniView";
	private static final String MENU_ITEM_ID_DELETE = "mniDelete";
	private static final String MENU_ITEM_ID_COPY = "mniCopy";
	private static final String MENU_ITEM_ID_DOWNLOAD = "mniDownload";
	private static final String MENU_ITEM_ID_IMPORT = "mniImport";
	private static final String MENU_ITEM_ID_EXPORT = "mniExport";
	private static final String MENU_ITEM_ID_REFRESH = "mniRefresh";
	private static final String CMD_ID_YES_DELETE = "cmdYesDelete";
	private static final String CMD_ID_YES_COPY = "cmdYesCopy";

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
	 * Press the 'Refresh' context-menu item
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void pressRefreshButton() {
		logger.debug("Press 'Refresh' button");

		clickContextMenuItem(MENU_ITEM_ID_REFRESH);
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
		assertNotNull("Parameter 'rowElement' must not be null!", rowElement);

		logger.debug("Double-click row");

		waitForPendingHTTPRequests();

		// Move the mouse pointer to a valid position that can be used to perform a double-click!
		new Actions(driver).moveToElement(rowElement, ROW_OFFSET_X, ROW_OFFSET_Y).doubleClick().build().perform();

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
		new Actions(driver).moveToElement(rowElement, ROW_OFFSET_X, ROW_OFFSET_Y).click().build().perform();
	}

	/**
	 * @param rowIndex
	 * @return the row with the given index or null if a row could not be found
	 * @throws AssertionError if the row either could not be found, or the row index is smaller than 1
	 */
	public WebElement getRowByRowIndex(int rowIndex) {
		logger.debug("Search for row with row index '{}'", rowIndex);

		assertTrue("Parameter 'rowIndex' index must be greater than 0!", rowIndex > 0);

		final WebElement row = findWebElementByXPath(getTableRowsXPath() + "[@data-ri='" + (rowIndex - 1) + "']");

		// Scroll to the row as it could be the case that it is located in the invisible area of the browser!
		scrollTo(row);

		return row;
	}

	/**
	 * @param objectId
	 * @param skipPagination if set to true searching in subsequent pages will be skipped
	 * @return the row with the given object ID or null if the row could not be found
	 * @throws AssertionError if the parameter <code>objectId</code> is null or empty
	 */
	public WebElement getRowByObjectId(String objectId, boolean skipPagination) {
		logger.debug("Search for row with object ID '{}'", objectId);

		return findRow(objectId, true, skipPagination);
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
	 * @param skipPagination if set to true searching in subsequent pages will be skipped
	 * @return the first row that contains a cell with the given value or null if an appropriate row could not be found
	 * @throws AssertionError if the parameter <code>cellValue</code> is null or empty
	 */
	public WebElement getRowByCellValue(String cellValue, boolean skipPagination) {
		logger.debug("Search for row containing cell value '{}'", cellValue);

		return findRow(cellValue, false, skipPagination);
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
	 * @return the number of rows displayed in the current page
	 * @throws AssertionError if the table element could not be found
	 */
	public int getRowCount() {
		logger.debug("Determine number of rows");

		return findWebElementsByXPath(getTableRowsXPath()).size();
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
	 * @param rowElement
	 * @return true if the given row doesn't contain data
	 * @throws AssertionError if the parameter <code>rowElement</code> is null
	 */
	public boolean isRowEmpty(WebElement rowElement) {
		logger.debug("Test if row is empty");

		assertNotNull("Parameter 'rowElement' must not be null!", rowElement);

		waitForPendingHTTPRequests();

		final String classAttribute = rowElement.getAttribute(ATTR_NAME_CLASS);

		return classAttribute != null && classAttribute.contains("ui-datatable-empty-message");
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
		new Actions(driver).moveToElement(rowElement, ROW_OFFSET_X, ROW_OFFSET_Y).contextClick().build().perform();
	}

	/**
	 * Open the context-menu and select an item that is identified by the given ID
	 * @param rowElement
	 * @param elementId
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuItem(WebElement rowElement, String elementId) {
		logger.debug("Click context-menu item '{}'", elementId);

		openContextMenu(rowElement);

		findWebElement(getPanelIdPrefix() + elementId).click();
	}

	/**
	 * Open the context-menu and select an item that is identified by the given ID
	 * @param elementId
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuItem(String elementId) {
		clickContextMenuItem(getFirstRow(), elementId);
	}

	/**
	 * Click on the context-menu item 'Delete' in order to delete the object that is bound to the given row
	 * @param rowElement
	 * @throws AssertionError if an element either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuDelete(WebElement rowElement) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_DELETE);

		logger.debug("Click 'Yes' button in order to confirm delete operation");

		// Click on the 'Yes' button in order to confirm the delete operation
		findWebElement(getPanelIdPrefix() + CMD_ID_YES_DELETE).click();
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
		clickContextMenuCopy(rowElement);

		return createPageObject(pageClass);
	}

	/**
	 * Click on the context-menu item 'Copy' in order to copy the object that is bound to the given row
	 * @param rowElement
	 * @throws AssertionError if an element either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuCopy(WebElement rowElement) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_COPY);

		logger.debug("Click 'Yes' button in order to confirm copy operation");

		// Click on the 'Yes' button in order to confirm the copy operation
		findWebElement(getPanelIdPrefix() + CMD_ID_YES_COPY).click();
	}

	/**
	 * Click on the menu bar item 'Create new' in order to create a new object
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the menu item could not be found
	 */
	public <T extends AbstractPageObject> T clickMenuBarCreateNew(Class<T> pageClass) {
		return clickMenuBarItem(pageClass, MENU_ITEM_ID_CREATE);
	}

	/**
	 * Click on the menu bar item 'Add' in order to add a new object
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the menu item could not be found
	 */
	public <T extends AbstractPageObject> T clickMenuBarAddNew(Class<T> pageClass) {
		return clickMenuBarItem(pageClass, MENU_ITEM_ID_ADD);
	}

	/**
	 * Click on the context-menu item 'Update'
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
	 * Click on the context-menu item 'Import'
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuImport() {
		clickContextMenuItem(getFirstRow(), MENU_ITEM_ID_IMPORT);
	}

	/**
	 * Click on the context-menu item 'Import' and upload the file that is defined in the respective test data object
	 * @param testData the test data object that provides the absolute path to the import file
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void clickContextMenuImport(PageElementTestData testData) {
		clickContextMenuItem(getFirstRow(), MENU_ITEM_ID_IMPORT);

		// Search for the file input element and enter the path of the file to be uploaded
		findWebElementByXPath("//div[@id='" + testData.getElementId() + "']//input[@type='file']").sendKeys(testData.getNewValue());

		// Click on the pop-up dialog's close button
		findWebElementByXPath("//div[@id='" + testData.getElementId() + "']//a/span[@class='ui-icon ui-icon-closethick']").click();
	}

	/**
	 * Click on the context-menu item 'Export'
	 * @param rowElement
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuExport(WebElement rowElement) {
		clickContextMenuItem(rowElement, MENU_ITEM_ID_EXPORT);
	}

	/**
	 * Click on the context-menu item 'Export'
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuExport() {
		clickContextMenuItem(getFirstRow(), MENU_ITEM_ID_EXPORT);
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
	 * @return the panel ID prefix
	 */
	protected String getPanelIdPrefix() {
		return tableElementId + "_";
	}

	/**
	 * @return the first row of the data table
	 */
	protected WebElement getFirstRow() {
		return findWebElementByXPath(getTableRowsXPath() + "[1]");
	}

	/**
	 * @param value
	 * @param byObjectId
	 * @param skipPagination
	 * @return the row or null if a row could not be found
	 */
	protected WebElement findRow(String value, boolean byObjectId, boolean skipPagination) {
		// Search for a row within the first page
		WebElement row = null;

		if (byObjectId)
			row = findRowByObjectId(value);
		else
			row = findRowByCellValue(value);

		if (row != null) {
			logger.debug("Row found!");

			return row;
		}

		if (!skipPagination) {
			logger.debug("Search for row by going through all pages");

			// Try to find the row in subsequent pages
			while (true) {
				final WebElement nextPageButton = getPaginatorNext();

				if (nextPageButton == null) {
					logger.debug("No further page available!");

					break;
				}

				logger.debug("Load next page by clicking on respective button");

				nextPageButton.click();

				if (byObjectId)
					row = findRowByObjectId(value);
				else
					row = findRowByCellValue(value);

				if (row != null) {
					logger.debug("Row found!");

					// Scroll to the row as it could be the case that it is located in the invisible area of the browser!
					scrollTo(row);

					return row;
				}
			}
		}

		if (byObjectId)
			logger.warn("Row with ID '{}' could not be found!", value);
		else
			logger.warn("Row with cell value '{}' could not be found!", value);

		return row;
	}

	/**
	 * @param objectId
	 * @return the element that represents the row or null if the row could not be found
	 * @throws AssertionError if the parameter <code>objectId</code> is null or empty
	 */
	protected WebElement findRowByObjectId(String objectId) {
		if (objectId == null || objectId.isEmpty())
			fail("Parameter 'objectId' must not be null or empty!");

		final Optional<WebElement> optionalRow = findWebElementsByXPath(getTableRowsXPath() + "[@data-rk='" + objectId + "']")
				.stream().findFirst();

		if (optionalRow.isPresent()) {
			final WebElement row = optionalRow.get();

			// Scroll to the row as it could be the case that it is located in the invisible area of the browser!
			scrollTo(row);

			return row;
		}

		// The row with the given object ID doesn't exist!
		return null;
	}

	/**
	 * @param cellValue
	 * @return the first row that contains a cell with the given value or null if an appropriate row could not be found
	 * @throws AssertionError if the parameter <code>cellValue</code> is null or empty
	 */
	protected WebElement findRowByCellValue(String cellValue) {
		if (cellValue == null || cellValue.isEmpty())
			fail("Parameter 'cellValue' must not be null or empty!");

		final Optional<WebElement> optionalRow = findWebElementsByXPath(getTableRowsXPath() + "/td[text()='" + cellValue + "']")
				.stream().findFirst();

		if (optionalRow.isPresent()) {
			final WebElement row = optionalRow.get();

			// Scroll to the row as it could be the case that it is located in the invisible area of the browser!
			scrollTo(row);

			return row;
		}

		return null;
	}

	/**
	 * @return the element that represents the 'Next Page' button or null if it could not be found
	 */
	protected WebElement getPaginatorNext() {
		final var expression = "//div[@id='" + tableElementId + "_paginator_top']/a";
		final List<WebElement> elements = findWebElementsByXPath(expression);

		for (final WebElement element : elements) {
			final String classAttribute = element.getAttribute(ATTR_NAME_CLASS);

			if (classAttribute != null && classAttribute.contains("ui-paginator-next") && !classAttribute.contains("ui-state-disabled"))
				return element;
		}

		return null;
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
	 * @return a XPath expression for finding all rows of the data table
	 */
	protected String getTableRowsXPath() {
		return "//tbody[@id='" + tableElementId + "_data']/tr";
	}

	/**
	 * Click on a menu item that belongs to the 'File' item
	 * @param itemId the ID of the menu item to be clicked
	 * @throws AssertionError if the menu item could not be found
	 */
	public void clickMenuBarItem(String itemId) {
		clickMenuBarItem(null, itemId);
	}

	/**
	 * Click on a menu item that belongs to the 'File' item
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @param itemId the ID of the menu item to be clicked
	 * @return an instance of the selected page class or null if no page class has been provided
	 * @throws AssertionError if the page object either could not be created, or the menu item could not be found
	 */
	public <T extends AbstractPageObject> T clickMenuBarItem(Class<T> pageClass, String itemId) {
		logger.debug("Click on menu bar item '{}'", itemId);

		final WebElement mniFile = findWebElement(getPanelIdPrefix() + MENU_ITEM_ID_FILE);
		final WebElement mni = findWebElement(getPanelIdPrefix() + itemId);

		// Click on menu item "File"
		new Actions(driver).clickAndHold(mniFile).build().perform();

		// Click on the selected menu item
		mni.click();

		if (pageClass == null)
			return null;

		return createPageObject(pageClass);
	}

}
