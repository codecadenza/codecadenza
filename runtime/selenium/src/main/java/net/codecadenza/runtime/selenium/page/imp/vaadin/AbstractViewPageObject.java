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

import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import org.openqa.selenium.WebElement;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base object for all page objects that represent views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractViewPageObject extends AbstractPageObject {
	protected final DataTableComponent dataTable;

	/**
	 * Constructor
	 * @param testContext
	 */
	protected AbstractViewPageObject(SeleniumTestContext testContext) {
		super(testContext);

		dataTable = new DataTableComponent(testContext);
		dataTable.setLogger(LoggerFactory.getLogger(getClass()));
	}

	/**
	 * Press the 'Refresh' button in the toolbar
	 * @throws AssertionError if the button could not be found
	 */
	public void pressRefreshButton() {
		dataTable.pressRefreshButton();
	}

	/**
	 * Perform a double-click on the given row
	 * @param <T> the type of the page object to be returned
	 * @param rowElement
	 * @param pageClass
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the parameter <code>rowElement</code> is null
	 */
	public <T extends AbstractPageObject> T doubleClickRow(WebElement rowElement, Class<T> pageClass) {
		return dataTable.doubleClickRow(rowElement, pageClass);
	}

	/**
	 * Perform a double-click on the given row
	 * @param rowElement
	 * @throws AssertionError if the parameter <code>rowElement</code> is null
	 */
	public void doubleClickRow(WebElement rowElement) {
		dataTable.doubleClickRow(rowElement);
	}

	/**
	 * Select a row
	 * @param rowElement
	 * @throws AssertionError if the parameter <code>rowElement</code> is null
	 */
	public void selectRow(WebElement rowElement) {
		dataTable.selectRow(rowElement);
	}

	/**
	 * @param rowIndex
	 * @return the row with the given index or null if a row could not be found
	 * @throws AssertionError if the row either could not be found, or the row index is smaller than 1
	 */
	public WebElement getRowByRowIndex(int rowIndex) {
		return dataTable.getRowByRowIndex(rowIndex);
	}

	/**
	 * @param cellValue
	 * @return the first row that contains a cell with the given value or null if an appropriate row could not be found
	 * @throws AssertionError if the parameter <code>cellValue</code> is null or empty
	 */
	public WebElement getRowByCellValue(String cellValue) {
		return dataTable.getRowByCellValue(cellValue);
	}

	/**
	 * @param cellValue
	 * @param skipPagination if set to true searching in subsequent pages will be skipped
	 * @return the first row that contains a cell with the given value or null if an appropriate row could not be found
	 * @throws AssertionError if the parameter <code>cellValue</code> is null or empty
	 */
	public WebElement getRowByCellValue(String cellValue, boolean skipPagination) {
		return dataTable.getRowByCellValue(cellValue, skipPagination);
	}

	/**
	 * @return the number of rows displayed in the current page
	 * @throws AssertionError if the table element could not be found
	 */
	public int getRowCount() {
		return dataTable.getRowCount();
	}

	/**
	 * Validate if the number of visible rows in the first page of the table component is equal to the expected row count
	 * @param testData
	 * @throws AssertionError if the validation either has failed, or the expected row count could not be determined
	 */
	public void validateRowCount(PageElementTestData testData) {
		dataTable.validateRowCount(testData);
	}

	/**
	 * Validate if the number of visible rows in the first page of the table component is greater (or equal) than a given lower
	 * limit
	 * @param testData
	 * @throws AssertionError if the validation either has failed, or the expected row count could not be determined
	 */
	public void validateMinRowCount(PageElementTestData testData) {
		dataTable.validateMinRowCount(testData);
	}

	/**
	 * Validate if the number of visible rows in the first page of the table component is smaller (or equal) than a given upper
	 * limit
	 * @param testData
	 * @throws AssertionError if the validation either has failed, or the expected row count could not be determined
	 */
	public void validateMaxRowCount(PageElementTestData testData) {
		dataTable.validateMaxRowCount(testData);
	}

	/**
	 * Open the context-menu
	 * @param rowElement
	 * @throws AssertionError if the context-menu either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void openContextMenu(WebElement rowElement) {
		dataTable.openContextMenu(rowElement);
	}

	/**
	 * Open the context-menu and select the item with the given ID
	 * @param rowElement
	 * @param elementId
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuItem(WebElement rowElement, String elementId) {
		dataTable.clickContextMenuItem(rowElement, elementId);
	}

	/**
	 * Open the context-menu and select the item with the given ID
	 * @param elementId
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuItem(String elementId) {
		dataTable.clickContextMenuItem(elementId);
	}

	/**
	 * Click on the context-menu item 'Delete' in order to delete the object that is bound to the given row
	 * @param rowElement
	 * @throws AssertionError if an element either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuDelete(WebElement rowElement) {
		dataTable.clickContextMenuDelete(rowElement);
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
		return dataTable.clickContextMenuCopy(rowElement, pageClass);
	}

	/**
	 * Click on the context-menu item 'Copy' in order to copy the object that is bound to the given row
	 * @param rowElement
	 * @throws AssertionError if an element either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuCopy(WebElement rowElement) {
		dataTable.clickContextMenuCopy(rowElement);
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
		return dataTable.clickContextMenuUpdate(rowElement, pageClass);
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
		return dataTable.clickContextMenuView(rowElement, pageClass);
	}

	/**
	 * Click on the context-menu item 'Import data'
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuImport() {
		dataTable.clickContextMenuImport();
	}

	/**
	 * Click on the context-menu item 'Import data' and upload the file that is defined in the respective test data object
	 * @param testData the test data object that provides the absolute path to the import file
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void clickContextMenuImport(PageElementTestData testData) {
		dataTable.clickContextMenuImport(testData);
	}

	/**
	 * Click on the context-menu item 'Export data'
	 * @param rowElement
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuExport(WebElement rowElement) {
		dataTable.clickContextMenuExport(rowElement);
	}

	/**
	 * Click on the context-menu item 'Export data'
	 * @throws AssertionError if the context-menu item could not be found
	 */
	public void clickContextMenuExport() {
		dataTable.clickContextMenuExport();
	}

	/**
	 * Click on the context-menu item 'Download'
	 * @param rowElement
	 * @throws AssertionError if the context-menu item either could not be found, or the parameter <code>rowElement</code> is null
	 */
	public void clickContextMenuDownload(WebElement rowElement) {
		dataTable.clickContextMenuDownload(rowElement);
	}

	/**
	 * Click on the button 'Create new'
	 * @param pageClass
	 * @param <T> the type of the page object to be returned
	 * @return an instance of the selected page class
	 * @throws AssertionError if the page object either could not be created, or the button could not be found
	 */
	public <T extends AbstractPageObject> T clickButtonCreateNew(Class<T> pageClass) {
		return dataTable.clickButtonCreateNew(pageClass);
	}

}
