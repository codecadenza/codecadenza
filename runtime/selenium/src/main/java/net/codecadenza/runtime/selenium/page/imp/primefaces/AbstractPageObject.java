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

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import net.codecadenza.runtime.selenium.data.PageActionResult;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

/**
 * <p>
 * Abstract base class for all page objects of a Primefaces application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractPageObject extends AbstractPrimefacesPageComponent {
	public static final String FORM_ID_PREFIX = "form:";
	public static final String BUTTON_ID_LOG_IN = FORM_ID_PREFIX + "cmdLogin";

	private static final String SLASH = "/";
	private static final String URL_PARAM_OBJ_ID = "?selectedObjectId=";
	private static final String ITEMS_PANEL_ID_SUFFIX = "_items";
	private static final String PANEL_SUFFIX = "_panel";
	private static final String INPUT_SUFFIX = "_input";
	private static final String CLASS_CHECKBOX_SELECTED = "ui-icon-check";
	private static final String XPATH_TREE_ITEM_LABEL = "//span[@class='ui-treenode-label ui-corner-all']";
	private static final String FIELD_ID_LOV_INPUT = FORM_ID_PREFIX + "txtInput";
	private static final String BUTTON_ID_LOG_OUT = "form_header:cmdLogout";
	private static final String BUTTON_ID_SELECT = FORM_ID_PREFIX + "cmdSelect";
	private static final String BUTTON_ID_RESET = FORM_ID_PREFIX + "cmdReset";
	private static final String BUTTON_ID_SAVE = FORM_ID_PREFIX + "cmdSave";
	private static final String BUTTON_ID_BACK = FORM_ID_PREFIX + "cmdBack";
	private static final String TABLE_ID_LOV = FORM_ID_PREFIX + "dataTable";
	private static final int TAB_PAGE_TIMEOUT_SECONDS = 5;
	private static final int AUTO_COMPLETE_DELAY_MILLIS = 400;

	/**
	 * Constructor
	 * @param testContext
	 */
	protected AbstractPageObject(SeleniumTestContext testContext) {
		super(testContext);
	}

	/**
	 * Open a page
	 * @param resourcePath
	 * @throws AssertionError if the parameter <code>resourcePath</code> is null
	 */
	public void open(String resourcePath) {
		open(resourcePath, null);
	}

	/**
	 * Open a page to display data for a specific object identified by the given ID
	 * @param resourcePath
	 * @param objectId
	 * @throws AssertionError if the parameter <code>resourcePath</code> is null
	 */
	public void open(String resourcePath, String objectId) {
		assertNotNull("Parameter 'resourcePath' must not be null!", resourcePath);

		waitForPendingHTTPRequests();

		String url = buildPageURL(testContext.getBaseURL(), resourcePath);

		if (objectId != null && !objectId.isEmpty())
			url += URL_PARAM_OBJ_ID + URLEncoder.encode(objectId, StandardCharsets.UTF_8);

		logger.debug("Open URL '{}'", url);

		driver.get(url);
	}

	/**
	 * Navigate to a page by selecting a tree view item with the specified navigation target! As the page object is created via
	 * introspection it is necessary that the respective class provides an appropriate constructor!
	 * @param <T> the type of the page object that should be returned
	 * @param navigationTarget
	 * @param pageClass
	 * @return a page instance whose type is defined by the respective parameter
	 * @throws AssertionError if the operation has failed
	 */
	public <T extends AbstractPageObject> T openPageByNavigator(String navigationTarget, Class<T> pageClass) {
		logger.debug("Navigate to '{}'", navigationTarget);

		final WebElement treeItem = findWebElementByXPath(
				XPATH_TREE_ITEM_LABEL + "/a[@href[contains(.,'" + navigationTarget + "')]]");
		treeItem.click();

		return createPageObject(pageClass);
	}

	/**
	 * Press the logout button
	 * @throws AssertionError if the button could not be found
	 */
	public void pressLogoutButton() {
		pressButton(BUTTON_ID_LOG_OUT);
	}

	/**
	 * Press the 'Save' button
	 * @throws AssertionError if the button could not be found
	 */
	public void pressSaveButton() {
		pressButton(BUTTON_ID_SAVE);
	}

	/**
	 * Press the 'Save' button
	 * @param pageClass
	 * @param <T> the type of the page object that should be returned
	 * @return a page instance whose type is defined by the respective parameter
	 * @throws AssertionError if the button either could not be found, or the page object could not be created
	 */
	public <T extends AbstractPageObject> T pressSaveButton(Class<T> pageClass) {
		pressSaveButton();

		return createPageObject(pageClass);
	}

	/**
	 * Press the 'Back' button
	 * @throws AssertionError if the button could not be found
	 */
	public void pressBackButton() {
		pressButton(BUTTON_ID_BACK);
	}

	/**
	 * Press a button with a given ID
	 * @param id
	 * @throws AssertionError if the button could not be found
	 */
	public void pressButton(String id) {
		logger.debug("Press button with ID '{}'", id);

		findWebElement(id).click();
	}

	/**
	 * Change the selection of a checkbox
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if changing of the checkbox selection has failed
	 */
	public void setCheckBoxValue(PageElementTestData testData) {
		assertNotNull("Value for checkbox field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Set selection of checkbox '{}'", testData.getElementId());

		final WebElement inputField = findWebElement(testData.getElementId());
		final boolean newSelection = testData.getNewValue().equalsIgnoreCase(Boolean.toString(true));
		final boolean currentSelection = isCheckBoxSelected(testData);

		if (currentSelection)
			logger.debug("Checkbox '{}' is selected", testData.getElementId());
		else
			logger.debug("Checkbox '{}' is not selected", testData.getElementId());

		// Skip further operations if the current value and the new value are equal!
		if (currentSelection == newSelection)
			return;

		logger.debug("Change selection of checkbox '{}'", testData.getElementId());

		// Click on the checkbox in order to toggle its selection
		inputField.click();
	}

	/**
	 * Validate the selection of a checkbox field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateCheckBoxValue(PageElementTestData testData) {
		assertNotNull("Expected value for checkbox field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate selection of checkbox '{}'", testData.getElementId());

		final boolean expectedSelection = testData.getExpectedValue().equalsIgnoreCase(Boolean.toString(true));
		final boolean currentSelection = isCheckBoxSelected(testData);
		final var message = "Validation of checkbox '" + testData.getElementId() + "' selection has failed!";

		assertTrue(message, expectedSelection == currentSelection);
	}

	/**
	 * Select an item of an auto-complete field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if an item could not be selected
	 */
	public void selectAutoCompleteItem(PageElementTestData testData) {
		assertNotNull("Value for auto-complete field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Enter text '{}' into auto-complete field '{}'", testData.getNewValue(), testData.getElementId());

		// Clear the input field and enter the search condition
		final WebElement inputField = findWebElement(testData.getElementId());
		inputField.clear();
		inputField.sendKeys(testData.getNewValue());

		// Skip searching for an item if the provided test data value is empty!
		if (testData.getNewValue().isEmpty())
			return;

		final int inputSuffixPos = testData.getElementId().lastIndexOf(INPUT_SUFFIX);

		if (inputSuffixPos == -1)
			fail("Cannot derive ID for dynamic element of field '" + testData.getElementId() + "'");

		final String panelId = testData.getElementId().substring(0, inputSuffixPos) + PANEL_SUFFIX;

		// Wait a short period of time until accessing the element containing the actual item!
		testContext.delayTest(AUTO_COMPLETE_DELAY_MILLIS);

		// When using Primefaces the results are added dynamically by using a different element
		final WebElement itemPanel = findWebElement(panelId);

		final boolean itemFound = selectItem(itemPanel, testData.getNewValue());

		if (!itemFound)
			fail("Could not find selectable item '" + testData.getNewValue() + "' for auto-complete field '" + testData.getElementId()
					+ "'!");
	}

	/**
	 * Validate the selected item of an auto-complete field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateAutoCompleteItem(PageElementTestData testData) {
		assertNotNull("Expected value for auto-complete field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		final String fieldId = testData.getElementId();

		logger.debug("Validate the selected item of auto-complete field '{}'", fieldId);

		final WebElement inputField = findWebElement(testData.getElementId());
		final var message = "Validation of auto-complete field '" + fieldId + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), inputField.getAttribute(ATTR_NAME_VALUE));
	}

	/**
	 * Select an item of a combobox field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if an item could not be selected
	 */
	public void selectComboboxItem(PageElementTestData testData) {
		assertNotNull("Value for combobox field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Search for item '{}' in combobox '{}'", testData.getNewValue(), testData.getElementId());

		// Search for the combobox field and click on it
		final WebElement combobox = findWebElement(testData.getElementId());
		combobox.click();

		// When using Primefaces the results are added dynamically by using a different element
		final String panelId = testData.getElementId() + ITEMS_PANEL_ID_SUFFIX;
		final WebElement itemsPanel = findWebElement(panelId);

		final boolean itemFound = selectItem(itemsPanel, testData.getNewValue());

		if (!itemFound)
			fail("Could not find selectable item '" + testData.getNewValue() + "' for combobox field '" + testData.getElementId()
					+ "'!");
	}

	/**
	 * Validate the selected item of a combobox field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateComboboxItem(PageElementTestData testData) {
		assertNotNull("Expected value for combobox field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		final String fieldId = testData.getElementId();

		logger.debug("Validate the selected item of combobox '{}'", fieldId);

		final List<WebElement> comboboxOptions = findWebElementsByXPath(
				"//div[@id='" + testData.getElementId() + "']/div/select/option");
		final var message = "Validation of combobox '" + fieldId + "' has failed!";

		// Iterate over all combobox items and determine the value of the selected item
		final Optional<WebElement> selectedElement = comboboxOptions.stream()
				.filter(item -> item.getAttribute(ATTR_NAME_SELECTED) != null && !item.getAttribute(ATTR_NAME_SELECTED).isEmpty())
				.findFirst();

		if (selectedElement.isPresent()) {
			final String nameAttribute = selectedElement.get().getAttribute(ATTR_NAME_VALUE);

			logger.trace("Selected combobox item '{}'", nameAttribute);

			assertEquals(message, testData.getExpectedValue(), nameAttribute);
		}
		else
			fail("The selected combobox item could not be found!");
	}

	/**
	 * Select an item by opening a list-of-values in a pop-up window
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if an item could not be selected
	 */
	public void selectLoVItem(PageElementTestData testData) {
		assertNotNull("Value for LoV field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Open LoV dialog");

		final WebElement lovField = findWebElement(testData.getElementId());

		// Perform a double-click on that field in order to open the list-of-values!
		doubleClickElement(lovField);

		// Determine the handles of the main and the list-of-values window
		final Set<String> windowId = driver.getWindowHandles();
		final Iterator<String> itererator = windowId.iterator();

		final String mainWindowId = itererator.next();
		final String lovWindowId = itererator.next();

		// Switch control to the list-of-values window
		driver.switchTo().window(lovWindowId);

		logger.debug("Enter text '{}' into LoV field '{}'", testData.getNewValue(), testData.getElementId());

		if (!testData.getNewValue().isEmpty()) {
			// Enter the filter text
			final WebElement lovSearchField = findWebElement(FIELD_ID_LOV_INPUT);
			lovSearchField.sendKeys(testData.getNewValue());

			final var tableLoV = new DataTableComponent(testContext, TABLE_ID_LOV);

			// The table component must contain only one row!
			if (tableLoV.getRowCount() != 1)
				fail("LoV table must contain exactly one row!");

			final WebElement row = tableLoV.getFirstRow();

			tableLoV.selectRow(row);

			// Click on the 'Select' button in order to apply the selection
			findWebElement(BUTTON_ID_SELECT).click();
		}
		else {
			logger.debug("Press the reset button");

			findWebElement(BUTTON_ID_RESET).click();
		}

		logger.debug("Close LoV dialog");

		// Switch control back to the main window
		driver.switchTo().window(mainWindowId);
	}

	/**
	 * Validate the selected item of a list-of-values field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateLoVItem(PageElementTestData testData) {
		assertNotNull("Expected value for LoV field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		final String fieldId = testData.getElementId();

		logger.debug("Validate the selected item of LoV field '{}'", fieldId);

		final WebElement lovField = findWebElement(fieldId);
		final var message = "Validation of LoV field '" + fieldId + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), lovField.getAttribute(ATTR_NAME_VALUE));
	}

	/**
	 * Enter a value into a date field
	 * @param testData the field's test data object that provides necessary information
	 */
	public void setDateFieldValue(PageElementTestData testData) {
		assertNotNull("Text for date field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Enter text '{}' into date field '{}'", testData.getNewValue(), testData.getElementId());

		final WebElement dateField = findWebElement(testData.getElementId());
		dateField.sendKeys(Keys.CONTROL + "a");
		dateField.sendKeys(Keys.DELETE);
		dateField.sendKeys(testData.getNewValue());
	}

	/**
	 * Validate a label field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateLabelText(PageElementTestData testData) {
		assertNotNull("Expected value for label field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate text of label field '{}'", testData.getElementId());

		final WebElement inputField = findWebElement(testData.getElementId());
		final var message = "Validation of label field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), inputField.getText());
	}

	/**
	 * Upload a file in a pop-up window
	 * @param dialogId the ID of the pop-up window
	 * @param testData the test data object that provides necessary information
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void uploadFile(String dialogId, PageElementTestData testData) {
		assertNotNull("The file upload path must not be null!", testData.getNewValue());

		logger.debug("Upload file '{}'", testData.getNewValue());

		// Search for the 'Upload' button and click on it in order to open a pop-up dialog
		findWebElement(testData.getElementId()).click();

		// Search for the file input element and enter the path of the file to be uploaded
		findWebElementByXPath("//div[@id='" + dialogId + "']//input[@type='file']").sendKeys(testData.getNewValue());

		// Click on the pop-up dialog's close button
		findWebElementByXPath("//div[@id='" + dialogId + "']//a/span[@class='ui-icon ui-icon-closethick']").click();
	}

	/**
	 * Open a tab page identified by the given ID
	 * @param tabPageId
	 * @throws AssertionError if the tab page could not be found
	 */
	public void openTabPage(String tabPageId) {
		logger.debug("Open tab page '{}'", tabPageId);

		final WebElement tabPage = findWebElementByXPath("//a[@href='#" + tabPageId + "']");
		tabPage.click();

		// Wait until the corresponding panel is visible!
		new WebDriverWait(driver, Duration.ofSeconds(TAB_PAGE_TIMEOUT_SECONDS))
				.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//div[@id='" + tabPageId + "']")));
	}

	/**
	 * Wait for a message dialog and perform a status validation check
	 * @param actionResult
	 * @return a message dialog
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public MessageDialog waitForMessageDialog(PageActionResult actionResult) {
		logger.debug("Waiting for message dialog...");

		final var dlg = new MessageDialog(this);
		dlg.validateStatus(actionResult);

		return dlg;
	}

	/**
	 * Wait for a growl notification message and perform a status validation check
	 * @param actionResult
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public void waitForNotificationMessage(PageActionResult actionResult) {
		logger.debug("Waiting for notification message...");

		final var notification = new NotificationMessage(this);
		notification.validateStatus(actionResult);
	}

	/**
	 * Compare the page title with the expected text provided by the test data object
	 * @param testData the test data object that provides necessary information
	 * @throws AssertionError if the validation either has failed, or the test data is invalid
	 */
	public void validatePageTitle(PageElementTestData testData) {
		assertNotNull("Expected page title must not be null!", testData.getExpectedValue());

		if (logger.isDebugEnabled())
			logger.debug("Validate page title '{}'", testData.getExpectedValue().trim());

		waitForPendingHTTPRequests();

		final String currentPageTitle = driver.getTitle();
		final String expectedTitle = testData.getExpectedValue().trim();

		if (currentPageTitle == null)
			fail("Page title is not available!");

		final var message = "The current page title '" + currentPageTitle + "' and the expected text '" + expectedTitle
				+ "' are different!";

		assertEquals(message, expectedTitle, currentPageTitle);
	}

	/**
	 * Search for an existing list item with the given value and select it
	 * @param element
	 * @param itemValue
	 * @return true if an item could be selected
	 */
	protected boolean selectItem(WebElement element, String itemValue) {
		final Stream<WebElement> itemStream = element.findElements(By.tagName(HTML_LIST_ITEM)).stream();
		final Optional<WebElement> item = itemStream.filter(e -> e.getText().equals(itemValue)).findFirst();

		if (item.isPresent()) {
			logger.trace("Click on item '{}'", itemValue);

			item.get().click();

			return true;
		}

		return false;
	}

	/**
	 * Determine the selection of a checkbox field
	 * @param testData the field's test data object that provides necessary information
	 * @return true if the checkbox is selected
	 */
	protected boolean isCheckBoxSelected(PageElementTestData testData) {
		// Search for the '<span>'-element that provides the information regarding the selection
		final var expression = "//div[@id='" + testData.getElementId() + "']/div/span";
		final WebElement spanElement = findWebElementByXPath(expression);
		final String classAttribute = spanElement.getAttribute(ATTR_NAME_CLASS);

		if (classAttribute == null) {
			fail("The 'class' attribute could not be found!");
			return false;
		}

		return classAttribute.contains(CLASS_CHECKBOX_SELECTED);
	}

	/**
	 * Build the page URL using the given base URL and a resource path. If necessary, both strings will be joined by using a '/'
	 * character!
	 * @param baseURL
	 * @param resourcePath
	 * @return the URL
	 */
	protected String buildPageURL(String baseURL, String resourcePath) {
		if (resourcePath == null || resourcePath.isEmpty())
			return baseURL;

		if (!baseURL.endsWith(SLASH) && !resourcePath.startsWith(SLASH))
			return baseURL + SLASH + resourcePath;

		return baseURL + resourcePath;
	}

}
