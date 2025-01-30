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

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.runtime.selenium.data.PageActionResult;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebElement;

/**
 * <p>
 * Abstract base class for all page objects of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractPageObject extends AbstractAngularPageComponent {
	public static final String BUTTON_ID_LOG_IN = "cmdLogin";

	private static final String SLASH = "/";
	private static final String BUTTON_ID_LOG_OUT = "cmdLogout";
	private static final String BUTTON_ID_SELECT = "cmdSelect";
	private static final String BUTTON_ID_RESET = "cmdReset";
	private static final String BUTTON_ID_SAVE = "cmdSave";
	private static final String BUTTON_ID_BACK = "cmdBack";
	private static final String TABLE_ID_LOV = "dataTable";
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

		if (objectId != null && !objectId.isEmpty()) {
			url += url.endsWith(SLASH) ? "" : SLASH;
			url += URLEncoder.encode(objectId, StandardCharsets.UTF_8);
		}

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

		final WebElement toggleButton = findWebElementByXPath("//cc-tree-navigator//div[@id='cmdCollapse']/i");
		boolean closeNavigator = false;
		final String classAttribute = toggleButton.getAttribute(ATTR_NAME_CLASS);

		if (classAttribute == null) {
			fail("The 'class' attribute could not be found!");
			return null;
		}

		if ("pi pi-bars".equals(classAttribute)) {
			// Click the collapse toggle button to show the tree navigator
			toggleButton.click();
			closeNavigator = true;
		}

		final var expression = "//cc-tree-navigator//a[@href[contains(.,'" + navigationTarget + "')]]";

		final WebElement treeItem = findWebElementByXPath(expression);
		treeItem.click();

		if (closeNavigator) {
			// Click the collapse toggle button again to hide the tree navigator
			findWebElementByXPath("//cc-tree-navigator//div[@id='cmdCollapse']").click();
		}

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
		final boolean currentSelection = inputField.isSelected();

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

		final WebElement inputField = findWebElement(testData.getElementId());
		final boolean expectedSelection = testData.getExpectedValue().equalsIgnoreCase(Boolean.toString(true));
		final boolean currentSelection = inputField.isSelected();
		final var message = "Validation of checkbox '" + testData.getElementId() + "' selection has failed!";

		assertEquals(message, expectedSelection, currentSelection);
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
		final WebElement inputField = findWebElementByXPath("//p-autocomplete[@id='" + testData.getElementId() + "']/div/input");
		inputField.clear();
		inputField.sendKeys(testData.getNewValue());

		// Skip searching for an item if the provided test data value is empty!
		if (testData.getNewValue().isEmpty())
			return;

		// Wait a short period of time until accessing the element containing the actual item!
		testContext.delayTest(AUTO_COMPLETE_DELAY_MILLIS);

		// Search for the first selectable item
		final WebElement listElement = findWebElementByXPath("//p-overlay/div/div/div/ul");

		final boolean itemFound = selectItem(listElement, testData.getNewValue());

		assertTrue("Could not find selectable item '" + testData.getNewValue() + "' for auto-complete field '"
				+ testData.getElementId() + "'!", itemFound);
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
		final var expression = "//p-autocomplete[@id='" + fieldId + "']/div/input";

		logger.debug("Validate the selected item of auto-complete field '{}'", fieldId);

		final WebElement inputField = findWebElementByXPath(expression);
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

		// When using PrimeNG the items are added dynamically by using an overlay element
		final var itemExpression = "//p-overlay//p-dropdownitem/li//*[text()='";

		final WebElement itemElement = findWebElementByXPath(itemExpression + testData.getNewValue() + "']");

		itemElement.click();
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

		final var message = "Validation of combobox '" + fieldId + "' has failed!";
		final String selectedItem = findWebElementByXPath("//p-dropdown[@id='" + fieldId + "']/div/span").getText();

		logger.trace("Found selected combobox item '{}'", selectedItem);

		assertEquals(message, testData.getExpectedValue(), selectedItem);
	}

	/**
	 * Select an item by opening a list-of-values dialog
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if an item could not be selected
	 */
	public void selectLoVItem(PageElementTestData testData) {
		assertNotNull("Value for LoV field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Open LoV dialog");

		findWebElementByXPath("//cc-lov-input-field[@id='" + testData.getElementId() + "']/div/button").click();

		logger.debug("Enter text '{}' into LoV field '{}'", testData.getNewValue(), testData.getElementId());

		if (!testData.getNewValue().isEmpty()) {
			// Enter the filter text
			final WebElement lovSearchField = findWebElementByXPath("//p-dialog[@id='lovDialog']//input");
			lovSearchField.sendKeys(testData.getNewValue());

			final var tableLoV = new DataTableComponent(testContext, TABLE_ID_LOV);

			assertEquals("The LoV must contain exactly one row!", 1, tableLoV.getRowCount());

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

		final WebElement lovField = findWebElementByXPath("//cc-lov-input-field[@id='" + fieldId + "']/div/input");
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

		final var expression = "//p-calendar[@id='" + testData.getElementId() + "']/span/input";

		final WebElement dateField = findWebElementByXPath(expression);
		dateField.sendKeys(Keys.CONTROL + "a");
		dateField.sendKeys(Keys.DELETE);
		dateField.sendKeys(testData.getNewValue());

		// The calendar pop-up should be closed after the date has been entered
		dateField.sendKeys(Keys.TAB);
	}

	/**
	 * Validate a date field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateDateFieldValue(PageElementTestData testData) {
		assertNotNull("Expected value for date field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate value of date field '{}'", testData.getElementId());

		final var expression = "//p-calendar[@id='" + testData.getElementId() + "']/span/input";
		final WebElement dateField = findWebElementByXPath(expression);
		final String actualValue = dateField.getAttribute(ATTR_NAME_VALUE);
		final String expectedValue = testData.getExpectedValue();
		final var message = "Validation of date field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, expectedValue, actualValue);
	}

	/**
	 * Validate an internal link field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateInternalLinkText(PageElementTestData testData) {
		assertNotNull("Expected value for internal link field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate value of internal link field '{}'", testData.getElementId());

		final var expression = "//cc-formlink[@id='" + testData.getElementId() + "']/a";
		final WebElement formLinkField = findWebElementByXPath(expression);
		final String actualValue = formLinkField.getText();
		final String expectedValue = testData.getExpectedValue();
		final var message = "Validation of internal link field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, expectedValue, actualValue);
	}

	/**
	 * Validate a mail link field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateMailLinkText(PageElementTestData testData) {
		assertNotNull("Expected value for mail link field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate value of mail link field '{}'", testData.getElementId());

		final var expression = "//cc-maillink[@id='" + testData.getElementId() + "']/a";
		final WebElement formLinkField = findWebElementByXPath(expression);
		final String actualValue = formLinkField.getText();
		final String expectedValue = testData.getExpectedValue();
		final var message = "Validation of mail link field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, expectedValue, actualValue);
	}

	/**
	 * Validate a web link field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateWebLinkText(PageElementTestData testData) {
		assertNotNull("Expected value for web link field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate value of web link field '{}'", testData.getElementId());

		final var expression = "//cc-weblink[@id='" + testData.getElementId() + "']/a";
		final WebElement webLinkField = findWebElementByXPath(expression);
		final String actualValue = webLinkField.getText();
		final String expectedValue = testData.getExpectedValue();
		final var message = "Validation of web link field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, expectedValue, actualValue);
	}

	/**
	 * Upload a file
	 * @param testData the test data object that provides necessary information
	 * @throws AssertionError if an element either could not be found, or test data is invalid
	 */
	public void uploadFile(PageElementTestData testData) {
		assertNotNull("The file upload path must not be null!", testData.getNewValue());

		logger.debug("Upload file '{}'", testData.getNewValue());

		waitForPendingHTTPRequests();

		// Search for the file input element and enter the path of the file to be uploaded
		driver.findElement(By.xpath("//p-fileupload[@id='" + testData.getElementId() + "']//input[@type='file']"))
				.sendKeys(testData.getNewValue());

		waitForPendingHTTPRequests();
	}

	/**
	 * Open a tab page identified by the given ID
	 * @param tabPageId
	 * @throws AssertionError if the tab page could not be found
	 */
	public void openTabPage(String tabPageId) {
		logger.debug("Open tab page '{}'", tabPageId);

		final WebElement pageContent = driver.findElement(By.xpath("//p-tabpanel[@id='" + tabPageId + "']/div"));
		final String pageContentId = pageContent.getAttribute("id");

		if (pageContentId == null) {
			fail("The 'id' attribute could not be found!");
			return;
		}

		final String pageHeaderId = pageContentId.replace("_content", "_header_action");

		findWebElement(pageHeaderId).click();
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
	 * Wait for a notification message and perform a status validation check
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

		var expression = "//div[contains(@class, 'template-content')]//";
		expression += "cc-view-container//div[contains(@class, 'view-header-text')]";

		final var currentPageTitle = findWebElementByXPath(expression).getText();
		final String expectedTitle = testData.getExpectedValue().trim();
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
