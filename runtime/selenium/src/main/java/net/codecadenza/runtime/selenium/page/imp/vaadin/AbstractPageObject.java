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

import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_BUTTON_RESET_XPATH;
import static net.codecadenza.runtime.selenium.page.imp.vaadin.PopUpDialog.DIALOG_XPATH;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import net.codecadenza.runtime.selenium.data.PageActionResult;
import net.codecadenza.runtime.selenium.data.PageElementTestData;
import net.codecadenza.runtime.selenium.junit.SeleniumTestContext;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.SearchContext;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;

/**
 * <p>
 * Abstract base class for all page objects of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractPageObject extends AbstractVaadinPageComponent {
	public static final String BUTTON_ID_LOG_IN = "cmdLogin";
	private static final String BUTTON_ID_CANCEL = "cmdCancel";
	private static final String BUTTON_ID_LOG_OUT = "cmdLogout";
	private static final String BUTTON_ID_SAVE = "cmdSave";
	private static final String ATTR_NAME_VALUE = "value";
	private static final String DATE_TIME_PICKER_ELEMENT_NAME = "vaadin-date-time-picker";
	private static final int LOV_ROW_SELECTION_DELAY = 500;
	private static final String NAVIGATOR_XPATH = "//vaadin-vertical-layout[@class='sidemenu-menu']//vaadin-grid";
	private static final int SCROLL_PAGE_SIZE = 10;
	private static final String SLASH = "/";
	private static final String SINGLE_LINE_INPUT = "input";
	private static final String MULTI_LINE_INPUT = "textarea";

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
			url += SLASH + URLEncoder.encode(objectId, StandardCharsets.UTF_8);

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
	 * @throws AssertionError if the page object could not be instantiated
	 */
	public <T extends AbstractPageObject> T openPageByNavigator(String navigationTarget, Class<T> pageClass) {
		logger.debug("Navigate to '{}'", navigationTarget);

		// We assume that the label uniquely identifies the view to be opened!
		final var expression = NAVIGATOR_XPATH + "//vaadin-grid-tree-toggle[text()='" + navigationTarget + "']";

		// Search for the appropriate tree item
		WebElement treeItem = findWebElementByXPath(expression);

		// Click on it in order to select it
		new Actions(driver).click(treeItem).build().perform();

		// Search for the tree item again in order to avoid a StaleElementReferenceException!
		treeItem = findWebElementByXPath(expression);

		// Click on the tree item again in order to open the navigation target
		new Actions(driver).click(treeItem).build().perform();

		return createPageObject(pageClass);
	}

	/**
	 * Press the logout button
	 * @throws AssertionError if the button could not be found
	 */
	public void pressLogoutButton() {
		pressButton(BUTTON_ID_LOG_OUT);

		logger.debug("Click 'Yes' button in order to confirm the logout operation");

		// Search for the confirmation button in the pop-up dialog!
		final WebElement confirmLogout = findWebElementByXPath(PopUpDialog.DIALOG_BUTTON_YES_XPATH);
		confirmLogout.click();
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
	 * Press the 'Cancel' button
	 * @throws AssertionError if the button could not be found
	 */
	public void pressCancelButton() {
		pressButton(BUTTON_ID_CANCEL);
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

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.page.AbstractPageComponent#
	 * setInputFieldValue(net.codecadenza.runtime.selenium.data.PageElementTestData)
	 */
	@Override
	public void setInputFieldValue(PageElementTestData testData) {
		setInputFieldValue(testData, SINGLE_LINE_INPUT);
	}

	/**
	 * Enter data into a multi-line text field
	 * @param testData the field's test data object that provides necessary information
	 */
	public void setMultiLineFieldValue(PageElementTestData testData) {
		setInputFieldValue(testData, MULTI_LINE_INPUT);
	}

	/**
	 * Change the selection of a checkbox field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if changing of the checkbox selection has failed
	 */
	public void setCheckBoxValue(PageElementTestData testData) {
		assertNotNull("Value for checkbox field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Set selection of checkbox '{}'", testData.getElementId());

		final WebElement inputField = findWebElementByXPath("//vaadin-checkbox[@id='" + testData.getElementId() + "']/input");
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
		final WebElement inputField = findWebElementByXPath("//vaadin-checkbox[@id='" + testData.getElementId() + "']");
		final boolean currentSelection = inputField.getAttribute("checked") != null;
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

		// Clear the input field and enter a search condition
		final WebElement inputField = findWebElementByXPath("//vaadin-combo-box[@id='" + testData.getElementId() + "']");
		inputField.sendKeys(Keys.CONTROL + "a");
		inputField.sendKeys(Keys.DELETE);
		inputField.sendKeys(testData.getNewValue());

		if (!testData.getNewValue().isEmpty()) {
			final boolean itemFound = searchAndSelectItem(inputField, testData.getNewValue());

			if (!itemFound)
				fail("Could not find selectable item '" + testData.getNewValue() + "' for auto-complete field '" + testData.getElementId()
						+ "'!");
		}
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

		final WebElement autoCompleteField = findWebElementByXPath("//vaadin-combo-box[@id='" + testData.getElementId() + "']");
		final WebElement textField = autoCompleteField.findElement(By.tagName(SINGLE_LINE_INPUT));
		final var message = "Validation of auto-complete field '" + fieldId + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), textField.getAttribute(ATTR_NAME_VALUE));
	}

	/**
	 * Select an item of a combobox field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if an item could not be selected
	 */
	public void selectComboboxItem(PageElementTestData testData) {
		assertNotNull("Value for combobox field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Search for item '{}' in combobox '{}'", testData.getNewValue(), testData.getElementId());

		// Search for the combobox field and click on it in order to display the available items
		final WebElement combobox = findWebElementByXPath("//vaadin-combo-box[@id='" + testData.getElementId() + "']");
		combobox.click();

		// Reset the selection if the test data contains a single whitespace!
		if (!" ".equals(testData.getNewValue())) {
			final boolean itemFound = searchAndSelectItem(combobox, testData.getNewValue());

			if (!itemFound)
				fail("Could not find selectable item '" + testData.getNewValue() + "' for combobox '" + testData.getElementId() + "'!");
		}
		else {
			combobox.sendKeys(Keys.CONTROL + "a");
			combobox.sendKeys(Keys.DELETE);
			combobox.sendKeys(Keys.ENTER);
		}
	}

	/**
	 * Validate the selected item of a combobox field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateComboboxItem(PageElementTestData testData) {
		final String fieldId = testData.getElementId();

		assertNotNull("Expected value for combobox field '" + fieldId + "' must not be null!", testData.getExpectedValue());

		logger.debug("Validate the selected item of combobox '{}'", fieldId);

		final WebElement combobox = findWebElementByXPath("//vaadin-combo-box[@id='" + fieldId + "']");
		final WebElement textField = combobox.findElement(By.tagName(SINGLE_LINE_INPUT));
		final var message = "Validation of combobox '" + fieldId + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), textField.getAttribute(ATTR_NAME_VALUE));
	}

	/**
	 * Select an item by opening a list-of-values in a pop-up window
	 * @param testData the field's test data object that provides necessary information
	 */
	public void selectLoVItem(PageElementTestData testData) {
		assertNotNull("Value for LoV field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Open LoV dialog");

		// Open the list-of-values by pressing the respective button
		findWebElementByXPath("//div[@id='" + testData.getElementId() + "']//vaadin-button").click();

		logger.debug("Enter text '{}' into LoV field '{}'", testData.getNewValue(), testData.getElementId());

		if (!testData.getNewValue().isEmpty()) {
			// Enter the filter text
			final WebElement txtFilterInput = findWebElementByXPath(DIALOG_XPATH + "vaadin-text-field[@id='txtInput']");
			txtFilterInput.sendKeys(Keys.CONTROL + "a");
			txtFilterInput.sendKeys(Keys.DELETE);
			txtFilterInput.sendKeys(testData.getNewValue());

			testContext.delayTest(LOV_ROW_SELECTION_DELAY);

			final var tableLoV = new DataTableComponent(testContext);
			final WebElement row = tableLoV.getRowByCellValue(testData.getNewValue(), true);

			// Double-click on the row in order to apply the selection
			tableLoV.doubleClickRow(row);
		}
		else {
			logger.debug("Press the reset button");

			findWebElementByXPath(DIALOG_BUTTON_RESET_XPATH).click();
		}
	}

	/**
	 * Validate the selected item of a list-of-values field
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateLoVItem(PageElementTestData testData) {
		final String fieldId = testData.getElementId();

		assertNotNull("Expected value for LoV field '" + fieldId + "' must not be null!", testData.getExpectedValue());

		logger.debug("Validate the selected item of LoV field '{}'", fieldId);

		final WebElement txtLoV = findWebElementByXPath("//div[@id='" + fieldId + "']/vaadin-horizontal-layout/vaadin-text-field");
		final var message = "Validation of LoV field '" + fieldId + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), txtLoV.getAttribute(ATTR_NAME_VALUE));
	}

	/**
	 * Enter a value into a date field
	 * @param testData the field's test data object that provides necessary information
	 */
	public void setDateFieldValue(PageElementTestData testData) {
		assertNotNull("Text for date field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Enter text '{}' into date field '{}'", testData.getNewValue(), testData.getElementId());

		final WebElement dateField = findWebElement(testData.getElementId());
		final String newDateValue;
		final String newTimeValue;

		if (DATE_TIME_PICKER_ELEMENT_NAME.equals(dateField.getTagName())) {
			final int firstSpace = testData.getNewValue().trim().indexOf(" ");

			assertTrue("Invalid test data! Date and time must be separated by a single whitespace!", firstSpace > 1);

			newDateValue = testData.getNewValue().substring(0, firstSpace);
			newTimeValue = testData.getNewValue().substring(firstSpace + 1);
		}
		else {
			newDateValue = testData.getNewValue();
			newTimeValue = null;
		}

		dateField.sendKeys(Keys.CONTROL + "a");
		dateField.sendKeys(Keys.DELETE);
		dateField.sendKeys(newDateValue);

		// Close the date selection dialog
		dateField.sendKeys(Keys.ENTER);

		if (newTimeValue != null) {
			final String expression = "//vaadin-date-time-picker[@id='" + testData.getElementId() + "']//vaadin-time-picker/input";

			final WebElement timeField = findWebElementByXPath(expression);
			timeField.sendKeys(Keys.CONTROL + "a");
			timeField.sendKeys(Keys.DELETE);
			timeField.sendKeys(newTimeValue);

			// Close the time selection list
			timeField.sendKeys(Keys.ENTER);
		}
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

		final WebElement field = findWebElement(testData.getElementId());
		final var message = "Validation of date field '" + testData.getElementId() + "' has failed!";
		final String actualValue;

		if (DATE_TIME_PICKER_ELEMENT_NAME.equals(field.getTagName())) {
			final WebElement dateField = field.findElement(By.tagName("vaadin-date-picker"));
			final WebElement dateFieldInput = dateField.findElement(By.tagName(SINGLE_LINE_INPUT));
			final WebElement timeField = field.findElement(By.tagName("vaadin-time-picker"));
			final WebElement timeFieldInput = timeField.findElement(By.tagName(SINGLE_LINE_INPUT));

			actualValue = dateFieldInput.getAttribute(ATTR_NAME_VALUE) + " " + timeFieldInput.getAttribute(ATTR_NAME_VALUE);
		}
		else {
			final WebElement dateFieldInput = field.findElement(By.tagName(SINGLE_LINE_INPUT));

			actualValue = dateFieldInput.getAttribute(ATTR_NAME_VALUE);
		}

		assertEquals(message, testData.getExpectedValue(), actualValue);
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

		final WebElement labelField = findWebElement(testData.getElementId());
		final var message = "Validation of label field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), labelField.getText());
	}

	/**
	 * Validate an internal link field by using the field's test data object
	 * @param testData the field's test data object that provides necessary information
	 * @throws AssertionError if the validation has failed
	 */
	public void validateInternalLinkText(PageElementTestData testData) {
		assertNotNull("Expected value for internal link field '" + testData.getElementId() + "' must not be null!",
				testData.getExpectedValue());

		logger.debug("Validate text of internal link field '{}'", testData.getElementId());

		final WebElement linkField = findWebElementByXPath(
				"//div[@id='" + testData.getElementId() + "']/vaadin-horizontal-layout/vaadin-text-field");
		final var message = "Validation of internal link field '" + testData.getElementId() + "' has failed!";

		assertEquals(message, testData.getExpectedValue(), linkField.getAttribute(ATTR_NAME_VALUE));
	}

	/**
	 * Upload a file in a pop-up window
	 * @param testData the test data object that provides necessary information
	 * @throws AssertionError if the upload button either could not be found, or test data is invalid
	 */
	public void uploadFile(PageElementTestData testData) {
		assertNotNull("The file upload path must not be null!", testData.getNewValue());

		logger.debug("Upload file '{}'", testData.getNewValue());

		// Search for the 'Upload' button and click on it in order to open a pop-up dialog
		findWebElement(testData.getElementId()).click();

		final WebElement upload = findWebElementByXPath(PopUpDialog.DIALOG_FILE_UPLOAD_XPATH);

		// Search for the file input element and enter the path of the file to be uploaded
		final SearchContext shadowRoot = getShadowRoot(upload);
		shadowRoot.findElement(By.id(PopUpDialog.DIALOG_FILE_INPUT_ID)).sendKeys(testData.getNewValue());
	}

	/**
	 * Open a tab page identified by the given ID
	 * @param tabPageId
	 * @throws AssertionError if the tab page could not be found
	 */
	public void openTabPage(String tabPageId) {
		logger.debug("Open tab page '{}'", tabPageId);

		findWebElement(tabPageId).click();
	}

	/**
	 * Wait for a message dialog and perform a status validation check
	 * @param actionResult
	 * @return a message dialog
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public PopUpDialog waitForMessageDialog(PageActionResult actionResult) {
		logger.debug("Waiting for message dialog...");

		final var dlg = new PopUpDialog(this);
		dlg.validateStatus(actionResult);

		return dlg;
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

		final WebElement titleElement = findWebElementByXPath("//vaadin-horizontal-layout[@class='sidemenu-header']/h1");
		final String currentPageTitle = titleElement.getText().trim();
		final String expectedTitle = testData.getExpectedValue().trim();
		final var message = "The current page title '" + currentPageTitle + "' and the expected text '" + expectedTitle
				+ "' are different!";

		assertTrue(message, currentPageTitle.equals(expectedTitle));
	}

	/**
	 * Build the page URL by using the given base URL and a resource path. If necessary, both strings will be joined by using a '/'
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

	/**
	 * Enter data into an input field
	 * @param testData the field's test data object that provides necessary information
	 * @param fieldType the field type, either 'input' or 'textarea'
	 */
	protected void setInputFieldValue(PageElementTestData testData, String fieldType) {
		assertNotNull("Text for input field '" + testData.getElementId() + "' must not be null!", testData.getNewValue());

		logger.debug("Enter text '{}' into input field '{}'", testData.getNewValue(), testData.getElementId());

		final WebElement textField = findWebElement(testData.getElementId());
		final WebElement input = textField.findElement(By.tagName(fieldType));

		input.sendKeys(Keys.CONTROL + "a");
		input.sendKeys(Keys.DELETE);

		// The ENTER key must be pressed for all new-line characters!
		final String textToEnter = testData.getNewValue().replace(NEW_LINE, Keys.ENTER);

		input.sendKeys(textToEnter);
	}

	/**
	 * Search for a given item and select it if it is contained in the respective list
	 * @param combobox
	 * @param item
	 * @return true if an item could be selected
	 */
	protected boolean searchAndSelectItem(WebElement combobox, String item) {
		final WebElement scroller = combobox.findElement(By.tagName("vaadin-combo-box-scroller"));
		final List<String> previousItems = new ArrayList<>();

		while (true) {
			// The list only contains the items that are really visible!
			final List<WebElement> visibleItems = scroller.findElements(By.tagName("vaadin-combo-box-item"));
			final Optional<WebElement> itemToSearchFor = visibleItems.stream().filter(i -> i.getText().equals(item)).findFirst();
			final List<String> actualItems = visibleItems.stream().map(WebElement::getText).toList();

			if (itemToSearchFor.isPresent()) {
				logger.trace("Item '{}' found", item);
				itemToSearchFor.get().click();
				return true;
			}

			if (actualItems.equals(previousItems)) {
				// It is assumed that the end is reached if both lists contain the same items. This requires that the actual items are
				// more or less unique.
				logger.trace("Scrolled down to the end");
				return false;
			}

			previousItems.clear();
			previousItems.addAll(actualItems);

			logger.trace("Scroll down to make further combobox items visible");

			for (int i = 0; i < SCROLL_PAGE_SIZE; i++)
				scroller.sendKeys(Keys.ARROW_DOWN);
		}
	}

}
