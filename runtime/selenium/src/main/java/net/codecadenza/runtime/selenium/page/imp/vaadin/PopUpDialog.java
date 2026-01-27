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

import net.codecadenza.runtime.selenium.data.PageActionResult;
import org.openqa.selenium.WebElement;

/**
 * <p>
 * Page object component that represents a pop-up dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PopUpDialog extends AbstractVaadinPageComponent {
	public static final String DIALOG_XPATH = "//vaadin-dialog//";
	public static final String DIALOG_BUTTON_CANCEL_XPATH = DIALOG_XPATH + "vaadin-button[@id='cmdCancel']";
	public static final String DIALOG_BUTTON_COUNT_XPATH = DIALOG_XPATH + "vaadin-button[@id='cmdCount']";
	public static final String DIALOG_BUTTON_OK_XPATH = DIALOG_XPATH + "vaadin-button[@id='cmdOK']";
	public static final String DIALOG_BUTTON_RESET_XPATH = DIALOG_XPATH + "vaadin-button[@id='cmdReset']";
	public static final String DIALOG_BUTTON_YES_XPATH = DIALOG_XPATH + "vaadin-button[@id='cmdYes']";
	public static final String DIALOG_FILE_UPLOAD_XPATH = DIALOG_XPATH + "vaadin-upload";
	public static final String DIALOG_FILE_INPUT_ID = "fileInput";
	private static final String STATUS_ICON_INFO = "vaadin:info-circle";
	private static final String STATUS_ICON_WARN = "vaadin:warning";
	private static final String STATUS_ICON_ERROR = "vaadin:exclamation-circle";

	/**
	 * Constructor
	 * @param pageObject
	 */
	public PopUpDialog(AbstractPageObject pageObject) {
		super(pageObject.getTestContext());

		this.logger = pageObject.getLogger();
	}

	/**
	 * Search for a visible pop-up dialog and compare its status with the expected action result
	 * @param actionResult
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public void validateStatus(PageActionResult actionResult) {
		logger.debug("Read information from '{}' pop-up dialog", actionResult.getStatus());

		final var statusExpr = DIALOG_XPATH + "vaadin-icon[@id='imgStatus']";
		final WebElement statusIconElement = findWebElementByXPath(statusExpr);
		final String statusIcon = statusIconElement.getAttribute("icon");
		PageActionResult.ActionResultStatus currentStatus = null;

		if (statusIcon == null) {
			fail("The 'icon' attribute could not be found!");
			return;
		}

		// Determine the current status by using the respective image
		if (statusIcon.equals(STATUS_ICON_INFO))
			currentStatus = PageActionResult.ActionResultStatus.INFO;
		else if (statusIcon.equals(STATUS_ICON_WARN))
			currentStatus = PageActionResult.ActionResultStatus.WARNING;
		else if (statusIcon.equals(STATUS_ICON_ERROR))
			currentStatus = PageActionResult.ActionResultStatus.ERROR;

		if (currentStatus == null) {
			fail("Could not determine the message status!");
			return;
		}

		// Search for the message text
		final var msgExpr = DIALOG_XPATH + "label[@id='lblMessage']";
		final WebElement messageElement = findWebElementByXPath(msgExpr);

		// Validate the dialog's status!
		assertEquals("Pop-up dialog has an unexpected status (" + currentStatus.name() + ")!", currentStatus,
				actionResult.getStatus());

		if (actionResult.getMessage() != null && !actionResult.getMessage().isEmpty()) {
			final String message = messageElement.getText();

			if (logger.isTraceEnabled())
				logger.trace("Pop-up dialog contains following message: '{}'", message.replace('\n', ' '));

			// Check if the expected message is available!
			assertTrue("Pop-up dialog doesn't contain expected message!", message.contains(actionResult.getMessage()));
		}
	}

	/**
	 * Close the dialog
	 * @throws AssertionError if the button for closing the dialog could not be found
	 */
	public void closeDialog() {
		logger.debug("Close pop-up dialog");

		// Click on the 'OK' button in order to close the dialog
		findWebElementByXPath(DIALOG_BUTTON_OK_XPATH).click();
	}

}
