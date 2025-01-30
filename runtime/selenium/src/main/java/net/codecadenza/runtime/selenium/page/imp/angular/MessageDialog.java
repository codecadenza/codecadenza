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

import net.codecadenza.runtime.selenium.data.PageActionResult;
import org.openqa.selenium.WebElement;

/**
 * <p>
 * Page object component that represents a message dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MessageDialog extends AbstractAngularPageComponent {
	private static final String CLASS_ERROR_ICON = "pi pi-exclamation-triangle";

	/**
	 * Constructor
	 * @param pageObject
	 */
	public MessageDialog(AbstractPageObject pageObject) {
		super(pageObject.getTestContext());

		this.logger = pageObject.getLogger();
	}

	/**
	 * Search for a visible message dialog and compare its status with the expected action result
	 * @param actionResult
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public void validateStatus(PageActionResult actionResult) {
		logger.debug("Read information from '{}' message dialog", actionResult.getStatus());

		final var statusExpr = "//p-dialog//i[@id='imgStatus']";
		final WebElement statusIcon = findWebElementByXPath(statusExpr);
		final String statusIconClass = statusIcon.getAttribute(ATTR_NAME_CLASS);
		PageActionResult.ActionResultStatus currentStatus = null;

		if (statusIconClass == null) {
			fail("The 'class' attribute could not be found!");
			return;
		}

		// Determine the status. Note that no status other than 'ERROR' is expected!
		if (statusIconClass.equals(CLASS_ERROR_ICON))
			currentStatus = PageActionResult.ActionResultStatus.ERROR;

		assertNotNull("Unexpected message status '" + statusIconClass + "'", currentStatus);

		// Validate the dialog's status!
		assertEquals("Message dialog has an unexpected status (" + currentStatus + ")!", actionResult.getStatus(), currentStatus);

		if (actionResult.getMessage() != null && !actionResult.getMessage().isEmpty()) {
			final var msgExpr = "//p-dialog//div[@id='lblDetails']";
			final String messageText = findWebElementByXPath(msgExpr).getText();

			if (logger.isTraceEnabled())
				logger.trace("Message dialog contains following message: '{}'", messageText.replace('\n', ' '));

			// Check if the expected message is available!
			assertTrue("Message dialog doesn't contain expected message!", messageText.contains(actionResult.getMessage()));
		}
	}

	/**
	 * Close the dialog
	 * @throws AssertionError if the respective button could not be found
	 */
	public void closeDialog() {
		logger.debug("Close message dialog");

		// Search for the dialog's close button and click on it
		findWebElementByXPath("//p-dialog//button[@id='cmdClose']").click();
	}

}
