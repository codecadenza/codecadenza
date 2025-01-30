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

import java.util.List;
import net.codecadenza.runtime.selenium.data.PageActionResult;
import net.codecadenza.runtime.selenium.data.PageActionResult.ActionResultStatus;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

/**
 * <p>
 * Page object component that represents a notification message
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class NotificationMessage extends AbstractAngularPageComponent {
	private static final String INFO_CLASS = "p-toast-message-info";
	private static final String ERROR_CLASS = "p-toast-message-error";
	private static final String WARNING_CLASS = "p-toast-message-warning";

	/**
	 * Constructor
	 * @param pageObject
	 */
	public NotificationMessage(AbstractPageObject pageObject) {
		super(pageObject.getTestContext());

		this.logger = pageObject.getLogger();
	}

	/**
	 * Search for a visible toast message and compare its status with the expected action result
	 * @param actionResult
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public void validateStatus(PageActionResult actionResult) {
		logger.debug("Search for notification with status '{}'", actionResult.getStatus());

		final List<WebElement> toastMessages = findWebElementsByXPath("//p-toastitem");
		boolean found = false;

		// Iterate through the toast messages and check their status and message
		for (final WebElement toastMessage : toastMessages) {
			final WebElement contentElement = toastMessage.findElement(By.cssSelector("div"));
			final String message = contentElement.getText();
			final String classAttribute = contentElement.getAttribute(ATTR_NAME_CLASS);
			boolean messageIsCorrect = true;
			ActionResultStatus currentStatus = null;

			if (classAttribute == null) {
				fail("The 'class' attribute could not be found!");
				return;
			}

			// Initialize the status
			if (classAttribute.contains(INFO_CLASS))
				currentStatus = PageActionResult.ActionResultStatus.INFO;
			else if (classAttribute.contains(WARNING_CLASS))
				currentStatus = PageActionResult.ActionResultStatus.WARNING;
			else if (classAttribute.contains(ERROR_CLASS))
				currentStatus = PageActionResult.ActionResultStatus.ERROR;

			logger.trace("Found with status '{}' and message '{}'", message, currentStatus);

			// Skip the check of the message if the action result doesn't contain a message!
			if (actionResult.getMessage() != null && !actionResult.getMessage().isEmpty()
					&& !actionResult.getMessage().contains(message)) {
				messageIsCorrect = false;
			}

			if (actionResult.getStatus() == currentStatus && messageIsCorrect) {
				found = true;
				break;
			}
		}

		// Check if the expected status was found
		assertTrue("Notification was not found!", found);
	}

}
