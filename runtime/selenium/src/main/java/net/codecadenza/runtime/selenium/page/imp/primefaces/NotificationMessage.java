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

import net.codecadenza.runtime.selenium.data.PageActionResult;
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
public class NotificationMessage extends AbstractPrimefacesPageComponent {
	private static final String CLASS_INFO_ICON = "ui-growl-image ui-growl-image-info";
	private static final String CLASS_WARN_ICON = "ui-growl-image ui-growl-image-warn";
	private static final String DIV_ID_GROWL = "form:growl_container";

	/**
	 * Constructor
	 * @param pageObject
	 */
	public NotificationMessage(AbstractPageObject pageObject) {
		super(pageObject.getTestContext());

		this.logger = pageObject.getLogger();
	}

	/**
	 * Search for a visible growl panel and compare its status with the expected action result
	 * @param actionResult
	 * @throws AssertionError if the status validation either has failed, or an element could not be found
	 */
	public void validateStatus(PageActionResult actionResult) {
		logger.debug("Read information from '{}' notification", actionResult.getStatus());

		final var statusExpr = "//div[@id='" + DIV_ID_GROWL + "']/div/div/span";
		final WebElement statusIcon = findWebElementByXPath(statusExpr);
		final String statusIconClass = statusIcon.getAttribute(ATTR_NAME_CLASS);
		final PageActionResult.ActionResultStatus currentStatus;

		if (statusIconClass == null) {
			fail("The 'class' attribute could not be found!");
			return;
		}

		// Initialize the status
		if (statusIconClass.equals(CLASS_INFO_ICON))
			currentStatus = PageActionResult.ActionResultStatus.INFO;
		else if (statusIconClass.equals(CLASS_WARN_ICON))
			currentStatus = PageActionResult.ActionResultStatus.WARNING;
		else
			currentStatus = PageActionResult.ActionResultStatus.ERROR;

		// Search for the element that contains the message
		final var msgExpr = "//div[@id='" + DIV_ID_GROWL + "']/div/div/div/span[@class='ui-growl-title']";
		final WebElement messageElement = findWebElementByXPath(msgExpr);

		// Validate the status of the notification message!
		assertEquals("Notification has an unexpected status!", currentStatus, actionResult.getStatus());

		// Check if the expected message is available!
		if (actionResult.getMessage() != null && !actionResult.getMessage().isEmpty()) {
			logger.trace("Notification contains following message: '{}'", messageElement.getText());

			assertTrue("Notification doesn't contain expected message!", messageElement.getText().contains(actionResult.getMessage()));
		}
	}

}
