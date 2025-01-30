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
import net.codecadenza.runtime.selenium.data.PageActionResult;
import org.openqa.selenium.By;
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
public class MessageDialog extends AbstractPrimefacesPageComponent {
	private static final String CLASS_INFO_ICON = "ui-messages-info-icon";
	private static final String CLASS_WARN_ICON = "ui-messages-warn-icon";
	private static final String DIV_ID_MESSAGES = "form:messages";

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

		final var statusExpr = "//div[@id='" + DIV_ID_MESSAGES + "']/div/span";
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

		// Search for all elements that contain parts of the displayed message
		final var msgExpr = "//div[@id='" + DIV_ID_MESSAGES + "']/div/ul/li/span";
		final List<WebElement> listElements = findWebElementsByXPath(msgExpr);

		// Build one message by collecting the text of all elements
		final Optional<String> messageText = listElements.stream().map(WebElement::getText).reduce((s1, s2) -> s1 + " " + s2);

		// Validate the dialog's status!
		assertEquals("Message dialog has an unexpected status (" + currentStatus.name() + ")!", actionResult.getStatus(),
				currentStatus);

		if (actionResult.getMessage() != null && !actionResult.getMessage().isEmpty()) {
			if (!messageText.isPresent())
				fail("Message dialog contains no message text!");

			if (logger.isTraceEnabled())
				logger.trace("Message dialog contains following message: '{}'", messageText.orElse("").replace('\n', ' '));

			// Check if the expected message is available!
			assertTrue("Message dialog doesn't contain expected message!", messageText.orElse("").contains(actionResult.getMessage()));
		}
	}

	/**
	 * Close the dialog
	 * @throws AssertionError if an element could not be found
	 */
	public void closeDialog() {
		logger.debug("Close message dialog");

		// Search for the root element of the dialog
		final WebElement dialogElement = findWebElementByXPath("//../../div[@id='" + DIV_ID_MESSAGES + "']");

		// Search for the dialog's close button and click on it
		dialogElement.findElement(By.xpath("//div/a/span[@class='ui-icon ui-icon-closethick']")).click();
	}

}
