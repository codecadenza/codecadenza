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
package net.codecadenza.eclipse.testing.dialog.client;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for editing view forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditViewFormDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Edit view form";
	private static final Logger log = LoggerFactory.getLogger(EditViewFormDialog.class);

	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 * @param operationMode
	 */
	public EditViewFormDialog(SWTWorkbenchBot bot, DomainObject domainObject, OperationMode operationMode) {
		super(bot, SHELL_TITLE, operationMode);

		this.domainObject = domainObject;
	}

	/**
	 * Constructor
	 * @param bot
	 */
	public EditViewFormDialog(SWTWorkbenchBot bot) {
		this(bot, null, OperationMode.NOT_SET);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		if (operationMode == OperationMode.FULL) {
			final var firstHeader = bot.table().columns().stream().findFirst().orElseThrow();

			log.debug("Edit table column '{}'", firstHeader);

			bot.table().header(firstHeader).click();
			new EditTableColumnDialog(bot).enterData();

			activateShellWithTitle(SHELL_TITLE);

			log.debug("Edit form action '{}'", domainObject.getToolbarTitleDelete());

			bot.toolbarButtonWithTooltip(domainObject.getToolbarTitleDelete()).click();
			new EditFormActionDialog(bot).enterData();

			activateShellWithTitle(SHELL_TITLE);
		}

		bot.button(CMD_OK).click();
	}

}
