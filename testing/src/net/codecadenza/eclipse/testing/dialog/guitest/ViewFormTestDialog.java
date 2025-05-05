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
package net.codecadenza.eclipse.testing.dialog.guitest;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.FormType;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating GUI test actions of a view form
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ViewFormTestDialog extends AbstractDialog {
	private static final String TOOL_BAR_ITEM_VALIDATE_ROW_COUNT = "Validate row count";

	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 */
	public ViewFormTestDialog(SWTWorkbenchBot bot, DomainObject domainObject) {
		super(bot, domainObject.getViewFormTitle());

		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		// Open the dialog for creating a new object
		bot.toolbarButtonWithTooltip(domainObject.getCreateFormTitle()).click();
		new SingleRecordFormTestDialog(bot, domainObject, domainObject.getCreateFormTitle(), FormType.CREATE).enterData();
		activateShellWithTitle(domainObject.getViewFormTitle());

		// Open the search dialog
		bot.toolbarButtonWithTooltip(TOOL_BAR_ITEM_SEARCH).click();
		new SearchInputDialog(bot).enterData();
		activateShellWithTitle(domainObject.getViewFormTitle());

		// Open the dialog for updating an existing object
		bot.toolbarButtonWithTooltip(domainObject.getEditFormTitle()).click();

		// Select the row that should be used
		new RowSelectionDialog(bot, false).enterData();

		// Update data
		new SingleRecordFormTestDialog(bot, domainObject, domainObject.getEditFormTitle(), FormType.UPDATE).enterData();
		activateShellWithTitle(domainObject.getViewFormTitle());

		// Refresh the view
		bot.toolbarButtonWithTooltip(TOOL_BAR_ITEM_REFRESH).click();

		// Open the read-only dialog
		bot.toolbarButtonWithTooltip(domainObject.getReadonlyFormTitle()).click();

		// Select the row that should be used
		new RowSelectionDialog(bot, true).enterData();

		// Verify data
		new SingleRecordFormTestDialog(bot, domainObject, domainObject.getReadonlyFormTitle(), FormType.READONLY).enterData();
		activateShellWithTitle(domainObject.getViewFormTitle());

		// Delete an object
		bot.toolbarButtonWithTooltip(domainObject.getToolbarTitleDelete()).click();

		// Select the row that should be deleted
		new RowSelectionDialog(bot, true).enterData();
		activateShellWithTitle(domainObject.getViewFormTitle());

		// Open the search dialog
		bot.toolbarButtonWithTooltip(TOOL_BAR_ITEM_SEARCH).click();
		new SearchInputDialog(bot).count();
		activateShellWithTitle(domainObject.getViewFormTitle());

		// Validate the row count
		bot.table().contextMenu(TOOL_BAR_ITEM_VALIDATE_ROW_COUNT).click();
		new RowCountValidationDialog(bot, 0).enterData();
		activateShellWithTitle(domainObject.getViewFormTitle());

		bot.button(CMD_OK).click();
	}

}
