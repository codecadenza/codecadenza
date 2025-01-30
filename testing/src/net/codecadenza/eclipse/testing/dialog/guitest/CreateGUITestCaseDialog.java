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
import net.codecadenza.eclipse.testing.domain.Project;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating a GUI test case
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateGUITestCaseDialog extends AbstractDialog {
	private static final String LBL_NAME = "Name:";
	private static final String LBL_OPEN_FORM = "Open form";
	private static final String MNU_ACTION_RESULT = "Edit test action result";
	private static final String MNU_ADD_LOGOUT_ACTION = "Add logout action";
	private static final String MNU_ADD_LOGIN_ACTION = "Add login action";
	private static final String MNU_AFTER = "After";
	private static final String MNU_DELAY = "Delay";
	private static final String SHELL_TITLE = "Create new GUI test case";
	private static final String TREE_ITEM_DELETE_PREFIX = "Delete selected row";

	private final DomainObject domainObject;
	private final Project project;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 * @param project
	 */
	public CreateGUITestCaseDialog(SWTWorkbenchBot bot, Project project, DomainObject domainObject) {
		super(bot, SHELL_TITLE);

		this.project = project;
		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var txtName = bot.textWithLabel(LBL_NAME);
		txtName.setText(domainObject.getTestCaseName());

		selectProposalItem(LBL_OPEN_FORM, domainObject.getViewFormName());

		bot.button(CMD_ADD).click();

		// Start every test by opening the respective view form
		new ViewFormTestDialog(bot, domainObject).enterData();
		activateShellWithTitle(SHELL_TITLE);

		// Add a login action
		final var firstAction = bot.tree().getAllItems()[0];

		bot.tree().getTreeItem(firstAction.getText()).contextMenu(MNU_ADD_LOGIN_ACTION).click();
		new LoginDialog(bot).enterData();
		activateShellWithTitle(SHELL_TITLE);

		// Add a logout action
		final var testActions = bot.tree().getAllItems();
		final var lastAction = testActions[testActions.length - 1];

		bot.tree().getTreeItem(lastAction.getText()).contextMenu(MNU_ADD_LOGOUT_ACTION).click();
		activateShellWithTitle(SHELL_TITLE);

		// Enter a delay after performing the login action
		bot.tree().getAllItems()[0].contextMenu(MNU_DELAY).contextMenu(MNU_AFTER).click();
		new DelayInputDialog(bot).enterData();
		activateShellWithTitle(SHELL_TITLE);

		// Edit the result of a delete operation
		for (final var treeItem : bot.tree().getAllItems())
			if (treeItem.getText().startsWith(TREE_ITEM_DELETE_PREFIX)) {
				treeItem.contextMenu(MNU_ACTION_RESULT).click();
				new EditActionResultDialog(bot, project).enterData();
				activateShellWithTitle(SHELL_TITLE);
				break;
			}

		bot.button(CMD_OK).click();
	}

}
