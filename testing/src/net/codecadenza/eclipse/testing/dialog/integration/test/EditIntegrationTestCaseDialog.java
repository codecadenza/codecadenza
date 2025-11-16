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
package net.codecadenza.eclipse.testing.dialog.integration.test;

import java.util.Arrays;
import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.dialog.DeleteDialog;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for editing an integration test case
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditIntegrationTestCaseDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Edit integration test case ";
	private static final Logger log = LoggerFactory.getLogger(EditIntegrationTestCaseDialog.class);

	private final String testCaseName;

	/**
	 * Constructor
	 * @param bot
	 * @param testCaseName
	 */
	public EditIntegrationTestCaseDialog(SWTWorkbenchBot bot, String testCaseName) {
		super(bot, SHELL_TITLE + testCaseName);

		this.testCaseName = testCaseName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var invocations = Arrays.asList(bot.tree().getAllItems()).stream().map(SWTBotTreeItem::getText).toList().reversed();
		final var tree = bot.tree();

		// Delete all invocations in reverse order
		for (final var invocation : invocations) {
			log.debug("Delete integration test invocation '{}'", invocation);

			tree.getTreeItem(invocation).contextMenu(MNU_DELETE).click();
			new DeleteDialog(bot).enterData();
		}

		activateShellWithTitle(SHELL_TITLE + testCaseName);

		bot.button(CMD_OK).click();
	}

}
