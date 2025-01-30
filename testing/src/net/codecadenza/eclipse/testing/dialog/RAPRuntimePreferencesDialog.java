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
package net.codecadenza.eclipse.testing.dialog;

import static org.junit.Assert.fail;

import java.io.File;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for adding an Eclipse RAP runtime
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RAPRuntimePreferencesDialog extends AbstractDialog {
	private static final String CMD_ADD = "Add...";
	private static final String CMD_APPLY = "Apply and Close";
	private static final String CMD_FINISH = "Finish";
	private static final String ITEM_DIRECTORY = "Directory";
	private static final String LBL_NAME = "Name:";
	private static final String RAP_RUNTIME_LOCATION = "/development/eclipse/rap";
	private static final String RAP_RUNTIME_NAME = "RAP";
	private static final String NODE_PLUG_IN_DEV = "Plug-in Development";
	private static final String NODE_TARGET_PLATFORM = "Target Platform";
	private static final String SHELL_TITLE = "Preferences";

	/**
	 * Constructor
	 * @param bot
	 */
	public RAPRuntimePreferencesDialog(SWTWorkbenchBot bot) {
		super(bot, SHELL_TITLE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var userHome = System.getProperty("user.home");
		final var runtimeLocation = new File(userHome, RAP_RUNTIME_LOCATION);

		if (!runtimeLocation.exists())
			fail("The Eclipse RAP runtime folder could not be found at " + runtimeLocation.getAbsolutePath());

		bot.tree().getTreeItem(NODE_PLUG_IN_DEV).expand().getNode(NODE_TARGET_PLATFORM).select().click();
		bot.button(CMD_ADD).click();
		bot.button(CMD_NEXT).click();
		bot.textWithLabel(LBL_NAME).setText(RAP_RUNTIME_NAME);
		bot.button(CMD_ADD).click();
		bot.table().select(ITEM_DIRECTORY);
		bot.table().getTableItem(ITEM_DIRECTORY).doubleClick();
		bot.comboBox().setText(runtimeLocation.getAbsolutePath());
		bot.button(CMD_FINISH).click();
		bot.button(CMD_FINISH).click();
		bot.table().select(RAP_RUNTIME_NAME);
		bot.table().getTableItem(RAP_RUNTIME_NAME).toggleCheck();
		bot.button(CMD_APPLY).click();
	}

}
