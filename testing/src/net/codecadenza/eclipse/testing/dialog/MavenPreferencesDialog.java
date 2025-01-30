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
 * Dialog for changing the Maven settings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MavenPreferencesDialog extends AbstractDialog {
	private static final String CMD_APPLY = "Apply and Close";
	private static final String MAVEN_SETTINGS = "/.m2/settings-cc.xml";
	private static final String SHELL_TITLE = "Preferences";
	private static final String TREE_ITEM_MAVEN = "Maven";
	private static final String TREE_ITEM_SETTINGS = "User Settings";

	/**
	 * Constructor
	 * @param bot
	 */
	public MavenPreferencesDialog(SWTWorkbenchBot bot) {
		super(bot, SHELL_TITLE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var userHome = System.getProperty("user.home");
		final var settingsPath = new File(userHome, MAVEN_SETTINGS);

		if (!settingsPath.exists())
			fail("The Maven settings file could not be found at " + settingsPath.getAbsolutePath());

		bot.tree().getTreeItem(TREE_ITEM_MAVEN).expand().getNode(TREE_ITEM_SETTINGS).click();
		bot.text(2).setText(settingsPath.getAbsolutePath());
		bot.button(CMD_APPLY).click();
	}

}
