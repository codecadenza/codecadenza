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
package net.codecadenza.eclipse.testing.bots;

import java.time.Duration;
import net.codecadenza.eclipse.testing.dialog.MavenPreferencesDialog;
import net.codecadenza.eclipse.testing.util.OperatingSystem;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.utils.SWTBotPreferences;

/**
 * <p>
 * Utility class for preparing the Eclipse workspace before starting the actual tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class WorkspaceManagerBot extends AbstractBot {
	private static final long FIND_WIDGET_TIMEOUT = Duration.ofSeconds(10).toMillis();
	private static final String KEYBOARD_LAYOUT_DE = "DE_DE";
	private static final String MNU_ECLIPSE = "Eclipse";
	private static final String MNU_OTHER = "Other...";
	private static final String MNU_PREFERENCES = "Preferences...";
	private static final String MNU_SHOW_VIEW = "Show View";
	private static final String MNU_WINDOW = "Window";
	private static final String SHELL_SHOW_VIEW = "Show View";
	private static final String VIEW_GROUP_GENERAL = "General";
	private static final String VIEW_GROUP_JAVA = "Java";
	private static final String VIEW_GROUP_CODECADENZA = "CodeCadenza";
	private static final String VIEW_PACKAGE_EXPLORER_TITLE = "Package Explorer";
	private static final String VIEW_CODECADENZA_PROJECTS_TITLE = "CodeCadenza projects";
	private static final String VIEW_PROBLEMS_TITLE = "Problems";
	private static final String VIEW_WELCOME_TITLE = "Welcome";
	private static final String VIEW_PROPERTIES_TITLE = "Properties";

	/**
	 * Constructor
	 * @param bot
	 */
	public WorkspaceManagerBot(SWTWorkbenchBot bot) {
		super(bot);

		SWTBotPreferences.TIMEOUT = FIND_WIDGET_TIMEOUT;
		SWTBotPreferences.KEYBOARD_LAYOUT = KEYBOARD_LAYOUT_DE;

		// Maximize the shell
		bot.shell().maximize(true);
	}

	/**
	 * Prepare the Eclipse workspace, open all necessary views and close the 'Welcome' view if necessary
	 */
	public void prepareWorkspace() {
		// Close the welcome view
		if (isViewOpen(VIEW_WELCOME_TITLE))
			bot.viewByTitle(VIEW_WELCOME_TITLE).close();

		if (!isViewOpen(VIEW_PACKAGE_EXPLORER_TITLE))
			open(VIEW_PACKAGE_EXPLORER_TITLE, VIEW_GROUP_JAVA);

		if (!isViewOpen(VIEW_CODECADENZA_PROJECTS_TITLE))
			open(VIEW_CODECADENZA_PROJECTS_TITLE, VIEW_GROUP_CODECADENZA);

		if (!isViewOpen(VIEW_PROPERTIES_TITLE))
			open(VIEW_PROPERTIES_TITLE, VIEW_GROUP_GENERAL);

		if (!isViewOpen(VIEW_PROBLEMS_TITLE))
			clickMenuItem(bot, MNU_WINDOW, MNU_SHOW_VIEW, VIEW_PROBLEMS_TITLE);

		if (OperatingSystem.isMacOS())
			clickMenuItem(bot, MNU_ECLIPSE, MNU_PREFERENCES);
		else
			clickMenuItem(bot, MNU_WINDOW, MNU_PREFERENCES);

		new MavenPreferencesDialog(bot).enterData();
	}

	/**
	 * @param viewTitle
	 * @return true if the view with the given title is open
	 */
	private boolean isViewOpen(String viewTitle) {
		final var openViews = bot.views();

		return openViews.stream().map(SWTBotView::getTitle).anyMatch(title -> title.equals(viewTitle));
	}

	/**
	 * Open the view with the given title
	 * @param viewTitle
	 * @param groupName
	 */
	private void open(String viewTitle, String groupName) {
		clickMenuItem(bot, MNU_WINDOW, MNU_SHOW_VIEW, MNU_OTHER);

		activateShellWithTitle(SHELL_SHOW_VIEW);

		bot.tree().getTreeItem(groupName).expand().getNode(viewTitle).select();
		bot.button(CMD_OPEN).click();
	}

}
