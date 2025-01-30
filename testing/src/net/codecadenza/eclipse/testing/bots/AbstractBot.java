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

import static org.junit.jupiter.api.Assertions.fail;

import net.codecadenza.eclipse.testing.util.OperatingSystem;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for all test bots
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractBot {
	protected static final String CMD_ADD = "Add";
	protected static final String CMD_CANCEL = "Cancel";
	protected static final String CMD_COUNT = "Count";
	protected static final String CMD_FINISH = "Finish";
	protected static final String CMD_NEXT = "Next >";
	protected static final String CMD_OK = "OK";
	protected static final String CMD_OPEN = "Open";
	protected static final String CMD_REMOVE = "Remove";
	protected static final String MNU_ADD = "Add";
	protected static final String MNU_DELETE = "Delete";
	protected static final String MNU_EDIT = "Edit";
	protected static final String MNU_REFRESH = "Refresh";
	protected static final String MNU_RENAME = "Rename";
	protected static final String NEW_NAME_SUFFIX = "Renamed";
	protected static final String TOOL_BAR_ITEM_REFRESH = "Refresh";
	protected static final String TOOL_BAR_ITEM_SEARCH = "Search";
	private static final int MAX_JOB_CHECKS = 6;
	private static final int MAX_MENU_CHECKS = 10;
	private static final long SLEEP_TIME_JOB = 100;
	private static final long SLEEP_TIME_MENU = 500;
	private static final Logger log = LoggerFactory.getLogger(AbstractBot.class);

	protected final SWTWorkbenchBot bot;

	/**
	 * Constructor
	 * @param bot
	 */
	protected AbstractBot(SWTWorkbenchBot bot) {
		this.bot = bot;
	}

	/**
	 * Wait until all background jobs have been finished
	 */
	public void waitForPendingBackgroundJobs() {
		int check = 1;

		while (Job.getJobManager().isIdle()) {
			log.trace("Waiting for job(s) to start...");
			bot.sleep(SLEEP_TIME_JOB);

			if (check == MAX_JOB_CHECKS) {
				log.trace("Stop waiting for job(s)");
				break;
			}

			check++;
		}

		while (!Job.getJobManager().isIdle()) {
			log.trace("Waiting for pending job(s)...");
			bot.sleep(SLEEP_TIME_JOB);
		}
	}

	/**
	 * Activate the shell with the given title
	 * @param title
	 * @return the respective {@link SWTBotShell}
	 */
	protected SWTBotShell activateShellWithTitle(String title) {
		log.trace("Activate shell with title '{}'", title);
		return bot.shell(title).activate();
	}

	/**
	 * Open the main menu, iterate over all sub-menu items and click on the last one
	 * @param bot
	 * @param rootMenuName
	 * @param subMenuItems
	 */
	public static void clickMenuItem(SWTWorkbenchBot bot, String rootMenuName, String... subMenuItems) {
		SWTBotMenu rootMenu = null;

		// On Linux it may be necessary to search for the root menu multiple times
		if (!OperatingSystem.isMacOS() && !OperatingSystem.isWindows())
			for (int i = 0; i < MAX_MENU_CHECKS; i++)
				try {
					rootMenu = bot.menu(rootMenuName);
				}
				catch (final WidgetNotFoundException e) {
					log.warn("Could not find menu '{}'", rootMenuName);
					bot.sleep(SLEEP_TIME_MENU);
				}

		if (rootMenu == null) {
			fail("The menu '" + rootMenuName + "' could not be found!");
			return;
		}

		SWTBotMenu subMenu = null;

		for (final var subMenuItemName : subMenuItems)
			if (subMenu == null)
				subMenu = rootMenu.menu(subMenuItemName);
			else
				subMenu = subMenu.menu(subMenuItemName);

		if (subMenu != null) {
			log.trace("Click menu item '{}'", subMenu.getText());
			subMenu.click();
		}
		else {
			log.trace("Click menu item '{}'", rootMenu.getText());
			rootMenu.click();
		}
	}

}
