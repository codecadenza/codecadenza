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
package net.codecadenza.eclipse.testing.editor;

import static org.junit.jupiter.api.Assertions.assertTrue;

import net.codecadenza.eclipse.testing.bots.AbstractBot;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Editor for performing a database synchronization
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBSyncEditor extends AbstractBot {
	private static final int MAX_NUMBER_OF_CHECKS = 10;
	private static final String STATUS_FINISHED = "FINISHED";
	private static final String TOOL_ITEM_SYNC = "Synchronize";
	private static final int WAIT_BEFORE_NEXT_CHECK = 500;
	private static final Logger log = LoggerFactory.getLogger(DBSyncEditor.class);

	private final SWTBotEditor editor;

	/**
	 * Constructor
	 * @param bot
	 */
	public DBSyncEditor(SWTWorkbenchBot bot) {
		super(bot);

		this.editor = bot.activeEditor();
	}

	/**
	 * Execute the database synchronization
	 */
	public void executeSync() {
		log.debug("Waiting for DDL statements");
		waitForPendingBackgroundJobs();

		assertTrue(bot.table().rowCount() > 0, "No DDL statements for synchronization available!");

		log.debug("Executing {} DDL statements", bot.table().rowCount());
		bot.toolbarButtonWithTooltip(TOOL_ITEM_SYNC).click();

		waitForPendingBackgroundJobs();

		final var rowCount = bot.table().rowCount();
		boolean finished = true;

		// Iterate over the table multiple times to check if all operations have been finished successfully
		for (int check = 0; check < MAX_NUMBER_OF_CHECKS; check++) {
			finished = true;

			log.trace("Perform {}. of max. {} checks", check + 1, MAX_NUMBER_OF_CHECKS);

			for (int row = 0; row < rowCount; row++) {
				final var status = bot.table().getTableItem(row).getText(3);

				if (!status.equals(STATUS_FINISHED))
					finished = false;
			}

			if (finished)
				break;

			bot.sleep(WAIT_BEFORE_NEXT_CHECK);
		}

		assertTrue(finished, "Error while performing database synchronization!");
		log.debug("Database synchronization finished successfully");

		editor.close();
	}

}
