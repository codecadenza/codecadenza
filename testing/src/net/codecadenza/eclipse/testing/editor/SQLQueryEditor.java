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

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import net.codecadenza.eclipse.testing.bots.AbstractBot;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Editor for performing SQL queries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLQueryEditor extends AbstractBot {
	private static final String LBL_QUERY_RESULTS = "Query results:";
	private static final String LBL_MESSAGES = "Messages:";
	private static final String STATUS_ERROR = "ERROR";
	private static final String STATUS_OK = "OK";
	private static final String TOOL_ITEM_EXECUTE = "Execute SQL";
	private static final Logger log = LoggerFactory.getLogger(SQLQueryEditor.class);

	private final SWTBotEditor editor;

	/**
	 * Constructor
	 * @param bot
	 */
	public SQLQueryEditor(SWTWorkbenchBot bot) {
		super(bot);

		this.editor = bot.activeEditor();
	}

	/**
	 * Execute different SQL queries
	 * @param queries
	 */
	public void executQueries(List<EditorQuery> queries) {
		for (final var query : queries)
			executeQuery(query.getCommand(), query.isCheckResults(), query.isValid());

		editor.saveAndClose();
	}

	private void executeQuery(String query, boolean checkResults, boolean validQuery) {
		final var inputField = bot.styledText();
		inputField.setText(query);

		log.debug("Execute query '{}'", query);

		bot.toolbarButtonWithTooltip(TOOL_ITEM_EXECUTE).click();

		waitForPendingBackgroundJobs();

		final var tableResults = bot.tableWithLabel(LBL_QUERY_RESULTS);

		if (checkResults && validQuery) {
			// A 'select'-query must return one row and one column at least!
			assertTrue(tableResults.columnCount() > 0);
			assertTrue(tableResults.rowCount() > 0);
		}
		else {
			assertEquals(0, tableResults.columnCount());
			assertEquals(0, tableResults.rowCount());
		}

		final var tableStatus = bot.tableWithLabel(LBL_MESSAGES);

		for (int row = 0; row < tableStatus.rowCount(); row++) {
			final String queryStatus = tableStatus.getTableItem(row).getText(1);
			final String queryStatusText = tableStatus.getTableItem(row).getText(3);

			log.debug("Query execution status: {} - {}", queryStatus, queryStatusText);
			assertEquals(validQuery ? STATUS_OK : STATUS_ERROR, queryStatus);
		}
	}

}
