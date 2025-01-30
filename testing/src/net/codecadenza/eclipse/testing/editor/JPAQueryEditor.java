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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Editor for performing JPA queries. Note, that the respective table must contain one row at least!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAQueryEditor extends AbstractBot {
	private static final String STATUS_SUCCESS = "Query returned";
	private static final String STATUS_ERROR = "Error while";
	private static final String TOOL_ITEM_EXECUTE = "Execute JPA";
	private static final Logger log = LoggerFactory.getLogger(JPAQueryEditor.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public JPAQueryEditor(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Execute different JPA queries
	 * @param queries
	 */
	public void executQueries(List<EditorQuery> queries) {
		for (final var query : queries)
			executeQuery(query.getCommand(), query.isCheckResults(), query.isValid());

		bot.activeEditor().saveAndClose();

		waitForPendingBackgroundJobs();
	}

	private void executeQuery(String query, boolean checkResults, boolean validQuery) {
		final var inputField = bot.styledText();

		if (query != null) {
			inputField.setText(query);

			log.debug("Execute query '{}'", query);
		}
		else
			log.debug("Execute query '{}'", inputField.getText());

		bot.toolbarButtonWithTooltip(TOOL_ITEM_EXECUTE).click();

		waitForPendingBackgroundJobs();

		final var lblStatus = bot.label(1);
		final String statusText = lblStatus.getText();

		log.debug("Query execution status: '{}'", statusText);

		if (validQuery) {
			final var resultSize = bot.tree().getAllItems().length;

			if (checkResults)
				assertTrue(resultSize > 0);
			else
				assertEquals(0, resultSize);

			assertTrue(statusText.startsWith(STATUS_SUCCESS));
		}
		else
			assertTrue(statusText.startsWith(STATUS_ERROR));
	}

}
