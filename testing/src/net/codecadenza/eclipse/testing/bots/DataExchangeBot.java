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

import net.codecadenza.eclipse.testing.dialog.DeleteDialog;
import net.codecadenza.eclipse.testing.dialog.exchange.CreateExchangeMethodDialog;
import net.codecadenza.eclipse.testing.dialog.exchange.EditExchangeMethodDialog;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing data exchange dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataExchangeBot extends AbstractBot {
	private static final Logger log = LoggerFactory.getLogger(DataExchangeBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public DataExchangeBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Create the data exchange objects of the given project
	 * @param project
	 */
	public void createExchangeMethods(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var exchangeMethod : project.getDataExchangeMethods()) {
			final var domainObject = exchangeMethod.getDomainObject();

			log.debug("Create data exchange bean '{}'", domainObject.getDataExchangeServiceName());

			projectView.clickContextMenuCreateExchangeMethod();
			new CreateExchangeMethodDialog(bot, project, exchangeMethod).enterData();
			projectView.openDataExchangeBeanInEditor(domainObject);
		}

		waitForPendingBackgroundJobs();
		bot.closeAllEditors();
	}

	/**
	 * Edit the data exchange objects of the given project
	 * @param project
	 */
	public void editExchangeMethods(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var exchangeMethod : project.getDataExchangeMethods()) {
			final var domainObject = exchangeMethod.getDomainObject();

			log.debug("Edit data exchange bean '{}'", domainObject.getDataExchangeServiceName());

			projectView.clickContextMenuEditDataExchangeBean(domainObject);
			new EditExchangeMethodDialog(bot, exchangeMethod).enterData();
		}
	}

	/**
	 * Delete all data exchange objects of the given project
	 * @param project
	 */
	public void deleteExchangeMethods(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var exchangeMethod : project.getDataExchangeMethods()) {
			final var domainObject = exchangeMethod.getDomainObject();

			log.debug("Delete data exchange bean '{}'", domainObject.getDataExchangeServiceName());

			projectView.clickContextMenuDeleteDataExchangeBean(domainObject);
			new DeleteDialog(bot).enterData();
		}
	}

}
