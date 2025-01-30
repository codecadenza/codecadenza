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
import net.codecadenza.eclipse.testing.dialog.RenameCompilationUnitDialog;
import net.codecadenza.eclipse.testing.dialog.RenameDialog;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing repositories
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryBot extends AbstractBot {
	private static final Logger log = LoggerFactory.getLogger(RepositoryBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public RepositoryBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Open the source code for all repositories of the given project
	 * @param project
	 */
	public void openRepositories(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			log.debug("Open repository '{}'", domainObject.getRepositoryName());

			projectView.openRepositoryInEditor(domainObject.getRepositoryName(), domainObject.getNamespace());
		}

		bot.closeAllEditors();
	}

	/**
	 * Rename all repositories of the given project
	 * @param project
	 */
	public void renameRepositories(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			final String newName = domainObject.getRepositoryName() + NEW_NAME_SUFFIX;

			log.debug("Rename repository '{}'", domainObject.getRepositoryName());

			projectView.clickContextMenuRenameRepository(domainObject.getRepositoryName(), domainObject.getNamespace());
			new RenameDialog(bot, newName).enterData();
			new RenameCompilationUnitDialog(bot).enterData();

			projectView.openRepositoryInEditor(newName, domainObject.getNamespace());
		}
	}

	/**
	 * Delete all repositories of the given project
	 * @param project
	 */
	public void deleteRepositories(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			log.debug("Delete repository '{}'", domainObject.getRepositoryName());

			projectView.clickContextMenuDeleteRepository(domainObject.getRepositoryName(), domainObject.getNamespace());
			new DeleteDialog(bot).enterData();
		}
	}

}
