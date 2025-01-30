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
import net.codecadenza.eclipse.testing.dialog.guitest.CreateGUITestCaseDialog;
import net.codecadenza.eclipse.testing.dialog.guitest.EditGUITestCaseDialog;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing GUI tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestBot extends AbstractBot {
	private static final Logger log = LoggerFactory.getLogger(GUITestBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public GUITestBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Create GUI tests for the existing domain objects
	 * @param project
	 */
	public void createGUITests(Project project) {
		for (final var domainObject : project.getNonAbstractDomainObjects())
			createGUITest(project, domainObject);

		waitForPendingBackgroundJobs();
		bot.closeAllEditors();
	}

	/**
	 * Edit all GUI tests
	 * @param project
	 */
	public void editGUITests(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			log.debug("Edit GUI test case for domain object '{}'", domainObject.getName());

			projectView.clickContextMenuEditGUITest(domainObject.getTestCaseName());
			new EditGUITestCaseDialog(bot).enterData();
		}
	}

	/**
	 * Delete all GUI tests
	 * @param project
	 */
	public void deleteGUITests(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			log.debug("Delete GUI test case for domain object '{}'", domainObject.getName());

			projectView.clickContextMenuDeleteGUITest(domainObject.getTestCaseName());
			new DeleteDialog(bot).enterData();
		}
	}

	private void createGUITest(Project project, DomainObject domainObject) {
		log.debug("Create GUI test case for domain object '{}'", domainObject.getName());

		final var projectView = new ProjectView(bot, project);
		projectView.clickContextMenuCreateGUITest();
		new CreateGUITestCaseDialog(bot, project, domainObject).enterData();

		projectView.openGUITestInEditor(domainObject.getTestCaseName());
	}

}
