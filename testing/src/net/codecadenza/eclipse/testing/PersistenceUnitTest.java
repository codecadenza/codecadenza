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
package net.codecadenza.eclipse.testing;

import net.codecadenza.eclipse.testing.bots.ProjectBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.dialog.AbstractDialog.OperationMode;
import net.codecadenza.eclipse.testing.dialog.project.EditPersistenceUnitPropertiesDialog;
import net.codecadenza.eclipse.testing.util.ProjectFactory;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProblemsView;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Test the handling of the persistence unit
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class PersistenceUnitTest {
	private static final Logger log = LoggerFactory.getLogger(PersistenceUnitTest.class);
	private static final SWTGefBot bot = new SWTGefBot();

	/**
	 * Initialize the test
	 */
	@BeforeAll
	static void initTest() {
		new WorkspaceManagerBot(bot).prepareWorkspace();
	}

	/**
	 * Test the handling of the persistence unit
	 */
	@Test
	void testPersistenceUnitHandling() {
		final var project = ProjectFactory.createProject();
		final var projectView = new ProjectView(bot, project);

		log.info("Starting persistence unit test");

		// Create the CodeCadenza project
		new ProjectBot(bot).createProject(project);
		new ProblemsView(bot, project).waitForNoErrors();

		bot.closeAllEditors();

		// Open the dialog for creating a new persistence unit property
		projectView.clickContextMenuEditPersistenceUnit();
		new EditPersistenceUnitPropertiesDialog(bot, OperationMode.ADD).enterData();

		// Open the dialog for editing the persistence unit property
		projectView.clickContextMenuEditPersistenceUnit();
		new EditPersistenceUnitPropertiesDialog(bot, OperationMode.EDIT).enterData();

		// Open the dialog for removing the persistence unit property
		projectView.clickContextMenuEditPersistenceUnit();
		new EditPersistenceUnitPropertiesDialog(bot, OperationMode.REMOVE).enterData();

		// Delete the project in the 'Package Explorer' view
		new PackageExplorerView(bot, project).deleteProject();

		log.info("Finish persistence unit test");
	}

}
