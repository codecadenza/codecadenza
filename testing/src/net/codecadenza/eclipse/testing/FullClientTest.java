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

import net.codecadenza.eclipse.testing.bots.ClientBot;
import net.codecadenza.eclipse.testing.bots.DomainBot;
import net.codecadenza.eclipse.testing.bots.ProjectBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.dialog.AbstractDialog.OperationMode;
import net.codecadenza.eclipse.testing.util.ProjectFactory;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProblemsView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Test the handling of form fields, form actions and table columns
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class FullClientTest {
	private static final Logger log = LoggerFactory.getLogger(FullClientTest.class);
	private static final SWTGefBot bot = new SWTGefBot();

	/**
	 * Initialize the test
	 */
	@BeforeAll
	static void initTest() {
		new WorkspaceManagerBot(bot).prepareWorkspace();
	}

	/**
	 * Test the handling of form fields, form actions and table columns
	 */
	@Test
	void testFullClient() {
		final var project = ProjectFactory.createProject();
		final var domainBot = new DomainBot(bot);
		final var clientBot = new ClientBot(bot, OperationMode.FULL);
		final var problemsView = new ProblemsView(bot, project);

		log.info("Starting full client test");

		// Create the CodeCadenza project
		new ProjectBot(bot).createProject(project);
		problemsView.waitForNoErrors();

		domainBot.drawDomainModel(project);
		clientBot.createForms(project);
		clientBot.editForms(project);

		// Wait until all compilation errors are gone
		problemsView.waitForNoErrors();

		// Delete the project in the 'Package Explorer' view
		new PackageExplorerView(bot, project).deleteProject();

		log.info("Finish full client test");
	}

}
