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

import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.ProjectType;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import net.codecadenza.eclipse.testing.util.ProjectFactory;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProblemsView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot that performs a general feature test of the UI plug-in and the domain diagram editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GeneralFeatureBot {
	private static final String PROJECT_NAME = "config-processor";
	private static final Logger log = LoggerFactory.getLogger(GeneralFeatureBot.class);

	private final SWTGefBot bot;

	/**
	 * Constructor
	 * @param bot
	 */
	public GeneralFeatureBot(SWTGefBot bot) {
		this.bot = bot;
	}

	/**
	 * Perform a general feature test
	 * @param projectType
	 * @param technologyPlatform
	 * @param clientPlatform
	 */
	public void performGeneralTest(ProjectType projectType, TechnologyPlatform technologyPlatform, ClientPlatform clientPlatform) {
		final var domainBot = new DomainBot(bot);
		final var clientBot = new ClientBot(bot);
		final var boundaryBot = new BoundaryBot(bot);
		final var repositoryBot = new RepositoryBot(bot);
		final var dtoBot = new DataTransferObjectBot(bot);
		final var dataExchangeBot = new DataExchangeBot(bot);
		final var integrationBeanBot = new IntegrationBeanBot(bot);
		final var guiTestBot = new GUITestBot(bot);
		final var project = ProjectFactory.createProject(PROJECT_NAME, projectType, technologyPlatform, clientPlatform,
				DatabaseVendor.MYSQL);

		log.info("Starting general feature test with configuration:");
		log.info("Technology platform: '{}'", technologyPlatform);
		log.info("Client platform: '{}'", clientPlatform);
		log.info("Project type: '{}'", projectType);

		// Create the CodeCadenza project
		new ProjectBot(bot).createProject(project);

		// Do not try to draw the domain model until the project is being built!
		new ProblemsView(bot, project).waitForNoErrors();

		domainBot.drawDomainModel(project);
		dataExchangeBot.createExchangeMethods(project);
		dataExchangeBot.editExchangeMethods(project);
		clientBot.createForms(project);
		clientBot.editForms(project);

		if (project.addGUITests()) {
			guiTestBot.createGUITests(project);
			guiTestBot.editGUITests(project);
		}

		if (technologyPlatform != TechnologyPlatform.JAVA_SE) {
			integrationBeanBot.createIntegrationBeans(project);
			integrationBeanBot.editIntegrationBeans(project);
		}

		boundaryBot.openBoundaryBeans(project);
		boundaryBot.editBoundaryBeans(project);
		dtoBot.openDTOs(project);
		dtoBot.editDTOs(project);
		repositoryBot.openRepositories(project);

		// Wait until all compilation errors are gone
		new ProblemsView(bot, project).waitForNoErrors();

		if (project.addGUITests())
			guiTestBot.deleteGUITests(project);

		clientBot.deleteForms(project);

		if (technologyPlatform != TechnologyPlatform.JAVA_SE)
			integrationBeanBot.deleteIntegrationBeans(project);

		boundaryBot.deleteBoundaryBeans(project);
		dataExchangeBot.deleteExchangeMethods(project);
		repositoryBot.deleteRepositories(project);
		dtoBot.deleteDTOs(project);

		if (projectType != ProjectType.INHERITANCE) {
			// Skip deleting domain objects with inheritance as SWTBot is not able to find the correct edit-part!
			domainBot.deleteDomainModel(project, Project.DEFAULT_NAMESPACE);
		}

		// Delete the project in the 'Package Explorer' view
		new PackageExplorerView(bot, project).deleteProject();

		log.info("Finish general feature test for technology platform '{}' and client technology '{}'", technologyPlatform,
				clientPlatform);
	}

}
