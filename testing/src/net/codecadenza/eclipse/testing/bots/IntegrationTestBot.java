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
import net.codecadenza.eclipse.testing.dialog.integration.test.CreateIntegrationTestCaseDialog;
import net.codecadenza.eclipse.testing.dialog.integration.test.EditIntegrationTestCaseDialog;
import net.codecadenza.eclipse.testing.domain.IntegrationBeanType;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing integration test cases
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestBot extends AbstractBot {
	private static final String INTEGRATION_TEST_SUFFIX = "Test";
	private static final Logger log = LoggerFactory.getLogger(IntegrationTestBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public IntegrationTestBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Create integration test cases for all domain objects of the given project
	 * @param project
	 */
	public void createIntegrationTestCases(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var moduleName : projectView.getIntegrationTestModuleNames()) {
			final var integrationBeanType = IntegrationBeanType.valueOf(moduleName);
			boolean addNestedInvocations = true;

			for (final var domainObject : project.getNonAbstractDomainObjects()) {
				final String integrationTestName = domainObject.getName() + INTEGRATION_TEST_SUFFIX;

				log.debug("Create {} integration test case '{}'", moduleName, integrationTestName);

				projectView.clickContextMenuCreateIntegrationTest(projectView.getTestModulesTreeItem().getNode(moduleName));
				new CreateIntegrationTestCaseDialog(bot, integrationBeanType, domainObject, addNestedInvocations).enterData();

				waitForPendingBackgroundJobs();

				projectView.openIntegrationTestCaseInEditor(moduleName, integrationTestName);

				addNestedInvocations = !addNestedInvocations;

				if (IntegrationBeanType.valueOf(moduleName) == IntegrationBeanType.KAFKA)
					break;
			}
		}

		bot.closeAllEditors();
	}

	/**
	 * Edit all integration test cases of the given project
	 * @param project
	 */
	public void editIntegrationTestCases(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var moduleName : projectView.getIntegrationTestModuleNames())
			for (final var domainObject : project.getNonAbstractDomainObjects()) {
				final String integrationTestName = domainObject.getName() + INTEGRATION_TEST_SUFFIX;

				log.debug("Edit {} integration test case '{}'", moduleName, integrationTestName);

				final var integrationTestCaseItem = projectView.getTestModulesTreeItem().getNode(moduleName).expand()
						.getNode(integrationTestName);

				integrationTestCaseItem.contextMenu(MNU_EDIT).click();
				new EditIntegrationTestCaseDialog(bot, integrationTestName).enterData();

				if (IntegrationBeanType.valueOf(moduleName) == IntegrationBeanType.KAFKA)
					break;
			}
	}

	/**
	 * Delete all integration test cases of the given project
	 * @param project
	 */
	public void deleteIntegrationTestCases(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var moduleName : projectView.getIntegrationTestModuleNames())
			for (final var domainObject : project.getNonAbstractDomainObjects()) {
				final String integrationTestName = domainObject.getName() + INTEGRATION_TEST_SUFFIX;

				log.debug("Delete {} integration test case '{}'", moduleName, integrationTestName);

				final var integrationTestCaseItem = projectView.getTestModulesTreeItem().getNode(moduleName).expand()
						.getNode(integrationTestName);

				integrationTestCaseItem.contextMenu(MNU_DELETE).click();
				new DeleteDialog(bot).enterData();

				if (IntegrationBeanType.valueOf(moduleName) == IntegrationBeanType.KAFKA)
					break;
			}
	}

}
