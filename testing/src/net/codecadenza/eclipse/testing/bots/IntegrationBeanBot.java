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

import java.util.Arrays;
import java.util.List;
import net.codecadenza.eclipse.testing.dialog.DeleteDialog;
import net.codecadenza.eclipse.testing.dialog.integration.CreateIntegrationBeanDialog;
import net.codecadenza.eclipse.testing.dialog.integration.EditIntegrationBeanDialog;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.IntegrationBeanType;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing integration beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationBeanBot extends AbstractBot {

	private static final String INTEGRATION_SERVICE_SUFFIX = "ServiceBean";
	private static final String KAFKA_CONSUMER_SUFFIX = "Consumer";
	private static final Logger log = LoggerFactory.getLogger(IntegrationBeanBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public IntegrationBeanBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Create integration beans for all domain objects of the given project
	 * @param project
	 */
	public void createIntegrationBeans(Project project) {
		final var projectView = new ProjectView(bot, project);
		final List<String> packageNames = Arrays.asList(projectView.getIntegrationBeansTreeItem().getItems()).stream()
				.map(SWTBotTreeItem::getText).toList();

		for (final var packageName : packageNames) {
			final var integrationBeanType = IntegrationBeanType.fromPackageName(packageName);

			for (final var domainObject : project.getNonAbstractDomainObjects()) {
				final var integrationBeanName = createIntegrationBeanName(integrationBeanType, domainObject);

				log.debug("Create integration bean '{}'", integrationBeanName);

				projectView.clickContextMenuCreateIntegrationBean(projectView.getIntegrationBeansTreeItem().getNode(packageName));
				new CreateIntegrationBeanDialog(bot, domainObject).enterData();

				waitForPendingBackgroundJobs();

				projectView.openIntegrationBeanInEditor(integrationBeanName);

				if (integrationBeanType == IntegrationBeanType.KAFKA) {
					// Just create a single Kafka integration bean as the Avro class generation takes too long time!
					break;
				}
			}
		}

		bot.closeAllEditors();
	}

	/**
	 * Edit all integration beans of the given project
	 * @param project
	 */
	public void editIntegrationBeans(Project project) {
		final var projectView = new ProjectView(bot, project);
		final List<String> packageNames = Arrays.asList(projectView.getIntegrationBeansTreeItem().getItems()).stream()
				.map(SWTBotTreeItem::getText).toList();

		for (final var packageName : packageNames) {
			final var integrationBeanType = IntegrationBeanType.fromPackageName(packageName);

			for (final var domainObject : project.getNonAbstractDomainObjects()) {
				final var integrationBeanName = createIntegrationBeanName(integrationBeanType, domainObject);

				log.debug("Edit integration bean '{}'", integrationBeanName);

				final var integrationBeanTreeItem = projectView.getIntegrationBeansTreeItem().getNode(packageName).expand()
						.getNode(integrationBeanName);

				integrationBeanTreeItem.contextMenu(MNU_EDIT).click();
				new EditIntegrationBeanDialog(bot, integrationBeanType).enterData();

				if (integrationBeanType == IntegrationBeanType.KAFKA)
					break;
			}
		}
	}

	/**
	 * Delete all integration beans of the given project
	 * @param project
	 */
	public void deleteIntegrationBeans(Project project) {
		final var projectView = new ProjectView(bot, project);
		final List<String> packageNames = Arrays.asList(projectView.getIntegrationBeansTreeItem().getItems()).stream()
				.map(SWTBotTreeItem::getText).toList();

		for (final var packageName : packageNames) {
			final var integrationBeanType = IntegrationBeanType.fromPackageName(packageName);

			for (final var domainObject : project.getNonAbstractDomainObjects()) {
				final var integrationBeanName = createIntegrationBeanName(integrationBeanType, domainObject);

				log.debug("Delete integration bean '{}'", integrationBeanName);

				final var integrationBeanTreeItem = projectView.getIntegrationBeansTreeItem().getNode(packageName).expand()
						.getNode(integrationBeanName);

				integrationBeanTreeItem.contextMenu(MNU_DELETE).click();
				new DeleteDialog(bot).enterData();

				if (integrationBeanType == IntegrationBeanType.KAFKA)
					break;
			}
		}
	}

	private String createIntegrationBeanName(final IntegrationBeanType type, final DomainObject domainObject) {
		var integrationBeanName = domainObject.getName() + type.name();

		if (type == IntegrationBeanType.KAFKA || type == IntegrationBeanType.JMS)
			integrationBeanName += KAFKA_CONSUMER_SUFFIX;
		else
			integrationBeanName += INTEGRATION_SERVICE_SUFFIX;

		return integrationBeanName;
	}

}
