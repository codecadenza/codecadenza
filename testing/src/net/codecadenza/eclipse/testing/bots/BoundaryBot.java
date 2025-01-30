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
import net.codecadenza.eclipse.testing.dialog.boundary.EditBoundaryBeanDialog;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing boundary beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryBot extends AbstractBot {
	private static final String BOUNDARY_SERVICE_SUFFIX = "BoundaryService";
	private static final String BOUNDARY_SERVICE_BEAN_SUFFIX = BOUNDARY_SERVICE_SUFFIX + "Bean";
	private static final Logger log = LoggerFactory.getLogger(BoundaryBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public BoundaryBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Open the source code for all boundary beans of the given project
	 * @param project
	 */
	public void openBoundaryBeans(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			final var domainObjectName = domainObject.getName();
			final var boundaryName = getBoundaryName(project, domainObjectName);

			log.debug("Open boundary bean '{}' in editor", boundaryName);

			projectView.openBoundaryBeanInEditor(boundaryName, domainObject.getNamespace());
		}

		bot.closeAllEditors();
	}

	/**
	 * Edit all boundary beans of the given project
	 * @param project
	 */
	public void editBoundaryBeans(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			final var domainObjectName = domainObject.getName();
			final var boundaryName = getBoundaryName(project, domainObjectName);

			log.debug("Edit boundary bean '{}'", boundaryName);

			projectView.clickContextMenuEditBoundaryBean(boundaryName, domainObject.getNamespace());
			new EditBoundaryBeanDialog(bot).enterData();
		}
	}

	/**
	 * Delete all boundary beans of the given project
	 * @param project
	 */
	public void deleteBoundaryBeans(Project project) {
		final var projectView = new ProjectView(bot, project);

		for (final var domainObject : project.getNonAbstractDomainObjects()) {
			final var domainObjectName = domainObject.getName();
			final var boundaryName = getBoundaryName(project, domainObjectName);

			log.debug("Delete boundary bean '{}'", boundaryName);

			projectView.clickContextMenuDeleteBoundaryBean(boundaryName, domainObject.getNamespace());
			new DeleteDialog(bot).enterData();
		}
	}

	private String getBoundaryName(Project project, String domainObjectName) {
		final var clientPlatform = project.getClientPlatform();

		if (clientPlatform == ClientPlatform.ANGULAR || clientPlatform == ClientPlatform.JSF_PRIMEFACES
				|| clientPlatform == ClientPlatform.VAADIN || clientPlatform == ClientPlatform.NONE)
			return domainObjectName + BOUNDARY_SERVICE_SUFFIX;

		return domainObjectName + BOUNDARY_SERVICE_BEAN_SUFFIX;
	}

}
