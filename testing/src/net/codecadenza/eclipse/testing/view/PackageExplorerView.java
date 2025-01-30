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
package net.codecadenza.eclipse.testing.view;

import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class for operations in the 'Package Explorer' view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PackageExplorerView extends AbstractView {
	private static final String FOLDER_MODEL = "model";
	private static final String MNU_BUILD_TEST = "Perform CodeCadenza build test";
	private static final String GUI_PROJECT_SUFFIX = "-gui";
	private static final String SHELL_DELETE_RESOURCES = "Delete Resources";
	private static final String VIEW_TITLE = "Package Explorer";
	private static final Logger log = LoggerFactory.getLogger(PackageExplorerView.class);

	private final Project project;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 */
	public PackageExplorerView(SWTWorkbenchBot bot, Project project) {
		super(bot, VIEW_TITLE);

		this.project = project;
	}

	/**
	 * Open the 'Package Explorer' view and delete the project
	 */
	public void deleteProject() {
		final var packageView = showView();
		final boolean hasDedicatedGUIProject = project.getClientPlatform() == ClientPlatform.RCP
				|| project.getClientPlatform() == ClientPlatform.RAP || project.getClientPlatform() == ClientPlatform.ANGULAR;

		if (hasDedicatedGUIProject && project.getTechnologyPlatform() != TechnologyPlatform.JAVA_SE)
			deleteProject(packageView, project.getName() + GUI_PROJECT_SUFFIX);

		deleteProject(packageView, project.getName());
	}

	private void deleteProject(final SWTBotView packageView, String projectName) {
		final var projectItem = packageView.bot().tree().getTreeItem(projectName);

		log.debug("Delete project '{}'", projectName);

		projectItem.select().contextMenu(MNU_DELETE).click();

		final var deleteDialog = activateShellWithTitle(SHELL_DELETE_RESOURCES);

		bot.checkBox(0).select();
		bot.button(CMD_OK).click();
		bot.waitUntil(Conditions.shellCloses(deleteDialog));
	}

	/**
	 * Open the dialog for performing an internal build test
	 */
	public void openInternalBuildTestDialog() {
		final var packageView = showView();

		packageView.viewMenu().menu(MNU_BUILD_TEST).click();
	}

	/**
	 * Open the domain diagram of the given namespace
	 * @param namespace
	 */
	public void openDomainDiagram(String namespace) {
		final String fileName = project.getDomainDiagramFileName(namespace);
		final var packageView = showView();

		packageView.bot().tree().getTreeItem(project.getName() + "-domain").expand().getNode(FOLDER_MODEL).expand().getNode(fileName)
				.doubleClick();
	}

	/**
	 * Refresh all projects
	 */
	public void refreshProjects() {
		final var packageView = showView();

		for (final var projectItem : packageView.bot().tree().getAllItems())
			projectItem.contextMenu(MNU_REFRESH).click();

		waitForPendingBackgroundJobs();
	}

}
