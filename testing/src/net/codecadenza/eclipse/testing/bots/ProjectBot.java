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

import net.codecadenza.eclipse.testing.dialog.project.CreateProjectWizard;
import net.codecadenza.eclipse.testing.dialog.project.EditProjectDialog;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Bot for testing projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectBot extends AbstractBot {
	private static final String NODE_CODECADENZA_PROJECT = "CodeCadenza project";
	private static final String TREE_ITEM_CODECADENZA = "CodeCadenza";
	private static final String SHELL_NEW_PROJECT_WIZARD = "New";
	private static final String MNU_OTHER = "Other...";
	private static final String MNU_FILE = "File";
	private static final String MNU_NEW = "New";
	private static final Logger log = LoggerFactory.getLogger(ProjectBot.class);

	/**
	 * Constructor
	 * @param bot
	 */
	public ProjectBot(SWTWorkbenchBot bot) {
		super(bot);
	}

	/**
	 * Create a new project
	 * @param project
	 */
	public void createProject(Project project) {
		log.debug("Create project '{}'", project.getName());

		// Open the wizard for creating a new project
		clickMenuItem(bot, MNU_FILE, MNU_NEW, MNU_OTHER);

		activateShellWithTitle(SHELL_NEW_PROJECT_WIZARD);

		bot.tree().getTreeItem(TREE_ITEM_CODECADENZA).expand().getNode(NODE_CODECADENZA_PROJECT).select();
		bot.button(CMD_NEXT).click();

		// Waiting here to avoid the test freezing!
		bot.sleep(500);

		new CreateProjectWizard(bot, project).enterData();

		waitForPendingBackgroundJobs();
	}

	/**
	 * Create an additional namespace
	 * @param project
	 */
	public void createAdditionalNamespace(Project project) {
		final var projectView = new ProjectView(bot, project);
		projectView.clickContextMenuEditProject();

		final var editProjectDialog = new EditProjectDialog(bot);
		editProjectDialog.enterData();

		waitForPendingBackgroundJobs();
	}

}
