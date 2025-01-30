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
package net.codecadenza.eclipse.testing.dialog.buildtest;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for performing internal build tests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class InternalBuildTestDialog extends AbstractDialog {
	private static final int[] ANGULAR_PROJECT_ROW_INDEXES = { 0, 10, 19 };
	private static final String CMD_TOGGLE = "Toggle";
	private static final String LBL_CLIENT_TECHNOLOGY = "Select the client technology:";
	private static final String LBL_SELENIUM = "Selenium";
	private static final String SHELL_TITLE = "Select projects to be (re)created";
	private static final Logger log = LoggerFactory.getLogger(InternalBuildTestDialog.class);

	private final ClientPlatform clientPlatform;
	private final int rowIndex;
	private Project project;
	private boolean buildProject = true;

	/**
	 * Constructor
	 * @param bot
	 * @param clientPlatform
	 * @param rowIndex
	 */
	public InternalBuildTestDialog(SWTWorkbenchBot bot, ClientPlatform clientPlatform, int rowIndex) {
		super(bot, SHELL_TITLE);

		this.clientPlatform = clientPlatform;
		this.rowIndex = rowIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		bot.comboBoxWithLabel(LBL_CLIENT_TECHNOLOGY).setSelection(clientPlatform.name());

		if (rowIndex + 1 > bot.table().rowCount()) {
			bot.button(CMD_CANCEL).click();
			return;
		}

		if (clientPlatform != ClientPlatform.NONE) {
			// Do not create integration beans for all client platforms!
			bot.button(CMD_TOGGLE).click();

			if (clientPlatform == ClientPlatform.ANGULAR || clientPlatform == ClientPlatform.JSF_PRIMEFACES
					|| clientPlatform == ClientPlatform.VAADIN)
				bot.checkBox(LBL_SELENIUM).click();
		}

		if (clientPlatform == ClientPlatform.ANGULAR) {
			// Reduce the test duration by limiting the number of Angular projects to be built
			buildProject = false;

			for (final int angularProjectRowIndex : ANGULAR_PROJECT_ROW_INDEXES)
				if (angularProjectRowIndex == rowIndex) {
					buildProject = true;
					break;
				}
		}

		final var table = bot.table();
		final var projectName = table.getTableItem(rowIndex).getText(0).toLowerCase();
		final var platform = TechnologyPlatform.valueOf(table.getTableItem(rowIndex).getText(1));
		project = new Project(projectName, platform, clientPlatform);

		if (buildProject) {
			log.debug("Build project '{}'", projectName);

			table.getTableItem(rowIndex).check();
		}
		else
			log.debug("Skipping project '{}'", projectName);

		bot.button(CMD_OK).click();
	}

	/**
	 * Get the project that has been generated
	 * @return the generated project
	 */
	public Project getProject() {
		return project;
	}

	/**
	 * @return true if the project generation has been skipped
	 */
	public boolean isProjectSkipped() {
		return !buildProject;
	}

}
