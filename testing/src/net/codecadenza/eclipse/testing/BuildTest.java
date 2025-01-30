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

import net.codecadenza.eclipse.testing.bots.AbstractBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.dialog.RAPRuntimePreferencesDialog;
import net.codecadenza.eclipse.testing.dialog.buildtest.InternalBuildTestDialog;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.util.OperatingSystem;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProblemsView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Run the internal build tests to check if the generated code compiles at least
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class BuildTest {
	private static final String MNU_ECLIPSE = "Eclipse";
	private static final String MNU_PREFERENCES = "Preferences";
	private static final String MNU_WINDOW = "Window";
	private static final Logger log = LoggerFactory.getLogger(BuildTest.class);
	private static final SWTGefBot bot = new SWTGefBot();

	/**
	 * Initialize the test
	 */
	@BeforeAll
	static void initTest() {
		new WorkspaceManagerBot(bot).prepareWorkspace();
	}

	/**
	 * Run the internal build tests
	 * @param clientPlatformName
	 */
	@ParameterizedTest
	@CsvSource({ "NONE", "ANGULAR", "JSF_PRIMEFACES", "VAADIN", "JAVAFX", "RCP", "RAP", "SWING" })
	void performProjectBuildTest(String clientPlatformName) {
		final var clientPlatform = ClientPlatform.valueOf(clientPlatformName);
		final var packageExplorerView = new PackageExplorerView(bot, null);
		int rowIndex = 0;

		log.info("Start build test for client technology '{}'", clientPlatformName);

		if (clientPlatform == ClientPlatform.RAP) {
			if (OperatingSystem.isMacOS())
				AbstractBot.clickMenuItem(bot, MNU_ECLIPSE, MNU_PREFERENCES);
			else
				AbstractBot.clickMenuItem(bot, MNU_WINDOW, MNU_PREFERENCES);

			new RAPRuntimePreferencesDialog(bot).enterData();
		}

		while (true) {
			packageExplorerView.openInternalBuildTestDialog();

			final var buildTestDialog = new InternalBuildTestDialog(bot, clientPlatform, rowIndex);
			buildTestDialog.enterData();

			final Project project = buildTestDialog.getProject();

			if (project == null)
				break;

			rowIndex++;

			if (buildTestDialog.isProjectSkipped())
				continue;

			buildTestDialog.waitForPendingBackgroundJobs();

			// Wait until all compilation errors are gone
			new ProblemsView(bot, project).waitForNoErrors();

			// Delete the project in the 'Package Explorer' view
			new PackageExplorerView(bot, project).deleteProject();
		}

		log.info("Finish build test for client technology '{}'", clientPlatformName);
	}

}
