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

import net.codecadenza.eclipse.testing.bots.DomainBot;
import net.codecadenza.eclipse.testing.bots.ProjectBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.dialog.project.EditDataSourceDialog;
import net.codecadenza.eclipse.testing.dialog.project.EditDatabasePropertiesDialog;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import net.codecadenza.eclipse.testing.editor.DBSyncEditor;
import net.codecadenza.eclipse.testing.util.DBSchemaManager;
import net.codecadenza.eclipse.testing.util.ProjectFactory;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProblemsView;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Test the functionality of the database synchronization editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class DBSyncEditorTest {
	private static final String PROJECT_NAME = "config-processor";
	private static final TechnologyPlatform TECHNOLOGY_PLATFORM = TechnologyPlatform.JAVA_SE;
	private static final ClientPlatform CLIENT_PLATFORM = ClientPlatform.RCP;
	private static final Logger log = LoggerFactory.getLogger(DBSyncEditorTest.class);
	private static final SWTGefBot bot = new SWTGefBot();

	private DBSchemaManager dbSchemaManager;

	/**
	 * Initialize the test
	 */
	@BeforeAll
	static void initTest() {
		new WorkspaceManagerBot(bot).prepareWorkspace();
	}

	/**
	 * Test the functionality of the database synchronization editor
	 * @param databaseVendorName
	 */
	@ParameterizedTest
	@ValueSource(strings = { "POSTGRESQL", "MYSQL" })
	void testDBSyncEditor(String databaseVendorName) {
		final var databaseVendor = DatabaseVendor.valueOf(databaseVendorName);
		final var project = ProjectFactory.createProject(PROJECT_NAME, TECHNOLOGY_PLATFORM, CLIENT_PLATFORM, databaseVendor);
		final var projectView = new ProjectView(bot, project);

		log.info("Start '{}' database synchronization editor test", databaseVendorName);

		// Create the database schema
		dbSchemaManager = new DBSchemaManager(project.getDataSource());
		dbSchemaManager.createSchema();

		// Create the CodeCadenza project
		new ProjectBot(bot).createProject(project);

		// Do not try to draw the domain model until the project is being built!
		new ProblemsView(bot, project).waitForNoErrors();

		new DomainBot(bot).drawDomainModel(project);

		// Enter the database schema
		projectView.clickContextMenuEditDataSource();
		new EditDataSourceDialog(bot, project.getDataSource()).enterData();

		// Change the database properties
		projectView.clickContextMenuDBProperties();
		new EditDatabasePropertiesDialog(bot, project.getDataSource()).enterData();

		// Perform the database synchronization
		projectView.clickContextMenuDBSync();
		new DBSyncEditor(bot).executeSync();

		// Delete the project in the 'Package Explorer' view
		new PackageExplorerView(bot, project).deleteProject();

		log.info("Finish database synchronization editor test");
	}

	/**
	 * Delete the database schema
	 */
	@AfterEach
	void deleteDatabaseSchema() {
		dbSchemaManager.deleteSchema();
	}

}
