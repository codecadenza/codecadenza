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

import static org.junit.jupiter.api.Assertions.fail;

import java.net.URL;
import net.codecadenza.eclipse.testing.bots.ProjectBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.dialog.project.EditDataSourceDialog;
import net.codecadenza.eclipse.testing.dialog.project.EditDatabasePropertiesDialog;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.DataSource;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import net.codecadenza.eclipse.testing.editor.ReverseEngineeringEditor;
import net.codecadenza.eclipse.testing.util.DBSchemaManager;
import net.codecadenza.eclipse.testing.util.ProjectFactory;
import net.codecadenza.eclipse.testing.view.PackageExplorerView;
import net.codecadenza.eclipse.testing.view.ProblemsView;
import net.codecadenza.eclipse.testing.view.ProjectView;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Test the reverse engineering standard functionality
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class ReverseEngineeringTest {
	private static final String TEST_DATA_PATH = "testdata/";
	private static final String PROJECT_NAME = "config-processor";
	private static final String SQL_FILE_SUFFIX = ".sql";
	private static final TechnologyPlatform TECHNOLOGY_PLATFORM = TechnologyPlatform.JAVA_SE;
	private static final ClientPlatform CLIENT_PLATFORM = ClientPlatform.JAVAFX;
	private static final Logger log = LoggerFactory.getLogger(ReverseEngineeringTest.class);
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
	 * Test reverse engineering
	 * @param databaseVendorName
	 */
	@ParameterizedTest
	@ValueSource(strings = { "POSTGRESQL", "MYSQL" })
	void testReverseEngineering(String databaseVendorName) {
		final var databaseVendor = DatabaseVendor.valueOf(databaseVendorName);
		final var project = ProjectFactory.createProject(PROJECT_NAME, TECHNOLOGY_PLATFORM, CLIENT_PLATFORM, databaseVendor);
		final var projectView = new ProjectView(bot, project);
		final var problemsView = new ProblemsView(bot, project);

		log.info("Start '{}' reverse engineering test", databaseVendorName);

		createDatabaseSchema(project.getDataSource());

		// Create the CodeCadenza project
		new ProjectBot(bot).createProject(project);

		// Wait until the project has been built successfully
		problemsView.waitForNoErrors();

		// The domain object diagram editor must be closed before performing the reverse engineering!
		bot.closeAllEditors();

		// Enter the database schema
		projectView.clickContextMenuEditDataSource();
		new EditDataSourceDialog(bot, project.getDataSource()).enterData();

		// Change the database properties
		projectView.clickContextMenuDBProperties();
		new EditDatabasePropertiesDialog(bot, project.getDataSource()).enterData();

		// Open the reverse engineering editor
		projectView.clickContextMenuReverseEngineering(Project.DEFAULT_NAMESPACE);
		new ReverseEngineeringEditor(bot, project).open();

		// Compare the generated domain model with the domain model of the project
		for (final DomainObject domainObject : project.getDomainObjects()) {
			final var treeItemDomainObj = projectView.getDomainObjectTreeItem(domainObject.getName(), Project.DEFAULT_NAMESPACE);

			domainObject.getAllAttributes().stream().map(Object::toString).forEach(text -> searchItem(treeItemDomainObj, text));
			domainObject.getAssociations().stream().map(Object::toString).forEach(text -> searchItem(treeItemDomainObj, text));
		}

		problemsView.waitForNoErrors();

		// Delete the project in the 'Package Explorer' view
		new PackageExplorerView(bot, project).deleteProject();

		log.info("Finish reverse engineering test");
	}

	/**
	 * Create the database schema and initialize it with the tables defined in the respective file
	 * @param dataSource
	 */
	private void createDatabaseSchema(DataSource dataSource) {
		final var databaseVendor = dataSource.getDatabaseVendor();
		final var schemaFileName = TEST_DATA_PATH + databaseVendor.name().toLowerCase() + SQL_FILE_SUFFIX;
		final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		final URL fileURL = classLoader.getResource(schemaFileName);

		dbSchemaManager = new DBSchemaManager(dataSource);
		dbSchemaManager.createSchema(fileURL);
	}

	/**
	 * Search for the given tree item. The test will fail if the item could not be found!
	 * @param treeItemDomainObj
	 * @param itemText
	 */
	private void searchItem(final SWTBotTreeItem treeItemDomainObj, final String itemText) {
		for (final var item : treeItemDomainObj.getItems())
			if (item.getText().equals(itemText))
				return;

		fail("Tree element '" + itemText + "' could not be found!");
	}

	/**
	 * Delete the database schema
	 */
	@AfterEach
	void deleteDatabaseSchema() {
		dbSchemaManager.deleteSchema();
	}

}
