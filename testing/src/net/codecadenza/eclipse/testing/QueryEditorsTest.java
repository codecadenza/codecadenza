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

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.testing.bots.DomainBot;
import net.codecadenza.eclipse.testing.bots.ProjectBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.dialog.project.EditDataSourceDialog;
import net.codecadenza.eclipse.testing.dialog.project.EditDatabasePropertiesDialog;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.DataSource;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import net.codecadenza.eclipse.testing.domain.ProjectType;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import net.codecadenza.eclipse.testing.editor.DBSyncEditor;
import net.codecadenza.eclipse.testing.editor.EditorQuery;
import net.codecadenza.eclipse.testing.editor.JPAQueryEditor;
import net.codecadenza.eclipse.testing.editor.SQLQueryEditor;
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
 * Test the functionality of the SQL and the JPA query editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class QueryEditorsTest {
	private static final String PROJECT_NAME = "config-processor";
	private static final TechnologyPlatform TECHNOLOGY_PLATFORM = TechnologyPlatform.JAVA_SE;
	private static final ClientPlatform CLIENT_PLATFORM = ClientPlatform.JAVAFX;
	private static final ProjectType PROJECT_TYPE = ProjectType.ENUM;
	private static final Logger log = LoggerFactory.getLogger(QueryEditorsTest.class);
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
	 * Test the functionality of the SQL and the JPA query editors
	 * @param databaseVendorName
	 */
	@ParameterizedTest
	@ValueSource(strings = { "POSTGRESQL", "MYSQL" })
	void testQueryEditors(String databaseVendorName) {
		final var databaseVendor = DatabaseVendor.valueOf(databaseVendorName);
		final var projectName = PROJECT_NAME + "-" + databaseVendorName.toLowerCase();
		final var project = ProjectFactory.createProject(projectName, PROJECT_TYPE, TECHNOLOGY_PLATFORM, CLIENT_PLATFORM,
				databaseVendor);
		final var projectView = new ProjectView(bot, project);
		final var domainObject = project.getDomainObjects().stream().findFirst().orElseThrow();

		log.info("Start '{}' database editor test", databaseVendorName);

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

		// Open the SQL query editor and execute some queries
		final List<EditorQuery> sqlQueries = createSQLQueries(project.getDataSource());
		final String tableName = project.getDataSource().getSchemaName() + "." + domainObject.getTableName();

		projectView.openTableInSQLEditor(tableName);
		new SQLQueryEditor(bot).executQueries(sqlQueries);

		// Open the JPA query editor and execute some queries
		final var jpaDefaultSelect = new EditorQuery(null, true, true);
		final var jpaSelect = new EditorQuery("select a from Application a where a.id > 0", true, true);
		final var jpaSelectFields = new EditorQuery("select a.id, a.name, a.status from Application a", true, true);
		final var jpaInvalidSelect = new EditorQuery("select invalid from Application a", false, true);
		final var jpqQueries = List.of(jpaDefaultSelect, jpaSelect, jpaSelectFields, jpaInvalidSelect);

		projectView.openDomainObjectInQueryEditor(domainObject.getName(), domainObject.getNamespace());
		new JPAQueryEditor(bot).executQueries(jpqQueries);

		// Delete the project in the 'Package Explorer' view
		new PackageExplorerView(bot, project).deleteProject();

		log.info("Finish database editor test");
	}

	/**
	 * @param dataSource
	 * @return a list with SQL queries
	 */
	private List<EditorQuery> createSQLQueries(DataSource dataSource) {
		final var queryList = new ArrayList<EditorQuery>();
		final DatabaseVendor databaseVendor = dataSource.getDatabaseVendor();
		final StringBuilder insertStatement = new StringBuilder("insert into ");
		final String tableName;

		if (databaseVendor == DatabaseVendor.MYSQL) {
			tableName = "application_tab";

			insertStatement.append(tableName);
			insertStatement.append(" (name, status) values ('Test application', 'NEW')");
		}
		else {
			tableName = dataSource.getSchemaName() + ".application_tab";

			insertStatement.append(tableName);
			insertStatement.append(" (id, name, status) values (nextval('");
			insertStatement.append(dataSource.getSchemaName());
			insertStatement.append(".application_seq'), 'Test application', 'NEW')");
		}

		queryList.add(new EditorQuery(insertStatement.toString(), true, false));
		queryList.add(new EditorQuery("select * from " + tableName, true, true));
		queryList.add(new EditorQuery("delete from " + tableName, true, false));
		queryList.add(new EditorQuery(insertStatement.toString(), true, false));
		queryList.add(new EditorQuery("select * from non_existing", false, true));

		return queryList;
	}

	/**
	 * Delete the database schema
	 */
	@AfterEach
	void deleteDatabaseSchema() {
		dbSchemaManager.deleteSchema();
	}

}
