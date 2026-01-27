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
package net.codecadenza.eclipse.testing.util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;
import net.codecadenza.eclipse.testing.domain.DataSource;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import org.eclipse.core.runtime.FileLocator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * This class is responsible for managing database schemas
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBSchemaManager {
	private static final String POSTGRES_MANAGEMENT_URL = "jdbc:postgresql://localhost:5432/postgres";
	private static final Logger log = LoggerFactory.getLogger(DBSchemaManager.class);

	private final String username;
	private final String password;
	private final String driverName;
	private final String schemaName;
	private final List<String> driverList;
	private final DatabaseVendor databaseVendor;
	private final String databaseName;
	private String url;
	private Driver driver;
	private URLClassLoader classLoader;

	/**
	 * Constructor
	 * @param dataSource the project's data source
	 */
	public DBSchemaManager(DataSource dataSource) {
		this.driverName = dataSource.getDriverName();
		this.url = dataSource.getUrl();
		this.schemaName = dataSource.getSchemaName();
		this.username = dataSource.getUsername();
		this.password = dataSource.getPassword();
		this.driverList = dataSource.getDriverList();
		this.databaseVendor = dataSource.getDatabaseVendor();
		this.databaseName = dataSource.getDatabaseName();
	}

	/**
	 * Create the database schema
	 * @throws IllegalStateException if the database schema could not be created
	 */
	public void createSchema() {
		if (databaseVendor == DatabaseVendor.POSTGRESQL) {
			final var originalURL = url;
			url = POSTGRES_MANAGEMENT_URL;

			log.debug("Create database '{}'", databaseName);

			executeStatement("create database " + databaseName + ";");

			url = originalURL;
		}

		log.debug("Create database schema '{}'", schemaName);

		executeStatement("create schema " + schemaName + ";");
	}

	/**
	 * Create the database schema and execute all statements that are contained in the given file
	 * @param schemaFileURL
	 * @throws IllegalStateException if the statements either could not be read or could not be executed
	 */
	public void createSchema(URL schemaFileURL) {
		createSchema();

		try {
			final var schemaFilePath = Path.of(FileLocator.resolve(schemaFileURL).toURI());
			final List<String> lines = Files.readAllLines(schemaFilePath);

			try (final var connection = getConnection()) {
				if (databaseVendor == DatabaseVendor.MYSQL)
					executeStatement(connection, "use " + schemaName + ";");
				else
					executeStatement(connection, "set search_path to " + schemaName + ";");

				for (final var line : lines)
					executeStatement(connection, line);
			}
		}
		catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		finally {
			closeClassLoader();
		}
	}

	/**
	 * Delete the database schema
	 */
	public void deleteSchema() {
		try {
			if (databaseVendor == DatabaseVendor.MYSQL) {
				log.debug("Delete database schema '{}'", schemaName);

				executeStatement("drop database " + schemaName + ";");
			}
			else {
				url = POSTGRES_MANAGEMENT_URL;
				log.debug("Delete database '{}'", databaseName);

				executeStatement("drop database " + databaseName + ";");
			}
		}
		catch (final Exception e) {
			log.error("Error while deleting database schema '{}'! Message: {}", schemaName, e.getMessage());
		}
	}

	/**
	 * Execute the given SQL statement
	 * @param statement
	 * @throws IllegalStateException if the statement could not be executed
	 */
	private void executeStatement(String statement) {
		try (final var connection = getConnection()) {
			executeStatement(connection, statement);
		}
		catch (final SQLException e) {
			log.debug("Error while executing statement '{}'", statement, e);

			throw new IllegalStateException(e);
		}
		finally {
			closeClassLoader();
		}
	}

	/**
	 * Execute the given SQL statement
	 * @param connection
	 * @param statement
	 * @throws SQLException if the statement execution has failed
	 */
	private void executeStatement(Connection connection, String statement) throws SQLException {
		log.debug("Execute statement: {}", statement);

		try (final var preparedStatement = connection.prepareStatement(statement)) {
			preparedStatement.execute();
		}
		catch (final SQLException e) {
			log.debug("Error while executing statement '{}'", statement, e);

			throw new IllegalStateException(e);
		}
	}

	/**
	 * Create a JDBC connection
	 * @return a new JDBC connection
	 * @throws IllegalArgumentException if either the JDBC URL, the user name or the password is missing
	 * @throws SQLException if a JDBC connection could not be created
	 */
	private Connection getConnection() throws SQLException {
		initJDBCDriver();

		if (url == null || url.isEmpty())
			throw new IllegalArgumentException("A connection URL is required!");

		if (username == null)
			throw new IllegalArgumentException("A user name is requried!");

		if (password == null)
			throw new IllegalArgumentException("A password is required!");

		final var connectionProps = new Properties();
		connectionProps.setProperty("user", username);
		connectionProps.setProperty("password", password);

		return driver.connect(url, connectionProps);
	}

	/**
	 * Initialize the JDBC driver
	 * @throws IllegalArgumentException if either the driver name or the list of JDBC drivers is missing
	 * @throws IllegalStateException if the initialization has failed
	 */
	private void initJDBCDriver() {
		if (driverName == null || driverName.isEmpty())
			throw new IllegalArgumentException("The database connection cannot be initialized without a valid JDBC driver name!");

		if (driverList == null || driverList.isEmpty())
			throw new IllegalArgumentException("The database connection cannot be initialized without a JDBC driver!");

		try {
			final var urls = new URL[driverList.size()];
			int i = 0;

			for (final String path : driverList) {
				final var driverFile = new File(path);

				if (!driverFile.exists())
					throw new IllegalStateException("The JDBC driver '" + driverFile.getName() + "' could not be found!");

				if (!driverFile.isFile())
					throw new IllegalStateException("The JDBC driver '" + driverFile.getName() + "' is not a file!");

				urls[i++] = driverFile.toURI().toURL();
			}

			classLoader = URLClassLoader.newInstance(urls);

			driver = (Driver) classLoader.loadClass(driverName).getDeclaredConstructor().newInstance();
		}
		catch (final Exception ex) {
			throw new IllegalStateException(ex);
		}
	}

	/**
	 * Close the {@link URLClassLoader}
	 */
	private void closeClassLoader() {
		if (classLoader != null)
			try {
				classLoader.close();
			}
			catch (final IOException _) {
				// Ignored!
			}
	}

}
