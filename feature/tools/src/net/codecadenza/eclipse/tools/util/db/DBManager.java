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
package net.codecadenza.eclipse.tools.util.db;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;

/**
 * <p>
 * This class is responsible for creating connections to relational databases via JDBC
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBManager implements AutoCloseable {
	private final String username;
	private final String password;
	private final String driverName;
	private final String url;
	private final List<String> driverList;
	private Driver driver;
	private Connection connection;
	private URLClassLoader classLoader;

	/**
	 * Constructor
	 * @param driverName the JDBC driver
	 * @param url the JDBC URL
	 * @param username the JDBC user
	 * @param password the JDBC password
	 * @param driverList a list containing fully qualified path names of all external JDBC drivers
	 */
	public DBManager(String driverName, String url, String username, String password, List<String> driverList) {
		this.driverName = driverName;
		this.url = url;
		this.username = username;
		this.password = password;
		this.driverList = driverList;
	}

	/**
	 * Create a new database manager instance from the provided project
	 * @param project
	 */
	public DBManager(Project project) {
		this(project.getDataSource());
	}

	/**
	 * Create a new database manager instance from the provided data source
	 * @param ds
	 */
	public DBManager(Datasource ds) {
		this(ds.getDriverName(), ds.getConnectionURL(), ds.getUserName(), ds.getPassword(), ds.getDriverList());
	}

	/**
	 * Initialize the JDBC driver
	 * @throws DBManagerException if the initialization has failed
	 */
	private void initJDBCDriver() throws DBManagerException {
		if (driverName == null || driverName.isEmpty())
			throw new DBManagerException("The database connection cannot be initialized without a valid JDBC driver name!");

		if (driverList == null || driverList.isEmpty())
			throw new DBManagerException("The database connection cannot be initialized without a JDBC driver!");

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
			throw new DBManagerException(ex);
		}
	}

	/**
	 * Create a JDBC connection
	 * @return a new JDBC connection
	 * @throws DBManagerException if a JDBC connection could not be created
	 */
	public synchronized Connection getConnection() throws DBManagerException {
		if (connection != null) {
			try {
				// It might be the case that the connection was already closed! If so, we create a new connection!
				if (!connection.isClosed())
					return connection;
			}
			catch (final Exception _) {
				// This exception will be ignored!
			}
		}

		initJDBCDriver();

		if (url == null || url.isEmpty())
			throw new DBManagerException("A connection URL is required!");

		if (username == null)
			throw new DBManagerException("A user name is requried!");

		if (password == null)
			throw new DBManagerException("A password is required!");

		try {
			final var connectionProps = new Properties();
			connectionProps.setProperty("user", username);
			connectionProps.setProperty("password", password);

			connection = driver.connect(url, connectionProps);

			return connection;
		}
		catch (final SQLException ex) {
			throw new DBManagerException(ex);
		}
	}

	/**
	 * Test the database connection
	 * @return an error message if the connection could not be established
	 */
	public synchronized String testConnection() {
		try {
			// Create a connection
			final Connection con = getConnection();

			// Get the catalog in order to test connection
			if (con != null)
				con.getCatalog();
			else
				return "The database connection could not be established!";
		}
		catch (final Exception | NoClassDefFoundError e) {
			CodeCadenzaToolsPlugin.getInstance().logInfo(null, e);
			return "The database connection could not be established! Message: " + e.getMessage();
		}

		return "";
	}

	/**
	 * Start a new transaction
	 * @return an open connection
	 * @throws DBManagerException if starting a new transaction has failed
	 */
	public synchronized Connection beginTransaction() throws DBManagerException {
		final Connection con = getConnection();

		if (con == null)
			return null;

		try {
			con.setAutoCommit(false);
		}
		catch (final SQLException ex) {
			throw new DBManagerException(ex);
		}

		return con;
	}

	/**
	 * Finish a transaction
	 * @param commit a flag to indicate if the current transaction should be committed or not.
	 * @throws DBManagerException if the commit or the rollback operation has failed
	 */
	public synchronized void endTransaction(boolean commit) throws DBManagerException {
		final Connection con = getConnection();

		if (con == null)
			return;

		try {
			if (commit)
				con.commit();
			else
				con.rollback();
		}
		catch (final SQLException ex) {
			throw new DBManagerException(ex);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.AutoCloseable#close()
	 */
	@Override
	public void close() throws Exception {
		if (connection != null)
			try {
				if (!connection.getAutoCommit())
					connection.rollback();

				connection.close();
				connection = null;
			}
			catch (final SQLException _) {
				// This exception will be ignored!
			}

		if (classLoader != null)
			classLoader.close();
	}

}
