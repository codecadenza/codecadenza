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
package net.codecadenza.eclipse.testing.domain;

import java.util.List;

/**
 * <p>
 * Domain object that contains all necessary information for creating a database connection
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataSource {
	private final String driverName;
	private final String url;
	private final String schemaName;
	private final String username;
	private final String password;
	private final List<String> driverList;
	private final DatabaseVendor databaseVendor;
	private final String databaseName;

	/**
	 * Constructor
	 * @param driverName the name of the JDBC driver
	 * @param url the JDBC URL
	 * @param schemaName the name of the database schema
	 * @param username the user name
	 * @param password the password
	 * @param driverList a list containing fully qualified path names of all external JDBC drivers
	 * @param databaseVendor the database vendor
	 * @param databaseName the name of the database
	 */
	public DataSource(String driverName, String url, String schemaName, String username, String password, List<String> driverList,
			DatabaseVendor databaseVendor, String databaseName) {
		this.driverName = driverName;
		this.url = url;
		this.schemaName = schemaName;
		this.username = username;
		this.password = password;
		this.driverList = driverList;
		this.databaseVendor = databaseVendor;
		this.databaseName = databaseName;
	}

	/**
	 * @return the name of the JDBC driver
	 */
	public String getDriverName() {
		return driverName;
	}

	/**
	 * @return the connection URL
	 */
	public String getUrl() {
		return url;
	}

	/**
	 * @return the database schema name
	 */
	public String getSchemaName() {
		return schemaName;
	}

	/**
	 * @return the user name
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @return the driver list
	 */
	public List<String> getDriverList() {
		return driverList;
	}

	/**
	 * @return the database vendor
	 */
	public DatabaseVendor getDatabaseVendor() {
		return databaseVendor;
	}

	/**
	 * @return the name of the database
	 */
	public String getDatabaseName() {
		return databaseName;
	}

}
