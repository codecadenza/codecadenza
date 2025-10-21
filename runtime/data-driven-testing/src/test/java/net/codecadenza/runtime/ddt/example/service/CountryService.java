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
package net.codecadenza.runtime.ddt.example.service;

import java.lang.invoke.MethodHandles;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.LockSupport;
import net.codecadenza.runtime.ddt.example.domain.CountryDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Example service for asynchronous handling of country objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CountryService {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String CREATE_TABLE_SQL = "CREATE TABLE IF NOT EXISTS COUNTRIES (CODE VARCHAR(5) PRIMARY KEY, NAME VARCHAR(255))";
	private static final String INSERT_SQL = "INSERT INTO countries (code, name) VALUES (?, ?)";
	private static final String DELETE_SQL = "DELETE FROM countries WHERE code = ?";
	private static final String UPDATE_SQL = "UPDATE countries SET name = ? WHERE code = ?";

	private final String databaseURL;
	private final String userName;
	private final String password;
	private final ExecutorService executor = Executors.newFixedThreadPool(2);

	/**
	 * Constructor
	 * @param databaseURL
	 * @param userName
	 * @param password
	 */
	public CountryService(String databaseURL, String userName, String password) {
		this.databaseURL = databaseURL;
		this.userName = userName;
		this.password = password;

		// Create the table for storing countries
		try (Connection connection = DriverManager.getConnection(databaseURL, userName, password)) {
			final Statement statement = connection.createStatement();
			statement.execute(CREATE_TABLE_SQL);
		}
		catch (final SQLException e) {
			throw new RuntimeException("Failed to initialize database", e);
		}
	}

	/**
	 * Create a new country
	 * @param country
	 */
	public void createCountry(CountryDTO country) {
		CompletableFuture.runAsync(() -> {
			LockSupport.parkNanos(Duration.ofMillis(150).toNanos());
			logger.debug("Create new country '{}'", country.getCode());

			try (Connection connection = DriverManager.getConnection(databaseURL, userName, password);
					PreparedStatement preparedStatement = connection.prepareStatement(INSERT_SQL)) {

				preparedStatement.setString(1, country.getCode());
				preparedStatement.setString(2, country.getName());
				preparedStatement.executeUpdate();
			}
			catch (final SQLException e) {
				logger.error("Failed to create country", e);
			}
		}, executor);
	}

	/**
	 * Update an existing country
	 * @param country
	 */
	public void updateCountry(CountryDTO country) {
		CompletableFuture.runAsync(() -> {
			LockSupport.parkNanos(Duration.ofMillis(100).toNanos());
			logger.debug("Update country '{}'", country.getCode());

			try (Connection connection = DriverManager.getConnection(databaseURL, userName, password);
					PreparedStatement preparedStatement = connection.prepareStatement(UPDATE_SQL)) {

				preparedStatement.setString(1, country.getName());
				preparedStatement.setString(2, country.getCode());
				preparedStatement.executeUpdate();
			}
			catch (final SQLException e) {
				logger.error("Failed to update country", e);
			}
		}, executor);
	}

	/**
	 * Delete a country
	 * @param code
	 */
	public void deleteCountry(String code) {
		CompletableFuture.runAsync(() -> {
			LockSupport.parkNanos(Duration.ofMillis(50).toNanos());
			logger.debug("Delete country '{}'", code);

			try (Connection connection = DriverManager.getConnection(databaseURL, userName, password);
					PreparedStatement preparedStatement = connection.prepareStatement(DELETE_SQL)) {

				preparedStatement.setString(1, code);
				preparedStatement.executeUpdate();
			}
			catch (final SQLException e) {
				logger.error("Failed to delete country", e);
			}
		}, executor);
	}

	/**
	 * Shutdown the {@link ExecutorService}
	 */
	public void shutdown() {
		executor.shutdown();
	}

}
