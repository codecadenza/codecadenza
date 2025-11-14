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
package net.codecadenza.runtime.ddt.service.preparation.imp;

import java.lang.invoke.MethodHandles;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import net.codecadenza.runtime.ddt.service.preparation.IStatementProcessor;
import net.codecadenza.runtime.ddt.service.preparation.StatementProcessorProperties;
import net.codecadenza.runtime.service.ServiceProcessingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Implementation of a statement processor that is can be used to either initialize or clean the database
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JDBCStatementProcessor implements IStatementProcessor {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final String databaseURL;
	private final String userName;
	private final String password;

	/**
	 * Constructor
	 * @param properties
	 */
	public JDBCStatementProcessor(StatementProcessorProperties properties) {
		this.databaseURL = properties.getResourceURL();
		this.userName = properties.getUserName();
		this.password = properties.getPassword();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.preparation.IStatementProcessor#executeStatements(java.util.List)
	 */
	@Override
	public void executeStatements(List<String> statements) {
		try (final Connection connection = DriverManager.getConnection(databaseURL, userName, password)) {
			for (final String statement : statements)
				try (final PreparedStatement preparedStatement = connection.prepareStatement(statement)) {
					if (logger.isDebugEnabled())
						logger.debug("Execute SQL statement: '{}'", statement.trim());

					preparedStatement.executeUpdate();
				}
		}
		catch (final SQLException e) {
			throw new ServiceProcessingException("Error while processing SQL statement", e);
		}
	}

}
