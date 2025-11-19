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
package net.codecadenza.runtime.ddt.service.completion.imp;

import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.LockSupport;
import net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerProperties;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionTimeoutException;
import net.codecadenza.runtime.service.ServiceProcessingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Implementation of an invocation completion handler for methods under test that save data in a relational database
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DatabaseInvocationCompletionHandler implements IInvocationCompletionHandler {
	private static final long DEFAULT_TIMEOUT_MILLIS = 1000;
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final String databaseURL;
	private final String userName;
	private final String password;
	private final long defaultTimeoutMillis;

	/**
	 * Constructor
	 * @param properties
	 */
	public DatabaseInvocationCompletionHandler(InvocationCompletionHandlerProperties properties) {
		this.databaseURL = properties.getResourceURL();
		this.userName = properties.getUserName();
		this.password = properties.getPassword();

		final long timeout = properties.getDefaultTimeout();

		if (timeout == 0)
			this.defaultTimeoutMillis = DEFAULT_TIMEOUT_MILLIS;
		else
			this.defaultTimeoutMillis = timeout;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler#waitForFinish(java.lang.String,
	 * java.lang.Object[])
	 */
	@Override
	public void waitForFinish(String statement, Object... filterCriteria) {
		waitForResult(statement, Boolean.class, Duration.of(defaultTimeoutMillis, ChronoUnit.MILLIS), true, filterCriteria);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler#waitForFinish(java.lang.String,
	 * java.lang.Class, java.lang.Object[])
	 */
	@Override
	public <T> T waitForFinish(String statement, Class<T> type, Object... filterCriteria) {
		return waitForResult(statement, type, Duration.of(defaultTimeoutMillis, ChronoUnit.MILLIS), false, filterCriteria);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler#waitForFinish(java.lang.String,
	 * java.time.Duration, java.lang.Object[])
	 */
	@Override
	public void waitForFinish(String statement, Duration completionTimeout, Object... filterCriteria) {
		waitForResult(statement, Boolean.class, completionTimeout, true, filterCriteria);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler#waitForFinish(java.lang.String,
	 * java.lang.Class, java.time.Duration, java.lang.Object[])
	 */
	@Override
	public <T> T waitForFinish(String statement, Class<T> type, Duration completionTimeout, Object... filterCriteria) {
		return waitForResult(statement, type, completionTimeout, false, filterCriteria);
	}

	/**
	 * Execute the given statement and wait until a return value could be found
	 * @param <T> the expected return type
	 * @param statement the statement that should be executed
	 * @param type the class that represents the type of the value that should be returned
	 * @param skipReturnValue flag that controls if the value should be ignored
	 * @param completionTimeout the maximum time to wait for the statement to complete
	 * @param filterCriteria a variable list of arguments used as filter criteria when processing the given statement
	 * @return the first value of the result set
	 * @throws IllegalArgumentException if either the requested target type or the filter value type isn't supported
	 * @throws ServiceProcessingException if the given statement could not be processed
	 * @throws InvocationCompletionTimeoutException if the statement took to long
	 */
	private <T> T waitForResult(String statement, Class<T> type, Duration completionTimeout, boolean skipReturnValue,
			Object... filterCriteria) {
		final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();

		try {
			final var future = scheduler.schedule(() -> runQuery(statement, type, skipReturnValue, filterCriteria), 0,
					TimeUnit.MILLISECONDS);

			// Wait for the result, or timeout
			return future.get(completionTimeout.toMillis(), TimeUnit.MILLISECONDS);
		}
		catch (final InterruptedException e) {
			Thread.currentThread().interrupt();

			throw new ServiceProcessingException("Execution has been interrupted!", e);
		}
		catch (final ExecutionException e) {
			if (e.getCause() instanceof final IllegalArgumentException ex)
				throw ex;

			throw new ServiceProcessingException("Error while processing SQL statement!", e);
		}
		catch (final TimeoutException e) {
			throw new InvocationCompletionTimeoutException("Waiting for invocation completion took to long!");
		}
		finally {
			scheduler.shutdownNow();
		}
	}

	/**
	 * Periodically execute the given statement until it returns a value
	 * @param <T> the requested return type
	 * @param statement the SQL statement that should be executed
	 * @param type the class that represents the requested return type
	 * @param skipReturnValue flag that controls if the value should be converted and returned
	 * @param filterCriteria a variable list of arguments used as filter criteria when processing the given statement
	 * @return the first value of the result set
	 * @throws IllegalArgumentException if either the requested target type or the filter value type isn't supported
	 * @throws ServiceProcessingException if a database access error occurred
	 */
	private <T> T runQuery(String statement, Class<T> type, boolean skipReturnValue, Object... filterCriteria) {
		if (logger.isDebugEnabled()) {
			logger.debug("Execute SQL statement: '{}'", statement.trim());
		}

		while (true) {
			try {
				final T value = executeSql(statement, type, skipReturnValue, filterCriteria);

				if (value != null) {
					logger.debug("SQL statement returned value '{}'", value);
					return value;
				}

				LockSupport.parkNanos(Duration.ofMillis(50).toNanos());
			}
			catch (final IllegalArgumentException e) {
				throw e;
			}
			catch (final SQLException e) {
				throw new ServiceProcessingException("Error while processing SQL statement!", e);
			}
		}
	}

	/**
	 * Execute a SQL statement with the provided filter criteria
	 * @param <T> the type of the value that should be returned
	 * @param sqlStatement the SQL statement to execute (including the WHERE clause and placeholders)
	 * @param type the class that represents the type
	 * @param skipReturnValue flag that controls if the value should be converted and returned
	 * @param filterCriteria a variable list of arguments used as filter criteria when processing the given statement
	 * @return the {@link ResultSet} containing the results of the query, or null if an error occurs
	 * @throws IllegalArgumentException if either the requested target type or the filter value type isn't supported
	 * @throws SQLException if a database access error occurred
	 */
	@SuppressWarnings("unchecked")
	private <T> T executeSql(String sqlStatement, Class<T> type, boolean skipReturnValue, Object... filterCriteria)
			throws SQLException {
		ResultSet resultSet = null;

		try (Connection connection = DriverManager.getConnection(databaseURL, userName, password);
				PreparedStatement preparedStatement = connection.prepareStatement(sqlStatement)) {
			int parameterIndex = 1;

			for (final Object filter : filterCriteria) {
				if (filter instanceof final String stringValue)
					preparedStatement.setString(parameterIndex++, stringValue);
				else if (filter instanceof final Character charValue)
					preparedStatement.setString(parameterIndex, new String(new char[] { charValue }));
				else if (filter instanceof final Boolean booleanValue)
					preparedStatement.setBoolean(parameterIndex, booleanValue);
				else if (filter instanceof final Integer integerValue)
					preparedStatement.setInt(parameterIndex++, integerValue);
				else if (filter instanceof final Long longValue)
					preparedStatement.setLong(parameterIndex++, longValue);
				else if (filter instanceof final Float floatValue)
					preparedStatement.setFloat(parameterIndex++, floatValue);
				else if (filter instanceof final Double doubleValue)
					preparedStatement.setDouble(parameterIndex++, doubleValue);
				else if (filter instanceof final BigDecimal decimalValue)
					preparedStatement.setBigDecimal(parameterIndex++, decimalValue);
				else if (filter instanceof final Date dateValue)
					preparedStatement.setTimestamp(parameterIndex++, new Timestamp(dateValue.getTime()));
				else if (filter instanceof final GregorianCalendar calendarValue)
					preparedStatement.setDate(parameterIndex++, new java.sql.Date(calendarValue.getTimeInMillis()));
				else if (filter instanceof final LocalDate dateValue)
					preparedStatement.setDate(parameterIndex++, java.sql.Date.valueOf(dateValue));
				else if (filter instanceof final LocalDateTime dateTimeValue)
					preparedStatement.setTimestamp(parameterIndex++, Timestamp.valueOf(dateTimeValue));
				else
					throw new IllegalArgumentException("Unsupported filter value type: " + filter.getClass().getName());
			}

			resultSet = preparedStatement.executeQuery();

			if (resultSet.next()) {
				if (skipReturnValue)
					return (T) resultSet.getObject(1);

				return convertValue(resultSet, type);
			}
		}

		return null;
	}

	/**
	 * Take the first value of the result set and try to convert it
	 * @param <T> the requested target type
	 * @param resultSet the {@link ResultSet} that contains the value
	 * @param type the class that represents the requested type
	 * @return the value
	 * @throws IllegalArgumentException if the requested target type isn't supported
	 * @throws SQLException if the value could not be extracted from the {@link ResultSet}
	 */
	@SuppressWarnings("unchecked")
	private <T> T convertValue(ResultSet resultSet, Class<T> type) throws SQLException {
		if (type == Long.class || type == long.class)
			return (T) Long.valueOf(resultSet.getLong(1));
		else if (type == Integer.class || type == int.class)
			return (T) Integer.valueOf(resultSet.getInt(1));
		else if (type == UUID.class)
			return (T) UUID.fromString(resultSet.getString(1));
		else if (type == String.class)
			return (T) resultSet.getString(1);
		else if (type == Boolean.class || type == boolean.class)
			return (T) Boolean.valueOf(resultSet.getBoolean(1));

		throw new IllegalArgumentException("Unsupported type '" + type.getName() + "'");
	}

}
