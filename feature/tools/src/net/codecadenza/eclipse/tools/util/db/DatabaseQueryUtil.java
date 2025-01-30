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

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Time;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;

/**
 * <p>
 * Utility class to execute SQL queries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DatabaseQueryUtil {
	private static final String NULL_STR = "[null]";
	public static final String EMPTY_STRING = "[empty]";

	/**
	 * Prevent instantiation
	 */
	private DatabaseQueryUtil() {

	}

	/**
	 * Format the result set
	 * @param r the result set
	 * @param dateFormat the date format
	 * @param timeFormat the time format
	 * @return the formatted result set
	 * @throws DBResultFormatException if the formatting of the given result set has failed
	 */
	private static DBQueryResultSet formatResultSet(ResultSet r, String dateFormat, String timeFormat)
			throws DBResultFormatException {
		final var appData = new DBQueryResultSet();

		try {
			if (r == null) {
				final var emptyResult = new ArrayList<String>();
				final var emptyHeader = new String[1];

				emptyHeader[0] = "Result";
				appData.setHeader(emptyHeader);

				emptyResult.add("No data found");
				appData.getRows().add(emptyResult);

				return appData;
			}

			final var dateFormatter = new SimpleDateFormat(dateFormat);
			final var timeFormatter = new SimpleDateFormat(timeFormat);
			final var timeStampFormatter = new SimpleDateFormat(dateFormat + " " + timeFormat);
			final ResultSetMetaData m = r.getMetaData();
			final int col = m.getColumnCount();
			final var header = new String[col];
			final var isDate = new boolean[col];
			final var isTime = new boolean[col];
			final var isTimeStamp = new boolean[col];

			for (int i = 1; i <= col; i++) {
				header[i - 1] = m.getColumnLabel(i);
				isDate[i - 1] = (m.getColumnType(i) == java.sql.Types.DATE);
				isTime[i - 1] = (m.getColumnType(i) == java.sql.Types.TIME);
				isTimeStamp[i - 1] = (m.getColumnType(i) == java.sql.Types.TIMESTAMP);
			}

			appData.setHeader(header);

			while (r.next()) {
				final var row = new ArrayList<String>();

				for (int i = 1; i <= col; i++) {
					if (isDate[i - 1]) {
						final var date = (Date) r.getObject(i);

						if (date != null)
							row.add(dateFormatter.format(date));
						else
							row.add(NULL_STR);
					}
					else if (isTime[i - 1]) {
						final var time = (Time) r.getObject(i);

						if (time != null)
							row.add(timeFormatter.format(time));
						else
							row.add(NULL_STR);
					}
					else if (isTimeStamp[i - 1]) {
						final Date date;

						// It must be considered that the current MySQL driver (8.0.31) returns a LocalDateTime!
						// For more information visit https://bugs.mysql.com/bug.php?id=102435
						if (r.getObject(i) instanceof final LocalDateTime localDateTime)
							date = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
						else
							date = (java.sql.Timestamp) r.getObject(i);

						if (date != null)
							row.add(timeStampFormatter.format(date));
						else
							row.add(NULL_STR);
					}
					else {
						final Object data = r.getObject(i);

						if (data != null) {
							String stringValue = data.toString();

							if (!stringValue.isEmpty()) {
								// Return only the first line!
								if (stringValue.contains("\n"))
									stringValue = stringValue.substring(0, stringValue.indexOf('\n')) + "...";
								else if (stringValue.contains("\r"))
									stringValue = stringValue.substring(0, stringValue.indexOf('\r')) + "...";
							}
							else
								stringValue = EMPTY_STRING;

							row.add(stringValue);
						}
						else
							row.add(NULL_STR);
					}
				}

				appData.getRows().add(row);
			}

			return appData;
		}
		catch (final Exception e) {
			throw new DBResultFormatException(e.getMessage());
		}
	}

	/**
	 * Execute a SQL command and format the result
	 * @param connection
	 * @param sql the command
	 * @param maxRowCount the max. number of rows to be fetched
	 * @param dateFormat the date format
	 * @param timeFormat the time format
	 * @return a database query result set object
	 * @throws SQLException if a database access error has occurred
	 * @throws DBResultFormatException if the record set could not be formatted
	 */
	public static DBQueryResultSet executeQuery(final Connection connection, String sql, int maxRowCount, String dateFormat,
			String timeFormat) throws SQLException, DBResultFormatException {
		try (Statement statement = connection.createStatement()) {
			// Set the max. number of rows to be fetched
			if (maxRowCount > 0)
				statement.setMaxRows(maxRowCount);

			// Execute the SQL command
			statement.execute(sql);

			try (ResultSet rs = statement.getResultSet()) {
				// Format the result
				return formatResultSet(rs, dateFormat, timeFormat);
			}
		}
	}

	/**
	 * Execute a SQL update command (insert, update or delete)
	 * @param connection
	 * @param sql the command
	 * @return the number of manipulated rows
	 * @throws SQLException if a database access error has occurred
	 */
	public static int executeUpdate(final Connection connection, String sql) throws SQLException {
		Statement statement = null;

		try {
			statement = connection.createStatement();

			// Execute the update statement
			return statement.executeUpdate(sql);
		}
		finally {
			if (statement != null)
				statement.close();
		}
	}

}
