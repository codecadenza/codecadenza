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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Random;
import java.util.UUID;
import net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerFactory;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerProperties;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionTimeoutException;
import net.codecadenza.runtime.random.RandomStringGenerator;
import net.codecadenza.runtime.service.ServiceProcessingException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link JDBCInvocationCompletionHandler}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class JDBCInvocationCompletionHandlerTest {
	private static final String TEST_NAME = "Test Name";

	private static IInvocationCompletionHandler invocationHandler;
	private static Connection connection;
	private static InvocationCompletionHandlerProperties properties;

	@BeforeAll
	static void setUp() throws Exception {
		properties = new InvocationCompletionHandlerProperties();
		properties.load();

		final String databaseURL = properties.getResourceURL();
		final String userName = properties.getUserName();
		final String password = properties.getPassword();

		connection = DriverManager.getConnection(databaseURL, userName, password);
		invocationHandler = InvocationCompletionHandlerFactory.getPostProcessor(properties);

		executeSql("CREATE TABLE INTEGER_TABLE (ID INT PRIMARY KEY, NAME VARCHAR(255))");
		executeSql("CREATE TABLE LONG_TABLE (ID BIGINT PRIMARY KEY, NAME VARCHAR(255))");
		executeSql("CREATE TABLE UUID_TABLE (ID UUID PRIMARY KEY, NAME VARCHAR(255))");
		executeSql("CREATE TABLE STRING_TABLE (ID VARCHAR(10) PRIMARY KEY, NAME VARCHAR(255))");
		executeSql("CREATE TABLE CHAR_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE VARCHAR(1))");
		executeSql("CREATE TABLE BOOL_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE BOOLEAN)");
		executeSql("CREATE TABLE INT_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE INT)");
		executeSql("CREATE TABLE LONG_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE LONG)");
		executeSql("CREATE TABLE FLOAT_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE REAL)");
		executeSql("CREATE TABLE DOUBLE_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE DOUBLE PRECISION)");
		executeSql("CREATE TABLE DECIMAL_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE NUMERIC(10,4))");
		executeSql("CREATE TABLE DATE_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE TIMESTAMP)");
		executeSql("CREATE TABLE LOCAL_DATE_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE DATE)");
		executeSql("CREATE TABLE LOCAL_DATE_TIME_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE TIMESTAMP)");
		executeSql("CREATE TABLE CALENDAR_FILTER_TABLE (ID INT PRIMARY KEY, FILTER_VALUE TIMESTAMP)");
	}

	@AfterAll
	static void tearDown() throws SQLException {
		if (connection != null)
			connection.close();
	}

	@Test
	void testWaitForGeneratedObjectWithTimeout() throws SQLException {
		final int intValue = new Random().nextInt();

		executeSql("INSERT INTO INTEGER_TABLE (ID, NAME) VALUES (" + intValue + ", 'An integer')");

		final String statement = "SELECT ID FROM INTEGER_TABLE WHERE NAME = ?";

		assertThrows(InvocationCompletionTimeoutException.class,
				() -> invocationHandler.waitForFinish(statement, "Non existing name"));
	}

	@Test
	void testWaitForGeneratedObjectWithStringFilter() throws SQLException {
		final int intValue = new Random().nextInt();
		final String stringValue = RandomStringGenerator.generateRandomString(100);

		executeSql("INSERT INTO INTEGER_TABLE (ID, NAME) VALUES (" + intValue + ", '" + stringValue + "')");

		final String statement = "SELECT ID FROM INTEGER_TABLE WHERE NAME = ?";

		invocationHandler.waitForFinish(statement, stringValue);
	}

	@Test
	void testWaitForGeneratedObjectWithCharFilter() throws SQLException {
		final char charValue = 'a';

		executeSql("INSERT INTO CHAR_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, '" + charValue + "')");

		final String statement = "SELECT ID FROM CHAR_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, charValue);
	}

	@Test
	void testWaitForGeneratedObjectWithBooleanFilter() throws SQLException {
		executeSql("INSERT INTO BOOL_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, true)");

		final String statement = "SELECT ID FROM BOOL_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, true);
	}

	@Test
	void testWaitForGeneratedObjectWithLongFilter() throws SQLException {
		final long longValue = new Random().nextLong();

		executeSql("INSERT INTO LONG_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, " + longValue + ")");

		final String statement = "SELECT ID FROM LONG_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, longValue);
	}

	@Test
	void testWaitForGeneratedObjectWithIntFilter() throws SQLException {
		final int intValue = new Random().nextInt();

		executeSql("INSERT INTO INT_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, " + intValue + ")");

		final String statement = "SELECT ID FROM INT_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, intValue);
	}

	@Test
	void testWaitForGeneratedObjectWithFloatFilter() throws SQLException {
		final float floatValue = new Random().nextFloat();

		executeSql("INSERT INTO FLOAT_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, " + floatValue + ")");

		final String statement = "SELECT ID FROM FLOAT_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, floatValue);
	}

	@Test
	void testWaitForGeneratedObjectWithDoubleFilter() throws SQLException {
		final double doubleValue = new Random().nextDouble();

		executeSql("INSERT INTO DOUBLE_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, " + doubleValue + ")");

		final String statement = "SELECT ID FROM DOUBLE_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, doubleValue);
	}

	@Test
	void testWaitForGeneratedObjectWithDecimalFilter() throws SQLException {
		final BigDecimal decimalValue = new BigDecimal("10000.9999");

		executeSql("INSERT INTO DECIMAL_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, " + decimalValue + ")");

		final String statement = "SELECT ID FROM DECIMAL_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, decimalValue);
	}

	@Test
	void testWaitForGeneratedObjectWithDateFilter() throws SQLException {
		final Date dateValue = new Date();
		final SimpleDateFormat dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		final Date filterValue = new Date(dateValue.getTime() - 1000);

		executeSql("INSERT INTO DATE_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, '" + dateTimeFormat.format(dateValue) + "')");

		final String statement = "SELECT ID FROM DATE_FILTER_TABLE WHERE FILTER_VALUE > ?";

		invocationHandler.waitForFinish(statement, filterValue);
	}

	@Test
	void testWaitForGeneratedObjectWithLocalDateFilter() throws SQLException {
		final LocalDate dateValue = LocalDate.now();
		final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd");

		executeSql("INSERT INTO LOCAL_DATE_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, '" + dateFormat.format(dateValue) + "')");

		final String statement = "SELECT ID FROM LOCAL_DATE_FILTER_TABLE WHERE FILTER_VALUE = ?";

		invocationHandler.waitForFinish(statement, dateValue);
	}

	@Test
	void testWaitForGeneratedObjectWithLocalDateTimeFilter() throws SQLException {
		final LocalDateTime dateTimeValue = LocalDateTime.now();
		final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SS");

		executeSql(
				"INSERT INTO LOCAL_DATE_TIME_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, '" + dateFormat.format(dateTimeValue) + "')");

		final String statement = "SELECT ID FROM LOCAL_DATE_TIME_FILTER_TABLE WHERE FILTER_VALUE > ?";

		invocationHandler.waitForFinish(statement, dateTimeValue.minusSeconds(1));
	}

	@Test
	void testWaitForGeneratedObjectWithCalendarFilter() throws SQLException {
		final var dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		final var calendarValue = new GregorianCalendar();

		final var filterValue = new GregorianCalendar();
		filterValue.setTimeInMillis(calendarValue.getTimeInMillis() - 1000);

		executeSql("INSERT INTO CALENDAR_FILTER_TABLE (ID, FILTER_VALUE) VALUES (1, '"
				+ dateTimeFormat.format(calendarValue.getTime()) + "')");

		final String statement = "SELECT ID FROM CALENDAR_FILTER_TABLE WHERE FILTER_VALUE > ?";

		invocationHandler.waitForFinish(statement, filterValue);
	}

	@Test
	void testWaitForGeneratedInteger() throws SQLException {
		final int intValue = new Random().nextInt();

		executeSql("INSERT INTO INTEGER_TABLE (ID, NAME) VALUES (" + intValue + ", '" + TEST_NAME + "')");

		final String statement = "SELECT ID FROM INTEGER_TABLE WHERE NAME = ?";

		final int id = invocationHandler.waitForFinish(statement, Integer.class, TEST_NAME);

		assertEquals(intValue, id);
	}

	@Test
	void testWaitForGeneratedLong() throws SQLException {
		final long longValue = new Random().nextLong();

		executeSql("INSERT INTO LONG_TABLE (ID, NAME) VALUES (" + longValue + ", '" + TEST_NAME + "')");

		final String statement = "SELECT ID FROM LONG_TABLE WHERE NAME = ?";

		final long id = invocationHandler.waitForFinish(statement, Long.class, TEST_NAME);

		assertEquals(longValue, id);
	}

	@Test
	void testWaitForGeneratedUUID() throws SQLException {
		final UUID uuidValue = UUID.randomUUID();

		executeSql("INSERT INTO UUID_TABLE (ID, NAME) VALUES ('" + uuidValue + "', '" + TEST_NAME + "')");

		final String statement = "SELECT ID FROM UUID_TABLE WHERE NAME = ?";

		final UUID id = invocationHandler.waitForFinish(statement, UUID.class, TEST_NAME);

		assertEquals(uuidValue, id);
	}

	@Test
	void testWaitForGeneratedString() throws SQLException {
		final String stringValue = RandomStringGenerator.generateRandomString(10);

		executeSql("INSERT INTO STRING_TABLE (ID, NAME) VALUES ('" + stringValue + "', '" + TEST_NAME + "')");

		final String statement = "SELECT ID FROM STRING_TABLE WHERE NAME = ?";

		final String id = invocationHandler.waitForFinish(statement, String.class, TEST_NAME);

		assertEquals(stringValue, id);
	}

	@Test
	void testWaitForGeneratedObjectIdWithNotSupportedType() throws SQLException {
		final String stringValue = RandomStringGenerator.generateRandomString(10);

		executeSql("INSERT INTO STRING_TABLE (ID, NAME) VALUES ('" + stringValue + "', '" + TEST_NAME + "')");

		final String statement = "SELECT ID FROM STRING_TABLE WHERE NAME = ?";

		assertThrows(IllegalArgumentException.class, () -> invocationHandler.waitForFinish(statement, TEST_NAME, Date.class));
	}

	@Test
	void testWaitForGeneratedObjectWithNotSupportedFilterType() {
		final String statement = "SELECT ID FROM STRING_TABLE WHERE NAME = ?";
		final BigInteger bigIntValue = BigInteger.valueOf(1000);

		assertThrows(IllegalArgumentException.class, () -> invocationHandler.waitForFinish(statement, bigIntValue));
	}

	@Test
	void testWaitForGeneratedObjectWithIllegalStatement() {
		final String statement = "SELECT NON_EXISTING_COLUMN FROM STRING_TABLE";

		assertThrows(ServiceProcessingException.class, () -> invocationHandler.waitForFinish(statement, Date.class, TEST_NAME));
	}

	private static void executeSql(String sql) throws SQLException {
		try (Statement statement = connection.createStatement()) {
			statement.execute(sql);
		}
	}

}
