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
package net.codecadenza.runtime.jpa.util;

import static net.codecadenza.runtime.jpa.util.JPAQueryStatementGenerator.AND;
import static net.codecadenza.runtime.jpa.util.JPAQueryStatementGenerator.BETWEEN;
import static net.codecadenza.runtime.jpa.util.JPAQueryStatementGenerator.IN;
import static net.codecadenza.runtime.jpa.util.JPAQueryStatementGenerator.NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_EQUAL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_GREATER;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_GREATER_OR_EQUAL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IS_NOT_NULL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IS_NULL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_SMALLER;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_IN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import net.codecadenza.runtime.search.exception.GeneralSearchException;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link JPAQueryStatementGenerator}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class JPAQueryStatementGeneratorTest {
	private static final String GREATER_THAN = " > ";
	private static final String GREATER_OR_EQUAL_THAN = " >= ";
	private static final String SMALLER_THAN = " < ";
	private static final String EQUAL = " = ";
	private static final String LIKE = " like ";
	private static final String IS_NOT_NULL = " is not null";
	private static final String IS_NULL = " is null";
	private static final String FILTER_STRING = "test%";
	private static final String FILTER_STRING_UPPER_CASE = "TEST";
	private static final String DATE_FORMAT = "dd.MM.yyyy";
	private static final String DATE_TIME_FORMAT = "dd.MM.yyyy HH:mm:ss";
	private static final String NUMBER_FORMAT = "###,###,##0.00";
	private static final String WRONG_BETWEEN_DELIMITER = "--";
	private static final String FIELD_ID = "a.id";
	private static final String FIELD_DESCRIPTION = "a.description";
	private static final String SELECT_ID = "select " + FIELD_ID + " from Entity a";
	private static final String FROM_CLAUSE = "from Entity a";
	private static final String SELECT_WHERE_ID = SELECT_ID + " where " + FIELD_ID;
	private static final String AND_ID_LIKE = " and " + FIELD_ID + " like ";
	private static final String WHERE_LOWER_ID_LIKE = " where lower(" + FIELD_ID + ") like ";
	private static final String WHERE_TITLE_IS_NOT_NULL = " where a.title" + IS_NOT_NULL;
	private static final String COUNT_STATEMENT = "select count(a) " + FROM_CLAUSE;
	private static final String COUNT_WHERE_ID = COUNT_STATEMENT + " where " + FIELD_ID;
	private static final String PARAM_NAME_ID_1 = "param0_1";
	private static final String PARAM_NAME_ID_2 = "param0_2";
	private static final String PARAM_NAME_DESCRIPTION = "param1_1";
	private static final String PARAM_VALUE_ID_1 = ":" + PARAM_NAME_ID_1;
	private static final String PARAM_VALUE_ID_2 = ":" + PARAM_NAME_ID_2;
	private static final String PARAM_VALUE_DESCRIPTION = ":" + PARAM_NAME_DESCRIPTION;
	private static final String ORDER_BY_ID = " order by " + FIELD_ID + " desc";
	private static final String ORDER_BY_ID_AND_DESCRIPTION = ORDER_BY_ID + "," + FIELD_DESCRIPTION + " asc";
	private static final String GROUP_BY_ID = " group by " + FIELD_ID;

	@Test
	void testSearchStatementWithFilterNotNull() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, OPERATOR_IS_NOT_NULL, null);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_WHERE_ID + IS_NOT_NULL, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithFilterIsNullAndNoOperator() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, null, null);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_ID, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithFilterIsNull() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, OPERATOR_IS_NULL, null);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_WHERE_ID + IS_NULL, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithNoFilterCriteria() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, OPERATOR_LIKE, null);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_ID, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithGivenOrderBy() throws Exception {
		final var searchDTO = new SearchDTO();
		searchDTO.setFromClause(SELECT_ID + ORDER_BY_ID);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(searchDTO.getFromClause(), statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithGivenGroupBy() throws Exception {
		final var searchDTO = new SearchDTO();
		searchDTO.setFromClause(SELECT_ID);
		searchDTO.setGroupBy(GROUP_BY_ID.trim());

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_ID + GROUP_BY_ID, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithGivenWhere() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING);
		searchDTO.setFromClause(SELECT_ID + WHERE_TITLE_IS_NOT_NULL);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(searchDTO.getFromClause() + AND_ID_LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithStringLike() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithStringEquals() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, OPERATOR_EQUAL, FILTER_STRING);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithSortingString() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING, true);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + LIKE + PARAM_VALUE_ID_1 + ORDER_BY_ID, statement);
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithStringNotCaseSensitive() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING_UPPER_CASE);
		searchDTO.setCaseSensitive(false);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_ID + WHERE_LOWER_ID_LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals(FILTER_STRING_UPPER_CASE.toLowerCase(), parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithStringNotCaseSensitiveNotExactMatch() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING_UPPER_CASE);
		searchDTO.setCaseSensitive(false);
		searchDTO.setExactFilterMatch(false);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_ID + WHERE_LOWER_ID_LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals("%" + FILTER_STRING_UPPER_CASE.toLowerCase() + "%", parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithStringCaseSensitiveNotExactMatch() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING_UPPER_CASE);
		searchDTO.setCaseSensitive(true);
		searchDTO.setExactFilterMatch(false);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals("%" + FILTER_STRING_UPPER_CASE + "%", parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithEnumNotCaseSensitive() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.ENUM, OPERATOR_LIKE, FILTER_STRING_UPPER_CASE);
		searchDTO.setCaseSensitive(false);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_ID + WHERE_LOWER_ID_LIKE + "'" + FILTER_STRING_UPPER_CASE.toLowerCase() + "'", statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithEnumIgnoreCaseSensitive() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.ENUM, OPERATOR_LIKE, FILTER_STRING_UPPER_CASE);
		searchDTO.setCaseSensitive(true);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_ID + WHERE_LOWER_ID_LIKE + "'" + FILTER_STRING_UPPER_CASE.toLowerCase() + "'", statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithMultipleFields() throws Exception {
		final var searchDTO = new SearchDTO();
		searchDTO.setFromClause(SELECT_ID);
		searchDTO.setCaseSensitive(true);
		searchDTO.setExactFilterMatch(true);

		final var searchField1 = searchDTO.addSearchField(FIELD_ID, SearchFieldDataTypeEnum.INTEGER);
		searchField1.setFilterCriteria("1");
		searchField1.setSortIndex(0);
		searchField1.setSortOrder(SortDirectionEnum.DESC);

		final var searchField2 = searchDTO.addSearchField(FIELD_DESCRIPTION, SearchFieldDataTypeEnum.STRING);
		searchField2.setFilterCriteria(FILTER_STRING);
		searchField2.setSortIndex(1);
		searchField2.setSortOrder(SortDirectionEnum.ASC);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		final var expectedStatement = new StringBuilder(SELECT_WHERE_ID);
		expectedStatement.append(EQUAL);
		expectedStatement.append(PARAM_VALUE_ID_1);
		expectedStatement.append(AND);
		expectedStatement.append(FIELD_DESCRIPTION);
		expectedStatement.append(LIKE);
		expectedStatement.append(PARAM_VALUE_DESCRIPTION);
		expectedStatement.append(ORDER_BY_ID_AND_DESCRIPTION);

		assertEquals(expectedStatement.toString(), statement);
		assertEquals(1, (int) parameters.get(PARAM_NAME_ID_1));
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_DESCRIPTION));
	}

	@Test
	void testSearchStatementWithIntegerEquals() throws Exception {
		final var filterValue = 123;
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, null, Integer.toString(filterValue));
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithBooleanEquals() throws Exception {
		final var filterValue = false;
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.BOOLEAN, null, Boolean.toString(filterValue));
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithUUIDStringLike() throws Exception {
		final var filterValue = UUID.randomUUID();
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.UUID_STRING, null, filterValue.toString());
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);

		assertEquals(SELECT_WHERE_ID + LIKE + "'" + filterValue.toString() + "'", statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testSearchStatementWithUUIDEqual() throws Exception {
		final var filterValue = UUID.randomUUID();
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.UUID_BINARY, null, filterValue.toString());
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithCharEqual() throws Exception {
		final var filterValue = 'a';
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.CHAR, null, Character.toString(filterValue));
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithFloatGreater() throws Exception {
		final var filterValue = 123.456f;
		final var filterCriteria = initDecimalFormat().format(filterValue);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.FLOAT, OPERATOR_GREATER, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + GREATER_THAN + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, (float) parameters.get(PARAM_NAME_ID_1), 0.01f);
	}

	@Test
	void testSearchStatementWithDoubleSmaller() throws Exception {
		final var filterValue = 123.456;
		final var filterCriteria = initDecimalFormat().format(filterValue);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.DOUBLE, OPERATOR_SMALLER, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + SMALLER_THAN + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, (double) parameters.get(PARAM_NAME_ID_1), 0.01);
	}

	@Test
	void testSearchStatementWithBigDecimalGreaterOrEqual() throws Exception {
		final var filterValue = new BigDecimal("123.45");
		final var filterCriteria = initDecimalFormat().format(filterValue);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.BIG_DECIMAL, OPERATOR_GREATER_OR_EQUAL, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + GREATER_OR_EQUAL_THAN + PARAM_VALUE_ID_1, statement);
		assertEquals(filterValue, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testSearchStatementWithDateEqual() throws Exception {
		final var filterValue = new Date();
		final var filterCriteria = new SimpleDateFormat(DATE_FORMAT).format(filterValue);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.DATE, null, filterCriteria);

		final var searchField = searchDTO.getSearchFields().stream().findFirst().orElseThrow();
		searchField.setDateTimeFormat(false);

		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);
		final var dateParameter = new SimpleDateFormat(DATE_FORMAT).format((Date) parameters.get(PARAM_NAME_ID_1));

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterCriteria, dateParameter);
	}

	@Test
	void testSearchStatementWithLocalDateEqual() throws Exception {
		final var filterValue = LocalDate.now();
		final var dateFormat = DateTimeFormatter.ofPattern(DATE_FORMAT).withZone(ZoneId.systemDefault());
		final var filterCriteria = dateFormat.format(filterValue);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.LOCAL_DATE, null, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);
		final var dateParameter = dateFormat.format((LocalDate) parameters.get(PARAM_NAME_ID_1));

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterCriteria, dateParameter);
	}

	@Test
	void testSearchStatementWithLocalDateTimeEqual() throws Exception {
		final var filterValue = LocalDateTime.now();
		final var dateFormat = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT).withZone(ZoneId.systemDefault());
		final var filterCriteria = dateFormat.format(filterValue);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.LOCAL_DATE_TIME, null, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);
		final var dateParameter = dateFormat.format((LocalDateTime) parameters.get(PARAM_NAME_ID_1));

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterCriteria, dateParameter);
	}

	@Test
	void testSearchStatementWithGregorianCalendarEqual() throws Exception {
		final var filterValue = new GregorianCalendar();
		final var filterCriteria = new SimpleDateFormat(DATE_TIME_FORMAT).format(filterValue.getTime());
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.GREGORIAN_CALENDAR, null, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);
		final var dateParameter = (GregorianCalendar) parameters.get(PARAM_NAME_ID_1);

		assertEquals(SELECT_WHERE_ID + EQUAL + PARAM_VALUE_ID_1, statement);
		assertEquals(filterCriteria, new SimpleDateFormat(DATE_TIME_FORMAT).format(dateParameter.getTime()));
	}

	@Test
	void testSearchStatementWithIntegerIn() throws Exception {
		final var filterValue1 = 123;
		final var filterValue2 = 456;
		final var filterCriteria = Integer.toString(filterValue1) + TOKEN_DELIMITER_IN + Integer.toString(filterValue2);

		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, OPERATOR_IN, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + IN + PARAM_VALUE_ID_1 + "," + PARAM_VALUE_ID_2 + ")", statement);
		assertEquals(filterValue1, parameters.get(PARAM_NAME_ID_1));
		assertEquals(filterValue2, parameters.get(PARAM_NAME_ID_2));
	}

	@Test
	void testSearchStatementWithIntegerNotIn() throws Exception {
		final var filterValue1 = 123;
		final var filterValue2 = 456;
		final var filterCriteria = Integer.toString(filterValue1) + TOKEN_DELIMITER_IN + Integer.toString(filterValue2);

		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, OPERATOR_NOT_IN, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + NOT_IN + PARAM_VALUE_ID_1 + "," + PARAM_VALUE_ID_2 + ")", statement);
		assertEquals(filterValue1, parameters.get(PARAM_NAME_ID_1));
		assertEquals(filterValue2, parameters.get(PARAM_NAME_ID_2));
	}

	@Test
	void testSearchStatementWithLongBetween() throws Exception {
		final var filterValue1 = 123L;
		final var filterValue2 = 456L;
		final var filterCriteria = Long.toString(filterValue1) + TOKEN_DELIMITER_BETWEEN + Long.toString(filterValue2);

		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.LONG, OPERATOR_BETWEEN, filterCriteria);
		final var statement = JPAQueryStatementGenerator.createStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(SELECT_WHERE_ID + BETWEEN + PARAM_VALUE_ID_1 + AND + PARAM_VALUE_ID_2, statement);
		assertEquals(filterValue1, parameters.get(PARAM_NAME_ID_1));
		assertEquals(filterValue2, parameters.get(PARAM_NAME_ID_2));
	}

	@Test
	void testSearchStatementWithWrongBetweenDelimiter() {
		final var filterValue1 = 123L;
		final var filterValue2 = 456L;
		final var filterCriteria = Long.toString(filterValue1) + WRONG_BETWEEN_DELIMITER + Long.toString(filterValue2);
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.LONG, OPERATOR_BETWEEN, filterCriteria);

		assertThrows(GeneralSearchException.class, () -> JPAQueryStatementGenerator.createStatement(searchDTO));
	}

	@Test
	void testCountStatement() throws Exception {
		final var searchDTO = new SearchDTO();
		searchDTO.setFromClause(FROM_CLAUSE);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);

		assertEquals(COUNT_STATEMENT, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testCountStatementWithGroupBy() throws Exception {
		final var searchDTO = new SearchDTO();
		searchDTO.setFromClause(FROM_CLAUSE);
		searchDTO.setGroupBy(GROUP_BY_ID);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);

		assertEquals(COUNT_STATEMENT + GROUP_BY_ID, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testCountStatementWithGivenWhere() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING);
		searchDTO.setFromClause(FROM_CLAUSE + WHERE_TITLE_IS_NOT_NULL);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(COUNT_STATEMENT + WHERE_TITLE_IS_NOT_NULL + AND_ID_LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_ID_1));
	}

	@Test
	void testCountStatementWithMultipleFields() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, null, "1");
		searchDTO.setFromClause(FROM_CLAUSE);

		final var searchField = searchDTO.addSearchField(FIELD_DESCRIPTION, SearchFieldDataTypeEnum.STRING);
		searchField.setFilterCriteria(FILTER_STRING);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		final var expectedStatement = new StringBuilder(COUNT_WHERE_ID);
		expectedStatement.append(EQUAL);
		expectedStatement.append(PARAM_VALUE_ID_1);
		expectedStatement.append(AND);
		expectedStatement.append(FIELD_DESCRIPTION);
		expectedStatement.append(LIKE);
		expectedStatement.append(PARAM_VALUE_DESCRIPTION);

		assertEquals(expectedStatement.toString(), statement);
		assertEquals(1, (int) parameters.get(PARAM_NAME_ID_1));
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_DESCRIPTION));
	}

	@Test
	void testCountStatementWithFilterIsNull() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, OPERATOR_GREATER, null);
		searchDTO.setFromClause(FROM_CLAUSE);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);

		assertEquals(COUNT_STATEMENT, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	@Test
	void testCountStatementWithFilterIsEmptyAndNoOperator() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.INTEGER, "", null);
		searchDTO.setFromClause(FROM_CLAUSE);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);

		assertEquals(COUNT_STATEMENT, statement);
		assertTrue(JPAQueryStatementGenerator.createParameters(searchDTO).isEmpty());
	}

	void testCountStatementWithWrongBetweenDelimiter() {
		final var filterValue1 = 123L;
		final var filterValue2 = 456L;
		final var filterCriteria = Long.toString(filterValue1) + WRONG_BETWEEN_DELIMITER + Long.toString(filterValue2);

		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.LONG, OPERATOR_BETWEEN, filterCriteria);
		searchDTO.setFromClause(FROM_CLAUSE);

		assertThrows(RuntimeException.class, () -> JPAQueryStatementGenerator.createCountStatement(searchDTO));
	}

	@Test
	void testRemoveOrderByFromCountStatement() throws Exception {
		final var searchDTO = initSearchDTO(SearchFieldDataTypeEnum.STRING, null, FILTER_STRING);
		searchDTO.setFromClause(FROM_CLAUSE + WHERE_TITLE_IS_NOT_NULL + ORDER_BY_ID);

		final var statement = JPAQueryStatementGenerator.createCountStatement(searchDTO);
		final var parameters = JPAQueryStatementGenerator.createParameters(searchDTO);

		assertEquals(COUNT_STATEMENT + WHERE_TITLE_IS_NOT_NULL + AND_ID_LIKE + PARAM_VALUE_ID_1, statement);
		assertEquals(FILTER_STRING, parameters.get(PARAM_NAME_ID_1));
	}

	private SearchDTO initSearchDTO(SearchFieldDataTypeEnum dataType, String operatorName, String filterCriteria) {
		return initSearchDTO(dataType, operatorName, filterCriteria, false);
	}

	private SearchDTO initSearchDTO(SearchFieldDataTypeEnum dataType, String operatorName, String filterCriteria,
			boolean sortField) {
		final var searchDTO = new SearchDTO();
		searchDTO.setFromClause(SELECT_ID);
		searchDTO.setCaseSensitive(true);
		searchDTO.setExactFilterMatch(true);
		searchDTO.setNumberFormat(NUMBER_FORMAT);
		searchDTO.setDateFormat(DATE_FORMAT);
		searchDTO.setDateTimeFormat(DATE_TIME_FORMAT);

		final SearchFieldDTO searchField = searchDTO.addSearchField(FIELD_ID, dataType);
		searchField.setFilterCriteria(filterCriteria);
		searchField.setColLabel(FIELD_ID);

		if (sortField) {
			searchField.setSortIndex(0);
			searchField.setSortOrder(SortDirectionEnum.DESC);
		}

		if (operatorName != null) {
			final var operator = new SearchOperatorDTO(0, operatorName, operatorName, true, true, true, true, true);
			searchField.setOperator(operator);
		}

		return searchDTO;
	}

	private DecimalFormat initDecimalFormat() {
		final var decimalSymbols = new DecimalFormatSymbols();
		decimalSymbols.setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		decimalSymbols.setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());

		return new DecimalFormat(NUMBER_FORMAT, decimalSymbols);
	}

}
