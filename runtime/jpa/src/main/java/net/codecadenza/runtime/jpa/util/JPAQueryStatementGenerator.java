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

import static net.codecadenza.runtime.search.SearchService.OPERATOR_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_EQUAL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IS_NOT_NULL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IS_NULL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_LIKE;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_IN;

import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.regex.Pattern;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import net.codecadenza.runtime.search.exception.GeneralSearchException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Class that is responsible for creating JPA query statements based on a given {@link SearchDTO}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAQueryStatementGenerator {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String WHERE = "where";
	public static final String SELECT = "select ";
	public static final String AND = " and ";
	public static final String ORDER_BY = " order by ";
	public static final String COUNT = "count(a) ";
	public static final String IN = " in (";
	public static final String BETWEEN = " between ";
	public static final String NOT_IN = " not in (";
	private static final String PARAM = "param";
	private static final String WHERE_TO_DETECT = " " + WHERE + " ";
	private static final Pattern TOKEN_DELIMITER_IN_PATTERN = Pattern.compile(TOKEN_DELIMITER_IN);
	private static final Pattern TOKEN_DELIMITER_BETWEEN_PATTERN = Pattern.compile(TOKEN_DELIMITER_BETWEEN);

	private static SearchOperatorDTO defaultOperatorEqual = new SearchOperatorDTO(0, OPERATOR_EQUAL, "equal", true, true, true,
			true, true);
	private static SearchOperatorDTO defaultOperatorLike = new SearchOperatorDTO(1, OPERATOR_LIKE, "like", true, false, false, true,
			false);

	/**
	 * Prevent instantiation
	 */
	private JPAQueryStatementGenerator() {

	}

	/**
	 * @param searchDTO
	 * @return a string with a parameterized JPA query
	 * @throws GeneralSearchException if the search statement could not be created
	 */
	public static String createStatement(SearchDTO searchDTO) {
		final var statement = new StringBuilder();
		final var orderBy = new StringBuilder();
		final String fromClauseLowerCase = searchDTO.getFromClause().toLowerCase();
		boolean whereSet = false;
		var defaultOrderBy = "";

		try {
			if (fromClauseLowerCase.contains(WHERE_TO_DETECT))
				whereSet = true;

			if (fromClauseLowerCase.contains(ORDER_BY)) {
				defaultOrderBy = searchDTO.getFromClause().substring(fromClauseLowerCase.indexOf(ORDER_BY));
				statement.append(searchDTO.getFromClause().substring(0, fromClauseLowerCase.indexOf(ORDER_BY)));
			}
			else {
				// Add the from clause
				statement.append(searchDTO.getFromClause());
			}

			// Add the where clause
			for (final SearchFieldDTO field : searchDTO.getSearchFields()) {
				if (!processField(field))
					continue;

				statement.append(addField(field, whereSet, searchDTO.isCaseSensitive(), searchDTO.isExactFilterMatch()));

				whereSet = true;
			}

			if (searchDTO.getGroupBy() != null && !searchDTO.getGroupBy().isEmpty())
				statement.append(" " + searchDTO.getGroupBy());

			boolean isFirstOrderBy = true;

			if (!defaultOrderBy.isEmpty()) {
				isFirstOrderBy = false;
				orderBy.append(defaultOrderBy);
			}

			// Sort fields by using the sort index
			searchDTO.getSearchFields().sort((a, b) -> {
				final Integer index1 = a.getSortIndex();
				final Integer index2 = b.getSortIndex();

				return index1.compareTo(index2);
			});

			// Add the order by clause
			for (final SearchFieldDTO field : searchDTO.getSearchFields()) {
				if (field.getSortOrder() != null && field.getSortOrder() != SortDirectionEnum.NONE) {
					if (!isFirstOrderBy)
						orderBy.append(",");
					else {
						statement.append(ORDER_BY);
						isFirstOrderBy = false;
					}

					orderBy.append(field.getColName());
					orderBy.append(" ");
					orderBy.append(field.getSortOrder().name().toLowerCase());
				}
			}

			// Check if the result set should be ordered at all and add an order by clause to the query if necessary
			if (!isFirstOrderBy)
				statement.append(orderBy);

			logger.debug("Created query statement '{}'", statement);

			return statement.toString();
		}
		catch (final RuntimeException e) {
			logger.error("Error while creating query statement!", e);

			throw new GeneralSearchException(e, true);
		}
	}

	/**
	 * @param searchDTO
	 * @return a string with a parameterized JPA query for counting records
	 * @throws GeneralSearchException if the count statement could not be created
	 */
	public static String createCountStatement(SearchDTO searchDTO) {
		final var statement = new StringBuilder(SELECT);
		boolean whereSet = false;

		try {
			statement.append(COUNT);

			// Add the from clause
			if (searchDTO.getFromClause().toLowerCase().contains(ORDER_BY)) {
				// Remove everything after 'order by'!
				statement.append(searchDTO.getFromClause().substring(0, searchDTO.getFromClause().indexOf(ORDER_BY)));
			}
			else
				statement.append(searchDTO.getFromClause());

			if (searchDTO.getFromClause().toLowerCase().contains(WHERE_TO_DETECT))
				whereSet = true;

			// Add the where clause
			for (final SearchFieldDTO field : searchDTO.getSearchFields()) {
				if (!processField(field))
					continue;

				statement.append(addField(field, whereSet, searchDTO.isCaseSensitive(), searchDTO.isExactFilterMatch()));

				whereSet = true;
			}

			if (searchDTO.getGroupBy() != null && !searchDTO.getGroupBy().isEmpty())
				statement.append(searchDTO.getGroupBy());

			logger.debug("Created count statement '{}'", statement);

			return statement.toString();
		}
		catch (final Exception e) {
			logger.error("Error while creating count statement!", e);

			throw new GeneralSearchException(e);
		}
	}

	/**
	 * @param searchDTO
	 * @return a map containing the parameters according to the query statement
	 * @throws ParseException if the parsing of one of the provided search field values has failed
	 */
	public static Map<String, Object> createParameters(SearchDTO searchDTO) throws ParseException {
		final var params = new HashMap<String, Object>();

		for (final SearchFieldDTO field : searchDTO.getSearchFields()) {
			if (!processField(field))
				continue;

			addParameters(params, field, searchDTO);
		}

		if (logger.isTraceEnabled())
			for (final Map.Entry<String, Object> entry : params.entrySet())
				logger.trace("Created parameter '{}' using value '{}'", entry.getKey(), entry.getValue());

		return params;
	}

	/**
	 * Add a JPA filter expression for a given field
	 * @param field
	 * @param whereSet
	 * @param caseSensitive
	 * @param exactMatch
	 * @return the created expression for that field
	 */
	private static String addField(SearchFieldDTO field, boolean whereSet, boolean caseSensitive, boolean exactMatch) {
		final var b = new StringBuilder();
		final SearchOperatorDTO operator = getOperator(field);

		if (whereSet)
			b.append(AND);
		else {
			b.append(" ");
			b.append(WHERE);
			b.append(" ");
		}

		if (operator.getValue().equals(OPERATOR_IS_NULL) || operator.getValue().equals(OPERATOR_IS_NOT_NULL)) {
			b.append(field.getColName());
			b.append(" ");
			b.append(operator.getValue());
		}
		else if (operator.getValue().equals(OPERATOR_BETWEEN))
			b.append(addBetweenStatement(field));
		else if (operator.getValue().equals(OPERATOR_IN) || operator.getValue().equals(OPERATOR_NOT_IN))
			b.append(addInStatement(field));
		else {
			if (!caseSensitive && (field.getDataType() == SearchFieldDataTypeEnum.STRING
					|| field.getDataType() == SearchFieldDataTypeEnum.CHAR || field.getDataType() == SearchFieldDataTypeEnum.ENUM)) {
				b.append("lower(");
				b.append(field.getColName());
				b.append(")");
			}
			else
				b.append(field.getColName());

			b.append(" ");
			b.append(operator.getValue());

			if (skipParameterBinding(field, operator))
				b.append(" '" + convertStringValue(field.getFilterCriteria(), exactMatch, caseSensitive) + "'");
			else {
				b.append(" :");
				b.append(PARAM);
				b.append(field.getColOrder());
				b.append("_1");
			}
		}

		return b.toString();
	}

	/**
	 * Add the query parameters of a given search field to the provided map
	 * @param params
	 * @param field
	 * @param searchDTO
	 * @throws ParseException if the parsing of a floating point or date value has failed
	 * @throws NumberFormatException if the conversion of an integer value has failed
	 * @throws IllegalArgumentException if the conversion of a UUID value has failed
	 */
	private static void addParameters(Map<String, Object> params, SearchFieldDTO field, SearchDTO searchDTO) throws ParseException {
		final SearchFieldDataTypeEnum dataType = field.getDataType();
		final SearchOperatorDTO operator = getOperator(field);
		final String[] paramValues;
		int paramIndex = 1;
		boolean exactMatch;
		String format;

		if (skipParameterBinding(field, operator))
			return;

		exactMatch = searchDTO.isExactFilterMatch() || operator.getValue().equals(OPERATOR_EQUAL);

		if (dataType == SearchFieldDataTypeEnum.GREGORIAN_CALENDAR || dataType == SearchFieldDataTypeEnum.DATE) {
			if (field.isDateTimeFormat())
				format = searchDTO.getDateTimeFormat();
			else
				format = searchDTO.getDateFormat();
		}
		else if (dataType == SearchFieldDataTypeEnum.LOCAL_DATE)
			format = searchDTO.getDateFormat();
		else if (dataType == SearchFieldDataTypeEnum.LOCAL_DATE_TIME)
			format = searchDTO.getDateTimeFormat();
		else
			format = searchDTO.getNumberFormat();

		if (operator.getValue().equals(OPERATOR_BETWEEN))
			paramValues = TOKEN_DELIMITER_BETWEEN_PATTERN.split(field.getFilterCriteria());
		else if (operator.getValue().equals(OPERATOR_IN) || operator.getValue().equals(OPERATOR_NOT_IN))
			paramValues = TOKEN_DELIMITER_IN_PATTERN.split(field.getFilterCriteria());
		else
			paramValues = new String[] { field.getFilterCriteria() };

		for (final String paramValue : paramValues) {
			if (dataType == SearchFieldDataTypeEnum.GREGORIAN_CALENDAR)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseCalendar(paramValue, format));
			else if (dataType == SearchFieldDataTypeEnum.DATE)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseDate(paramValue, format));
			else if (dataType == SearchFieldDataTypeEnum.FLOAT)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseFloat(paramValue, searchDTO));
			else if (dataType == SearchFieldDataTypeEnum.DOUBLE)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseDouble(paramValue, searchDTO));
			else if (dataType == SearchFieldDataTypeEnum.BIG_DECIMAL)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseBigDecimal(paramValue, searchDTO));
			else if (dataType == SearchFieldDataTypeEnum.INTEGER)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, Integer.parseInt(paramValue));
			else if (dataType == SearchFieldDataTypeEnum.LONG)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, Long.parseLong(paramValue));
			else if (dataType == SearchFieldDataTypeEnum.LOCAL_DATE)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseLocalDate(paramValue, format));
			else if (dataType == SearchFieldDataTypeEnum.LOCAL_DATE_TIME)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, parseLocalDateTime(paramValue, format));
			else if (dataType == SearchFieldDataTypeEnum.UUID_BINARY || dataType == SearchFieldDataTypeEnum.UUID_STRING)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, UUID.fromString(paramValue));
			else if (dataType == SearchFieldDataTypeEnum.BOOLEAN)
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, Boolean.valueOf(paramValue));
			else if (dataType == SearchFieldDataTypeEnum.STRING) {
				final String stringValue = convertStringValue(paramValue, exactMatch, searchDTO.isCaseSensitive());
				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, stringValue);
			}
			else if (dataType == SearchFieldDataTypeEnum.CHAR) {
				final String filterString = searchDTO.isCaseSensitive() ? paramValue : paramValue.toLowerCase();
				final char charValue = filterString.charAt(0);

				params.put(PARAM + field.getColOrder() + "_" + paramIndex++, charValue);
			}
		}
	}

	/**
	 * Determine if a search field provides appropriate data for creating a respective query fragment
	 * @param field
	 * @return true if the field should be processed
	 */
	private static boolean processField(SearchFieldDTO field) {
		if (field.getOperator() != null) {
			final SearchOperatorDTO op = field.getOperator();

			// For all operators that basically do not evaluate the filter criteria it is allowed that the respective field is null!
			// In all other cases the search field should not be processed!
			if (field.getFilterCriteria() == null && !op.getValue().equals(OPERATOR_IS_NULL)
					&& !op.getValue().equals(OPERATOR_IS_NOT_NULL))
				return false;
		}
		else if (field.getFilterCriteria() == null || field.getFilterCriteria().isEmpty())
			return false;

		return true;
	}

	/**
	 * @param field
	 * @return the 'in' statement for the given field
	 */
	private static String addInStatement(SearchFieldDTO field) {
		final var b = new StringBuilder();
		b.append(field.getColName());

		if (field.getOperator().getValue().equals(OPERATOR_IN))
			b.append(IN);
		else
			b.append(NOT_IN);

		final String[] tokens = TOKEN_DELIMITER_IN_PATTERN.split(field.getFilterCriteria());
		boolean isFirstToken = true;

		for (int tokenIndex = 0; tokenIndex < tokens.length; tokenIndex++) {
			if (isFirstToken)
				isFirstToken = false;
			else
				b.append(",");

			b.append(":" + PARAM + field.getColOrder() + "_" + (tokenIndex + 1));
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * @param field
	 * @return the 'between' statement for the given field
	 * @throws IllegalArgumentException if the filter criteria field doesn't contain the necessary value delimiter
	 */
	private static String addBetweenStatement(SearchFieldDTO field) {
		// Check if the filter criteria field contains the necessary value delimiter!
		if (!field.getFilterCriteria().contains(TOKEN_DELIMITER_BETWEEN)) {
			final var msg = "Cannot create between statement for field '" + field.getColLabel()
					+ "' as the necessary value delimiter is missing!";

			throw new IllegalArgumentException(msg);
		}

		final var b = new StringBuilder();
		b.append(field.getColName());
		b.append(BETWEEN);
		b.append(":");
		b.append(PARAM);
		b.append(field.getColOrder() + "_1");
		b.append(AND);
		b.append(":");
		b.append(PARAM);
		b.append(field.getColOrder() + "_2");

		return b.toString();
	}

	/**
	 * Determine if the filter criteria should be bound to a query parameter
	 * @param field
	 * @param operator
	 * @return true if the filter criteria should not be bound to a query parameter
	 */
	private static boolean skipParameterBinding(SearchFieldDTO field, SearchOperatorDTO operator) {
		return operator.getValue().equals(OPERATOR_IS_NULL) || operator.getValue().equals(OPERATOR_IS_NOT_NULL)
				|| field.getDataType() == SearchFieldDataTypeEnum.ENUM
				|| (field.getDataType() == SearchFieldDataTypeEnum.UUID_STRING || field.getDataType() == SearchFieldDataTypeEnum.CHAR
						&& (operator.getValue().equals(OPERATOR_LIKE) || operator.getValue().equals(OPERATOR_NOT_LIKE)));
	}

	/**
	 * Parse a string to produce a {@link GregorianCalendar}
	 * @param date the date value as string
	 * @param format
	 * @return the parsed {@link java.util.GregorianCalendar} value
	 * @throws ParseException if the provided date value could not be parsed
	 */
	private static GregorianCalendar parseCalendar(String date, String format) throws ParseException {
		final var dateFormat = new SimpleDateFormat(format);

		logger.debug("Parse string '{}' to produce a GregorianCalendar by using format '{}'", date, format);

		final Date paramDate = dateFormat.parse(date);

		final var calParam = new GregorianCalendar();
		calParam.setTime(paramDate);

		return calParam;
	}

	/**
	 * Parse a string to produce a {@link Date}
	 * @param date the date value as string
	 * @param format
	 * @return the parsed {@link java.util.Date} value
	 * @throws ParseException if the provided date value could not be parsed
	 */
	private static Date parseDate(String date, String format) throws ParseException {
		final var dateFormat = new SimpleDateFormat(format);

		logger.debug("Parse string '{}' to produce a Date by using format '{}'", date, format);

		return dateFormat.parse(date);
	}

	/**
	 * Parse a string to produce a {@link LocalDate}
	 * @param date the date value as string
	 * @param format
	 * @return the {@link LocalDate} value
	 */
	private static LocalDate parseLocalDate(String date, String format) {
		final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(format).withZone(ZoneId.systemDefault());

		logger.debug("Parse string '{}' to produce a LocalDate by using format '{}'", date, format);

		return LocalDate.from(dateFormat.parse(date));
	}

	/**
	 * Parse a string to produce a {@link LocalDateTime}
	 * @param date the date value as string
	 * @param format
	 * @return the {@link LocalDateTime} value
	 */
	private static LocalDateTime parseLocalDateTime(String date, String format) {
		final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(format).withZone(ZoneId.systemDefault());

		logger.debug("Parse string '{}' to produce a LocalDateTime by using format '{}'", date, format);

		return LocalDateTime.from(dateFormat.parse(date));
	}

	/**
	 * Parse a string to produce a float
	 * @param floatValue
	 * @param searchObj
	 * @return the parsed float value
	 * @throws ParseException if the provided float value could not be parsed
	 */
	private static float parseFloat(String floatValue, SearchDTO searchObj) throws ParseException {
		final var decimalSymbols = new DecimalFormatSymbols();
		decimalSymbols.setDecimalSeparator(searchObj.getDecimalSeparator());
		decimalSymbols.setGroupingSeparator(searchObj.getGroupingSeparator());

		final var decimalFormat = new DecimalFormat(searchObj.getNumberFormat(), decimalSymbols);

		logger.debug("Parse string '{}' to produce a float by using format '{}'", floatValue, searchObj.getNumberFormat());

		return decimalFormat.parse(floatValue).floatValue();
	}

	/**
	 * Parse a string to produce a double
	 * @param doubleValue
	 * @param searchObj
	 * @return the parsed double value
	 * @throws ParseException if the provided double value could not be parsed
	 */
	private static double parseDouble(String doubleValue, SearchDTO searchObj) throws ParseException {
		final var decimalSymbols = new DecimalFormatSymbols();
		decimalSymbols.setDecimalSeparator(searchObj.getDecimalSeparator());
		decimalSymbols.setGroupingSeparator(searchObj.getGroupingSeparator());

		final var decimalFormat = new DecimalFormat(searchObj.getNumberFormat(), decimalSymbols);

		logger.debug("Parse string '{}' to produce a double by using format '{}'", doubleValue, searchObj.getNumberFormat());

		return decimalFormat.parse(doubleValue).doubleValue();
	}

	/**
	 * Parse a string to produce a {@link BigDecimal}
	 * @param decimalValue
	 * @param searchObj
	 * @return the parsed {@link java.math.BigDecimal} value
	 * @throws ParseException if the provided big decimal value could not be parsed
	 */
	private static BigDecimal parseBigDecimal(String decimalValue, SearchDTO searchObj) throws ParseException {
		final var decimalSymbols = new DecimalFormatSymbols();
		decimalSymbols.setDecimalSeparator(searchObj.getDecimalSeparator());
		decimalSymbols.setGroupingSeparator(searchObj.getGroupingSeparator());

		final var decimalFormat = new DecimalFormat(searchObj.getNumberFormat(), decimalSymbols);
		decimalFormat.setParseBigDecimal(true);

		logger.debug("Parse string '{}' to produce a BigDecimal by using format '{}'", decimalValue, searchObj.getNumberFormat());

		return (BigDecimal) decimalFormat.parse(decimalValue);
	}

	/**
	 * @param value
	 * @param exactMatch
	 * @param caseSensitive
	 * @return the converted filter string
	 */
	private static String convertStringValue(String value, boolean exactMatch, boolean caseSensitive) {
		if (caseSensitive) {
			if (exactMatch)
				return value;

			return "%" + value + "%";
		}
		else if (exactMatch)
			return value.toLowerCase();

		return "%" + value.toLowerCase() + "%";
	}

	/**
	 * @param field
	 * @return the field's operator. If the operator is null a default parameter will be returned.
	 */
	private static SearchOperatorDTO getOperator(SearchFieldDTO field) {
		final SearchFieldDataTypeEnum dataType = field.getDataType();

		if (field.getOperator() == null) {
			if (dataType == SearchFieldDataTypeEnum.STRING || dataType == SearchFieldDataTypeEnum.UUID_STRING)
				return defaultOperatorLike;

			return defaultOperatorEqual;
		}

		return field.getOperator();
	}

}
