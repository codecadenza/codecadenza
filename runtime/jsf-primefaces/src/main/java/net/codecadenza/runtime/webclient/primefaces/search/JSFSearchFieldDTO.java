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
package net.codecadenza.runtime.webclient.primefaces.search;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.Date;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;

/**
 * <p>
 * JSF adapter for search fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFSearchFieldDTO extends SearchFieldDTO {
	private static final long serialVersionUID = 178233912071735138L;

	private String stringCriterion;
	private String stringBetweenCriterion;
	private SearchDTO searchObj;
	private Date dateCriterion;
	private Date dateBetweenCriterion;
	private Long integerCriterion;
	private Long integerBetweenCriterion;
	private Double doubleCriterion;
	private Double doubleBetweenCriterion;
	private BigDecimal bigDecimalCriterion;
	private BigDecimal bigDecimalBetweenCriterion;
	private boolean between;

	/**
	 * Constructor
	 * @param searchObj
	 * @param colDisplayOrder the display order of the column
	 * @param colName the name of property which is used in the select clause of the query
	 * @param colLabel the displayed name of the column
	 * @param dataType the data type
	 * @param width the width of the column
	 * @param dateTimeFormat controls if a temporal field should use either a date or a date time format pattern to format the
	 *          corresponding values
	 */
	public JSFSearchFieldDTO(SearchDTO searchObj, int colDisplayOrder, String colName, String colLabel,
			SearchFieldDataTypeEnum dataType, int width, boolean dateTimeFormat) {
		super(colDisplayOrder, colName, colLabel, dataType, width);

		setOperator(JSFSearchOperatorHelper.getDefaultOperator());
		setDateTimeFormat(dateTimeFormat);

		searchObj.getSearchFields().add(this);

		this.between = false;
		this.searchObj = searchObj;
	}

	/**
	 * Constructor
	 * @param searchObj
	 * @param colDisplayOrder the display order of the column
	 * @param colName the name of property which is used in the select clause of the query
	 * @param colLabel the displayed name of the column
	 * @param dataType the data type
	 * @param width the width of the column
	 */
	public JSFSearchFieldDTO(SearchDTO searchObj, int colDisplayOrder, String colName, String colLabel,
			SearchFieldDataTypeEnum dataType, int width) {
		this(searchObj, colDisplayOrder, colName, colLabel, dataType, width, true);
	}

	/**
	 * Constructor
	 */
	public JSFSearchFieldDTO() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.search.dto.SearchFieldDTO#getFilterCriteria()
	 */
	@Override
	public String getFilterCriteria() {
		if (hasTemporalDataType()) {
			var filterCriteria = "";

			if (dateCriterion == null)
				return filterCriteria;

			if (isDateTimeFormat())
				filterCriteria = new SimpleDateFormat(searchObj.getDateTimeFormat()).format(dateCriterion);
			else
				filterCriteria = new SimpleDateFormat(searchObj.getDateFormat()).format(dateCriterion);

			if (getOperator() != null && getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN)) {
				filterCriteria += SearchService.TOKEN_DELIMITER_BETWEEN;

				if (isDateTimeFormat())
					filterCriteria += new SimpleDateFormat(searchObj.getDateTimeFormat()).format(dateBetweenCriterion);
				else
					filterCriteria += new SimpleDateFormat(searchObj.getDateFormat()).format(dateBetweenCriterion);
			}

			return filterCriteria;
		}
		else if (getDataType() == SearchFieldDataTypeEnum.INTEGER || getDataType() == SearchFieldDataTypeEnum.LONG) {
			if (integerCriterion == null)
				return "";

			String filterCriteria = Long.toString(integerCriterion);

			if (getOperator() != null && getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN)) {
				filterCriteria += SearchService.TOKEN_DELIMITER_BETWEEN;
				filterCriteria += integerBetweenCriterion;
			}

			return filterCriteria;
		}
		else if (getDataType() == SearchFieldDataTypeEnum.STRING || getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
				|| getDataType() == SearchFieldDataTypeEnum.UUID_STRING) {
			if (stringCriterion == null)
				return "";

			String filterCriteria = stringCriterion;

			if (getOperator() != null && getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN)) {
				filterCriteria += SearchService.TOKEN_DELIMITER_BETWEEN;
				filterCriteria += stringBetweenCriterion;
			}

			return filterCriteria;
		}
		else if (getDataType() == SearchFieldDataTypeEnum.DOUBLE || getDataType() == SearchFieldDataTypeEnum.FLOAT) {
			if (doubleCriterion == null)
				return "";

			final var decimalSymbols = new DecimalFormatSymbols();
			decimalSymbols.setDecimalSeparator(searchObj.getDecimalSeparator());
			decimalSymbols.setGroupingSeparator(searchObj.getGroupingSeparator());

			final var df = new DecimalFormat(searchObj.getNumberFormat(), decimalSymbols);

			String filterCriteria = df.format(doubleCriterion);

			if (getOperator() != null && getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN)) {
				filterCriteria += SearchService.TOKEN_DELIMITER_BETWEEN;
				filterCriteria += df.format(doubleBetweenCriterion);
			}

			return filterCriteria;
		}
		else if (getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL) {
			if (bigDecimalCriterion == null)
				return "";

			final var decimalSymbols = new DecimalFormatSymbols();
			decimalSymbols.setDecimalSeparator(searchObj.getDecimalSeparator());
			decimalSymbols.setGroupingSeparator(searchObj.getGroupingSeparator());

			final var df = new DecimalFormat(searchObj.getNumberFormat(), decimalSymbols);

			String filterCriteria = df.format(bigDecimalCriterion);

			if (getOperator() != null && getOperator().getValue().equals(SearchService.OPERATOR_BETWEEN)) {
				filterCriteria += SearchService.TOKEN_DELIMITER_BETWEEN;
				filterCriteria += df.format(bigDecimalBetweenCriterion);
			}

			return filterCriteria;
		}

		return stringCriterion;
	}

	/**
	 * @return the string criterion
	 */
	public String getStringCriterion() {
		return stringCriterion;
	}

	/**
	 * @param stringCriterion
	 */
	public void setStringCriterion(String stringCriterion) {
		this.stringCriterion = stringCriterion;
	}

	/**
	 * @return the date criterion
	 */
	public Date getDateCriterion() {
		return dateCriterion;
	}

	/**
	 * @param dateCriterion
	 */
	public void setDateCriterion(Date dateCriterion) {
		this.dateCriterion = dateCriterion;
	}

	/**
	 * @return the date criterion that is used for 'between' operations
	 */
	public Date getDateBetweenCriterion() {
		return dateBetweenCriterion;
	}

	/**
	 * @param dateBetweenCriterion
	 */
	public void setDateBetweenCriterion(Date dateBetweenCriterion) {
		this.dateBetweenCriterion = dateBetweenCriterion;
	}

	/**
	 * @return the integer criterion
	 */
	public Long getIntegerCriterion() {
		return integerCriterion;
	}

	/**
	 * @param integerCriterion
	 */
	public void setIntegerCriterion(Long integerCriterion) {
		this.integerCriterion = integerCriterion;
	}

	/**
	 * @return the integer criterion that is used for 'between' operations
	 */
	public Long getIntegerBetweenCriterion() {
		return integerBetweenCriterion;
	}

	/**
	 * @param integerBetweenCriterion
	 */
	public void setIntegerBetweenCriterion(Long integerBetweenCriterion) {
		this.integerBetweenCriterion = integerBetweenCriterion;
	}

	/**
	 * @return the double criterion
	 */
	public Double getDoubleCriterion() {
		return doubleCriterion;
	}

	/**
	 * @param doubleCriterion
	 */
	public void setDoubleCriterion(Double doubleCriterion) {
		this.doubleCriterion = doubleCriterion;
	}

	/**
	 * @return the double criterion that is used for 'between' operations
	 */
	public Double getDoubleBetweenCriterion() {
		return doubleBetweenCriterion;
	}

	/**
	 * @param doubleBetweenCriterion
	 */
	public void setDoubleBetweenCriterion(Double doubleBetweenCriterion) {
		this.doubleBetweenCriterion = doubleBetweenCriterion;
	}

	/**
	 * @return true if the 'between' operator is used
	 */
	public boolean isBetween() {
		return between;
	}

	/**
	 * @param between
	 */
	public void setBetween(boolean between) {
		this.between = between;
	}

	/**
	 * @return the string criterion that is used for 'between' operations
	 */
	public String getStringBetweenCriterion() {
		return stringBetweenCriterion;
	}

	/**
	 * @param stringBetweenCriterion
	 */
	public void setStringBetweenCriterion(String stringBetweenCriterion) {
		this.stringBetweenCriterion = stringBetweenCriterion;
	}

	/**
	 * @return the BigDecimal criterion
	 */
	public BigDecimal getBigDecimalCriterion() {
		return bigDecimalCriterion;
	}

	/**
	 * @param bigDecimalCriterion
	 */
	public void setBigDecimalCriterion(BigDecimal bigDecimalCriterion) {
		this.bigDecimalCriterion = bigDecimalCriterion;
	}

	/**
	 * @return the BigDecimal criterion that is used for between-operations
	 */
	public BigDecimal getBigDecimalBetweenCriterion() {
		return bigDecimalBetweenCriterion;
	}

	/**
	 * @param bigDecimalBetweenCriterion
	 */
	public void setBigDecimalBetweenCriterion(BigDecimal bigDecimalBetweenCriterion) {
		this.bigDecimalBetweenCriterion = bigDecimalBetweenCriterion;
	}

}
