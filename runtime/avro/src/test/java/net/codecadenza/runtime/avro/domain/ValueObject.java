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
package net.codecadenza.runtime.avro.domain;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * Domain object for testing different data types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ValueObject implements Serializable {
	private static final long serialVersionUID = -5608703583101725501L;

	private long id;
	private Long longValue;
	private String stringValue;
	private char characterValue;
	private Double doubleValue;
	private Float floatValue;
	private boolean booleanValue;
	private BigDecimal bigDecimalValue;
	private Date dateValue;
	private LocalDateTime dateTimeValue;
	private LocalDate localDateValue;
	private GregorianCalendar calendarValue;
	private ObjectStatus status;
	private ReferenceObject singleReference;
	private final List<ReferenceObject> references = new ArrayList<>();
	private final List<LocalDate> localDateList = new ArrayList<>();
	private final List<Date> dateList = new ArrayList<>();
	private final List<GregorianCalendar> calendarList = new ArrayList<>();
	private final List<LocalDateTime> dateTimeList = new ArrayList<>();
	private final Set<Character> characterSet = new HashSet<>();

	/**
	 * @return the id
	 */
	public long getId() {
		return this.id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(long id) {
		this.id = id;
	}

	/**
	 * @return the long value
	 */
	public Long getLongValue() {
		return this.longValue;
	}

	/**
	 * @param longValue the long value to set
	 */
	public void setLongValue(Long longValue) {
		this.longValue = longValue;
	}

	/**
	 * @return the string value
	 */
	public String getStringValue() {
		return this.stringValue;
	}

	/**
	 * @param stringValue the string value to set
	 */
	public void setStringValue(String stringValue) {
		this.stringValue = stringValue;
	}

	/**
	 * @return the character value
	 */
	public char getCharacterValue() {
		return this.characterValue;
	}

	/**
	 * @param charValue the character value to set
	 */
	public void setCharacterValue(char charValue) {
		this.characterValue = charValue;
	}

	/**
	 * @return the double value
	 */
	public Double getDoubleValue() {
		return this.doubleValue;
	}

	/**
	 * @param doubleValue the double value to set
	 */
	public void setDoubleValue(Double doubleValue) {
		this.doubleValue = doubleValue;
	}

	/**
	 * @return the float value
	 */
	public Float getFloatValue() {
		return this.floatValue;
	}

	/**
	 * @param floatValue the float value to set
	 */
	public void setFloatValue(Float floatValue) {
		this.floatValue = floatValue;
	}

	/**
	 * @return true if the boolean value is true
	 */
	public boolean getBooleanValue() {
		return this.booleanValue;
	}

	/**
	 * @param booleanValue the boolean value to set
	 */
	public void setBooleanValue(boolean booleanValue) {
		this.booleanValue = booleanValue;
	}

	/**
	 * @return the big decimal value
	 */
	public BigDecimal getBigDecimalValue() {
		return this.bigDecimalValue;
	}

	/**
	 * @param bigDecimalValue the big decimal value to set
	 */
	public void setBigDecimalValue(BigDecimal bigDecimalValue) {
		this.bigDecimalValue = bigDecimalValue;
	}

	/**
	 * @return the date value
	 */
	public Date getDateValue() {
		return this.dateValue;
	}

	/**
	 * @param dateValue the date value to set
	 */
	public void setDateValue(Date dateValue) {
		this.dateValue = dateValue;
	}

	/**
	 * @return the date time value
	 */
	public LocalDateTime getDateTimeValue() {
		return this.dateTimeValue;
	}

	/**
	 * @param dateTimeValue the date time value to set
	 */
	public void setDateTimeValue(LocalDateTime dateTimeValue) {
		this.dateTimeValue = dateTimeValue;
	}

	/**
	 * @return the local date value
	 */
	public LocalDate getLocalDateValue() {
		return this.localDateValue;
	}

	/**
	 * @param localDateValue the local date value to set
	 */
	public void setLocalDateValue(LocalDate localDateValue) {
		this.localDateValue = localDateValue;
	}

	/**
	 * @return the calendar value
	 */
	public GregorianCalendar getCalendarValue() {
		return this.calendarValue;
	}

	/**
	 * @param calendarValue the calendar value to set
	 */
	public void setCalendarValue(GregorianCalendar calendarValue) {
		this.calendarValue = calendarValue;
	}

	/**
	 * @return the status
	 */
	public ObjectStatus getStatus() {
		return status;
	}

	/**
	 * @param status the status to set
	 */
	public void setStatus(ObjectStatus status) {
		this.status = status;
	}

	/**
	 * @return a single reference object
	 */
	public ReferenceObject getSingleReference() {
		return this.singleReference;
	}

	/**
	 * @param singleReference the single reference object to set
	 */
	public void setSingleReference(ReferenceObject singleReference) {
		this.singleReference = singleReference;
	}

	/**
	 * @return a list of reference objects
	 */
	public List<ReferenceObject> getReferences() {
		return this.references;
	}

	/**
	 * @return a list of {@link LocalDate} values
	 */
	public List<LocalDate> getLocalDateList() {
		return this.localDateList;
	}

	/**
	 * @return a list of {@link Date} values
	 */
	public List<Date> getDateList() {
		return dateList;
	}

	/**
	 * @return the list of {@link GregorianCalendar} values
	 */
	public List<GregorianCalendar> getCalendarList() {
		return calendarList;
	}

	/**
	 * @return the list of {@link LocalDateTime} values
	 */
	public List<LocalDateTime> getDateTimeList() {
		return dateTimeList;
	}

	/**
	 * @return a set with characters
	 */
	public Set<Character> getCharacterSet() {
		return characterSet;
	}

}