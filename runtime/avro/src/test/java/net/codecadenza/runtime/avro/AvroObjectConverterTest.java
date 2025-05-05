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
package net.codecadenza.runtime.avro;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import net.codecadenza.runtime.avro.domain.ObjectStatus;
import net.codecadenza.runtime.avro.domain.ReferenceObject;
import net.codecadenza.runtime.avro.domain.ValueObject;
import net.codecadenza.runtime.avro.util.AvroObjectConverter;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link AvroObjectConverter}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class AvroObjectConverterTest {
	private static final BigDecimal BIG_DECIMAL_VALUE = BigDecimal.valueOf(123.45);
	private static final char CHARACTER_VALUE = 'X';
	private static final GregorianCalendar CALENDAR_VALUE = new GregorianCalendar(2025, 3, 20);
	private static final double DOUBLE_VALUE = 123.45678;
	private static final Date DATE_VALUE = Date.from(CALENDAR_VALUE.toInstant());
	private static final float FLOAT_VALUE = 123.45f;
	private static final LocalDate LOCAL_DATE_VALUE = LocalDate.of(2025, 3, 20);
	private static final LocalDateTime LOCAL_DATE_TIME_VALUE = LocalDateTime.of(2025, 3, 20, 10, 11, 12);
	private static final Long LONG_VALUE = 987654321L;
	private static final long REF_OBJECT_ID = 12345678L;
	private static final String REF_OBJECT_NAME = "Reference object";
	private static final String STRING_VALUE = "A string value";
	private static final long VALUE_OBJECT_ID = 12345678L;

	@Test
	void testConvertToAvro() {
		final var referenceObject = new ReferenceObject();
		referenceObject.setId(REF_OBJECT_ID);
		referenceObject.setName(REF_OBJECT_NAME);

		final var valueObject = new ValueObject();
		valueObject.setId(VALUE_OBJECT_ID);
		valueObject.setBigDecimalValue(BIG_DECIMAL_VALUE);
		valueObject.setBooleanValue(true);
		valueObject.setCalendarValue(CALENDAR_VALUE);
		valueObject.setCharacterValue(CHARACTER_VALUE);
		valueObject.setDateTimeValue(LOCAL_DATE_TIME_VALUE);
		valueObject.setDateValue(DATE_VALUE);
		valueObject.setDoubleValue(DOUBLE_VALUE);
		valueObject.setFloatValue(FLOAT_VALUE);
		valueObject.setLocalDateValue(LOCAL_DATE_VALUE);
		valueObject.setLongValue(LONG_VALUE);
		valueObject.setStatus(ObjectStatus.OPEN);
		valueObject.setStringValue(STRING_VALUE);
		valueObject.setSingleReference(referenceObject);
		valueObject.getReferences().add(referenceObject);
		valueObject.getLocalDateList().add(LOCAL_DATE_VALUE);
		valueObject.getCalendarList().add(CALENDAR_VALUE);
		valueObject.getDateTimeList().add(LOCAL_DATE_TIME_VALUE);
		valueObject.getDateList().add(DATE_VALUE);
		valueObject.getCharacterSet().add(CHARACTER_VALUE);

		final var avroValueObject = AvroObjectConverter.toAvro(valueObject, net.codecadenza.runtime.avro.namespace.ValueObject.class);

		assertEquals(VALUE_OBJECT_ID, avroValueObject.getId());
		assertTrue(avroValueObject.getBooleanValue());
		assertEquals(BIG_DECIMAL_VALUE, avroValueObject.getBigDecimalValue());
		assertEquals(CALENDAR_VALUE.toInstant(), avroValueObject.getCalendarValue());
		assertEquals(String.valueOf(CHARACTER_VALUE), avroValueObject.getCharacterValue());
		assertEquals(DATE_VALUE.toInstant(), avroValueObject.getDateValue());
		assertEquals(LOCAL_DATE_VALUE, avroValueObject.getLocalDateValue());
		assertEquals(LOCAL_DATE_TIME_VALUE.atZone(ZoneId.systemDefault()).toInstant(), avroValueObject.getDateTimeValue());
		assertEquals(DOUBLE_VALUE, avroValueObject.getDoubleValue());
		assertEquals(FLOAT_VALUE, avroValueObject.getFloatValue());
		assertEquals(LONG_VALUE, avroValueObject.getLongValue());
		assertEquals(ObjectStatus.OPEN.name(), avroValueObject.getStatus().name());
		assertEquals(STRING_VALUE, avroValueObject.getStringValue());
		assertEquals(REF_OBJECT_ID, avroValueObject.getSingleReference().getId());
		assertEquals(REF_OBJECT_NAME, avroValueObject.getSingleReference().getName());
		assertEquals(1, avroValueObject.getLocalDateList().size());
		assertEquals(LOCAL_DATE_VALUE, valueObject.getLocalDateList().get(0));
		assertEquals(1, avroValueObject.getDateList().size());
		assertEquals(DATE_VALUE.toInstant(), avroValueObject.getDateList().get(0));
		assertEquals(1, avroValueObject.getCalendarList().size());
		assertEquals(CALENDAR_VALUE.toInstant(), avroValueObject.getCalendarList().get(0));
		assertEquals(1, avroValueObject.getDateTimeList().size());
		assertEquals(LOCAL_DATE_TIME_VALUE.atZone(ZoneId.systemDefault()).toInstant(), avroValueObject.getDateTimeList().get(0));
		assertEquals(1, avroValueObject.getCharacterSet().size());
		assertEquals(String.valueOf(CHARACTER_VALUE), avroValueObject.getCharacterSet().get(0));

		assertEquals(1, avroValueObject.getReferences().size());

		final var avroReferenceObject = avroValueObject.getReferences().get(0);
		assertEquals(REF_OBJECT_ID, avroReferenceObject.getId());
		assertEquals(REF_OBJECT_NAME, avroReferenceObject.getName());
	}

	@Test
	void testConvertToObject() {
		final var avroReferenceObject = new net.codecadenza.runtime.avro.namespace.ReferenceObject();
		avroReferenceObject.setId(REF_OBJECT_ID);
		avroReferenceObject.setName(REF_OBJECT_NAME);

		final var avroValueObject = new net.codecadenza.runtime.avro.namespace.ValueObject();
		avroValueObject.setId(VALUE_OBJECT_ID);
		avroValueObject.setBigDecimalValue(BIG_DECIMAL_VALUE);
		avroValueObject.setBooleanValue(true);
		avroValueObject.setCalendarValue(CALENDAR_VALUE.toInstant());
		avroValueObject.setCharacterValue(String.valueOf(CHARACTER_VALUE));
		avroValueObject.setDateTimeValue(LOCAL_DATE_TIME_VALUE.atZone(ZoneId.systemDefault()).toInstant());
		avroValueObject.setDateValue(DATE_VALUE.toInstant());
		avroValueObject.setDoubleValue(DOUBLE_VALUE);
		avroValueObject.setFloatValue(FLOAT_VALUE);
		avroValueObject.setLocalDateValue(LOCAL_DATE_VALUE);
		avroValueObject.setLongValue(LONG_VALUE);
		avroValueObject.setStatus(net.codecadenza.runtime.avro.namespace.ObjectStatus.OPEN);
		avroValueObject.setStringValue(STRING_VALUE);
		avroValueObject.setSingleReference(avroReferenceObject);
		avroValueObject.setLocalDateList(new ArrayList<>());
		avroValueObject.getLocalDateList().add(LOCAL_DATE_VALUE);
		avroValueObject.setDateList(new ArrayList<>());
		avroValueObject.getDateList().add(DATE_VALUE.toInstant());
		avroValueObject.setCalendarList(new ArrayList<>());
		avroValueObject.getCalendarList().add(CALENDAR_VALUE.toInstant());
		avroValueObject.setDateTimeList(new ArrayList<>());
		avroValueObject.getDateTimeList().add(LOCAL_DATE_TIME_VALUE.atZone(ZoneId.systemDefault()).toInstant());
		avroValueObject.setCharacterSet(new ArrayList<>());
		avroValueObject.getCharacterSet().add(String.valueOf(CHARACTER_VALUE));
		avroValueObject.setReferences(new ArrayList<>());
		avroValueObject.getReferences().add(avroReferenceObject);

		final var valueObject = AvroObjectConverter.toObject(avroValueObject, ValueObject.class);

		assertEquals(VALUE_OBJECT_ID, valueObject.getId());
		assertTrue(valueObject.getBooleanValue());
		assertEquals(BIG_DECIMAL_VALUE, valueObject.getBigDecimalValue());
		assertEquals(CALENDAR_VALUE.toInstant(),
				valueObject.getCalendarValue().toInstant().atZone(ZoneId.systemDefault()).toInstant());
		assertEquals(CHARACTER_VALUE, valueObject.getCharacterValue());
		assertEquals(DATE_VALUE, valueObject.getDateValue());
		assertEquals(LOCAL_DATE_VALUE, valueObject.getLocalDateValue());
		assertEquals(LOCAL_DATE_TIME_VALUE, valueObject.getDateTimeValue());
		assertEquals(DOUBLE_VALUE, valueObject.getDoubleValue());
		assertEquals(FLOAT_VALUE, valueObject.getFloatValue());
		assertEquals(LONG_VALUE, valueObject.getLongValue());
		assertEquals(ObjectStatus.OPEN.name(), valueObject.getStatus().name());
		assertEquals(STRING_VALUE, valueObject.getStringValue());
		assertEquals(REF_OBJECT_ID, valueObject.getSingleReference().getId());
		assertEquals(REF_OBJECT_NAME, valueObject.getSingleReference().getName());
		assertEquals(1, valueObject.getLocalDateList().size());
		assertEquals(LOCAL_DATE_VALUE, valueObject.getLocalDateList().get(0));
		assertEquals(1, valueObject.getDateList().size());
		assertEquals(DATE_VALUE, valueObject.getDateList().get(0));
		assertEquals(1, valueObject.getCalendarList().size());
		assertEquals(CALENDAR_VALUE.toInstant(),
				valueObject.getCalendarList().get(0).toInstant().atZone(ZoneId.systemDefault()).toInstant());
		assertEquals(1, valueObject.getDateTimeList().size());
		assertEquals(LOCAL_DATE_TIME_VALUE, valueObject.getDateTimeList().get(0));
		assertEquals(1, valueObject.getCharacterSet().size());
		assertTrue(valueObject.getCharacterSet().contains(CHARACTER_VALUE));
		assertEquals(1, valueObject.getReferences().size());

		final var referenceObject = valueObject.getReferences().get(0);
		assertEquals(REF_OBJECT_ID, referenceObject.getId());
		assertEquals(REF_OBJECT_NAME, referenceObject.getName());
	}
}
