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
package net.codecadenza.runtime.ddt.service.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import net.codecadenza.runtime.ddt.example.domain.CountryCollector;
import net.codecadenza.runtime.ddt.example.domain.CountryDTO;
import net.codecadenza.runtime.ddt.example.domain.CustomerDTO;
import net.codecadenza.runtime.ddt.example.domain.CustomerStatus;
import net.codecadenza.runtime.ddt.example.domain.IllegalPojo;
import net.codecadenza.runtime.ddt.example.domain.ValueObject;
import net.codecadenza.runtime.ddt.model.TestField;
import net.codecadenza.runtime.ddt.model.xml.XMLTestField;
import net.codecadenza.runtime.ddt.model.xml.XMLTestObject;
import net.codecadenza.runtime.ddt.service.TestDataProviderProperties;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the basic functionality of the {@link TestObjectInitizalizer}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class TestObjectInitizalizerTest {
	private static TestDataProviderProperties properties;
	private static TestObjectInitizalizer initializer;

	@BeforeAll
	static void init() {
		properties = new TestDataProviderProperties();
		properties.load();

		initializer = new TestObjectInitizalizer(properties);
	}

	@Test
	void testInitializationOfSimpleObject() {
		final var fieldCountryCode = new XMLTestField("code", "DE");
		final var fieldCountryName = new XMLTestField("name", "Germany");

		final var countryTestObject = new XMLTestObject();
		countryTestObject.setFields(List.of(fieldCountryCode, fieldCountryName));

		final var country = initializer.initObject(countryTestObject, CountryDTO.class);

		assertEquals(fieldCountryCode.getValue(), country.getCode());
		assertEquals(fieldCountryName.getValue(), country.getName());
	}

	@Test
	void testInitializationOfComplexObject() {
		final var fieldCountryCode = new XMLTestField("code", "DE");

		final var countryTestObject = new XMLTestObject();
		countryTestObject.setFields(List.of(fieldCountryCode));

		final var fieldCustomerCode = new XMLTestField("code", "CUSTOMER_CODE");
		final var fieldCustomerId = new XMLTestField("id", "1");
		final var fieldCustomerName = new XMLTestField("name", "CUSTOMER_NAME");
		final var fieldCustomerZip = new XMLTestField("zip", "ZIPCODE");
		final var fieldCustomerCreditLimit = new XMLTestField("creditLimit", "10,000.99");
		final var fieldCustomerStatus = new XMLTestField("customerStatus", CustomerStatus.NEW.toString());
		final var fieldCustomerTags = createTagsField();
		final var fieldCustomerRatings = createRatingsField();

		final var customerTestObject = new XMLTestObject();
		customerTestObject.setFields(List.of(fieldCustomerCode, fieldCustomerId, fieldCustomerCreditLimit, fieldCustomerTags,
				fieldCustomerName, fieldCustomerZip, fieldCustomerStatus, fieldCustomerRatings));

		final var customer = initializer.initObject(customerTestObject, CustomerDTO.class);

		assertEquals(fieldCustomerCode.getValue(), customer.getCode());
		assertEquals(fieldCustomerName.getValue(), customer.getName());
		assertEquals(fieldCustomerZip.getValue(), customer.getZip());
		assertEquals(fieldCustomerId.getValue(), Long.toString(customer.getId()));
	}

	@Test
	void testInitializationOfDefaultValues() {
		final var fieldBooleanValue = new XMLTestField("booleanValue");
		final var fieldCharacterValue = new XMLTestField("charValue");
		final var fieldIntegerValue = new XMLTestField("integerValue");
		final var fieldLongValue = new XMLTestField("longValue");
		final var fieldShortValue = new XMLTestField("shortValue");
		final var fieldDoubleValue = new XMLTestField("doubleValue");
		final var fieldFloatValue = new XMLTestField("floatValue");
		final var fieldByteValue = new XMLTestField("byteValue");
		final var fieldStringValue = new XMLTestField("stringValue");

		// By adding the fields without providing values the initializer is forced to determine the default values by itself!
		final var testObject = new XMLTestObject();
		testObject.setFields(List.of(fieldBooleanValue, fieldCharacterValue, fieldIntegerValue, fieldLongValue, fieldShortValue,
				fieldDoubleValue, fieldFloatValue, fieldByteValue, fieldStringValue));

		final var valueObject = initializer.initObject(testObject, ValueObject.class);
		assertEquals(0, valueObject.getIntegerValue());
		assertEquals(0, valueObject.getLongValue());
		assertEquals(0x0, valueObject.getByteValue());
		assertEquals(0, valueObject.getShortValue());
		assertEquals(0.0, valueObject.getDoubleValue());
		assertEquals(0.0, valueObject.getFloatValue());
		assertEquals('\u0000', valueObject.getCharValue());
		assertFalse(valueObject.isBooleanValue());
		assertNull(valueObject.getStringValue());
	}

	@Test
	void testInitializationOfSimpleFields() {
		final var fieldBooleanValue = new XMLTestField("booleanValue", "true");
		final var fieldCharacterValue = new XMLTestField("charValue", "a");
		final var fieldIntegerValue = new XMLTestField("integerValue", "100");
		final var fieldLongValue = new XMLTestField("longValue", "1000000");
		final var fieldShortValue = new XMLTestField("shortValue", "256");
		final var fieldDoubleValue = new XMLTestField("doubleValue", "123456.123");
		final var fieldFloatValue = new XMLTestField("floatValue", "123456.123");
		final var fieldByteValue = new XMLTestField("byteValue", "10");
		final var fieldStringValue = new XMLTestField("stringValue", "test");

		final var testObject = new XMLTestObject();
		testObject.setFields(List.of(fieldBooleanValue, fieldCharacterValue, fieldIntegerValue, fieldLongValue, fieldShortValue,
				fieldDoubleValue, fieldFloatValue, fieldByteValue, fieldStringValue));

		final var valueObject = initializer.initObject(testObject, ValueObject.class);
		assertEquals(100, valueObject.getIntegerValue());
		assertEquals(1000000, valueObject.getLongValue());
		assertEquals(0xA, valueObject.getByteValue());
		assertEquals(256, valueObject.getShortValue());
		assertEquals(123456.123, valueObject.getDoubleValue());
		assertEquals(123456.123, valueObject.getFloatValue(), 0.01);
		assertEquals('a', valueObject.getCharValue());
		assertTrue(valueObject.isBooleanValue());
		assertEquals("test", valueObject.getStringValue());
	}

	@Test
	void testCollectionHandling() {
		final var countryDE = new XMLTestObject();
		countryDE.setFields(List.of(new XMLTestField("code", "DE")));

		final var countryUS = new XMLTestObject();
		countryUS.setFields(List.of(new XMLTestField("code", "US")));

		final var countryDK = new XMLTestObject();
		countryDK.setFields(List.of(new XMLTestField("code", "DK")));

		final var fieldActiveCountries = new XMLTestField("activeCountries");
		fieldActiveCountries.setTestObjects(List.of(countryDE, countryUS, countryDK));

		final var countryAT = new XMLTestObject();
		countryAT.setFields(List.of(new XMLTestField("code", "AT")));

		final var countryGB = new XMLTestObject();
		countryGB.setFields(List.of(new XMLTestField("code", "GB")));

		final var fieldInactiveCountries = new XMLTestField("inactiveCountries");
		fieldInactiveCountries.setTestObjects(List.of(countryAT, countryGB));

		final var testObject = new XMLTestObject();
		testObject.setFields(List.of(fieldActiveCountries, fieldInactiveCountries));

		final var countryCollector = initializer.initObject(testObject, CountryCollector.class);
		assertEquals(3, countryCollector.getActiveCountries().size());
		assertEquals(2, countryCollector.getInactiveCountries().length);
	}

	@Test
	void testInitializationOfUnknownField() {
		final var fieldNonExisting = new XMLTestField("nonExisting");

		final var testObject = new XMLTestObject();
		testObject.getFields().add(fieldNonExisting);

		assertThrows(ObjectInitializationException.class, () -> initializer.initObject(testObject, ValueObject.class));
	}

	@Test
	void testInitializationOfIllegalField() {
		// It is not allowed to set the value and add test objects!
		final var fieldIllegalConfig = new XMLTestField("booleanValue", "true");
		fieldIllegalConfig.getTestObjects().add(new XMLTestObject());

		final var testObject = new XMLTestObject();
		testObject.getFields().add(fieldIllegalConfig);

		assertThrows(ObjectInitializationException.class, () -> initializer.initObject(testObject, ValueObject.class));
	}

	@Test
	void testInitializationOfFieldWithWrongSetterName() {
		final var testObject = new XMLTestObject();
		testObject.getFields().add(new XMLTestField("value", "test"));

		assertThrows(ObjectInitializationException.class, () -> initializer.initObject(testObject, IllegalPojo.class));
	}

	private TestField createTagsField() {
		final var firstTag = new XMLTestObject("low margin");
		final var secondTag = new XMLTestObject("quality problems");

		final var fieldTags = new XMLTestField("tags", null);
		fieldTags.setTestObjects(List.of(firstTag, secondTag));

		return fieldTags;
	}

	private TestField createRatingsField() {
		final var firstRating = new XMLTestObject("10");
		final var secondRating = new XMLTestObject("100");

		final var fieldRatings = new XMLTestField("ratings", null);
		fieldRatings.setTestObjects(List.of(firstRating, secondRating));

		return fieldRatings;
	}

}
