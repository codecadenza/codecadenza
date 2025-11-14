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
package net.codecadenza.runtime.ddt.example;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.util.UUID;
import net.codecadenza.runtime.ddt.example.domain.CustomerDTO;
import net.codecadenza.runtime.ddt.example.service.CustomerService;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderFactory;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import net.codecadenza.runtime.search.dto.SearchInput;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test synchronous operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class CustomerServiceTest {
	private static final String XML_FILE_PATH = "src/test/resources/data/customer_testdata.xml";
	private static final UUID FIELD_ID = UUID.fromString("ddb63a49-26f7-4b9b-983b-f83403a309c7");
	private static final UUID FIELD_RATINGS = UUID.fromString("13256a2b-4fed-43f0-a46e-d7869ef073e2");

	private static TestDataProviderProperties properties;
	private static ITestDataProvider testDataProvider;
	private static CustomerService customerService;

	/**
	 * Initialize the {@link ITestDataProvider} and the service that should be tested
	 */
	@BeforeAll
	static void init() {
		properties = new TestDataProviderProperties();
		properties.load();

		testDataProvider = TestDataProviderFactory.getTestDataProvider(new File(XML_FILE_PATH), properties);

		final TestData testData = testDataProvider.loadTestData();

		customerService = new CustomerService(testData.getUserName(), testData.getPassword());
	}

	/**
	 * Test the functionality of the {@link CustomerService}
	 */
	@Test
	void testBasicWorkflow() {
		createCustomer();

		fetchCustomer();

		updateCustomer();

		searchCustomers();

		saveCustomers();

		deleteCustomer();
	}

	/**
	 * Test creating a new customer
	 */
	private void createCustomer() {
		testDataProvider.getNextInvocation();

		final var paramCustomer = testDataProvider.getNextParameter(CustomerDTO.class);
		final var expectedCustomer = testDataProvider.getReturnValue(CustomerDTO.class);

		final var createdCustomer = customerService.createNewCustomer(paramCustomer);

		final Integer expectedRatings = testDataProvider.getExpectedSizeOfField(FIELD_RATINGS);

		testDataProvider.setGeneratedFieldValue(FIELD_ID, createdCustomer.getId(), long.class);

		assertEquals(expectedRatings, createdCustomer.getRatings().length);
		assertEquals(expectedCustomer.getCode(), createdCustomer.getCode());
		assertEquals(expectedCustomer.getName(), createdCustomer.getName());
		assertEquals(expectedCustomer.getZip(), createdCustomer.getZip());
		assertEquals(expectedCustomer.getCreditLimit(), createdCustomer.getCreditLimit());
		assertNotNull(createdCustomer.getCountry());
	}

	/**
	 * Test fetching an existing customer
	 */
	private void fetchCustomer() {
		testDataProvider.getNextInvocation();

		final var expectedFetchedCustomer = testDataProvider.getReturnValue(CustomerDTO.class);
		final var paramId = testDataProvider.getNextParameter(long.class);
		final var fetchedCustomer = customerService.getCustomer(paramId);

		assertEquals(expectedFetchedCustomer.getCode(), fetchedCustomer.getCode());
		assertEquals(expectedFetchedCustomer.getName(), fetchedCustomer.getName());
		assertEquals(expectedFetchedCustomer.getZip(), fetchedCustomer.getZip());
		assertEquals(expectedFetchedCustomer.getCreditLimit(), fetchedCustomer.getCreditLimit());
		assertNotNull(expectedFetchedCustomer.getCountry());
	}

	/**
	 * Test updating an existing customer
	 */
	private void updateCustomer() {
		final var updateInvocation = testDataProvider.getNextInvocation();

		final var paramCustomer = testDataProvider.getNextParameter(CustomerDTO.class);
		final var expectedCustomerUpdate = testDataProvider.getReturnValue(CustomerDTO.class);

		final var updatedCustomer = assertTimeoutPreemptively(updateInvocation.getTimeout(),
				() -> customerService.updateCustomer(paramCustomer));

		assertTrue(expectedCustomerUpdate.getId() > 0);
		assertEquals(expectedCustomerUpdate.getId(), updatedCustomer.getId());
		assertEquals(expectedCustomerUpdate.getCode(), updatedCustomer.getCode());
	}

	/**
	 * Test searching for customer objects
	 */
	private void searchCustomers() {
		final MethodInvocation invocation = testDataProvider.getNextInvocation();

		final var paramSearchInput = testDataProvider.getNextParameter(SearchInput.class);
		paramSearchInput.setDateFormat(properties.getDateFormat());
		paramSearchInput.setDateTimeFormat(properties.getDateTimeFormat());
		paramSearchInput.setNumberFormat(properties.getDecimalFormat());
		paramSearchInput.setGroupingSeparator(properties.getGroupingSeparator());
		paramSearchInput.setDecimalSeparator(properties.getDecimalSeparator());

		final var expectedResult = testDataProvider.getReturnListValue(CustomerDTO.class);

		final var customerList = customerService.searchCustomers(paramSearchInput);

		assertEquals(invocation.getExpectedSize(), customerList.size());
		assertEquals(expectedResult.getFirst().getName(), customerList.getFirst().getName());
		assertEquals(expectedResult.getFirst().getZip(), customerList.getFirst().getZip());
	}

	/**
	 * Test saving a customer
	 */
	private void saveCustomers() {
		final var invocation = testDataProvider.getNextInvocation();
		final var paramCustomers = testDataProvider.getNextListParameter(CustomerDTO.class);
		final var expectedList = testDataProvider.getReturnListValue(CustomerDTO.class);

		final var resultList = customerService.saveCustomers(paramCustomers);

		assertNull(invocation.getTimeout());
		assertEquals(expectedList.size(), resultList.size());
	}

	/**
	 * Test deleting a customer
	 */
	private void deleteCustomer() {
		final var invocation = testDataProvider.getNextInvocation();
		final var paramId = testDataProvider.getNextParameter(long.class);

		assertTimeoutPreemptively(invocation.getTimeout(), () -> customerService.deleteCustomer(paramId));

		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnValue(String.class));
		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnListValue(String.class));
		assertThrows(IllegalStateException.class, () -> testDataProvider.getNextParameter(String.class));
		assertThrows(IllegalStateException.class, () -> testDataProvider.getNextListParameter(String.class));

		// There should be no further invocation available!
		final var nonExistingInvocation = testDataProvider.getNextInvocation();

		assertNull(nonExistingInvocation);
		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnValue(String.class));
		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnListValue(String.class));
	}

}
