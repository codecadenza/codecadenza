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
import java.util.List;
import java.util.UUID;
import net.codecadenza.runtime.ddt.example.domain.CustomerDTO;
import net.codecadenza.runtime.ddt.example.service.CustomerService;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.service.data.FileBasedTestDataSource;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderFactory;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import net.codecadenza.runtime.search.dto.SearchInput;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;

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
@TestInstance(Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class CustomerServiceTest {
	private static final String XML_FILE_PATH = "src/test/resources/data/customer_testdata.xml";
	private static final UUID FIELD_ID = UUID.fromString("ddb63a49-26f7-4b9b-983b-f83403a309c7");
	private static final UUID FIELD_RATINGS = UUID.fromString("13256a2b-4fed-43f0-a46e-d7869ef073e2");

	private static TestDataProviderProperties properties;
	private static CustomerService customerService;

	/**
	 * Initialize the {@link ITestDataProvider} and the service that should be tested
	 */
	@BeforeAll
	static void init() {
		properties = new TestDataProviderProperties();
		properties.load();

		final ITestDataProvider testDataProvider = TestDataProviderFactory.getTestDataProvider(new File(XML_FILE_PATH), properties);
		final TestData testData = testDataProvider.loadTestData();

		customerService = new CustomerService(testData.getUserName(), testData.getPassword());
	}

	/**
	 * Test creating a new customer
	 * @param actualCustomer
	 * @param expectedCustomer
	 * @param testDataProvider
	 */
	@Order(1)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testCreateNewCustomer(CustomerDTO actualCustomer, CustomerDTO expectedCustomer, ITestDataProvider testDataProvider) {
		final var createdCustomer = customerService.createNewCustomer(actualCustomer);

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
	 * @param id
	 * @param expectedCustomer
	 */
	@Order(2)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testFetchCustomer(long id, CustomerDTO expectedCustomer) {
		final var fetchedCustomer = customerService.getCustomer(id);

		assertEquals(expectedCustomer.getCode(), fetchedCustomer.getCode());
		assertEquals(expectedCustomer.getName(), fetchedCustomer.getName());
		assertEquals(expectedCustomer.getZip(), fetchedCustomer.getZip());
		assertEquals(expectedCustomer.getCreditLimit(), fetchedCustomer.getCreditLimit());
		assertNotNull(expectedCustomer.getCountry());
	}

	/**
	 * Test updating an existing customer
	 * @param customer
	 * @param expectedCustomer
	 * @param testDataProvider
	 */
	@Order(3)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testUpdateCustomer(CustomerDTO customer, CustomerDTO expectedCustomer, ITestDataProvider testDataProvider) {
		final var updatedCustomer = assertTimeoutPreemptively(testDataProvider.getTimeout(),
				() -> customerService.updateCustomer(customer));

		assertTrue(expectedCustomer.getId() > 0);
		assertEquals(expectedCustomer.getId(), updatedCustomer.getId());
		assertEquals(expectedCustomer.getCode(), updatedCustomer.getCode());
	}

	/**
	 * Test searching for customer objects
	 * @param searchInput
	 * @param expectedList
	 * @param testDataProvider
	 */
	@Order(4)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testSearchCustomers(SearchInput searchInput, List<CustomerDTO> expectedList, ITestDataProvider testDataProvider) {
		searchInput.setDateFormat(properties.getDateFormat());
		searchInput.setDateTimeFormat(properties.getDateTimeFormat());
		searchInput.setNumberFormat(properties.getDecimalFormat());
		searchInput.setGroupingSeparator(properties.getGroupingSeparator());
		searchInput.setDecimalSeparator(properties.getDecimalSeparator());

		final var customerList = customerService.searchCustomers(searchInput);

		assertEquals(testDataProvider.getExpectedSize(), customerList.size());
		assertEquals(expectedList.getFirst().getName(), customerList.getFirst().getName());
		assertEquals(expectedList.getFirst().getZip(), customerList.getFirst().getZip());
	}

	/**
	 * Test saving a customer
	 * @param customers
	 * @param expectedList
	 * @param testDataProvider
	 */
	@Order(5)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testSaveCustomers(List<CustomerDTO> customers, List<CustomerDTO> expectedList, ITestDataProvider testDataProvider) {
		final var resultList = customerService.saveCustomers(customers);

		assertNull(testDataProvider.getTimeout());
		assertEquals(expectedList.size(), resultList.size());
	}

	/**
	 * Test deleting a customer
	 * @param id
	 * @param testDataProvider
	 */
	@Order(6)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testDeleteCustomer(long id, ITestDataProvider testDataProvider) {
		assertTimeoutPreemptively(testDataProvider.getTimeout(), () -> customerService.deleteCustomer(id));

		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnValue(String.class));
		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnListValue(String.class));

		// There should be no further invocation available!
		final var nonExistingInvocation = testDataProvider.getNextInvocation();

		assertNull(nonExistingInvocation);
		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnValue(String.class));
		assertThrows(IllegalStateException.class, () -> testDataProvider.getReturnListValue(String.class));
	}

}
