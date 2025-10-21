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
package net.codecadenza.runtime.ddt.example.service;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.codecadenza.runtime.ddt.example.domain.CustomerDTO;
import net.codecadenza.runtime.search.dto.SearchInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Example service for handling customer objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CustomerService {
	private static final Map<Long, CustomerDTO> customers = new ConcurrentHashMap<>();
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * Constructor
	 * @param userName
	 * @param password
	 */
	public CustomerService(String userName, String password) {
		logger.debug("Creating service for '{}' with password '{}'", userName, password);
	}

	/**
	 * Create a new customer
	 * @param customer the customer to be created
	 * @return the created customer
	 */
	public CustomerDTO createNewCustomer(CustomerDTO customer) {
		final Long id = (long) customers.size() + 1;

		logger.debug("Create new customer with ID {}", id);

		customer.setId(id);
		customers.put(id, customer);

		return customer;
	}

	/**
	 * Get a customer with the given ID
	 * @param id the customer ID
	 * @return the customer
	 */
	public CustomerDTO getCustomer(long id) {
		logger.debug("Get customer with ID {}", id);

		return customers.get(id);
	}

	/**
	 * Update a customer
	 * @param customer the customer to update
	 * @return the updated customer
	 */
	public CustomerDTO updateCustomer(CustomerDTO customer) {
		logger.debug("Update customer with ID {}", customer.getId());

		customers.put(customer.getId(), customer);

		return customer;
	}

	/**
	 * Save customers
	 * @param customers a list of customers that should be saved
	 * @return the list of customers that have been saved
	 */
	public List<CustomerDTO> saveCustomers(List<CustomerDTO> customers) {
		final var resultList = new ArrayList<CustomerDTO>();

		logger.debug("Save {} customers", customers.size());

		CustomerDTO savedCustomer;

		for (final CustomerDTO customer : customers) {
			if (getCustomer(customer.getId()) != null)
				savedCustomer = updateCustomer(customer);
			else
				savedCustomer = createNewCustomer(customer);

			resultList.add(savedCustomer);
		}

		return resultList;
	}

	/**
	 * Search customers
	 * @param searchInput the filter criteria
	 * @return the customers that have been found
	 */
	@SuppressWarnings("unused")
	public List<CustomerDTO> searchCustomers(SearchInput searchInput) {
		logger.debug("Searching for customers");

		return customers.values().stream().toList();
	}

	/**
	 * Delete a customer
	 * @param id the customer ID
	 */
	public void deleteCustomer(long id) {
		logger.debug("Delete customer with ID {}", id);

		customers.remove(id);
	}

}
