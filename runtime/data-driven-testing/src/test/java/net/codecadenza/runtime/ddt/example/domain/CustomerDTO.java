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
package net.codecadenza.runtime.ddt.example.domain;

import java.util.List;

/**
 * <p>
 * Data transfer object for customers
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CustomerDTO {
	private long id;
	private String code;
	private String name;
	private String zip;
	private double creditLimit;
	private CountryDTO country;
	private CustomerStatus customerStatus;
	private List<String> tags;
	private Long[] ratings;

	/**
	 * @return the customer ID
	 */
	public long getId() {
		return id;
	}

	/**
	 * Set the customer ID
	 * @param id
	 */
	public void setId(long id) {
		this.id = id;
	}

	/**
	 * @return the code
	 */
	public String getCode() {
		return code;
	}

	/**
	 * Set the code
	 * @param code
	 */
	public void setCode(String code) {
		this.code = code;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Set the name
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the zip code
	 */
	public String getZip() {
		return zip;
	}

	/**
	 * Set the zip code
	 * @param zip
	 */
	public void setZip(String zip) {
		this.zip = zip;
	}

	/**
	 * @return the credit limit
	 */
	public double getCreditLimit() {
		return creditLimit;
	}

	/**
	 * Set the credit limit
	 * @param creditLimit
	 */
	public void setCreditLimit(double creditLimit) {
		this.creditLimit = creditLimit;
	}

	/**
	 * @return the country
	 */
	public CountryDTO getCountry() {
		return country;
	}

	/**
	 * Set the country
	 * @param country
	 */
	public void setCountry(CountryDTO country) {
		this.country = country;
	}

	/**
	 * @return the status
	 */
	public CustomerStatus getCustomerStatus() {
		return customerStatus;
	}

	/**
	 * Set the status
	 * @param customerStatus
	 */
	public void setCustomerStatus(CustomerStatus customerStatus) {
		this.customerStatus = customerStatus;
	}

	/**
	 * @return the list of tags
	 */
	public List<String> getTags() {
		return tags;
	}

	/**
	 * Set the tags
	 * @param tags
	 */
	public void setTags(List<String> tags) {
		this.tags = tags;
	}

	/**
	 * @return a rating list
	 */
	public Long[] getRatings() {
		return ratings;
	}

	/**
	 * Set the ratings
	 * @param ratings
	 */
	public void setRatings(Long[] ratings) {
		this.ratings = ratings;
	}

}
