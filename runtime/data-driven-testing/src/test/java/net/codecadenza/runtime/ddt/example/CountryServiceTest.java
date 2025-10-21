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

import java.io.File;
import java.time.Duration;
import net.codecadenza.runtime.ddt.example.domain.CountryDTO;
import net.codecadenza.runtime.ddt.example.service.CountryService;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.service.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.TestDataProviderFactory;
import net.codecadenza.runtime.ddt.service.TestDataProviderProperties;
import net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerFactory;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerProperties;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test asynchronous operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class CountryServiceTest {
	private static final String XML_FILE_PATH = "src/test/resources/data/country_testdata.xml";

	private static ITestDataProvider testDataProvider;
	private static IInvocationCompletionHandler completionHandler;
	private static CountryService countryService;

	/**
	 * Initialize the {@link ITestDataProvider} and the service that should be tested
	 */
	@BeforeAll
	static void init() {
		final var testDataProviderProperties = new TestDataProviderProperties();
		testDataProviderProperties.load();

		testDataProvider = TestDataProviderFactory.getTestDataProvider(new File(XML_FILE_PATH), testDataProviderProperties);
		testDataProvider.loadTestData();

		final var invocationHandlerProperties = new InvocationCompletionHandlerProperties();
		invocationHandlerProperties.load();

		completionHandler = InvocationCompletionHandlerFactory.getPostProcessor(invocationHandlerProperties);
		countryService = new CountryService(invocationHandlerProperties.getResourceURL(), invocationHandlerProperties.getUserName(),
				invocationHandlerProperties.getPassword());
	}

	/**
	 * Test the functionality of the {@link CountryService}
	 */
	@Test
	void testBasicWorkflow() {
		MethodInvocation invocation = testDataProvider.getNextInvocation();

		while (true) {
			final var groupId = invocation.getGroupId();

			createCountry(invocation);

			invocation = testDataProvider.getNextInvocation();

			if (!groupId.equals(invocation.getGroupId()))
				break;
		}

		updateCountry(invocation);

		deleteCountry(testDataProvider.getNextInvocation());
	}

	/**
	 * Test creating a new country
	 * @param invocation
	 */
	private void createCountry(MethodInvocation invocation) {
		final var paramCountry = testDataProvider.getNextParameter(CountryDTO.class);

		countryService.createCountry(paramCountry);

		completionHandler.waitForFinish(invocation.getPostProcessingStatement(), paramCountry.getName());
	}

	/**
	 * Test updating a country
	 * @param invocation
	 */
	private void updateCountry(MethodInvocation invocation) {
		final var paramCountry = testDataProvider.getNextParameter(CountryDTO.class);

		countryService.updateCountry(paramCountry);

		completionHandler.waitForFinish(invocation.getPostProcessingStatement(), paramCountry.getName());
	}

	/**
	 * Test deleting a country
	 * @param invocation
	 */
	private void deleteCountry(MethodInvocation invocation) {
		final Duration completionTimeout = invocation.getTimeout();
		final var paramCode = testDataProvider.getNextParameter(String.class);

		countryService.deleteCountry(paramCode);

		completionHandler.waitForFinish(invocation.getPostProcessingStatement(), completionTimeout, paramCode);
	}

}
