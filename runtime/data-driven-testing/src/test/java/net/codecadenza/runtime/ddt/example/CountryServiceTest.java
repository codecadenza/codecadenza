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
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.service.completion.IInvocationCompletionHandler;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerFactory;
import net.codecadenza.runtime.ddt.service.completion.InvocationCompletionHandlerProperties;
import net.codecadenza.runtime.ddt.service.data.FileBasedTestDataSource;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderFactory;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import net.codecadenza.runtime.ddt.service.preparation.IStatementProcessor;
import net.codecadenza.runtime.ddt.service.preparation.StatementProcessorFactory;
import net.codecadenza.runtime.ddt.service.preparation.StatementProcessorProperties;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;

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
@TestInstance(Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class CountryServiceTest {
	private static final String XML_FILE_PATH = "src/test/resources/data/country_testdata.xml";

	private static IInvocationCompletionHandler completionHandler;
	private static IStatementProcessor statementProcessor;
	private static CountryService countryService;
	private static TestData testData;

	/**
	 * Initialize the {@link ITestDataProvider} and the service that should be tested
	 */
	@BeforeAll
	static void init() {
		final var testDataProviderProperties = new TestDataProviderProperties();
		testDataProviderProperties.load();

		final var testDataProvider = TestDataProviderFactory.getTestDataProvider(new File(XML_FILE_PATH), testDataProviderProperties);
		testData = testDataProvider.loadTestData();

		final var invocationHandlerProperties = new InvocationCompletionHandlerProperties();
		invocationHandlerProperties.load();

		completionHandler = InvocationCompletionHandlerFactory.getPostProcessor(invocationHandlerProperties);

		countryService = new CountryService(invocationHandlerProperties.getResourceURL(), invocationHandlerProperties.getUserName(),
				invocationHandlerProperties.getPassword());

		final var statementProcessorProperties = new StatementProcessorProperties();
		statementProcessorProperties.load();

		statementProcessor = StatementProcessorFactory.getStatementProcessor(statementProcessorProperties);
		statementProcessor.executeStatements(testData.getPreProcessingStatements());
	}

	/**
	 * Run all post-processing commands
	 */
	@AfterAll
	static void cleanUp() {
		if (statementProcessor != null)
			statementProcessor.executeStatements(testData.getPostProcessingStatements());
	}

	/**
	 * Test creating a new country
	 * @param country
	 * @param testDataProvider
	 */
	@Order(1)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testCreateCountry(CountryDTO country, ITestDataProvider testDataProvider) {
		countryService.createCountry(country);

		completionHandler.waitForFinish(testDataProvider.getPostProcessingStatement(), country.getName());
	}

	/**
	 * Test updating a country
	 * @param country
	 * @param testDataProvider
	 */
	@Order(2)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testUpdateCountry(CountryDTO country, ITestDataProvider testDataProvider) {
		countryService.updateCountry(country);

		completionHandler.waitForFinish(testDataProvider.getPostProcessingStatement(), country.getName());
	}

	/**
	 * Test deleting a country
	 * @param code
	 * @param testDataProvider
	 */
	@Order(3)
	@ParameterizedTest
	@FileBasedTestDataSource(XML_FILE_PATH)
	void testDeleteCountry(String code, ITestDataProvider testDataProvider) {
		final Duration completionTimeout = testDataProvider.getTimeout();

		countryService.deleteCountry(code);

		completionHandler.waitForFinish(testDataProvider.getPostProcessingStatement(), completionTimeout, code);
	}

}
