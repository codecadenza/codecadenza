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
package net.codecadenza.runtime.ddt.service.data.imp;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderFactory;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import net.codecadenza.runtime.ddt.service.data.util.MethodInvocationGroupIterator;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Provider for the arguments of a {@link ParameterizedTest} based on the test data of a {@link ITestDataProvider}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <I> the type of the identifier to find the correct test data of a specific test case
 */
public abstract class AbstractTestDataArgumentsProvider<I> implements ArgumentsProvider {
	private static final ExtensionContext.Namespace NAMESPACE = ExtensionContext.Namespace
			.create(AbstractTestDataArgumentsProvider.class);
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	protected String identifierString;

	/*
	 * (non-Javadoc)
	 * @see org.junit.jupiter.params.provider.ArgumentsProvider#provideArguments(org.junit.jupiter.api.extension.ExtensionContext)
	 */
	@Override
	public Stream<? extends Arguments> provideArguments(ExtensionContext context) throws Exception {
		final ITestDataProvider testDataProvider = getSharedProvider(context);
		final Method testMethod = context.getRequiredTestMethod();
		final Parameter[] testMethodParameters = testMethod.getParameters();
		final List<Arguments> argumentsList = new ArrayList<>();
		final MethodInvocationGroupIterator invocationGroupIterator = testDataProvider.getMethodInvocationGroupIterator();
		final int numberOfParameters = invocationGroupIterator.getCurrentInvocation().getParameters().size();

		// Collect all invocations that belong to the same group
		while (invocationGroupIterator.hasNext()) {
			argumentsList.add(createArguments(testDataProvider, testMethodParameters, numberOfParameters));

			invocationGroupIterator.next();
		}

		if (argumentsList.isEmpty()) {
			// Create a single list of arguments as there are no nested invocations!
			argumentsList.add(createArguments(testDataProvider, testMethodParameters, numberOfParameters));

			logger.debug("Created arguments for a single invocation of test method '{}'", testMethod.getName());
		}
		else
			logger.debug("Created arguments for {} invocation(s) of test method '{}'", argumentsList.size(), testMethod.getName());

		return argumentsList.stream().map(arg -> {
			// Always load the next invocation before the actual test method will be executed!
			testDataProvider.getNextInvocation();
			return arg;
		});
	}

	/**
	 * @return the created identifier
	 */
	protected abstract I createIdentifier();

	/**
	 * @param testDataProvider the test data provider
	 * @param testMethodParameters an array containing the test method parameters
	 * @param numberOfParameters the number of the required method parameters of the method that is invoked during the test
	 * @return the arguments for a {@link ParameterizedTest}
	 */
	protected Arguments createArguments(final ITestDataProvider testDataProvider, final Parameter[] testMethodParameters,
			final int numberOfParameters) {
		final Object[] args = new Object[testMethodParameters.length];

		// Fill the array with method parameters from the test data provider
		for (int paramIndex = 0; paramIndex < numberOfParameters; paramIndex++) {
			final Parameter param = testMethodParameters[paramIndex];
			final Class<?> type = param.getType();

			logger.trace("Preparing argument for parameter '{}' of type '{}'", param.getName(), type.getName());

			if (List.class.isAssignableFrom(type))
				args[paramIndex] = testDataProvider.getNextListParameter(getGenericType(param));
			else
				args[paramIndex] = testDataProvider.getNextParameter(type);
		}

		final int additionalParameters = args.length - numberOfParameters;

		// Add optional parameters like the expected return value and the test data provider
		for (int addIndex = 0; addIndex < additionalParameters; addIndex++) {
			final int paramIndex = numberOfParameters + addIndex;
			final Parameter param = testMethodParameters[paramIndex];
			final Class<?> type = testMethodParameters[paramIndex].getType();

			logger.trace("Preparing argument for parameter '{}' of type '{}'", param.getName(), type.getName());

			if (type.isAssignableFrom(ITestDataProvider.class))
				args[paramIndex] = testDataProvider;
			else if (List.class.isAssignableFrom(type))
				args[paramIndex] = testDataProvider.getReturnListValue(getGenericType(param));
			else
				args[paramIndex] = testDataProvider.getReturnValue(type);
		}

		return Arguments.of(args);
	}

	/**
	 * Get the shared {@link ITestDataProvider}
	 * @param context the context in which the test is being executed
	 * @return the shared {@link ITestDataProvider}
	 */
	protected ITestDataProvider getSharedProvider(ExtensionContext context) {
		// Use the root context to ensure sharing across all tests in the run
		final ExtensionContext.Store store = context.getRoot().getStore(NAMESPACE);

		// Create and store the instance only if it doesn't exist
		return store.getOrComputeIfAbsent(identifierString, _ -> {
			final var properties = new TestDataProviderProperties();
			properties.load();

			final var identifier = createIdentifier();
			final ITestDataProvider testDataProvider = TestDataProviderFactory.getTestDataProvider(identifier, properties);

			// Load data only once when the provider is first created
			testDataProvider.loadTestData();
			return testDataProvider;
		}, ITestDataProvider.class);
	}

	/**
	 * Get the generic type of the given method parameter
	 * @param param the method parameter
	 * @return the generic type
	 */
	protected Class<?> getGenericType(Parameter param) {
		try {
			final ParameterizedType pt = (ParameterizedType) param.getParameterizedType();
			return (Class<?>) pt.getActualTypeArguments()[0];
		}
		catch (final Exception e) {
			logger.warn("Could not determine generic type of parameter '{}'", param.getName(), e);
			return Object.class;
		}
	}

}
