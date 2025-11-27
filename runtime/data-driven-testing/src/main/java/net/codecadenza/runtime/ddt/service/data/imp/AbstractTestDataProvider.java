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
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.codecadenza.runtime.conversion.ValueConverter;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.model.Parameter;
import net.codecadenza.runtime.ddt.model.ReturnValue;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.model.TestField;
import net.codecadenza.runtime.ddt.model.TestObject;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import net.codecadenza.runtime.ddt.service.data.util.MethodInvocationGroupIterator;
import net.codecadenza.runtime.ddt.service.data.util.ReferenceResolver;
import net.codecadenza.runtime.ddt.service.data.util.TestObjectInitizalizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class that can be used by all {@link ITestDataProvider} implementations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <I> the type of the identifier for {@link TestData} objects
 */
public abstract class AbstractTestDataProvider<I> implements ITestDataProvider {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	protected final Deque<MethodInvocation> methodInvocations = new ArrayDeque<>();
	protected final Deque<Parameter> parameters = new ArrayDeque<>();
	protected final I identifier;
	protected final TestObjectInitizalizer objectInitializer;
	protected TestData testData;
	protected MethodInvocation methodInvocation;
	protected Map<UUID, TestField> fieldsWithID = new HashMap<>();
	protected Map<UUID, List<TestField>> referencedFieldsMap = new HashMap<>();
	protected List<TestObject> objectValueReferences = new ArrayList<>();
	private boolean initialized;

	/**
	 * Constructor
	 * @param identifier the identifier of the {@link TestData} object
	 * @param properties the properties used to initialize the {@link ITestDataProvider}
	 */
	protected AbstractTestDataProvider(I identifier, final TestDataProviderProperties properties) {
		this.identifier = identifier;
		this.objectInitializer = new TestObjectInitizalizer(properties);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#getNextInvocation()
	 */
	@Override
	public MethodInvocation getNextInvocation() {
		if (testData == null) {
			testData = loadTestData();

			init(testData);
		}

		methodInvocation = methodInvocations.poll();

		if (methodInvocation != null) {
			logger.debug("Found next invocation {}::{}", methodInvocation.getClassName(), methodInvocation.getMethodName());

			parameters.clear();
			parameters.addAll(methodInvocation.getParameters());
		}

		return methodInvocation;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.data.ITestDataProvider#pushBackInvocation(net.codecadenza.runtime.ddt.model.
	 * MethodInvocation)
	 */
	@Override
	public void pushBackInvocation(MethodInvocation methodInvocation) {
		methodInvocations.addFirst(methodInvocation);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#getNextParameter(java.lang.Class)
	 */
	@Override
	public <T> T getNextParameter(Class<T> type) {
		final Parameter parameter = parameters.poll();

		if (parameter == null)
			throw new IllegalStateException("The next parameter could not be found!");

		logger.debug("Found next parameter '{}'", parameter.getName());

		if (parameter.getTestObjects().isEmpty())
			return TestObjectInitizalizer.getDefaultValue(type);
		else if (parameter.getTestObjects().size() > 1)
			throw new IllegalArgumentException("Multiple test objects found for parameter '" + parameter.getName() + "'");
		else
			return objectInitializer.initObject(parameter.getTestObjects().getFirst(), type);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#getNextListParameter(java.lang.Class)
	 */
	@Override
	public <T> List<T> getNextListParameter(Class<T> type) {
		final Parameter parameter = parameters.poll();

		if (parameter == null)
			throw new IllegalStateException("The next parameter could not be found!");

		logger.debug("Found next list parameter '{}'", parameter.getName());
		return convertTestDataToList(parameter.getTestObjects(), type);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#getReturnValue(java.lang.Class)
	 */
	@Override
	public <T> T getReturnValue(Class<T> type) {
		if (methodInvocation == null)
			throw new IllegalStateException("No method invocation available!");

		final ReturnValue returnValue = methodInvocation.getReturnValue();

		if (returnValue == null)
			throw new IllegalStateException("The return value could not be found!");
		else if (returnValue.getTestObjects().size() > 1)
			throw new IllegalArgumentException(
					"Multiple test objects found for return value of method '" + methodInvocation.getMethodName() + "'");
		else
			return objectInitializer.initObject(returnValue.getTestObjects().getFirst(), type);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#getReturnListValue(java.lang.Class)
	 */
	@Override
	public <T> List<T> getReturnListValue(Class<T> type) {
		if (methodInvocation == null)
			throw new IllegalStateException("No method invocation available!");

		final ReturnValue returnValue = methodInvocation.getReturnValue();

		if (returnValue == null)
			throw new IllegalStateException("The return value could not be found!");

		return convertTestDataToList(returnValue.getTestObjects(), type);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#setGeneratedFieldValue(java.util.UUID, java.lang.Object,
	 * java.lang.Class)
	 */
	@Override
	public <T> void setGeneratedFieldValue(UUID id, T value, Class<T> type) {
		final ValueConverter<T> valueConverter = objectInitializer.getValueConverter(type);
		final TestField testField = fieldsWithID.get(id);

		if (testField != null) {
			final String fieldValue = valueConverter.convertToString(value);
			logger.debug("Set value '{}' of field with ID '{}'", fieldValue, id);

			testField.setValue(fieldValue);

			if (referencedFieldsMap.containsKey(id))
				for (final TestField referencedField : referencedFieldsMap.get(id)) {
					logger.debug("Set value '{}' of referenced field with ID '{}'", fieldValue, id);
					referencedField.setValue(fieldValue);
				}
		}

		for (final TestObject testObject : objectValueReferences)
			if (id.equals(testObject.getReferencedFieldId())) {
				final String fieldValue = valueConverter.convertToString(value);
				logger.debug("Set value of object '{}' to '{}' using field with ID '{}'", testObject.getId(), fieldValue, id);

				testObject.setValue(fieldValue);
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#getExpectedSizeOfField(java.util.UUID)
	 */
	@Override
	public Integer getExpectedSizeOfField(UUID id) {
		final TestField testField = fieldsWithID.get(id);

		if (testField == null)
			return null;

		return testField.getExpectedSize();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.data.ITestDataProvider#getPostProcessingStatement()
	 */
	@Override
	public String getPostProcessingStatement() {
		if (methodInvocation == null)
			throw new IllegalStateException("No method invocation available!");

		return methodInvocation.getPostProcessingStatement();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.data.ITestDataProvider#getTimeout()
	 */
	@Override
	public Duration getTimeout() {
		if (methodInvocation == null)
			throw new IllegalStateException("No method invocation available!");

		return methodInvocation.getTimeout();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.data.ITestDataProvider#getGroupId()
	 */
	@Override
	public UUID getGroupId() {
		if (methodInvocation == null)
			throw new IllegalStateException("No method invocation available!");

		return methodInvocation.getGroupId();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.data.ITestDataProvider#getMethodInvocationGroupIterator()
	 */
	@Override
	public MethodInvocationGroupIterator getMethodInvocationGroupIterator() {
		return new MethodInvocationGroupIterator(this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.data.ITestDataProvider#getExpectedSize()
	 */
	@Override
	public Integer getExpectedSize() {
		if (methodInvocation == null)
			throw new IllegalStateException("No method invocation available!");

		return methodInvocation.getExpectedSize();
	}

	/**
	 * Initialize the provider by resolving all test object references
	 * @param testData
	 */
	protected void init(TestData testData) {
		if (initialized)
			return;

		final var referenceResolver = new ReferenceResolver(testData);
		referenceResolver.resolveReferences();

		fieldsWithID = referenceResolver.getFieldMap();
		referencedFieldsMap = referenceResolver.getReferencedFieldMap();
		objectValueReferences = referenceResolver.getObjectValueReferences();

		methodInvocations.addAll(testData.getMethodInvocations());

		initialized = true;
	}

	/**
	 * Convert the given list with test objects to a list using the requested type
	 * @param <T> the type of objects that should be contained in the list
	 * @param testObjects a list of test objects that should be converted
	 * @param type the requested return type
	 * @return the converted list
	 */
	private <T> List<T> convertTestDataToList(List<TestObject> testObjects, Class<T> type) {
		final List<T> objectList = new ArrayList<>();

		for (final TestObject testObject : testObjects)
			objectList.add(objectInitializer.initObject(testObject, type));

		return objectList;
	}

}
