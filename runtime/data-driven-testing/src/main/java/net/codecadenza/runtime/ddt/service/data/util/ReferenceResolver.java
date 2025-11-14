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
package net.codecadenza.runtime.ddt.service.data.util;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.model.Parameter;
import net.codecadenza.runtime.ddt.model.ReturnValue;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.model.TestField;
import net.codecadenza.runtime.ddt.model.TestObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class that fills test objects with the fields provided by respective reference objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReferenceResolver {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final Map<UUID, TestObject> objectMap = new HashMap<>();
	private final Map<UUID, TestField> fieldMap = new HashMap<>();
	private final Map<UUID, List<TestField>> referencedFieldsMap = new HashMap<>();
	private final List<TestObject> objectValueReferences = new ArrayList<>();
	private final TestData testData;

	/**
	 * Constructor
	 * @param testData
	 */
	public ReferenceResolver(TestData testData) {
		this.testData = testData;
	}

	/**
	 * Resolve all references
	 */
	public void resolveReferences() {
		// Collect all test objects that can be used
		for (final MethodInvocation methodInvocation : testData.getMethodInvocations()) {
			collectTestObjectsFromParameters(methodInvocation.getParameters());
			collectTestObjectsFromReturnValue(methodInvocation.getReturnValue());
		}

		// Iterate over all respective test objects recursively to find the test objects that should get fields of other test objects
		for (final MethodInvocation methodInvocation : testData.getMethodInvocations()) {
			processTestObjectsOfParameters(methodInvocation.getParameters());
			processTestObjectsOfReturnValue(methodInvocation.getReturnValue());
		}
	}

	/**
	 * @return a map that contains the test fields with an ID
	 */
	public Map<UUID, TestField> getFieldMap() {
		return fieldMap;
	}

	/**
	 * @return a list with test objects whose value is set by a generated value
	 */
	public List<TestObject> getObjectValueReferences() {
		return objectValueReferences;
	}

	/**
	 * @return a map with referenced fields where the key represents the ID of the referenced field
	 */
	public Map<UUID, List<TestField>> getReferencedFieldMap() {
		return referencedFieldsMap;
	}

	/**
	 * Collect the test objects from all parameters
	 * @param parameters
	 */
	private void collectTestObjectsFromParameters(List<Parameter> parameters) {
		if (parameters == null)
			return;

		for (final Parameter parameter : parameters)
			collectTestObjects(parameter.getTestObjects());
	}

	/**
	 * Collect the test objects from the return value parameters
	 * @param returnValue
	 */
	private void collectTestObjectsFromReturnValue(ReturnValue returnValue) {
		if (returnValue == null)
			return;

		collectTestObjects(returnValue.getTestObjects());
	}

	/**
	 * Collect the test objects from the given list of test objects
	 * @param testObjects
	 */
	private void collectTestObjects(List<TestObject> testObjects) {
		if (testObjects == null)
			return;

		for (final TestObject testObject : testObjects)
			if (testObject.getId() != null) {
				logger.trace("Found test object with ID '{}'", testObject.getId());
				objectMap.put(testObject.getId(), testObject);

				collectTestObjectsFromFields(testObject.getFields());
			}
	}

	/**
	 * Collect the test objects from the given list of fields
	 * @param fields
	 */
	private void collectTestObjectsFromFields(List<TestField> fields) {
		if (fields == null)
			return;

		for (final TestField field : fields)
			collectTestObjects(field.getTestObjects());
	}

	/**
	 * Process the test objects of all given parameters
	 * @param parameters
	 */
	private void processTestObjectsOfParameters(List<Parameter> parameters) {
		if (parameters == null)
			return;

		for (final Parameter parameter : parameters)
			processTestObjects(parameter.getTestObjects());
	}

	/**
	 * Process the test objects of the given return value
	 * @param returnValue
	 */
	private void processTestObjectsOfReturnValue(ReturnValue returnValue) {
		if (returnValue == null)
			return;

		processTestObjects(returnValue.getTestObjects());
	}

	/**
	 * Process the test objects that reference other test objects
	 * @param testObjects
	 */
	private void processTestObjects(List<TestObject> testObjects) {
		if (testObjects == null)
			return;

		for (final TestObject testObject : testObjects) {
			if (testObject.getReferencedFieldId() != null)
				objectValueReferences.add(testObject);

			for (final TestField testField : testObject.getFields()) {
				if (testField.getId() == null)
					continue;

				logger.trace("Track test field with ID '{}'", testObject.getId());
				fieldMap.put(testField.getId(), testField);
			}

			for (final TestField testField : testObject.getFields()) {
				if (testField.getReferencedId() == null)
					continue;

				logger.trace("Track field '{}' with reference to field with ID '{}'", testField.getName(), testField.getReferencedId());

				referencedFieldsMap.putIfAbsent(testField.getReferencedId(), new ArrayList<>());
				referencedFieldsMap.get(testField.getReferencedId()).add(testField);
			}

			if (testObject.getReferencedObjectId() != null) {
				final TestObject sourceTestObject = objectMap.get(testObject.getReferencedObjectId());

				if (sourceTestObject == null)
					throw new IllegalStateException(
							"The test object with ID '" + testObject.getReferencedObjectId() + "' could not be found!");
				else {
					logger.debug("Track fields of test object with ID '{}'", testObject.getReferencedObjectId());

					addFields(sourceTestObject, testObject);
				}
			}

			processTestObjectsOfFields(testObject.getFields());
		}
	}

	/**
	 * Process the test objects of the given fields
	 * @param testFields
	 */
	private void processTestObjectsOfFields(List<TestField> testFields) {
		if (testFields == null)
			return;

		for (final TestField testField : testFields)
			processTestObjects(testField.getTestObjects());
	}

	/**
	 * Add the fields of a test object to its reference
	 * @param from the original test object
	 * @param to the test object that should be filled
	 */
	private void addFields(TestObject from, TestObject to) {
		for (final TestField fromField : from.getFields()) {
			final Optional<TestField> optionalField = to.getFields().stream().filter(a -> a.getName().equals(fromField.getName()))
					.findFirst();

			if (optionalField.isEmpty()) {
				logger.debug("Add field '{}' to test object", fromField.getName());
				to.getFields().add(fromField);
			}
			else
				logger.debug("Skip field '{}'", fromField.getName());
		}
	}

}
