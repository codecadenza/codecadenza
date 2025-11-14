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
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.codecadenza.runtime.conversion.ValueConverter;
import net.codecadenza.runtime.ddt.model.TestField;
import net.codecadenza.runtime.ddt.model.TestObject;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * An utility class that converts a {@link TestObject} to an object of the specified type using introspection. This implementation
 * works for most POJOs where the target object's fields can be populated by matching field names and compatible types in the
 * {@link TestObject}. Note: This converter relies on public setters in the target object. It will throw an exception if a
 * required setter doesn't exist or if type conversion fails.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestObjectInitizalizer {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static Map<Class<?>, ValueConverter<?>> converters = new ConcurrentHashMap<>();
	private final TestDataProviderProperties properties;

	/**
	 * Constructor
	 * @param properties
	 */
	public TestObjectInitizalizer(TestDataProviderProperties properties) {
		this.properties = properties;
	}

	/**
	 * Initialize an object of the requested type by using the data provided by the {@link TestObject}
	 * @param <T> the type that should be returned
	 * @param testObject the test object
	 * @param objectType the {@link Class} of the requested type
	 * @return the new object
	 * @throws ObjectInitializationException if the object could not be initialized
	 */
	public <T> T initObject(TestObject testObject, Class<T> objectType) {
		final long start = System.nanoTime();

		try {
			if (testObject.getValue() != null) {
				logger.debug("Create value of type '{}'", objectType.getName());
				return getValueConverter(objectType).convertToValue(testObject.getValue());
			}

			logger.debug("Create object of type '{}'", objectType.getName());

			// Create an instance of the requested type using the default constructor
			final T object = objectType.getConstructor().newInstance();

			// Iterate over all fields and set them
			for (final TestField testField : testObject.getFields()) {
				final String fieldName = testField.getName();

				logger.debug("Initialize field '{}'", fieldName);

				if (testField.getValue() != null && testField.getTestObjects() != null && !testField.getTestObjects().isEmpty())
					throw new IllegalStateException("Invalid configuration for field '" + fieldName + "' detected!");

				initField(testField, object);
			}

			return object;
		}
		catch (final Exception e) {
			throw new ObjectInitializationException(objectType, e);
		}
		finally {
			if (logger.isDebugEnabled()) {
				final double duration = (System.nanoTime() - start) / 1E6;
				logger.debug("Initialization of '{}' object took {} milliseconds", objectType.getName(), duration);
			}
		}
	}

	/**
	 * Initialize a field of the given object
	 * @param testField the test field that contains the data for setting the object's field value
	 * @param object the object that should be initialized
	 * @throws FieldInitializationException if the field could not be initialized
	 */
	private void initField(TestField testField, Object object) {
		try {
			final Field field = object.getClass().getDeclaredField(testField.getName());

			if (testField.useDefaultValue()) {
				logger.debug("Set default value for field '{}'", field.getName());
				setFieldValue(field, object, getDefaultValue(field.getType()));
			}
			else {
				Object fieldValue = null;

				if (testField.getValue() != null)
					fieldValue = initSimpleField(testField, field);
				else if (Collection.class.isAssignableFrom(field.getType()))
					fieldValue = initCollection(testField, field);
				else if (field.getType().isArray())
					fieldValue = initArray(testField, field);
				else
					fieldValue = initObject(testField.getTestObjects().getFirst(), field.getType());

				setFieldValue(field, object, fieldValue);
			}
		}
		catch (final Exception e) {
			throw new FieldInitializationException("Error while initializing field '" + testField.getName() + "'", e);
		}
	}

	/**
	 * Initialize a collection of the given object
	 * @param testField the test field that contains the data that should be added to the collection
	 * @param field the field that should be initialized
	 * @return the initialized collection
	 * @throws FieldInitializationException if the collection could not be initialized
	 */
	@SuppressWarnings("unchecked")
	private Collection<?> initCollection(TestField testField, Field field) {
		try {
			final ParameterizedType paramType = (ParameterizedType) field.getGenericType();
			final Class<?> targetType = Class.forName(paramType.getActualTypeArguments()[0].getTypeName());
			Collection<Object> itemContainer;

			if (field.getType().isInterface()) {
				if (Set.class.isAssignableFrom(field.getType()))
					itemContainer = new HashSet<>();
				else
					itemContainer = new ArrayList<>();
			}
			else {
				// An implementation must provide a default constructor!
				itemContainer = (Collection<Object>) field.getType().getConstructor().newInstance();
			}

			for (final TestObject testObject : testField.getTestObjects()) {
				if (testObject.useDefaultValue())
					logger.debug("Skip item that has no values and no fields");
				else {
					Object itemValue;

					if (testObject.getValue() != null && !testObject.getValue().isEmpty()) {
						logger.debug("Add item '{}' of type '{}' to list", testObject.getValue(), targetType.getSimpleName());
						itemValue = getValueConverter(targetType).convertToValue(testObject.getValue());
					}
					else {
						logger.debug("Add item of type '{}' to list", targetType.getSimpleName());
						itemValue = initObject(testObject, targetType);
					}

					itemContainer.add(itemValue);
				}
			}

			return itemContainer;
		}
		catch (final Exception e) {
			throw new FieldInitializationException("Error while initializing collection '" + field.getName() + "'", e);
		}
	}

	/**
	 * Initialize an array of the given object
	 * @param testField the test field that contains the items that should be added to the array
	 * @param field the field that should be initialized
	 * @return the initialized array object
	 */
	private Object initArray(TestField testField, Field field) {
		final Class<?> itemType = field.getType().getComponentType();
		final Object itemArray = Array.newInstance(field.getType().getComponentType(), testField.getTestObjects().size());
		int index = 0;

		if (testField.getTestObjects() != null && !testField.getTestObjects().isEmpty()) {
			for (final TestObject testObject : testField.getTestObjects())
				if (testObject.useDefaultValue()) {
					logger.debug("Set default value at array index {}", index);
					Array.set(itemArray, index, getDefaultValue(itemType));
				}
				else {
					Object itemValue;

					if (testObject.getValue() != null && !testObject.getValue().isEmpty()) {
						logger.debug("Add item '{}' of type '{}' to array", testObject.getValue(), itemType.getSimpleName());
						itemValue = getValueConverter(itemType).convertToValue(testObject.getValue());
					}
					else {
						logger.debug("Add item of type '{}' to array", itemType.getSimpleName());
						itemValue = initObject(testObject, itemType);
					}

					Array.set(itemArray, index++, itemValue);
				}
		}

		return itemArray;
	}

	/**
	 * Initialize a field using the test data of the provided test field
	 * @param testField the test field that contains the data
	 * @param field the field that should be initialized
	 * @return the field value
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Object initSimpleField(TestField testField, Field field) {
		logger.debug("Set value '{}' for field '{}'", testField.getValue(), testField.getName());

		if (field.getType().isEnum())
			return Enum.valueOf((Class<Enum>) field.getType(), testField.getValue());

		return getValueConverter(field.getType()).convertToValue(testField.getValue());
	}

	/**
	 * Get a {@link ValueConverter} for the given type
	 * @param <T> the type that should be converted by the {@link ValueConverter}
	 * @param fieldType the class that represents the type
	 * @return the {@link ValueConverter}
	 */
	@SuppressWarnings({ "unchecked" })
	public <T> ValueConverter<T> getValueConverter(Class<T> fieldType) {
		return (ValueConverter<T>) converters.computeIfAbsent(fieldType,
				type -> new ValueConverter<>(properties.getDecimalFormat(), properties.getDecimalSeparator(),
						properties.getGroupingSeparator(), properties.getDateTimeFormat(), properties.getDateFormat(), fieldType));
	}

	/**
	 * Try to find a public setter method for the given field of the given object and invoke it
	 * @param field the field for which the setter should be used
	 * @param object the object whose field's setter is to be invoked
	 * @param value the value to set on the field
	 * @throws FieldInitializationException if the setter either could not be found or if it could not be invoked
	 */
	private static void setFieldValue(Field field, Object object, Object value) {
		try {
			final String fieldName = field.getName();
			final var setterName = "set" + fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
			final Method setterMethod = object.getClass().getMethod(setterName, field.getType());

			setterMethod.invoke(object, value);
		}
		catch (final Exception e) {
			throw new FieldInitializationException("Error while setting field '" + field.getName() + "'", e);
		}
	}

	/**
	 * Get the default value of the requested type
	 * @param <T> the type of the default value
	 * @param type the {@link Class} of the requested type
	 * @return the default value
	 */
	@SuppressWarnings("unchecked")
	public static <T> T getDefaultValue(Class<T> type) {
		if (type == int.class)
			return (T) Integer.valueOf(0);
		else if (type == double.class)
			return (T) Double.valueOf(0.0);
		else if (type == boolean.class)
			return (T) Boolean.valueOf(false);
		else if (type == float.class)
			return (T) Float.valueOf(0.0F);
		else if (type == long.class)
			return (T) Long.valueOf(0L);
		else if (type == short.class)
			return (T) Short.valueOf((short) 0);
		else if (type == byte.class)
			return (T) Byte.valueOf((byte) 0);
		else if (type == char.class)
			return (T) Character.valueOf('\0');
		else if (Set.class.isAssignableFrom(type))
			return (T) new HashSet<>();
		else if (Collection.class.isAssignableFrom(type))
			return (T) new ArrayList<>();
		else if (type.isArray())
			return (T) Array.newInstance(type, 0);

		return null;
	}

}
