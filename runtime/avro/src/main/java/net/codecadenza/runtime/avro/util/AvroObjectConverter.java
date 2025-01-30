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
package net.codecadenza.runtime.avro.util;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import net.codecadenza.runtime.avro.types.Uuid;
import org.apache.avro.Schema;
import org.apache.avro.specific.SpecificRecordBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class that converts Avro objects into similar objects and vice versa by using reflection. The converter methods expect
 * that both classes have the same fields with the same name. The respective field types can be different. When converting an
 * object into its Avro counterpart the fields of type {@link Date} and {@link GregorianCalendar} are converted to
 * {@link Instant}. The converter also supports converting complex structures (e.g. an Avro object that contains a field of type
 * {@link SpecificRecordBase} or a list with {@link SpecificRecordBase} objects). If different enum fields must be converted the
 * literals of both enums must be equal!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AvroObjectConverter {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final String ENUM_METHOD_VALUE_OF = "valueOf";
	private static final String ENUM_LITERAL_METHOD_NAME = "name";

	/**
	 * Private constructor
	 */
	private AvroObjectConverter() {

	}

	/**
	 * Convert the given object into an Avro object
	 * @param <T> the desired Avro type
	 * @param object the source object
	 * @param avroType the Avro class
	 * @return the converted Avro object
	 * @throws AvroObjectConversionException if the conversion has failed
	 */
	@SuppressWarnings("unchecked")
	public static <T extends SpecificRecordBase> T toAvro(Object object, Class<T> avroType) {
		logger.debug("Converting object of type {} to {}", object.getClass().getName(), avroType.getName());

		try {
			final var avroRecord = avroType.getConstructor().newInstance();

			for (final var field : object.getClass().getDeclaredFields()) {
				final var avroField = avroRecord.getSchema().getField(field.getName());

				if (avroField == null)
					continue;

				final var sourceFieldType = field.getType();
				Object fieldValue = null;

				logger.debug("Processing field '{}' with type {}", field.getName(), sourceFieldType.getName());

				field.setAccessible(true);

				if (field.get(object) != null) {
					if (omitConversion(sourceFieldType))
						fieldValue = field.get(object);
					else if (sourceFieldType == GregorianCalendar.class)
						fieldValue = ((GregorianCalendar) field.get(object)).toInstant();
					else if (sourceFieldType == Date.class)
						fieldValue = ((Date) field.get(object)).toInstant();
					else if (sourceFieldType == LocalDateTime.class)
						fieldValue = ((LocalDateTime) field.get(object)).atZone(ZoneId.systemDefault()).toInstant();
					else if (sourceFieldType == UUID.class)
						fieldValue = UuidConverter.from((UUID) field.get(object));
					else if (sourceFieldType == char.class)
						fieldValue = String.valueOf(((char) field.get(object)));
					else if (sourceFieldType.isEnum()) {
						final Method valueOf = Class.forName(avroField.schema().getFullName()).getMethod(ENUM_METHOD_VALUE_OF, String.class);
						final Method name = sourceFieldType.getMethod(ENUM_LITERAL_METHOD_NAME);

						fieldValue = valueOf.invoke(null, name.invoke(field.get(object)));
					}
					else if (sourceFieldType == Collection.class || sourceFieldType == List.class) {
						final var objectList = (Collection<Object>) field.get(object);
						final var elementType = avroField.schema().getElementType();
						final var targetType = (Class<SpecificRecordBase>) Class.forName(elementType.getFullName());

						// It is assumed that a list contains complex objects!
						fieldValue = objectList.stream().map(item -> toAvro(item, targetType)).toList();
					}
					else {
						String typeName = avroField.schema().getFullName();

						if (avroField.schema().isUnion())
							typeName = avroField.schema().getTypes().stream()
									.filter(type -> type.getName().equals(sourceFieldType.getSimpleName())).map(Schema::getFullName).findFirst()
									.orElseThrow();

						fieldValue = toAvro(field.get(object), (Class<SpecificRecordBase>) Class.forName(typeName));
					}
				}

				avroRecord.put(avroField.name(), fieldValue);
			}

			return avroRecord;
		}
		catch (final Exception e) {
			final var errorMsg = "Error while converting an object of type " + object.getClass().getName() + "!";

			logger.error(errorMsg, e);

			throw new AvroObjectConversionException(errorMsg, e);
		}
	}

	/**
	 * Convert an object list into a list of Avro objects
	 * @param <T> the desired Avro type
	 * @param <S> the type of the objects that are contained in the list
	 * @param objectList the list that contains all objects that should be converted
	 * @param avroType the Avro class
	 * @return a list with the converted Avro objects
	 * @throws AvroObjectConversionException if the conversion has failed
	 */
	public static <T extends SpecificRecordBase, S> List<T> toAvroList(List<S> objectList, Class<T> avroType) {
		return objectList.stream().map(obj -> toAvro(obj, avroType)).toList();
	}

	/**
	 * Convert the given Avro object into an object of the desired type
	 * @param <T> the desired object type
	 * @param avroRecord the Avro object to be converted
	 * @param type the object class
	 * @return the target object
	 * @throws AvroObjectConversionException if the conversion has failed
	 */
	@SuppressWarnings("unchecked")
	public static <T> T toObject(SpecificRecordBase avroRecord, Class<T> type) {
		logger.debug("Converting Avro record {} to {}", avroRecord.getClass().getName(), type.getName());

		try {
			final T object = type.getConstructor().newInstance();

			for (final var avroField : avroRecord.getSchema().getFields()) {
				final var targetField = object.getClass().getDeclaredField(avroField.name());
				targetField.setAccessible(true);

				Object fieldValue = null;

				if (avroRecord.get(avroField.name()) != null) {
					final var sourceFieldType = avroRecord.get(avroField.name()).getClass();
					final var targetFieldType = targetField.getType();

					logger.debug("Processing field '{}' with type {}", avroField.name(), sourceFieldType.getName());

					if (sourceFieldType == Instant.class) {
						final var instant = (Instant) avroRecord.get(avroField.name());

						if (targetFieldType == GregorianCalendar.class)
							fieldValue = GregorianCalendar.from(ZonedDateTime.ofInstant(instant, ZoneId.systemDefault()));
						else if (targetFieldType == Date.class)
							fieldValue = Date.from(instant);
						else if (targetFieldType == LocalDate.class)
							fieldValue = LocalDate.ofInstant(instant, ZoneId.systemDefault());
						else if (targetFieldType == LocalDateTime.class)
							fieldValue = LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
						else
							fieldValue = avroRecord.get(avroField.name());
					}
					else if (sourceFieldType == String.class && targetFieldType == char.class) {
						final var string = (String) avroRecord.get(avroField.name());
						fieldValue = string.charAt(0);
					}
					else if (sourceFieldType == Uuid.class)
						fieldValue = UuidConverter.getUUID((Uuid) avroRecord.get(avroField.name()));
					else if (sourceFieldType.isEnum()) {
						final Method valueOf = targetFieldType.getMethod(ENUM_METHOD_VALUE_OF, String.class);
						final Method name = avroRecord.get(avroField.name()).getClass().getMethod(ENUM_LITERAL_METHOD_NAME);

						fieldValue = valueOf.invoke(null, name.invoke(avroRecord.get(avroField.name())));
					}
					else if (avroRecord.get(avroField.name()) instanceof List) {
						// It is assumed that a list contains complex objects
						final var paramType = (ParameterizedType) targetField.getGenericType();
						final var targetType = Class.forName(paramType.getActualTypeArguments()[0].getTypeName());
						final var avroList = (List<SpecificRecordBase>) avroRecord.get(avroField.name());

						fieldValue = avroList.stream().map(item -> toObject(item, targetType)).toList();
					}
					else if (avroRecord.get(avroField.name()) instanceof final SpecificRecordBase specificRecordBase)
						fieldValue = toObject(specificRecordBase, targetFieldType);
					else
						fieldValue = avroRecord.get(avroField.name());
				}

				targetField.set(object, fieldValue);
			}

			return object;
		}
		catch (final Exception e) {
			final var errorMsg = "Error while converting an Avro object of type " + avroRecord.getClass().getName() + "!";

			logger.error(errorMsg, e);

			throw new AvroObjectConversionException(errorMsg, e);
		}
	}

	/**
	 * Convert an Avro object list into a list of objects
	 * @param <T> the desired object type
	 * @param <S> the type of the objects that are contained in the list
	 * @param avroList the list that contains all Avro objects that should be converted
	 * @param objectType the object class
	 * @return a list with the converted objects
	 * @throws AvroObjectConversionException if the conversion has failed
	 */
	public static <T, S extends SpecificRecordBase> List<T> toObjectList(List<S> avroList, Class<T> objectType) {
		return avroList.stream().map(obj -> toObject(obj, objectType)).toList();
	}

	/**
	 * @param type the type that should be checked
	 * @return true if the provided type doesn't need to be converted
	 */
	private static boolean omitConversion(Class<?> type) {
		return type == int.class || type == long.class || type == float.class || type == double.class || type == boolean.class
				|| type == Boolean.class || type == Long.class || type == Integer.class || type == String.class || type == Double.class
				|| type == Float.class || type == LocalDate.class || type == BigDecimal.class;
	}

}
