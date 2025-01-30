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
package net.codecadenza.runtime.spring.kafka;

import java.lang.invoke.MethodHandles;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.UUID;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.Header;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class that is used to read and to validate header elements of a Kafka message
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class KafkaHeaderHelper {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String SCHEMA_KEY = "SCHEMA";
	public static final String CORRELATION_ID_KEY = "CORRELATION_ID";
	public static final String PARTITION_ID_KEY = "PARTITION_ID";

	/**
	 * Private constructor
	 */
	private KafkaHeaderHelper() {

	}

	/**
	 * Get the schema from the header of the provided record
	 * @param consumerRecord the Kafka consumer record
	 * @return the schema
	 * @throws IllegalStateException if the schema could not be found
	 */
	public static String getRequestSchemaFromHeader(ConsumerRecord<String, byte[]> consumerRecord) {
		final var headerValue = getHeaderValue(SCHEMA_KEY, consumerRecord);
		final String schema = new String(headerValue, StandardCharsets.UTF_8);

		logger.debug("Extracted schema'{}' from header", schema);

		return schema;
	}

	/**
	 * Get the selected partition ID from the header of the provided record
	 * @param consumerRecord the Kafka consumer record
	 * @return the partition ID
	 * @throws IllegalStateException if the partition ID could not be found
	 */
	public static int getResponsePartitionIdFromHeader(ConsumerRecord<String, byte[]> consumerRecord) {
		final var headerValue = getHeaderValue(PARTITION_ID_KEY, consumerRecord);
		final int partitionId = new BigInteger(headerValue).intValue();

		logger.debug("Extracted partition ID '{}' from header", partitionId);

		return partitionId;
	}

	/**
	 * Get the correlation ID from the header of the provided record
	 * @param consumerRecord the Kafka consumer record
	 * @return the correlation ID
	 * @throws IllegalStateException if the correlation ID could not be found
	 * @throws IllegalArgumentException if the provided value could not be converted to a {@link UUID}
	 */
	public static byte[] getCorrelationIdFromHeader(ConsumerRecord<String, byte[]> consumerRecord) {
		final var headerValue = getHeaderValue(CORRELATION_ID_KEY, consumerRecord);
		final var correlationId = UUID.fromString(new String(headerValue, StandardCharsets.UTF_8));

		logger.debug("Extracted correlation ID '{}' from header", correlationId);

		return headerValue;
	}

	/**
	 * Get the value of the provided key from the record header
	 * @param key the key in the header to search for
	 * @param consumerRecord the Kafka consumer record that contains the actual header
	 * @return the header value as a byte array
	 * @throws IllegalStateException if the key either could not be found or if the corresponding byte array is empty
	 */
	public static byte[] getHeaderValue(String key, ConsumerRecord<String, byte[]> consumerRecord) {
		logger.debug("Extract element '{}' from header", key);

		final Header header = consumerRecord.headers().lastHeader(key);

		if (header == null)
			throw new IllegalStateException("No '" + key + "' element found in message header!");

		if (header.value() == null || header.value().length == 0)
			throw new IllegalStateException("No data found for element '" + key + "!");

		return header.value();
	}

}
