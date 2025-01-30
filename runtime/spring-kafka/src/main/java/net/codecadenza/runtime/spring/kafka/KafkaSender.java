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
import java.util.concurrent.ExecutionException;
import net.codecadenza.runtime.avro.util.AvroObjectSerializationException;
import net.codecadenza.runtime.avro.util.AvroObjectSerializer;
import org.apache.avro.specific.SpecificRecordBase;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.header.internals.RecordHeader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

/**
 * <p>
 * Component that is used to send messages to a given Kafka topic
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Component
public class KafkaSender {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final KafkaTemplate<String, byte[]> kafkaTemplate;

	/**
	 * Constructor
	 * @param kafkaTemplate
	 */
	public KafkaSender(KafkaTemplate<String, byte[]> kafkaTemplate) {
		this.kafkaTemplate = kafkaTemplate;
	}

	/**
	 * Send the given object to the selected topic and add the provided correlation ID to the message header
	 * @param <T> the type of the object
	 * @param topic the name of the topic
	 * @param responseObject the Avro object that should be sent
	 * @param correlationId the unique ID of the original request message
	 * @throws KafkaSenderException if the response message could not be sent
	 * @throws AvroObjectSerializationException if the response object could not be serialized
	 */
	public <T extends SpecificRecordBase> void sendResponse(String topic, T responseObject, byte[] correlationId) {
		sendResponse(topic, responseObject, correlationId, null);
	}

	/**
	 * Send the given object to the selected topic partition and add the provided correlation ID to the message header
	 * @param <T> the type of the response object
	 * @param topic the name of the topic
	 * @param responseObject the Avro object that should be sent
	 * @param correlationId the unique ID of the original request message
	 * @param partitionId the ID of the partition that should be used. If the value is null a random partition will be selected by
	 *          the Kafka broker.
	 * @throws KafkaSenderException if the response message could not be sent
	 * @throws AvroObjectSerializationException if the response object could not be serialized
	 */
	public <T extends SpecificRecordBase> void sendResponse(String topic, T responseObject, byte[] correlationId,
			Integer partitionId) {
		if (logger.isDebugEnabled()) {
			if (partitionId == null)
				logger.debug("Send message to topic {}", topic);
			else
				logger.debug("Send message to partition {} of topic {}", partitionId, topic);
		}

		// Serialize the Avro response object
		final var responsePayload = new AvroObjectSerializer<>(responseObject).serialize();

		final var timeStamp = System.currentTimeMillis();

		// Create the producer record with the payload and the correlation ID header
		final var producerRecord = new ProducerRecord<String, byte[]>(topic, partitionId, timeStamp, null, responsePayload);
		producerRecord.headers().add(new RecordHeader(KafkaHeaderHelper.CORRELATION_ID_KEY, correlationId));

		// Send the message to the selected topic
		try {
			// Block until the response message has been sent!
			kafkaTemplate.send(producerRecord).get();

			if (logger.isDebugEnabled()) {
				if (partitionId == null)
					logger.debug("Message sent to topic {}", topic);
				else
					logger.debug("Message sent to partition {} of topic {}", partitionId, topic);
			}
		}
		catch (final InterruptedException e) {
			Thread.currentThread().interrupt();

			if (partitionId == null)
				logger.warn("Waiting for message to be sent to topic {} has been interrupted!", topic);
			else
				logger.warn("Waiting for message to be sent to partition {} of topic {} has been interrupted!", partitionId, topic);

			throw new KafkaSenderException(e);
		}
		catch (final ExecutionException e) {
			if (partitionId == null)
				logger.warn("Error while sending message to topic {}!", topic);
			else
				logger.warn("Error while sending message to partition {} of topic {}!", partitionId, topic);

			throw new KafkaSenderException(e);
		}
	}

}
