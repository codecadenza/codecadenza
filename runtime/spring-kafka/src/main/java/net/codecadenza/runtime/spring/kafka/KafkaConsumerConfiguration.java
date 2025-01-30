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
import java.util.HashMap;
import java.util.Map;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.common.serialization.ByteArrayDeserializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;

/**
 * <p>
 * Class for the configuration of Kafka consumers in a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Configuration
public class KafkaConsumerConfiguration {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * @return the Kafka consumer properties
	 */
	@Bean
	@ConfigurationProperties(prefix = "codecadenza.application.kafka.consumer")
	public KafkaConsumerProperties getConsumerProperties() {
		return new KafkaConsumerProperties();
	}

	/**
	 * Create a container factory for Kafka listeners
	 * @return a {@link ConcurrentKafkaListenerContainerFactory}
	 */
	@Bean
	public ConcurrentKafkaListenerContainerFactory<String, byte[]> kafkaListenerContainerFactory() {
		final var factory = new ConcurrentKafkaListenerContainerFactory<String, byte[]>();
		factory.setConsumerFactory(createConsumerFactory());

		return factory;
	}

	/**
	 * Create a Kafka consumer factory
	 * @return a {@link ConsumerFactory}
	 */
	@Bean
	public ConsumerFactory<String, byte[]> createConsumerFactory() {
		return new DefaultKafkaConsumerFactory<>(getConfigurationProperties());
	}

	/**
	 * Get the consumer configuration
	 * @return a {@link Map} with the configuration
	 */
	protected Map<String, Object> getConfigurationProperties() {
		final var props = new HashMap<String, Object>();
		props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, getConsumerProperties().getAddress());
		props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
		props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, ByteArrayDeserializer.class);
		props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, getConsumerProperties().isEnableAutoCommit());

		if (getConsumerProperties().getAutoOffsetReset() != null)
			props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, getConsumerProperties().getAutoOffsetReset());

		logger.debug("Consumer configuration: {}", props);

		return props;
	}

}
