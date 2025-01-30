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
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.ByteArraySerializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;

/**
 * <p>
 * Class for the configuration of Kafka producers in a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Configuration
public class KafkaProducerConfiguration {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * @return the Kafka producer properties
	 */
	@Bean
	@ConfigurationProperties(prefix = "codecadenza.application.kafka.producer")
	public KafkaProducerProperties getProducerProperties() {
		return new KafkaProducerProperties();
	}

	/**
	 * Get a Kafka template for sending messages to a respective topic
	 * @return a new {@link KafkaTemplate}
	 */
	@Bean
	public KafkaTemplate<String, byte[]> getKafkaTemplate() {
		return new KafkaTemplate<>(getProducerFactory());
	}

	/**
	 * Get a producer factory
	 * @return a new {@link ProducerFactory}
	 */
	@Bean
	public ProducerFactory<String, byte[]> getProducerFactory() {
		return new DefaultKafkaProducerFactory<>(getConfigurationProperties());
	}

	/**
	 * Get the producer configuration
	 * @return a {@link Map} with the configuration
	 */
	protected Map<String, Object> getConfigurationProperties() {
		final var props = new HashMap<String, Object>();
		props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, getProducerProperties().getAddress());
		props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
		props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, ByteArraySerializer.class);

		logger.debug("Producer configuration: {}", props);

		return props;
	}

}
