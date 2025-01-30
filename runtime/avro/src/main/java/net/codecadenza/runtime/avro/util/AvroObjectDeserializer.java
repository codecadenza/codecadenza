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

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import org.apache.avro.Schema;
import org.apache.avro.io.DecoderFactory;
import org.apache.avro.specific.SpecificDatumReader;
import org.apache.avro.specific.SpecificRecordBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class for deserializing Avro objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the object that should be deserialized
 */
public class AvroObjectDeserializer<T extends SpecificRecordBase> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final Schema schema;

	/**
	 * Constructor
	 * @param schema the schema that should be used
	 */
	public AvroObjectDeserializer(Schema schema) {
		this.schema = schema;
	}

	/**
	 * Deserialize an Avro object from a byte array
	 * @param data the byte array that should be used for deserialization
	 * @return the deserialized object
	 * @throws AvroObjectDeserializationException if the object could not be deserialized
	 */
	public T deserialize(byte[] data) {
		logger.debug("Deserialize byte array with schema '{}'", schema);

		final var decoder = DecoderFactory.get().binaryDecoder(data, null);
		final var reader = new SpecificDatumReader<T>(schema);

		try {
			return reader.read(null, decoder);
		}
		catch (final IOException e) {
			final var errorMsg = "Failed to deserialize Avro object with schema '" + schema + "'!";

			logger.error(errorMsg, e);

			throw new AvroObjectDeserializationException(errorMsg, e);
		}
	}

}
