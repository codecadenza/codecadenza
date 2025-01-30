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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import org.apache.avro.io.EncoderFactory;
import org.apache.avro.specific.SpecificDatumWriter;
import org.apache.avro.specific.SpecificRecordBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class for serializing Avro objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the object that should be serialized
 */
public class AvroObjectSerializer<T extends SpecificRecordBase> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final T object;

	/**
	 * Constructor
	 * @param object the object that should be serialized
	 */
	public AvroObjectSerializer(T object) {
		this.object = object;
	}

	/**
	 * Serialize a given Avro object
	 * @return a byte array
	 * @throws AvroObjectSerializationException if the object could not be serialized
	 */
	public byte[] serialize() {
		try (var out = new ByteArrayOutputStream()) {
			logger.debug("Serialize object: {}", object);

			final var encoder = EncoderFactory.get().binaryEncoder(out, null);

			final var datumWriter = new SpecificDatumWriter<T>(object.getSchema());
			datumWriter.write(object, encoder);

			encoder.flush();

			return out.toByteArray();
		}
		catch (final IOException e) {
			final var errorMsg = "Failed to serialize Avro object with schema '" + object.getSchema() + "'!";

			logger.error(errorMsg, e);

			throw new AvroObjectSerializationException(errorMsg, e);
		}
	}

}
