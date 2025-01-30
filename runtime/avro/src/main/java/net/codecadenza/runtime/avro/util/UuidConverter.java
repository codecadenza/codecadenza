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

import java.nio.ByteBuffer;
import java.util.UUID;
import net.codecadenza.runtime.avro.types.Uuid;

/**
 * <p>
 * Utility class for converting fields of type {@link UUID} to {@link Uuid} and vice versa
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UuidConverter {
	/**
	 * Private constructor
	 */
	private UuidConverter() {
	}

	/**
	 * Create an Avro {@link Uuid} from a given {@link UUID}
	 * @param uuid the {@link UUID} to convert
	 * @return a {@link Uuid}
	 */
	public static Uuid from(final UUID uuid) {
		final ByteBuffer byteBuffer = ByteBuffer.wrap(new byte[16]);
		byteBuffer.putLong(uuid.getMostSignificantBits());
		byteBuffer.putLong(uuid.getLeastSignificantBits());

		return new Uuid(byteBuffer.array());
	}

	/**
	 * Convert an Avro {@link Uuid} to a {@link UUID}
	 * @param value the Avro {@link Uuid} to convert
	 * @return a {@link UUID}
	 */
	public static UUID getUUID(Uuid value) {
		final var mostSigBits = new byte[Long.BYTES];
		System.arraycopy(value.bytes(), 0, mostSigBits, 0, Long.BYTES);

		final var leastSigBits = new byte[Long.BYTES];
		System.arraycopy(value.bytes(), Long.BYTES, leastSigBits, 0, Long.BYTES);

		return new UUID(bytesToLong(mostSigBits), bytesToLong(leastSigBits));
	}

	/**
	 * Convert a byte array into a long value
	 * @param bytes a byte array with a long value
	 * @return a long value from the given byte array
	 */
	private static long bytesToLong(byte[] bytes) {
		final ByteBuffer byteBuffer = ByteBuffer.allocate(Long.BYTES);
		byteBuffer.put(bytes);
		byteBuffer.flip();

		return byteBuffer.getLong();
	}

}
