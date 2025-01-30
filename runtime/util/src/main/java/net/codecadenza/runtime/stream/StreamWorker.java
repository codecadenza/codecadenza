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
package net.codecadenza.runtime.stream;

import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import net.codecadenza.runtime.crypto.CipherFactory;

/**
 * <p>
 * Utility class to provide common operations upon streams that are used for the communication between client and server
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StreamWorker {
	public static final int DEFAULT_BUFFER_SIZE = 1024;

	private final int bufferSize;
	private final String secret;

	/**
	 * Constructor
	 * @param secret
	 */
	public StreamWorker(String secret) {
		this(secret, DEFAULT_BUFFER_SIZE);
	}

	/**
	 * Constructor to define the size of the internal buffer
	 * @param secret
	 * @param bufferSize
	 */
	public StreamWorker(String secret, int bufferSize) {
		this.secret = secret;
		this.bufferSize = bufferSize;
	}

	/**
	 * Write data from an input stream to an output stream
	 * @param in
	 * @param out
	 * @param encryptOutput
	 * @param decryptInput
	 * @throws Exception if the stream operation has failed
	 */
	public void writeToOutput(InputStream in, OutputStream out, boolean encryptOutput, boolean decryptInput) throws Exception {
		final var buf = new byte[bufferSize <= 0 ? DEFAULT_BUFFER_SIZE : bufferSize];
		int numRead = 0;

		if (encryptOutput) {
			final Cipher cypher = new CipherFactory(secret).getEncryptionCipher();

			out = new CipherOutputStream(out, cypher);
		}

		if (decryptInput) {
			final Cipher cypher = new CipherFactory(secret).getDecryptionCipher();

			in = new CipherInputStream(in, cypher);
		}

		while ((numRead = in.read(buf)) >= 0)
			out.write(buf, 0, numRead);
	}

	/**
	 * Read a serialized object from an input stream
	 * @param in
	 * @param decrypt
	 * @return the object
	 * @throws Exception if the object could not be created from the provided input stream
	 */
	public Object readObjectFromStream(InputStream in, boolean decrypt) throws Exception {
		if (decrypt) {
			final Cipher cypher = new CipherFactory(secret).getDecryptionCipher();

			try (final var ois = new ObjectInputStream(new CipherInputStream(in, cypher))) {
				return ois.readObject();
			}
		}

		try (final var ois = new ObjectInputStream(in)) {
			return ois.readObject();
		}
	}

	/**
	 * Write an object to an output stream
	 * @param object
	 * @param out
	 * @param encrypt
	 * @throws Exception if the object could not be serialized
	 */
	public void writeObjectToStream(Serializable object, OutputStream out, boolean encrypt) throws Exception {
		if (encrypt) {
			final Cipher cypher = new CipherFactory(secret).getEncryptionCipher();

			try (final var oos = new ObjectOutputStream(new CipherOutputStream(out, cypher))) {
				oos.writeObject(object);
			}
		}
		else
			try (final var oos = new ObjectOutputStream(out)) {
				oos.writeObject(object);
			}
	}

}
